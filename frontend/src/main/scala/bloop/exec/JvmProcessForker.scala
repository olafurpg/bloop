package bloop.exec

import bloop.cli.CommonOptions
import bloop.data.JdkConfig
import bloop.engine.tasks.RunMode
import bloop.io.{AbsolutePath, Paths, RelativePath}
import bloop.logging.{DebugFilter, Logger}
import bloop.util.CrossPlatform
import java.io.File.pathSeparator
import java.net.URLClassLoader
import java.nio.file.Files
import java.util.jar.{Attributes, JarEntry, JarOutputStream, Manifest}
import monix.eval.Task
import scala.util.{Failure, Success, Try}
import java.nio.charset.StandardCharsets
import java.nio.file.StandardCopyOption

/**
 * Collects configuration to start a new program in a new process
 *
 * The name comes from a similar utility https://github.com/sshtools/forker.
 */
trait JvmProcessForker {

  /**
   * Creates a `ClassLoader` from the classpath of this `ForkProcess`
   *
   * @param parent A parent classloader
   * @return A classloader constructed from the classpath of this `ForkProcess`
   */
  def newClassLoader(parent: Option[ClassLoader]): ClassLoader

  /**
   * Run the main function in class `className`, passing it `args`
   *
   * @param cwd            The directory in which to start the forked JVM
   * @param mainClass      The fully qualified name of the class to run
   * @param args0          The arguments to pass to the main method. If they contain args
   *                       starting with `-J`, they will be interpreted as jvm options.
   * @param skipJargs      Skip the interpretation of `-J` options in `args`.
   * @param logger         Where to log the messages from execution
   * @param opts           The options to run the program with
   * @param extraClasspath Paths to append to the classpath before running
   * @return 0 if the execution exited successfully, a non-zero number otherwise
   *
   *
   */
  final def runMain(
      cwd: AbsolutePath,
      mainClass: String,
      args0: Array[String],
      skipJargs: Boolean,
      logger: Logger,
      opts: CommonOptions,
      extraClasspath: Array[AbsolutePath] = Array.empty
  ): Task[Int] = {
    val (userJvmOptions, userArgs) =
      if (skipJargs) (Array.empty[String], args0)
      else args0.partition(_.startsWith("-J"))

    runMain(cwd, mainClass, userArgs, userJvmOptions, logger, opts, extraClasspath)
  }

  def runMain(
      cwd: AbsolutePath,
      mainClass: String,
      args: Array[String],
      jargs: Array[String],
      logger: Logger,
      opts: CommonOptions,
      extraClasspath: Array[AbsolutePath]
  ): Task[Int]
}

object JvmProcessForker {
  // Windows max cmd line length is 32767, which seems to be the least of the common shells.
  val classpathCharLimit: Int = 30000

  def apply(config: JdkConfig, classpath: Array[AbsolutePath]): JvmProcessForker =
    new JvmForker(config, classpath)

  def apply(
      config: JdkConfig,
      classpath: Array[AbsolutePath],
      mode: RunMode
  ): JvmProcessForker = {
    mode match {
      case RunMode.Normal => new JvmForker(config, classpath)
      case RunMode.Debug => new JvmDebuggingForker(new JvmForker(config, classpath))
    }
  }
}

/**
 * @param config The configuration describing how to start the new JVM
 * @param classpath Classpath with which the code should be executed
 */
final class JvmForker(config: JdkConfig, classpath: Array[AbsolutePath]) extends JvmProcessForker {

  /**
   * Creates a `ClassLoader` from the classpath of this `ForkProcess`
   *
   * @param parent A parent classloader
   * @return A classloader constructed from the classpath of this `ForkProcess`
   */
  override def newClassLoader(parent: Option[ClassLoader]): ClassLoader = {
    val classpathEntries = classpath.map(_.underlying.toUri.toURL)
    new URLClassLoader(classpathEntries, parent.orNull)
  }

  override def runMain(
      cwd: AbsolutePath,
      mainClass: String,
      args: Array[String],
      jargs: Array[String],
      logger: Logger,
      opts: CommonOptions,
      extraClasspath: Array[AbsolutePath]
  ): Task[Int] = {
    val jvmOptions = jargs.map(_.stripPrefix("-J")) ++ config.javaOptions
    val fullClasspath = classpath ++ extraClasspath
    for {
      fullClasspathStr <- Task.fromTry(ensureClasspathLength(fullClasspath))
      java <- Task.fromTry(javaExecutable)
      argsFile = {
        val tmp = Files.createTempFile("bloop", "args.txt");
        val argsText = new StringBuilder()
          .append(fullClasspathStr)
          .append("\n")
          .append(mainClass)
        args.foreach { arg =>
          argsText.append("\n").append(arg)
        }
        Files.write(tmp, argsText.toString.getBytes(StandardCharsets.UTF_8))
      }
      forkLauncher = classOf[ForkLauncher]
      forkLauncherClasspath = {
        val tmp = Files.createTempDirectory("bloop")
        val out = tmp.resolve("bloop").resolve("exec").resolve(s"${forkLauncher.getSimpleName()}.class")
        val classfile = forkLauncher.getCanonicalName().replace('.', '/') + ".class"
        val in = this.getClass.getClassLoader.getResourceAsStream(classfile)
        Files.createDirectories(out.getParent)
        Files.copy(in, out, StandardCopyOption.REPLACE_EXISTING)
        tmp.toString()
      }
      classpathOption = "-cp" :: forkLauncherClasspath :: Nil
      appOptions = forkLauncher.getCanonicalName :: argsFile.toString :: Nil
      cmd = java.syntax :: jvmOptions.toList ::: classpathOption ::: appOptions

      logTask <- if (logger.isVerbose) {
        val debugOptions =
          s"""
             |Fork options:
             |   command      = '${cmd.mkString(" ")}'
             |   cwd          = '$cwd'""".stripMargin
        Task(logger.debug(debugOptions)(DebugFilter.All))
      } else {
        Task.unit
      }
      res <- Forker.run(cwd, cmd, logger, opts)
    } yield {
      res
    }
  }

  private def ensureClasspathLength(classpath: Array[AbsolutePath]): Try[String] = {
    val fullClasspathStr = classpath.map(_.syntax).mkString(pathSeparator)

    if (false && fullClasspathStr.length > JvmProcessForker.classpathCharLimit)
      createTempDependenciesJar(classpath).map(_.syntax)
    else
      Try(fullClasspathStr)
  }

  private def createTempDependenciesJar(classpath: Array[AbsolutePath]): Try[AbsolutePath] = Try {
    val prefix = "jvm-forker-deps"
    val depsJar = Files.createTempFile(prefix, ".jar").toAbsolutePath

    // We need to manually collect .class files from directories into the jar since
    // manifest files only work with .jar files for classpath
    val directories = Nil
    val files = classpath

    val (globbedJars, globbedClasses) = directories
      .flatMap(expandDirectory)
      .partition(_._2.syntax.endsWith(".jar"))

    val classpathStr = (files ++ globbedJars.map(_._2)).map(_.syntax).mkString(" ")

    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(Attributes.Name.CLASS_PATH, classpathStr)

    val jos = new JarOutputStream(Files.newOutputStream(depsJar), manifest)
    globbedClasses.foreach {
      case (relpath, abspath) =>
        jos.putNextEntry(new JarEntry(relpath.syntax))
        jos.write(abspath.readAllBytes)
        jos.closeEntry()
    }
    jos.close()

    AbsolutePath(depsJar)
  }

  private def expandDirectory(root: AbsolutePath): List[(RelativePath, AbsolutePath)] =
    Paths
      .pathFilesUnder(root, "glob:**.{jar,class}")
      .map(path => (path.toRelative(root), path))

  private def javaExecutable: Try[AbsolutePath] = {
    val javaPath = config.javaHome.resolve("bin").resolve("java")
    if (javaPath.exists) Success(javaPath)
    else {
      val javaExePath = config.javaHome.resolve("bin").resolve("java.exe")
      if (CrossPlatform.isWindows && javaExePath.exists) Success(javaExePath)
      else Failure(new IllegalStateException(s"Missing java executable at $javaPath!"))
    }
  }
}

final class JvmDebuggingForker(underlying: JvmProcessForker) extends JvmProcessForker {

  override def newClassLoader(parent: Option[ClassLoader]): ClassLoader =
    underlying.newClassLoader(parent)

  override def runMain(
      cwd: AbsolutePath,
      mainClass: String,
      args: Array[String],
      jargs0: Array[String],
      logger: Logger,
      opts: CommonOptions,
      extraClasspath: Array[AbsolutePath]
  ): Task[Int] = {
    val jargs = jargs0 :+ enableDebugInterface
    underlying.runMain(cwd, mainClass, args, jargs, logger, opts, extraClasspath)
  }

  private def enableDebugInterface: String = {
    s"-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n"
  }
}
