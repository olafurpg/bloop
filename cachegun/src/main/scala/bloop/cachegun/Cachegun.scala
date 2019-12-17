package bloop.cachegun

import scala.collection.JavaConverters._
import com.martiansoftware.nailgun.NGContext
import java.{util => ju}
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import com.github.plokhotnyuk.jsoniter_scala.{core => jsoniter}
import java.net.URLClassLoader
import java.lang.ref.WeakReference
import java.lang.reflect.Method
import java.lang.reflect.InvocationTargetException
import com.martiansoftware.nailgun.NGServer
import com.martiansoftware.nailgun.NGSession
import scala.util.control.NonFatal
import scala.util.control.ControlThrowable
import scala.tools.ant.ClassloadVerify
import scala.util.Try
import sbt.internal.util.ConsoleLogger
import java.util.function.Supplier

object Cachegun {
  def nailMain(ctx: NGContext): Unit = {
    Server.out.println(ctx.getArgs.mkString("command: ", " ", ""))
    new Cachegun(ctx).main(ctx.getArgs().toList)
  }
  def version = "1.0.0"
  def help: String = {
    s"""cachegun ${version}
       |Usage: cachegun [OPTIONS] COMMAND
       |
       |Commands:
       |  help               Print this help message and exit.
       |  run <args_file>    Launch a process with the given argument file.
       |  clear-cache        Empty the classloader cache, resetting the process.
       |  exit               Shutdown this nailgun server.
       |
       |Arguments file:
       |The <args_file> argument to the 'run' command should be a UFT-8
       |encoded JSON configuration file with the following schema:
       |{
       |  "cachedClasspath": List[List[Path]],     // jar classpath entries to cache
       |  "uncachedClasspath": List[Path],         // classpath entries that are not cached 
       |  "mainClass": String,                     // the main class to run, cannot call System.exit
       |  "mainMethod": Option[String],            // optional main method name, defaults to "main"
       |  "arguments": List[String],               // the arguments to pass into the main class.
       |  "systemProperties": Map[String, String], // system properties to enable during the run
       |  "verbose": Option[Boolean]               // if true, print debugging information.
       |}
       |The final application runs in a classpath that is the combination of
       |'cachedClasspath' and 'uncachedClasspath'.
       |
       |The classpath entries in 'cachedClasspath' are cached in a layered
       |URLClassLoader. The first List[Path] element of 'cachedClasspath' becomes the root
       |classloader, the second List[Path] element of cachedClasspath is the
       |child of the root classloader, and so forth until the last
       |'cachedClasspath' element. The 'cachedClasspath' cannot contain jar entries,
       |directory must be added to the 'uncachedClasspath'.
       |
       |The classpath entries in 'uncachedClasspath' are not cached. This entry
       |may contain directory classpath entries along with jar classpath
       |entries that should not be cached.
       |""".stripMargin
  }
}

class Cachegun(ctx: NGContext) {

  def println(message: String): Unit = {
    ctx.out.println(message)
  }

  def main(args: List[String]): Unit = {
    args match {
      case Nil =>
        exit("error: missing argument <args_file>\n" + Cachegun.help)
      case "exit" :: Nil =>
        ctx.getNGServer().signalExit()
      case "clear-cache" :: Nil =>
        CachegunCache.clear()
      case ("help" | "-h" | "--help") :: Nil =>
        println(Cachegun.help)
        ctx.exit(0)
      case "run" :: "--help" :: Nil =>
        main(List("--help"))
      case "run" :: argsFile :: Nil =>
        run(newPath(argsFile))
      case args =>
        exit("error: invalid arguments\n" + Cachegun.help)
    }
  }

  def newPath(argsFile: String): Path = {
    val path = Paths.get(argsFile)
    if (path.isAbsolute()) path
    else Paths.get(ctx.getWorkingDirectory()).resolve(path)
  }

  def run(args: Path): Unit = {
    if (!Files.isRegularFile(args)) {
      exit(s"error: no such file '$args'")
    } else {
      val bytes = Files.readAllBytes(args)
      run(jsoniter.readFromArray[CachegunArguments](bytes))
    }
  }

  def run(args: CachegunArguments): Unit = {
    val logger = ConsoleLogger(ctx.out)
    val classloader = CachegunCache.getClassLoader(args)
    val mainClassName = args.mainClass
    val mainClass: Class[_] =
      try classloader.loadClass(mainClassName)
      catch {
        case _: ClassNotFoundException =>
          exit(s"error: class ${mainClassName} not found")
      }
    val mainMethodName = args.mainMethod.getOrElse("main")
    val mainMethod: Method =
      try {
        try mainClass.getMethod(mainMethodName, classOf[Array[String]], classOf[ClassLoader])
        catch {
          case _: ClassNotFoundException =>
            mainClass.getMethod(mainMethodName, classOf[Array[String]])
        }
      } catch {
        case _: ClassNotFoundException =>
          exit(
            s"error: method '${mainMethodName}(Array[String])' not found in class ${args.mainClass}"
          )
      }
    val mainArgs: Object = args.arguments.toArray
    val exitCode =
      try {
        args.withSystemProperties { () =>
          if (mainMethod.getParameterCount() == 1) mainMethod.invoke(null, mainArgs)
          else mainMethod.invoke(null, mainArgs, classloader)
          0
        }
      } catch {
        case ex: InvocationTargetException =>
          trimStacktrace(ex, classOf[NGSession].getName(), args.mainClass)
          val cause =
            if (ex.getCause() != null) ex.getCause()
            else ex
          fail(cause, args)
        case ex @ (NonFatal(_) | _: InterruptedException | _: ControlThrowable) =>
          trimStacktrace(ex, classOf[NGSession].getName(), args.mainClass)
          fail(ex, args)
      }
    ctx.exit(exitCode)
  }

  private def trimStacktrace(ex: Throwable, fromClassName: String, toClassName: String): Unit = {
    var cause = ex.getCause()
    while (cause != null) {
      val stackTrace = cause.getStackTrace()
      if (stackTrace.last.getClassName() == fromClassName) {
        var end = stackTrace.length - 1
        while (end >= 0 && stackTrace(end).getClassName() != toClassName) {
          end -= 1
        }
        cause.setStackTrace(stackTrace.slice(0, end + 1))
      }
      cause = cause.getCause()
    }
  }

  private val errorCode = 255
  private def fail(ex: Throwable, args: CachegunArguments): Int = {
    ex.printStackTrace(ctx.out)
    errorCode
  }
  private def exit(message: String): Nothing = {
    println(message)
    ctx.exit(errorCode)
    impossible()
  }
  private def impossible(): Nothing = {
    throw new Exception("this program should already have exited")
  }

}
