package bloop.bloopgun

import bloop.bloopgun.core.Shell.StatusCommand
import bloop.bloopgun.core.LocatedServer
import snailgun.logging.Logger
import bloop.bloopgun.util.Feedback
import java.nio.file.Path
import java.nio.file.Files
import bloop.bloopgun.core.Shell
import bloop.bloopgun.util.Environment
import bloop.bloopgun.core.AvailableAtPath
import bloop.bloopgun.core.AvailableWithCommand
import bloop.bloopgun.core.ResolvedAt

object StartServer {
  type ExitServerStatus = (List[String], StatusCommand)

  /**
   * Start a server with the executable present in `binary`.
   *
   * This operation can take a while in some operating systems (most notably
   * Windows, where antivirus programs slow down the execution of startup).
   * Therefore, call sites usually start the server in background and wait on
   * the value of `exitPromise`. The promise will not be completed so long as
   * the server is running and at the end it will contain either a success or
   * failure.
   *
   * @param serverToRun The server that we want to run.
   * @param config The configuration for the server we want to launch.
   * @param redirectOutErr Whether we should forward logs from the system process
   *        to the inherited streams. This is typically used when `bloop server` runs.
   */
  def startServer(
      serverToRun: LocatedServer,
      config: ServerConfig,
      redirectOutErr: Boolean,
      logger: Logger,
      shell: Shell
  ): ExitServerStatus = {
    // Always keep a server running in the background by making it a daemon thread
    val serverArgs = {
      if (!config.serverArgs.isEmpty) config.serverArgs
      else {
        (config.host, config.port) match {
          case (Some(host), Some(port)) => List(host, port.toString)
          case (None, Some(port)) => List(port.toString)
          case (None, None) => Nil
          case (Some(host), None) =>
            logger.warn(Feedback.unexpectedServerArgsSyntax(host))
            Nil
        }
      }
    }

    def cmdWithArgs(
        found: LocatedServer,
        extraJvmOpts: List[String]
    ): (List[String], Boolean) = {
      var usedExtraJvmOpts = false
      def finalJvmOpts(jvmOpts: List[String]): List[String] = {
        if (extraJvmOpts.forall(opt => jvmOpts.contains(opt))) jvmOpts
        else {
          usedExtraJvmOpts = true
          jvmOpts ++ extraJvmOpts
        }
      }

      def deriveCmdForPath(path: Path): List[String] = {
        val fullPath = path.toAbsolutePath().toString()
        if (Files.isExecutable(path.toRealPath())) {
          val jargs = finalJvmOpts(Nil).map(arg => s"-J$arg")
          val cmd = fullPath :: (serverArgs ++ jargs)
          shell.deriveCommandForPlatform(cmd, attachTerminal = false)
        } else {
          val jvmOpts = Environment.detectJvmOptionsForServer(found, serverArgs, logger)
          List("java") ++ finalJvmOpts(jvmOpts) ++ List("-jar", fullPath) ++ serverArgs
        }
      }

      found match {
        case AvailableAtPath(path) => deriveCmdForPath(path) -> usedExtraJvmOpts
        case AvailableWithCommand(cmd) =>
          val jargs = finalJvmOpts(Nil).map(arg => s"-J$arg")
          (cmd ++ serverArgs ++ jargs) -> usedExtraJvmOpts
        case ResolvedAt(classpath) =>
          val delimiter = java.io.File.pathSeparator
          val jvmOpts = Environment.detectJvmOptionsForServer(found, serverArgs, logger)
          val stringClasspath = classpath.map(_.normalize.toAbsolutePath).mkString(delimiter)
          val cmd = List("java") ++ finalJvmOpts(jvmOpts) ++ List(
            "-classpath",
            stringClasspath,
            config.fullyQualifiedName
          ) ++ serverArgs
          cmd -> usedExtraJvmOpts
      }
    }

    /*
     * The process to launch the server is as follows:
     *
     *   - First we run the server with some JVM options that the launcher knows
     *     speeds up the execution of the server. This execution can fail because
     *     the JVM options are specific to the JVM.
     *   - When the first run fails, we attempt to run the server with no
     *     additional JVM options and report that to the user.
     */

    def sysproc(cmd: List[String]): StatusCommand = {
      logger.info(Feedback.startingServer(config.name, config))
      logger.info(s"-> Command: $cmd")

      // Don't use `shell.runCommand` b/c it uses an executor that gets shut down upon `System.exit`
      val process = new ProcessBuilder()
        .command(cmd.toArray: _*)
        .directory(Environment.cwd.toFile)

      if (redirectOutErr) {
        process.redirectOutput()
        process.redirectError()
      }

      process.redirectErrorStream()
      val started = process.start()
      val is = started.getInputStream()
      val code = started.waitFor()
      val output = scala.io.Source.fromInputStream(is).mkString
      StatusCommand(code, output)
    }

    val start = System.currentTimeMillis()

    // Run bloop server with special performance-sensitive JVM options
    val performanceSensitiveOpts = Environment.PerformanceSensitiveOptsForBloop
    val (firstCmd, usedExtraJvmOpts) = cmdWithArgs(serverToRun, performanceSensitiveOpts)
    val firstStatus = sysproc(firstCmd)

    val end = System.currentTimeMillis()
    val elapsedFirstCmd = end - start

    // Don't run server twice, exit was successful or user args already contain performance-sensitive args
    if (firstStatus.code == 0 || !usedExtraJvmOpts) firstCmd -> firstStatus
    else {
      val isExitRelatedToPerformanceSensitiveOpts = {
        performanceSensitiveOpts.exists(firstStatus.output.contains(_)) ||
        // Output can be empty when `bloop server` bc it redirects streams; use timing as proxy
        elapsedFirstCmd <= 8000 // Use large number because launching JVMs on Windows is expensive
      }

      if (!isExitRelatedToPerformanceSensitiveOpts) firstCmd -> firstStatus
      else {
        val (secondCmd, _) = cmdWithArgs(serverToRun, Nil)
        secondCmd -> sysproc(secondCmd)
      }
    }
  }
}
