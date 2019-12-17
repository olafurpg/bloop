package bloop.bloopgun

import bloop.bloopgun.core.Shell
import bloop.bloopgun.core.DependencyResolution
import bloop.bloopgun.util.Environment
import bloop.bloopgun.util.Feedback
import bloop.bloopgun.core.AvailableAtPath
import bloop.bloopgun.core.AvailableWithCommand
import bloop.bloopgun.core.ListeningAndAvailableAt
import bloop.bloopgun.core.ServerStatus
import bloop.bloopgun.core.ResolvedAt
import bloopgun.internal.build.BloopgunInfo

import java.io.PrintStream
import java.io.InputStream
import java.util.concurrent.atomic.AtomicBoolean
import java.net.ConnectException
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.charset.StandardCharsets

import snailgun.TcpClient
import snailgun.protocol.Streams
import snailgun.logging.SnailgunLogger

import scala.collection.mutable

import scopt.OParser

import scala.sys.process.ProcessIO
import java.lang.ProcessBuilder.Redirect
import scala.util.Try
import java.net.URLDecoder
import java.net.URLClassLoader
import bloop.bloopgun.core.Shell.StatusCommand
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Promise
import snailgun.logging.Logger
import bloop.bloopgun.core.LocatedServer
import java.io.InputStreamReader
import java.io.BufferedReader

/**
 *
 * The main library entrypoint for bloopgun, the Bloop binary CLI.
 *
 * It uses Nailgun to communicate to the Bloop server and has support for:
 *
 *   1. Prints Bloop-specific feedback to the user to improve the CLI UX.
 *      * Print custom error if user types `bloop repl`.
 *      * Recommend using `bloop --nailgun-help` if `help` set to print CLI help.
 *   2. Starts the server if already not running or if `bloop server` is used.
 *   3. Invokes Ammonite if `bloop console` is used and repl kind is default or
 *      matches Ammonite.
 *
 * For the moment, this client doesn't do any kind of version handling so if
 * the client and the server have serious incompatibilities the communication
 * could crash. To avoid that users can forcefully exit the server and let the
 * client start a session. Exiting requires the intervention of the user
 * because it can affect already connected clients to the server instance.
 */
class BloopgunCli(
    bloopVersion: String,
    in: InputStream,
    out: PrintStream,
    err: PrintStream,
    shell: Shell
) {
  def run(args: Array[String]): Int = {
    var setServer: Boolean = false
    var setPort: Boolean = false
    var parsedServerOptionFlag: Option[String] = None
    var additionalCmdArgs: List[String] = Nil

    val cliParser = {
      val builder = OParser.builder[BloopgunParams]
      val nailgunServerOpt = builder
        .opt[String]("nailgun-server")
        .action((server, params) => { setServer = true; params.copy(nailgunServer = server) })
        .text("Specify the host name of the target Bloop server")
      val nailgunPortOpt = builder
        .opt[Int]("nailgun-port")
        .action((port, params) => { setPort = true; params.copy(nailgunPort = port) })
        .text("Specify the port of the target Bloop server")
      val helpOpt = builder
        .opt[Unit]('h', "help")
        .action((_, params) => params.copy(help = true))
        .text("Print help of the Bloop server")
      val nailgunShowVersionOpt = builder
        .opt[Unit]("nailgun-showversion")
        .action((_, params) => params.copy(nailgunShowVersion = true))
        .text("Print Bloopgun version before running command")
      val nailgunHelpOpt = builder
        .help("nailgun-help")
        .text("Print Bloopgun's help client information")
      val nailgunVerboseOpt = builder
        .opt[Unit]("nailgun-verbose")
        .action((_, params) => params.copy(verbose = true))
        .text("Enable verbosity of Bloopgun's logs")
      val cmdOpt = builder
        .arg[String]("<cmd>...")
        .optional()
        .unbounded()
        .action { (arg, params) =>
          additionalCmdArgs = additionalCmdArgs ++ List(arg)
          params
        }
        .text("The command and arguments for the Bloop server")

      // Here for backwards bincompat reasons with previous Python-based bloop client
      val serverCmd = builder
        .cmd("server")
        .action((_, params) => params.copy(server = true))
        .children(
          builder
            .arg[Int]("<nailgun-port>")
            .optional()
            .maxOccurs(1)
            .action {
              case (arg, params) =>
                params.copy(serverConfig = params.serverConfig.copy(port = Some(arg)))
            },
          builder
            .opt[String]("server-location")
            .action {
              case (arg, params) =>
                val path = Some(Paths.get(arg))
                params.copy(serverConfig = params.serverConfig.copy(serverLocation = path))
            },
          builder
            .opt[Unit]("fire-and-forget")
            .action {
              case (_, params) =>
                params.copy(serverConfig = params.serverConfig.copy(fireAndForget = true))
            },
          builder
            .arg[String]("<server-args>...")
            .optional()
            .unbounded()
            .action(
              (arg, params) =>
                params.copy(
                  serverConfig = params.serverConfig.copy(serverArgs = params.args ++ List(arg))
                )
            )
            .text("The command and arguments for the Bloop server")
        )
      OParser
        .sequence(
          builder.programName("bloopgun"),
          builder.head("bloopgun", Defaults.Version),
          builder.note(
            s"""Bloopgun is a bloop CLI client that communicates with the Bloop server via Nailgun.
               |""".stripMargin
          ),
          nailgunServerOpt,
          nailgunPortOpt,
          helpOpt,
          nailgunHelpOpt,
          nailgunShowVersionOpt,
          nailgunVerboseOpt,
          cmdOpt,
          serverCmd
        )
    }

    import scopt.{OParserSetup, DefaultOParserSetup}
    val setup: OParserSetup = new DefaultOParserSetup {
      override def errorOnUnknownArgument: Boolean = false
      override def reportWarning(msg: String): Unit = {
        if (msg.startsWith("Unknown option ")) {
          additionalCmdArgs = additionalCmdArgs ++ List(msg.stripPrefix("Unknown option "))
        } else {
          err.println(msg)
        }
      }
    }

    val (cliArgsToParse, extraArgsForServer) =
      if (args.contains("--")) args.span(_ == "--") else (args, Array.empty[String])
    OParser.parse(cliParser, cliArgsToParse, BloopgunParams(), setup) match {
      case None => 1
      case Some(params0) =>
        val params = params0.copy(args = additionalCmdArgs ++ extraArgsForServer)
        val logger = new SnailgunLogger("log", out, isVerbose = params.verbose)
        if (params.nailgunShowVersion)
          logger.info(s"Nailgun protocol v${Defaults.Version}")

        if (params.server) {
          val config = params.serverConfig
          shell.connectToBloopPort(config, logger) match {
            case true => logger.info(s"Server is already running at $config, exiting!"); 0
            case _ if config.fireAndForget =>
              fireCommand("about", Array.empty, params, config, logger)
            case _ =>
              // Fire server and wait until it exits, this is the default `bloop server` mode
              fireServer(FireAndWaitForExit(shell), params, config, bloopVersion, logger) match {
                case Some((cmd, status)) =>
                  logger.info(s"Command '$cmd' finished with ${status.code}, bye!"); 0
                case None =>
                  logger.error("Failed to locate server, aborting start of server!"); 1
              }
          }
        } else {
          val config = ServerConfig(
            if (setServer) Some(params.nailgunServer)
            else Defaults.env.get("BLOOP_SERVER"),
            if (setPort) Some(params.nailgunPort)
            else Defaults.env.get("BLOOP_PORT").map(_.toInt)
          )

          params.args match {
            case Nil if params.help => fireCommand("help", Array.empty, params, config, logger)
            case Nil => logger.error("Missing CLI command for Bloop server!"); 1
            case cmd :: cmdArgs => fireCommand(cmd, cmdArgs.toArray, params, config, logger)
          }
        }
    }
  }

  // Disable interactivity of nailgun, let's just assume all clients are not interactive
  private val isInteractive = false
  private def fireCommand(
      cmd: String,
      initialCmdArgs: Array[String],
      params: BloopgunParams,
      config: ServerConfig,
      logger: SnailgunLogger
  ): Int = {
    var consoleCmdOutFile: Path = null
    val cmdArgs = {
      if (cmd != "console") initialCmdArgs
      else {
        val outFileIndex = initialCmdArgs.indexOf("--out-file")
        if (outFileIndex >= 0) {
          if (outFileIndex + 1 < initialCmdArgs.length) {
            consoleCmdOutFile = Paths.get(initialCmdArgs(outFileIndex + 1))
          }
          initialCmdArgs
        } else {
          consoleCmdOutFile = Files.createTempFile("ammonite-cmd", "out")
          initialCmdArgs ++ Array("--out-file", consoleCmdOutFile.toAbsolutePath.toString)
        }
      }
    }

    import Defaults.env
    import Environment.cwd
    val streams = Streams(in, out, err)
    val client = TcpClient(config.userOrDefaultHost, config.userOrDefaultPort)
    val noCancel = new AtomicBoolean(false)

    def executeCmd(client: TcpClient) = {
      val code = client.run(cmd, cmdArgs, cwd, env, streams, logger, noCancel, isInteractive)
      logger.debug(s"Return code is $code")
      runAfterCommand(cmd, cmdArgs, consoleCmdOutFile, code, logger)
      code
    }

    try executeCmd(client)
    catch {
      case _: ConnectException =>
        cmd match {
          case "ng-stop" | "exit" =>
            logger.info(s"No server running at ${config}, skipping 'exit' or 'ng-stop' command!")
            0

          case _ =>
            // Attempt to start server here, move launcher logic
            logger.info(s"No server running at ${config}, let's fire one...")
            fireServer(FireInBackground(shell), params, config, bloopVersion, logger) match {
              case Some(true) => executeCmd(client)
              case _ => logger.error(Feedback.serverCouldNotBeStarted(config)); 1
            }
        }

    } finally {
      if (consoleCmdOutFile != null) {
        Files.deleteIfExists(consoleCmdOutFile)
      }
      ()
    }
  }

  private def fireServer(
      mode: FireMode,
      params: BloopgunParams,
      config: ServerConfig,
      bloopVersion: String,
      logger: SnailgunLogger
  ): Option[mode.Out] = {
    ServerStatus
      .resolveServer(bloopVersion, logger)
      .map(mode.fire(_, config, logger))
  }

  private def runAfterCommand(
      cmd: String,
      cmdArgs: Array[String],
      cmdOutFile: Path,
      exitCode: Int,
      logger: SnailgunLogger
  ): Int = {
    if (exitCode == 0 && cmdArgs.contains("--help")) {
      logger.info("Type `--nailgun-help` for help on the Nailgun CLI tool.")
    }

    if (exitCode != 0 && cmd == "repl") {
      // Assumes `repl` is not a valid Bloop command, provides user hint to use console instead
      logger.error(s"Command `repl` doesn't exist in Bloop, did you mean `console`?")
      1
    } else {
      val requiresAmmonite = exitCode == 0 && cmdOutFile != null && cmd == "console"
      if (!requiresAmmonite) 0
      else {
        def processAmmoniteOutFile: Int = {
          val contents = new String(Files.readAllBytes(cmdOutFile), StandardCharsets.UTF_8)
          val replCoursierCmd = contents.trim.split(" ")
          if (replCoursierCmd.length == 0) {
            logger.error("Unexpected empty REPL command after running console in Bloop server!")
            1
          } else {
            val status = shell.runCommandInheritingIO(
              replCoursierCmd.toList,
              Environment.cwd,
              None,
              attachTerminal = true
            )

            status.code
          }
        }

        val replKindFlagIndex = cmdArgs.indexOf("--repl")
        if (replKindFlagIndex < 0) processAmmoniteOutFile
        val replKind = cmdArgs(replKindFlagIndex + 1)
        if (replKind != "ammonite") 0
        else processAmmoniteOutFile
      }
    }
  }
}

object Bloopgun
    extends BloopgunCli(BloopgunInfo.version, System.in, System.out, System.err, Shell.default) {
  def main(args: Array[String]): Unit = System.exit(run(args))
}
