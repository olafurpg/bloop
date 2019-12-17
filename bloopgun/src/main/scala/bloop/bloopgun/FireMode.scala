package bloop.bloopgun

import bloop.bloopgun.core.LocatedServer
import snailgun.logging.SnailgunLogger
import scala.concurrent.Promise
import bloop.bloopgun.core.Shell

sealed trait FireMode {
  type Out

  def fire(
      found: LocatedServer,
      config: ServerConfig,
      logger: SnailgunLogger
  ): Out
}

case class FireAndWaitForExit(shell: Shell) extends FireMode {
  type Out = StartServer.ExitServerStatus
  def fire(
      found: LocatedServer,
      config: ServerConfig,
      logger: SnailgunLogger
  ): Out = {
    StartServer.startServer(found, config, true, logger, shell)
  }
}

case class FireInBackground(shell: Shell) extends FireMode {
  type Out = Boolean
  def fire(
      found: LocatedServer,
      config: ServerConfig,
      logger: SnailgunLogger
  ): Out = {
    val exitPromise = Promise[StartServer.ExitServerStatus]()
    val serverThread = shell.startThread(s"${config.name}-server-background", true) {
      exitPromise.success(StartServer.startServer(found, config, false, logger, shell))
    }

    val port = config.userOrDefaultPort
    var isConnected: Boolean = false

    /*
     * Retry connecting to the server for a bunch of times until we get some
     * response the server is up and running. The server can be slow to start
     * up, particularly in Windows systems where antivirus usually have a
     * high tax on application startup times. This operation usually takes
     * around a second on Linux and Unix systems.
     */
    while (!exitPromise.isCompleted && !isConnected) {
      val waitMs = 125.toLong
      Thread.sleep(waitMs)
      logger.info(s"Sleeping for ${waitMs}ms until we connect to server port $port")
      isConnected = shell.connectToBloopPort(config, logger)
    }

    // Either listening exists or status promise is completed
    if (isConnected) true
    else {
      exitPromise.future.value match {
        case Some(scala.util.Success((runCmd, status))) =>
          val cmd = runCmd.mkString(" ")
          logger.error(s"Command '$cmd' finished with ${status.code}: '${status.output}'")
        case Some(scala.util.Failure(t)) =>
          logger.error(s"Unexpected exception thrown by thread starting server: '$t'")
        case unexpected =>
          logger.error(s"Unexpected error when starting server: $unexpected")
      }

      // Required to protect ourselves from other clients racing to start the server
      logger.info("Attempt connection a last time before giving up...")
      Thread.sleep(500.toLong)
      isConnected = shell.connectToBloopPort(config, logger)
      isConnected
    }
  }
}
