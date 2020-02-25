package bloop.bsp

import bloop.logging.Logger
import bloop.io.AbsolutePath
import scala.sys.process.ProcessLogger
import ch.epfl.scala.bsp.ShowMessageParams
import ch.epfl.scala.bsp.MessageType
import ch.epfl.scala.bsp.endpoints

object RefreshProjects {
  def run(workspaceDir: AbsolutePath, command: List[String], logger: Logger): Unit = {
    val exit = scala.sys.process
      .Process(command, cwd = Some(workspaceDir.toFile))
      .!(ProcessLogger(logger.info(_), logger.info(_)))
    if (exit != 0) {
      logger.error(
        s"Refresh projects command '${command.mkString(" ")}' failed with exit code '$exit'"
      )
    }
  }
}
