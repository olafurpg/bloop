package bloop.bsp

import scala.meta.jsonrpc.JsonRpcClient
import bloop.io.AbsolutePath
import scala.sys.process.ProcessLogger
import ch.epfl.scala.bsp.ShowMessageParams
import ch.epfl.scala.bsp.MessageType
import ch.epfl.scala.bsp.endpoints

object RefreshProjects {
  def run(workspaceDir: AbsolutePath, command: List[String], client: JsonRpcClient): Unit = {
    def showMessage(msg: String): Unit = {
      endpoints.Build.showMessage.notify(
        ShowMessageParams(MessageType.Error, None, None, msg)
      )(client)
      ()
    }
    val exit = scala.sys.process
      .Process(command, cwd = Some(workspaceDir.toFile))
      .!(ProcessLogger(showMessage, showMessage))
    if (exit != 0) {
      showMessage("failed to refresh projects")
    }
  }
}
