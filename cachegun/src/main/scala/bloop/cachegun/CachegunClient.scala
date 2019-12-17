package bloop.cachegun

import java.nio.file.Files
import java.io.OutputStream
import snailgun.logging.SnailgunLogger
import java.io.PrintStream
import java.io.InputStream
import snailgun.protocol.Streams
import snailgun.TcpClient
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean
import java.net.ConnectException

class CachegunClient(
    environmentVariables: Map[String, String],
    cwd: Path,
    logger: SnailgunLogger,
    cachegunIn: InputStream,
    cachegunOut: OutputStream,
    cachegunErr: OutputStream,
    startCachegunServer: () => Boolean
) {
  def exit(): Unit = {
    val _ = run(Array("exit"))
  }
  def run(args: CachegunArguments): Int = {
    val argsFile = Files.createTempFile("bloop", "args.json")
    val command = Array("run", argsFile.toString())
    Files.write(argsFile, args.toBytes)
    try {
      run(command)
    } catch {
      case _: ConnectException =>
        if (startCachegunServer()) {
          run(command)
        } else {
          -1
        }
    } finally {
      val _ = Files.deleteIfExists(argsFile)
    }
  }

  private def run(command: Array[String]): Int = {
    val carry = new StringBuilder()
    val streams = Streams(cachegunIn, cachegunOut, cachegunErr)
    val client = TcpClient(Server.Host, Server.Port)
    val noCancel = new AtomicBoolean(false)
    val interactiveSession = false
    client.run(
      classOf[Cachegun].getCanonicalName(),
      command,
      cwd,
      environmentVariables,
      streams,
      logger,
      noCancel,
      interactiveSession
    )
  }
}
