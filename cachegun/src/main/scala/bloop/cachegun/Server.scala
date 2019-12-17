package bloop.cachegun

import com.martiansoftware.nailgun.NGServer
import scala.util.Try
import java.net.InetAddress
import java.io.InputStream
import java.io.PrintStream
import bloop.logging._
import com.martiansoftware.nailgun.NGConstants
import com.martiansoftware.nailgun.NGListeningAddress
import bloop.io.Paths
import bloop.io.AbsolutePath
import snailgun.logging.SnailgunLogger
import scala.util.control.NonFatal

class Server
object Server {
  val Host = "127.0.0.1"
  val Port = 8213
  val out = System.out

  def signalExit(): Unit = {
    try {
      val client = new CachegunClient(
        Map.empty,
        AbsolutePath.workingDirectory.underlying,
        new SnailgunLogger("log", System.out, false),
        System.in,
        System.out,
        System.err,
        () => false
      )
      client.exit()
    } catch {
      case NonFatal(_) =>
    }
  }
  def main(args: Array[String]): Unit = {
    instantiateServer(args).run()
  }

  private def instantiateServer(args: Array[String]): NGServer = {
    def toPortNumber(userPort: String) = Try(userPort.toInt).getOrElse(Port)
    val (addr, port) = {
      if (args.length == 0) (InetAddress.getLoopbackAddress(), toPortNumber(""))
      else if (args.length == 1) (InetAddress.getLoopbackAddress(), toPortNumber(args(0)))
      else if (args.length == 2) {
        val addr = InetAddress.getByName(args(0))
        (addr, toPortNumber(args(1)))
      } else {
        throw new IllegalArgumentException(
          s"Invalid arguments to bloop server: $args, expected: [address] [port]"
        )
      }
    }

    val logger = BloopLogger.default("bloop-nailgun-main")
    launchServer(System.in, System.out, System.err, addr, port, logger)
  }

  private def launchServer(
      in: InputStream,
      out: PrintStream,
      err: PrintStream,
      addr: InetAddress,
      port: Int,
      logger: Logger
  ): NGServer = {
    val javaLogger = new Slf4jAdapter(logger)
    val address = new NGListeningAddress(addr, port)
    val poolSize = NGServer.DEFAULT_SESSIONPOOLSIZE
    val heartbeatMs = NGConstants.HEARTBEAT_TIMEOUT_MILLIS.toInt
    val server = new NGServer(address, poolSize, heartbeatMs, in, out, err, javaLogger)
    // registerAliases(server)
    // ProxySetup.init()
    server
  }

}
