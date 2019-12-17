package bloop.cachegun

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

case class CachegunArguments(
    cachedClasspath: Vector[Vector[String]],
    uncachedClasspath: Vector[String],
    mainClass: String,
    mainMethod: Option[String],
    arguments: Array[String],
    systemProperties: Map[String, String],
    verbose: Option[Boolean]
) {
  def isVerbose: Boolean = verbose.contains(true)
  def toBytes: Array[Byte] = writeToArray(this, WriterConfig.withIndentionStep(2))

  def withSystemProperties[T](thunk: () => T): T = {
    val oldSystemPropertiesCopy = systemProperties.keysIterator.flatMap { key =>
      Option(System.getProperty(key)) match {
        case None => Nil
        case Some(value) => List(key -> value)
      }
    }.toMap
    try {
      updateSystemProperties(systemProperties)
      thunk()
    } finally {
      updateSystemProperties(oldSystemPropertiesCopy)
    }
  }

  private def updateSystemProperties(newProperties: Map[String, String]): Unit = {
    newProperties.foreach {
      case (key, value) =>
        System.setProperty(key, value)
    }
  }
}

object CachegunArguments {
  implicit val codec: JsonValueCodec[CachegunArguments] =
    JsonCodecMaker.make[CachegunArguments](CodecMakerConfig)
}
