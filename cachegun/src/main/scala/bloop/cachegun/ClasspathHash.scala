package bloop.cachegun

import bloop.io.ByteHasher
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import java.nio.file.Path
import java.nio.file.Files

case class ClasspathHash(hashes: Int)

object ClasspathHash {
  implicit val codec: JsonValueCodec[CachegunArguments] =
    JsonCodecMaker.make[CachegunArguments](CodecMakerConfig)

  def fromClasspaths(classpaths: Vector[Vector[Path]]): ClasspathHash = {
    val hashes = classpaths.map { classpath =>
      // TODO: get hash from the client via args file
      classpath.map { path =>
        path.hashCode()
      }
    }
    ClasspathHash(hashes.##)
  }
}
