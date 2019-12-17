package bloop.cachegun

import scala.collection.JavaConverters._
import java.net.{URL, URLClassLoader}
import java.util
import sun.misc.Unsafe
import scala.collection.Seq
import java.nio.file.Path
import bloop.bloopgun.core.ResolvedAt
import snailgun.logging.Logger
import bloop.bloopgun.core.ServerStatus
import scala.util.Try
import java.nio.file.Paths

object CachegunClasspath {
  def resolveWithFallbackToThisClasspath(version: String, slogger: Logger): ResolvedAt = {
    ServerStatus.resolveCachegun(version, slogger).getOrElse {
      ResolvedAt(getURLs(this.getClass().getClassLoader()).flatMap { url =>
        Try(Paths.get(url.toURI())).toOption
      })
    }
  }

  /**
   * Utility to get SystemClassLoader/ClassLoader urls in java8 and java9+
   *   Based upon: https://gist.github.com/hengyunabc/644f8e84908b7b405c532a51d8e34ba9
   */
  private def getURLs(classLoader: ClassLoader): Seq[URL] = {
    if (classLoader.isInstanceOf[URLClassLoader]) {
      classLoader.asInstanceOf[URLClassLoader].getURLs()
      // java9+
    } else if (classLoader
                 .getClass()
                 .getName()
                 .startsWith("jdk.internal.loader.ClassLoaders$")) {
      try {
        val field = classOf[Unsafe].getDeclaredField("theUnsafe")
        field.setAccessible(true)
        val unsafe = field.get(null).asInstanceOf[Unsafe]

        // jdk.internal.loader.ClassLoaders.AppClassLoader.ucp
        val ucpField = classLoader.getClass().getDeclaredField("ucp")
        val ucpFieldOffset: Long = unsafe.objectFieldOffset(ucpField)
        val ucpObject = unsafe.getObject(classLoader, ucpFieldOffset)

        // jdk.internal.loader.URLClassPath.path
        val pathField = ucpField.getType().getDeclaredField("path")
        val pathFieldOffset = unsafe.objectFieldOffset(pathField)
        val paths: Seq[URL] = unsafe
          .getObject(ucpObject, pathFieldOffset)
          .asInstanceOf[util.ArrayList[URL]]
          .asScala

        paths
      } catch {
        case ex: Exception =>
          ex.printStackTrace()
          Nil
      }
    } else {
      Nil
    }
  }
}
