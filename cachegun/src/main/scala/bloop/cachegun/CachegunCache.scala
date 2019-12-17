package bloop.cachegun

import java.{util => ju}
import java.lang.ref.WeakReference
import java.net.URLClassLoader
import java.nio.file.Path
import java.nio.file.Paths
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodType

object CachegunCache {
  private val cache: ju.Map[ClasspathHash, WeakReference[URLClassLoader]] =
    new ju.WeakHashMap[ClasspathHash, WeakReference[URLClassLoader]]()

  def clear(): Unit = {
    cache.clear()
  }

  def getClassLoader(args: CachegunArguments): ClassLoader = synchronized {
    val directories = for {
      classpath <- args.cachedClasspath
      path <- classpath
      if !path.endsWith(".jar")
    } yield Paths.get(path)
    if (directories.nonEmpty) {
      throw new IllegalArgumentException(
        s"cachedClasspath must only contain jar entries, found: $directories"
      )
    }
    val jars = args.cachedClasspath.flatMap { classpath =>
      val jarEntries = classpath.collect {
        case entry if entry.endsWith(".jar") => Paths.get(entry)
      }
      if (jarEntries.isEmpty) Nil
      else Vector(jarEntries)
    }
    val cachedClassloader = createLayeredClassloader(jars, args)
    if (args.uncachedClasspath.isEmpty) {
      cachedClassloader
    } else {
      val unchangedUrls =
        args.uncachedClasspath.map(path => Paths.get(path).toUri().toURL()).toArray
      new URLClassLoader(unchangedUrls, cachedClassloader)
    }
  }

  private def createLayeredClassloader(
      classpaths: Vector[Vector[Path]],
      args: CachegunArguments
  ): ClassLoader = {
    if (classpaths.isEmpty) {
      bootClassLoader
    } else {
      val lookup = cacheLookup(classpaths, args)
      lookup match {
        case Some(result) =>
          result
        case None =>
          val parent = createLayeredClassloader(classpaths.init, args)
          val classpath = classpaths.last
          val urls = classpath.iterator.map(_.toUri().toURL()).toArray
          val result = new URLClassLoader(urls, parent)
          cachePut(classpaths, result, args)
          result
      }
    }
  }

  private def cachePut(
      classpaths: Vector[Vector[Path]],
      classloader: URLClassLoader,
      args: CachegunArguments
  ): Unit = {
    val hash = ClasspathHash.fromClasspaths(classpaths)
    val _ = cache.put(hash, new WeakReference(classloader))
  }

  private def cacheLookup(
      classpaths: Vector[Vector[Path]],
      args: CachegunArguments
  ): Option[URLClassLoader] = {
    val hash = ClasspathHash.fromClasspaths(classpaths)
    Option(cache.get(hash)) match {
      case Some(value) =>
        val classloader = value.get()
        if (classloader == null) {
          cache.remove(hash, value)
          None
        } else {
          Some(classloader)
        }
      case None => None
    }
  }

  private val bootClassLoader: ClassLoader = {
    if (!scala.util.Properties.isJavaAtLeast("9")) null
    else {
      try {
        MethodHandles
          .lookup()
          .findStatic(
            classOf[ClassLoader],
            "getPlatformClassLoader",
            MethodType.methodType(classOf[ClassLoader])
          )
          .invoke()
      } catch {
        case _: Throwable =>
          null
      }
    }
  }
}
