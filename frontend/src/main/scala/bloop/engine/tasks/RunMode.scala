package bloop.engine.tasks

sealed trait RunMode
object RunMode {
  def fromCachegunBoolean(isCachegunEnabled: Boolean): RunMode =
    if (isCachegunEnabled) Cachegun
    else Normal
  final case object Debug extends RunMode
  final case object Normal extends RunMode
  final case object Cachegun extends RunMode
}
