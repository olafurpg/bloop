package bloop.tracing

import brave.{Span, Tracer}
import brave.propagation.TraceContext
import monix.eval.Task
import monix.execution.misc.NonFatal

import scala.util.Failure
import scala.util.Success
import java.util.concurrent.TimeUnit

abstract class BraveTracer {
  def startNewChildTracer(name: => String, tags: (String, String)*): BraveTracer
  def trace[T](name: => String, tags: (String, String)*)(
      thunk: BraveTracer => T
  ): T
  def traceTask[T](name: => String, tags: (String, String)*)(
      thunk: BraveTracer => Task[T]
  ): Task[T]
  def terminate(): Unit
  def toIndependentTracer(name: => String, tags: (String, String)*): BraveTracer
  def currentSpan: Span
}

class EmptyBraveTracer(val currentSpan: Span) extends BraveTracer {
  def startNewChildTracer(name: => String, tags: (String, String)*): BraveTracer = this
  def trace[T](name: => String, tags: (String, String)*)(thunk: BraveTracer => T): T = thunk(this)
  def traceTask[T](name: => String, tags: (String, String)*)(
      thunk: BraveTracer => Task[T]
  ): Task[T] = thunk(this)
  def terminate(): Unit = ()
  def toIndependentTracer(name: => String, tags: (String, String)*): BraveTracer = this
}
final class ActiveBraveTracer private (
    tracer: Tracer,
    val currentSpan: Span,
    closeCurrentSpan: () => Unit
) extends BraveTracer {
  def startNewChildTracer(name: => String, tags: (String, String)*): BraveTracer = {
    import brave.propagation.TraceContext
    val span = tags.foldLeft(tracer.newChild(currentSpan.context).name(name)) {
      case (span, (tagKey, tagValue)) => span.tag(tagKey, tagValue)
    }

    span.start()
    new ActiveBraveTracer(tracer, span, () => span.finish())
  }

  def trace[T](name: => String, tags: (String, String)*)(
      thunk: BraveTracer => T
  ): T = {
    val newTracer = startNewChildTracer(name, tags: _*)
    try thunk(newTracer) // Don't catch and report errors in spans
    catch {
      case NonFatal(t) =>
        newTracer.currentSpan.error(t)
        throw t
    } finally {
      try newTracer.terminate()
      catch { case NonFatal(t) => () }
    }
  }

  def traceTask[T](name: => String, tags: (String, String)*)(
      thunk: BraveTracer => Task[T]
  ): Task[T] = {
    val newTracer = startNewChildTracer(name, tags: _*)
    thunk(newTracer)
      .doOnCancel(Task(newTracer.terminate()))
      .doOnFinish {
        case None => Task.eval(newTracer.terminate())
        case Some(value) =>
          Task.eval {
            newTracer.currentSpan.error(value)
            newTracer.terminate()
          }
      }
  }

  def terminate(): Unit = this.synchronized {
    // Guarantee we never throw, even though brave APIs should already
    try closeCurrentSpan()
    catch { case t: Throwable => () }
  }

  /** Create an independent tracer that propagates this current context
   * and that whose completion in zipkin will happen independently. This
   * is ideal for tracing background tasks that outlive their parent trace. */
  def toIndependentTracer(name: => String, tags: (String, String)*): BraveTracer =
    BraveTracer(name, Some(currentSpan.context), tags: _*)
}

object BraveTracer {
  import brave._
  import brave.sampler.Sampler
  import zipkin2.reporter.AsyncReporter
  import zipkin2.reporter.urlconnection.URLConnectionSender
  val zipkinServerUrl = Option(System.getProperty("zipkin.server.url")).getOrElse(
    "http://127.0.0.1:9411/api/v2/spans"
  )

  val sender = URLConnectionSender.create(zipkinServerUrl)
  val spanReporter = AsyncReporter.builder(sender).build()
  def apply(name: String, tags: (String, String)*): BraveTracer = {
    BraveTracer(name, None, tags: _*)
  }

  def apply(name: String, ctx: Option[TraceContext], tags: (String, String)*): BraveTracer = {
    import java.util.concurrent.TimeUnit
    val tracing = Tracing
      .newBuilder()
      .localServiceName("bloop")
      .spanReporter(spanReporter)
      .build()
    val tracer = tracing.tracer()
    val newParentTrace = ctx.map(c => tracer.newChild(c)).getOrElse(tracer.newTrace())
    val rootSpan = tags.foldLeft(newParentTrace.name(name)) {
      case (span, (tagKey, tagValue)) => span.tag(tagKey, tagValue)
    }
    new EmptyBraveTracer(rootSpan)
    // rootSpan.start()
    // val closeEverything = () => {
    //   rootSpan.finish()
    //   tracing.close()
    //   spanReporter.flush()
    // }
    // new BraveTracer(tracer, rootSpan, closeEverything)
  }
}
