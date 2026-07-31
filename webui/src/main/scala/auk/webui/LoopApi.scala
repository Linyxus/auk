package auk.webui

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import org.scalajs.dom

/** Browser → host, on demand: the two loop payloads that are too big to stream to
  * every client and cheap to serve to the one asking.
  *
  * A settled generation's transcript is a file the host already has, and a patch is
  * a `git diff` it can run; both are answered as `text/plain`, and a 404 is a real
  * answer — a generation whose tee was never written, an attempt that snapshotted
  * nothing — so it is reported rather than retried. The caller records the outcome
  * in [[AppState]], which is what stops a missing payload being asked for twice.
  */
object LoopApi:

  def transcript(loopId: String, label: String, onOk: String => Unit, onError: String => Unit): Unit =
    get(s"/api/loop/${enc(loopId)}/transcript/${enc(label)}", "no transcript was recorded", onOk, onError)

  def diff(loopId: String, gen: Int, attempt: Int, onOk: String => Unit, onError: String => Unit): Unit =
    get(s"/api/loop/${enc(loopId)}/diff/$gen/$attempt", "no diff is available", onOk, onError)

  private def get(path: String, missing: String, onOk: String => Unit, onError: String => Unit): Unit =
    val answered: Future[Unit] =
      for
        res <- (dom.fetch(path): Future[dom.Response])
        _ <-
          if res.status == 404 then Future.successful(onError(missing))
          else if !res.ok then Future.successful(onError(s"the host answered ${res.status}"))
          else (res.text(): Future[String]).map(onOk)
      yield ()
    answered.failed.foreach(t => onError(Option(t.getMessage).getOrElse("the request failed")))

  private def enc(s: String): String = js.URIUtils.encodeURIComponent(s)
