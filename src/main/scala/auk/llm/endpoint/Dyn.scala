package auk.llm.endpoint

import scala.scalajs.js

/** Small helpers for reading optional fields off a `js.Dynamic` response. */
private[endpoint] object Dyn:
  def defined(v: js.Dynamic): Boolean = !js.isUndefined(v) && v != null

  def str(v: js.Dynamic): Option[String] =
    if defined(v) then Some(v.asInstanceOf[String]) else None

  def num(v: js.Dynamic): Option[Double] =
    if defined(v) then Some(v.asInstanceOf[Double]) else None

  def arr(v: js.Dynamic): List[js.Dynamic] =
    if defined(v) then v.asInstanceOf[js.Array[js.Dynamic]].toList else Nil
