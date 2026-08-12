package auk.platform.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

@js.native
@JSImport("node:sea", JSImport.Namespace)
private[platform] object NodeSea extends js.Object:
  def isSea(): Boolean = js.native
  def getAsset(key: String): ArrayBuffer = js.native

/** Assets embedded in the binary when running as a Node single-executable
  * (SEA); see `packageBinary` in build.mill for the producing side. Everything
  * degrades to "absent" outside a SEA, so callers need no environment check
  * beyond [[isSea]].
  */
object SeaAssets:
  def isSea: Boolean =
    try NodeSea.isSea()
    catch case _: Throwable => false

  /** The embedded asset bytes, or None when absent (or not running as a SEA). */
  def bytes(key: String): Option[Uint8Array] =
    try Some(new Uint8Array(NodeSea.getAsset(key)))
    catch case _: Throwable => None

  /** The embedded asset decoded as UTF-8, or None when absent. */
  def string(key: String): Option[String] =
    try Some(new TextDecoder().decode(NodeSea.getAsset(key)))
    catch case _: Throwable => None
