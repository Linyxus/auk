package auk.platform

import auk.platform.js.NodePath

/** Lexical path arithmetic, replacing `java.nio.file.Path` operations.
  *
  * Backed by `node:path`, which gives the same purely-lexical semantics as
  * `java.nio.file.Path` (no symlink resolution): `..`/`.` are collapsed by
  * normalisation, and an absolute second argument to [[resolve]] wins.
  */
object PathOps:
  /** Resolve `path` against `base`. Absolute `path` is returned normalised;
    * a relative one is anchored at `base`. Mirrors `base.resolve(path).normalize()`. */
  def resolve(base: String, path: String): String =
    NodePath.normalize(NodePath.resolve(base, path))

  def normalize(path: String): String = NodePath.normalize(path)

  /** The parent directory, or `None` at a filesystem root / for a bare name. */
  def parent(path: String): Option[String] =
    val d = NodePath.dirname(path)
    if d == path || d == "." || d.isEmpty then None else Some(d)

  def join(a: String, b: String): String = NodePath.join(a, b)

  def basename(path: String): String = NodePath.basename(path)
