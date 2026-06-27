package auk.platform.js

import scala.scalajs.js

/** Regression coverage for [[AssetCache.publish]], the shared publish step for
  * the SEA asset caches.
  *
  * The bug it guards: a `dir` left incomplete by an interrupted earlier run (a
  * non-empty cache missing its entry file) wedged every future run forever —
  * `rename(2)` refuses to replace a non-empty dir (`ENOTEMPTY`), and that
  * failure was swallowed as "lost the race", so the dashboard/REPL never came
  * back until the stale dir was deleted by hand. Real filesystem under tmp. */
class AssetCacheSuite extends munit.FunSuite:

  private val fs = js.Dynamic.global.require("node:fs")
  private val osPath = js.Dynamic.global.require("node:path")
  private val os = js.Dynamic.global.require("node:os")
  private var counter = 0

  private val Entry = "index.html"

  private def base(): String =
    counter += 1
    val b = osPath.join(os.tmpdir(), s"auk-assetcache-test-${js.Dynamic.global.process.pid}-$counter").asInstanceOf[String]
    fs.mkdirSync(b, js.Dynamic.literal(recursive = true))
    b

  private def mkdir(p: String): Unit = fs.mkdirSync(p, js.Dynamic.literal(recursive = true))
  private def write(p: String, name: String): Unit = fs.writeFileSync(osPath.join(p, name), s"contents-of-$name", "utf8")
  private def exists(p: String, name: String): Boolean = fs.existsSync(osPath.join(p, name)).asInstanceOf[Boolean]

  /** A fully-populated staging dir (entry file + a couple of assets). */
  private def stage(b: String, name: String): String =
    val tmp = osPath.join(b, name).asInstanceOf[String]
    mkdir(tmp)
    write(tmp, Entry)
    write(tmp, "main.js")
    tmp

  private def complete(dir: String): Boolean = exists(dir, Entry)

  test("publishes a staging dir when the target does not yet exist"):
    val b = base()
    val dir = osPath.join(b, "dir").asInstanceOf[String]
    val tmp = stage(b, "tmp")
    assert(AssetCache.publish(tmp, dir, complete(dir)), "publish should succeed onto a missing dir")
    assert(complete(dir), "entry file should be present after publish")
    assert(!fs.existsSync(tmp).asInstanceOf[Boolean], "staging dir should have been moved, not copied")

  test("heals a stale, non-empty incomplete cache instead of failing forever"):
    val b = base()
    val dir = osPath.join(b, "dir").asInstanceOf[String]
    // A wedged cache from an interrupted earlier run: present, non-empty, but
    // missing the entry file. This is exactly the on-disk state that broke the
    // workflow dashboard (auk-webui-<tag> with only a couple of font files).
    mkdir(dir)
    write(dir, "CommitMono-Italic.woff2")
    assert(!complete(dir), "precondition: stale cache is incomplete")
    val tmp = stage(b, "tmp")
    assert(AssetCache.publish(tmp, dir, complete(dir)), "publish should heal the stale cache, not fail")
    assert(complete(dir), "entry file should be present after healing")
    assert(!exists(dir, "CommitMono-Italic.woff2"), "stale file should be gone after replacement")

  test("uses a concurrent winner's complete dir without disturbing it"):
    val b = base()
    val dir = osPath.join(b, "dir").asInstanceOf[String]
    // Simulate a winner that already published a complete cache.
    mkdir(dir)
    write(dir, Entry)
    write(dir, "main.js")
    val tmp = stage(b, "tmp")
    assert(AssetCache.publish(tmp, dir, complete(dir)), "publish should accept the winner's copy")
    assert(complete(dir), "winner's entry file should remain")
    // Our losing staging dir is left for the caller's finally to clean up.
    assert(fs.existsSync(tmp).asInstanceOf[Boolean], "losing staging dir is not consumed by publish")

  test("reports failure when the staging dir is genuinely unusable"):
    val b = base()
    val dir = osPath.join(b, "dir").asInstanceOf[String]
    val tmp = osPath.join(b, "does-not-exist").asInstanceOf[String]
    assert(!AssetCache.publish(tmp, dir, complete(dir)), "publish should fail when there is nothing to move")
