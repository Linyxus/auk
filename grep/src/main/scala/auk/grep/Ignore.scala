package auk.grep

import scala.util.matching.Regex

/** One parsed `.gitignore` rule: the [[Glob]] translation of the pattern (with
 *  its `!`, trailing-`/`, and anchoring markers stripped) plus those markers as
 *  flags. */
final case class IgnoreRule(regex: Regex, negated: Boolean, dirOnly: Boolean, anchored: Boolean):
  /** Whether this rule matches an entry, given its path `rel` relative to the
   *  rule's `.gitignore` directory (`/`-separated), its base `name`, and whether
   *  it `isDir`. A `dirOnly` rule matches directories only; an anchored rule
   *  matches the full relative path; an unanchored rule matches the base name at
   *  any depth (git's "no interior slash → match anywhere" rule). */
  def matches(rel: String, name: String, isDir: Boolean): Boolean =
    if dirOnly && !isDir then false
    else if anchored then regex.matches(rel)
    else regex.matches(name)

/** A small `.gitignore` engine: parse rules, match entries. Deliberately a
 *  subset of git's syntax — comments and blanks, the `*` `**` `?` globs (via
 *  [[Glob]]), leading-`/` anchoring, trailing-`/` dir-only, and `!` negation
 *  with last-match-wins. Not supported (git has them; a later stage may add
 *  them): backslash escapes such as `\#` and character classes `[...]`.
 *
 *  Rules are consulted from the search root downward only — never parent
 *  directories or global config — and, git-compatibly, a `!` negation cannot
 *  resurrect a path nested inside a directory an earlier rule already pruned
 *  (the traversal never descends a pruned directory). */
object Ignore:
  /** Parse `.gitignore` content into rules in file order; blank lines and `#`
   *  comments are dropped. */
  def parse(text: String): List[IgnoreRule] =
    Lines.split(text).flatMap(parseLine)

  private def parseLine(raw: String): Option[IgnoreRule] =
    if raw.isEmpty || raw.startsWith("#") then None
    else
      var s = raw
      while s.nonEmpty && s.last == ' ' do s = s.dropRight(1) // git drops trailing spaces
      if s.isEmpty then None
      else
        val negated = s.startsWith("!")
        if negated then s = s.substring(1)
        val dirOnly = s.endsWith("/")
        if dirOnly then s = s.substring(0, s.length - 1)
        val anchored = s.contains("/")
        if s.startsWith("/") then s = s.substring(1)
        if s.isEmpty then None // a bare "/" (or "!/") anchors nothing
        else Some(IgnoreRule(Glob.toRegex(s), negated, dirOnly, anchored))
