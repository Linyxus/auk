package auk.library

/** The operations auk exposes to code evaluated in an eval_scala REPL session.
  *
  * This subproject is compiled with auk's regular toolchain, packed into
  * `library.bin` (`.tasty` for the REPL compiler, `.sjsir` for its
  * interpreter — see `packLibraryBin` in build.sbt) and preloaded into every
  * session via the worker's `--classpath` flag, so evaluated code can call
  * [[AukImpl]] directly.
  *
  * Placeholder surface for now: the real operations (files, shell, search)
  * come later.
  */
trait AukInterface:
  /** Placeholder: pure function, proves typechecking against the library. */
  def hello(name: String): String

  /** Placeholder: pure arithmetic. */
  def add(x: Int, y: Int): Int

  /** Placeholder: reaches Node through JS interop, proving the library's
    * `.sjsir` can touch the host environment from inside the interpreter.
    */
  def cwd(): String
