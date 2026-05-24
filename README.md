# Auk

A coding agent.

---

## Dependencies

You need three things on your machine. The easiest way to get all of them is
**coursier** — it can install a JDK and sbt for you.

| Tool | Why | Version used here |
|------|-----|-------------------|
| **JDK** | runs the JVM bytecode | 17 or newer (tested on 21) |
| **coursier** (`cs`) | resolves dependencies & provides the `auk` launcher | 2.1.x |
| **sbt** | builds and publishes the project | 1.12.x |

### Install coursier (and, optionally, the JDK + sbt)

**macOS / Linux:**

```sh
# Install the `cs` launcher (Homebrew shown; see https://get-coursier.io for others)
brew install coursier/formulas/coursier

# Let coursier set up a JDK, sbt, and the PATH for you
cs setup
```

`cs setup` installs a JDK and common Scala tools, and adds coursier's bin
directory to your `PATH`. If you already manage your own JDK/sbt, you can skip
`cs setup` and just make sure `java`, `sbt`, and `cs` are on your `PATH`.

> Other install methods (curl, Windows, etc.) are documented at
> <https://get-coursier.io/docs/cli-installation>.

---

## Configuration

Auk reads its credentials from the environment. By default it uses OpenRouter:

| Variable | Required | Default |
|----------|----------|---------|
| `OPENROUTER_API_KEY` | **yes** | — |
| `OPENROUTER_BASE_URL` | no | `https://openrouter.ai/api/v1` |

Get a key at <https://openrouter.ai/keys>, then export it:

```sh
export OPENROUTER_API_KEY="sk-or-v1-..."
```

The default model is `deepseek/deepseek-v4-flash` (see `src/main/scala/auk/Main.scala`).

---

## Install & run

From the project root, run the installer once:

```sh
./scripts/install.sh
```

This does two things:

1. Publishes the project to your local Ivy repository (`~/.ivy2/local`) via
   `sbt publishLocal`.
2. Installs a small `auk` launcher onto your `PATH` (next to `cs`).

Then start the agent:

```sh
auk
```

### Keybindings

| Key | Action |
|-----|--------|
| `Enter` | send the message |
| `Ctrl+A` / `Ctrl+E` | jump to start / end of line |
| `Ctrl+K` | kill to end of line |
| `↑` / `↓` | navigate input history |
| `Ctrl+Q` | quit |

---

## Development loop

After the one-time install, rebuilding is just a republish — the `auk` command
always runs your latest build, no reinstall needed:

```sh
sbt publishLocal   # rebuild + republish
auk                # runs the new build
```

You can also run the tests or the app directly through sbt:

```sh
sbt test
sbt run
```

