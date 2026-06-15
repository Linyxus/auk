#!/bin/sh
# Build the standalone `auk` binary for Apple Silicon and publish it as a
# GitHub release.
#
#   sh release.sh v0.0.1
#
# To bake a demo ZAI_API_KEY into the binary (advertisement builds), run with
# both the gate and the key set — see build.sbt's packageBinary task:
#
#   DANGEROUSLY_PACK_KEY=1 ZAI_API_KEY=... sh release.sh v0.0.1
#
# NOTE: a baked key is stored in PLAINTEXT inside the binary and is recoverable
# with `strings dist/auk`. Only ever bake a throwaway / rate-limited demo key.

set -eu

# --- args ------------------------------------------------------------------
VERSION="${1:-}"
if [ -z "$VERSION" ]; then
  echo "usage: sh release.sh <version>   (e.g. sh release.sh v0.0.1)" >&2
  exit 2
fi
case "$VERSION" in
  v*) ;;
  *) echo "release.sh: version must start with 'v' (got '$VERSION')" >&2; exit 2 ;;
esac

# --- preflight -------------------------------------------------------------
# This produces an Apple Silicon (arm64) binary: packageBinary copies the
# running Node executable, so the host must be arm64 macOS.
OS="$(uname -s)"
ARCH="$(uname -m)"
if [ "$OS" != "Darwin" ] || [ "$ARCH" != "arm64" ]; then
  echo "release.sh: must run on Apple Silicon macOS (got $OS/$ARCH)" >&2
  exit 1
fi

command -v sbt >/dev/null 2>&1 || { echo "release.sh: sbt not found on PATH" >&2; exit 1; }
command -v gh  >/dev/null 2>&1 || { echo "release.sh: gh (GitHub CLI) not found on PATH" >&2; exit 1; }
gh auth status >/dev/null 2>&1 || { echo "release.sh: not logged in to gh — run 'gh auth login'" >&2; exit 1; }

if [ "${DANGEROUSLY_PACK_KEY:-}" = "1" ]; then
  echo "release.sh: DANGEROUSLY_PACK_KEY=1 — baking ZAI_API_KEY into the binary (plaintext, extractable)."
fi

# --- build -----------------------------------------------------------------
echo "release.sh: building dist/auk via 'sbt packageBinary'..."
# DANGEROUSLY_PACK_KEY / ZAI_API_KEY are inherited from the environment and
# read by the packageBinary task.
sbt -batch packageBinary

BIN="dist/auk"
[ -f "$BIN" ] || { echo "release.sh: expected $BIN to exist after build" >&2; exit 1; }

# Name the uploaded asset by platform so future arch/OS builds don't collide.
ASSET="auk-darwin-arm64"
cp "$BIN" "dist/$ASSET"

# --- publish ---------------------------------------------------------------
echo "release.sh: creating GitHub release ${VERSION}..."
gh release create "$VERSION" \
  --title "$VERSION" \
  --generate-notes \
  "dist/$ASSET#auk (macOS Apple Silicon)"

echo "release.sh: published $VERSION with asset $ASSET"
