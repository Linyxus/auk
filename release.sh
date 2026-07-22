#!/bin/sh
# Build the standalone `auk` binary for Apple Silicon, publish it as a GitHub
# release, and update the Homebrew formula (auk.rb) to point at it.
#
#   sh release.sh v0.0.1
#
# What it does, in order:
#   1. build dist/auk via `sbt packageBinary`, with AUK_VERSION=<version> so the
#      binary reports exactly this release version (see build.sbt; a plain
#      `sbt packageBinary` instead builds a `<next-version>-SNAPSHOT` test build)
#   2. verify `dist/auk --version` reports that version
#   3. rewrite auk.rb's version/url/sha256 to the new release
#   4. commit the formula bump ("Release vX.Y.Z") and push
#   5. tag that commit and push the tag (the local tag is what future SNAPSHOT
#      version derivation reads), then create the GitHub release on it,
#      uploading the binary
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
TAG="${1:-}"
if [ -z "$TAG" ]; then
  echo "usage: sh release.sh <version>   (e.g. sh release.sh v0.0.1)" >&2
  exit 2
fi
case "$TAG" in
  v*) ;;
  *) echo "release.sh: version must start with 'v' (got '$TAG')" >&2; exit 2 ;;
esac
# Homebrew's `version` field has no leading 'v'; the git tag and download URL do.
VER="${TAG#v}"

# --- preflight -------------------------------------------------------------
# This produces an Apple Silicon (arm64) binary: packageBinary copies the
# running Node executable, so the host must be arm64 macOS (also why BSD sed -i
# below is safe to assume).
OS="$(uname -s)"
ARCH="$(uname -m)"
if [ "$OS" != "Darwin" ] || [ "$ARCH" != "arm64" ]; then
  echo "release.sh: must run on Apple Silicon macOS (got $OS/$ARCH)" >&2
  exit 1
fi

command -v sbt >/dev/null 2>&1 || { echo "release.sh: sbt not found on PATH" >&2; exit 1; }
command -v gh  >/dev/null 2>&1 || { echo "release.sh: gh (GitHub CLI) not found on PATH" >&2; exit 1; }
command -v git >/dev/null 2>&1 || { echo "release.sh: git not found on PATH" >&2; exit 1; }
gh auth status >/dev/null 2>&1 || { echo "release.sh: not logged in to gh — run 'gh auth login'" >&2; exit 1; }

# Run from the repo root so relative paths (auk.rb, dist/) resolve regardless of
# the caller's cwd.
cd "$(git rev-parse --show-toplevel)"
[ -f auk.rb ] || { echo "release.sh: auk.rb not found at repo root" >&2; exit 1; }

# Refuse if the tag/release already exists — gh would fail mid-run otherwise, and
# we'd rather stop before building.
if gh release view "$TAG" >/dev/null 2>&1; then
  echo "release.sh: release $TAG already exists — bump the version or delete it first" >&2
  exit 1
fi

if [ "${DANGEROUSLY_PACK_KEY:-}" = "1" ]; then
  echo "release.sh: DANGEROUSLY_PACK_KEY=1 — baking ZAI_API_KEY into the binary (plaintext, extractable)."
fi

# --- build -----------------------------------------------------------------
echo "release.sh: building dist/auk via 'sbt packageBinary' (AUK_VERSION=$TAG)..."
# AUK_VERSION pins the build's version to this release (instead of the
# git-derived -SNAPSHOT default). DANGEROUSLY_PACK_KEY / ZAI_API_KEY are
# inherited from the environment and read by the packageBinary task.
AUK_VERSION="$TAG" sbt -batch packageBinary

BIN="dist/auk"
[ -f "$BIN" ] || { echo "release.sh: expected $BIN to exist after build" >&2; exit 1; }

# The artifact must report the exact release version — catches a build that
# somehow missed AUK_VERSION before anything is published.
GOT="$("$BIN" --version)"
[ "$GOT" = "auk $TAG" ] || {
  echo "release.sh: $BIN --version reports '$GOT', expected 'auk $TAG' — aborting" >&2
  exit 1
}

# Name the uploaded asset by platform so future arch/OS builds don't collide.
# This is also the filename the formula's bin.install expects.
ASSET="auk-darwin-arm64"
cp "$BIN" "dist/$ASSET"

# --- update the Homebrew formula -------------------------------------------
# The asset uploaded below is byte-identical to this local file, so its checksum
# is valid for the formula.
SHA="$(shasum -a 256 "dist/$ASSET" | awk '{print $1}')"
[ -n "$SHA" ] || { echo "release.sh: failed to compute sha256 of dist/$ASSET" >&2; exit 1; }

echo "release.sh: updating auk.rb -> version $VER, sha256 $SHA"
URL="https://github.com/Linyxus/auk/releases/download/${TAG}/${ASSET}"
# BSD sed (macOS): -i '' for in-place; \1 preserves each line's indentation.
sed -E -i '' \
  -e "s|^([[:space:]]*)version \".*\"|\1version \"${VER}\"|" \
  -e "s|^([[:space:]]*)sha256 \".*\"|\1sha256 \"${SHA}\"|" \
  -e "s|releases/download/[^/]+/auk-darwin-arm64|releases/download/${TAG}/auk-darwin-arm64|" \
  auk.rb

# Sanity-check the rewrite actually landed all three fields.
grep -q "version \"${VER}\"" auk.rb && grep -q "sha256 \"${SHA}\"" auk.rb \
  && grep -q "download/${TAG}/${ASSET}" auk.rb \
  || { echo "release.sh: auk.rb did not update as expected — aborting" >&2; exit 1; }

# --- commit + push the release ---------------------------------------------
git add auk.rb
if git diff --cached --quiet; then
  echo "release.sh: auk.rb already up to date for $TAG (committing nothing)"
else
  git commit -m "Release $TAG"
fi
echo "release.sh: pushing release commit..."
git push origin HEAD

# --- publish ---------------------------------------------------------------
# Tag the just-pushed commit LOCALLY and push the tag, rather than letting gh
# create it remote-only: build.sbt derives the next -SNAPSHOT version from
# `git describe` in this clone, so the tag must exist here too.
echo "release.sh: tagging ${TAG} and pushing..."
git tag "$TAG"
git push origin "$TAG"

echo "release.sh: creating GitHub release ${TAG}..."
gh release create "$TAG" \
  --title "$TAG" \
  --generate-notes \
  "dist/$ASSET#auk (macOS Apple Silicon)"

echo "release.sh: published $TAG with asset $ASSET; auk.rb updated and committed"
