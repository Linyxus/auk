# Homebrew formula for auk. Auto-updated by release.sh on each release — the
# version, url, and sha256 lines below are rewritten there, so edit the rest of
# the file (desc/test/etc.) by hand but leave those three to the release script.
#
# Install by tapping this repo:
#   brew tap linyxus/auk https://github.com/Linyxus/auk
#   brew trust linyxus/auk          # required: custom-remote taps must be trusted
#   brew install linyxus/auk/auk
class Auk < Formula
  desc "Coding agent in Scala 3 (Node single-executable build)"
  homepage "https://github.com/Linyxus/auk"
  version "0.0.12"
  url "https://github.com/Linyxus/auk/releases/download/v0.0.12/auk-darwin-arm64"
  sha256 "ee7c563eeaa4392a41d32336d774d835b2e10505d49fd6fdfa9c3afe6818c3fb"

  # The packaged binary is the host's Node executable with auk embedded, so the
  # release ships Apple Silicon only.
  depends_on arch: :arm64
  depends_on :macos

  def install
    bin.install "auk-darwin-arm64" => "auk"
  end

  test do
    assert_predicate bin/"auk", :executable?
  end
end
