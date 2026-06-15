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
  version "0.0.6"
  url "https://github.com/Linyxus/auk/releases/download/v0.0.6/auk-darwin-arm64"
  sha256 "b5eab1121e60d4672930b9ab81f71ea65f77088833ed3d69ba68485eb963ba55"

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
