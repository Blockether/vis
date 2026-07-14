#!/usr/bin/env bash
#
# deploy-ios.sh — archive the native iOS app and upload it to TestFlight,
# the plain Xcode/CLI way (no EAS). Runs three stages:
#
#   1. xcodebuild archive       → build/vis.xcarchive   (signed, distribution)
#   2. xcodebuild -exportArchive → build/ipa/*.ipa       (App Store .ipa)
#   3. xcrun altool --upload-app → App Store Connect      → TestFlight
#
# PREREQUISITES (one-time, on this Mac):
#   • Paid Apple Developer membership + your Apple ID added in Xcode
#     (Settings → Accounts) so automatic signing can mint the dist cert.
#   • An App Store Connect API key (Users and Access → Integrations → App Store
#     Connect API → +). Download the AuthKey_XXXXXXXXXX.p8 ONCE and drop it in
#     ~/.appstoreconnect/private_keys/ (or ~/.private_keys/). Then export:
#         export ASC_KEY_ID=XXXXXXXXXX          # the key's Key ID
#         export ASC_ISSUER_ID=aaaa-bbbb-....   # the team's Issuer ID
#   • The app record must exist in App Store Connect with bundle id
#     com.blockether.vis (create it once under Apps → +).
#
# Bump the build number in app.json (ios.buildNumber) before re-running, or
# App Store Connect rejects a duplicate build.

set -euo pipefail

cd "$(dirname "$0")/.."

WORKSPACE="ios/vis.xcworkspace"
SCHEME="vis"
BUILD_DIR="build"
ARCHIVE="$BUILD_DIR/vis.xcarchive"
IPA_DIR="$BUILD_DIR/ipa"
EXPORT_OPTS="ios/ExportOptions.plist"

if [[ ! -d "$WORKSPACE" ]]; then
  echo "✗ $WORKSPACE not found — run 'npm run prebuild:ios && npm run pods' first." >&2
  exit 1
fi

rm -rf "$ARCHIVE" "$IPA_DIR"
mkdir -p "$BUILD_DIR"

echo "▸ [1/3] Archiving $SCHEME (Release)…"
xcodebuild \
  -workspace "$WORKSPACE" \
  -scheme "$SCHEME" \
  -configuration Release \
  -destination "generic/platform=iOS" \
  -archivePath "$ARCHIVE" \
  archive

echo "▸ [2/3] Exporting App Store .ipa…"
xcodebuild \
  -exportArchive \
  -archivePath "$ARCHIVE" \
  -exportPath "$IPA_DIR" \
  -exportOptionsPlist "$EXPORT_OPTS"

IPA="$(find "$IPA_DIR" -name '*.ipa' -maxdepth 1 | head -n1)"
if [[ -z "$IPA" ]]; then
  echo "✗ No .ipa produced in $IPA_DIR" >&2
  exit 1
fi
echo "  → $IPA"

if [[ -z "${ASC_KEY_ID:-}" || -z "${ASC_ISSUER_ID:-}" ]]; then
  cat >&2 <<EOF
▸ [3/3] SKIPPED upload — ASC_KEY_ID / ASC_ISSUER_ID not set.
  The signed .ipa is ready at:
      $IPA
  Upload it by either:
    • exporting the two vars above and re-running this script, or
    • opening Xcode → Organizer → Distribute App, or
    • drag-drop into Apple's Transporter app.
EOF
  exit 0
fi

echo "▸ [3/3] Uploading to App Store Connect (TestFlight)…"
xcrun altool --upload-app \
  --type ios \
  --file "$IPA" \
  --apiKey "$ASC_KEY_ID" \
  --apiIssuer "$ASC_ISSUER_ID"

echo "✓ Uploaded. It appears in TestFlight after Apple processes it (~5–15 min)."
