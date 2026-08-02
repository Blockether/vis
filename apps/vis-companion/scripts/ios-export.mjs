// ExportOptions and argument construction for `xcodebuild -exportArchive`.
//
// This lives apart from `ios-release.mjs` because that script is a top-level
// side-effecting release runner: importing it starts a release, so the decisions
// that have already broken a shipping build cannot be tested in place.

const xml = (value) =>
  String(value).replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');

/**
 * Decide how the export signs, given EVERY bundle the archive ships.
 *
 * `signingStyle` = manual makes xcodebuild demand a named profile for every one
 * of them: an unnamed bundle fails the export outright with `"VisShare.appex"
 * requires a provisioning profile`, and the only remedy Apple offers is a
 * complete `provisioningProfiles` dictionary — no flag talks it out of that.
 * We pin one profile, for the app, so the share extension (`<app>.share`) has
 * none and never will unless someone mints a second secret for it.
 *
 * The archive step resolves its own profiles automatically, so the honest
 * fallback is to export the same way: pin manually only when every shipped
 * bundle is named, and otherwise let xcodebuild resolve all of them.
 *
 * @param {object} options
 * @param {string[]} options.bundleIds every bundle id inside the archive
 * @param {Record<string, string|undefined>} [options.profileNames] pinned profile per bundle id
 * @returns {{signingStyle: 'manual'|'automatic', provisioningProfiles: Record<string, string>, unnamed: string[]}}
 */
export function signingPlan({ bundleIds, profileNames = {} }) {
  const named = (id) => profileNames[id]?.trim();
  const unnamed = bundleIds.filter((id) => !named(id));
  if (bundleIds.length === 0 || unnamed.length > 0) {
    return { signingStyle: 'automatic', provisioningProfiles: {}, unnamed };
  }
  return {
    signingStyle: 'manual',
    provisioningProfiles: Object.fromEntries(bundleIds.map((id) => [id, named(id)])),
    unnamed: [],
  };
}

/**
 * Render the ExportOptions plist for an App Store Connect export.
 *
 * `ios/` is gitignored, so this file is generated on every release rather than
 * committed; it is a pure function of the team, the plan, and nothing else.
 *
 * @param {object} options
 * @param {string} options.teamId Apple Developer team
 * @param {'manual'|'automatic'} options.signingStyle from {@link signingPlan}
 * @param {Record<string, string>} [options.provisioningProfiles] profile per bundle id
 * @returns {string} plist contents
 */
export function exportOptionsPlist({ teamId, signingStyle, provisioningProfiles = {} }) {
  const profiles = Object.entries(provisioningProfiles)
    .map(([bundleId, name]) => `\t\t<key>${xml(bundleId)}</key>\n\t\t<string>${xml(name)}</string>\n`)
    .join('');
  const manualSigningXml =
    signingStyle === 'manual'
      ? `\t<key>signingCertificate</key>
\t<string>iOS Distribution</string>
\t<key>provisioningProfiles</key>
\t<dict>
${profiles}\t</dict>
`
      : '';
  return `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
\t<key>method</key>
\t<string>app-store-connect</string>
\t<key>destination</key>
\t<string>export</string>
\t<key>teamID</key>
\t<string>${xml(teamId)}</string>
\t<key>signingStyle</key>
\t<string>${signingStyle}</string>
${manualSigningXml}\t<key>stripSwiftSymbols</key>
\t<true/>
\t<key>uploadSymbols</key>
\t<true/>
</dict>
</plist>
`;
}

/**
 * Build the `xcodebuild -exportArchive` argument list.
 *
 * `-allowProvisioningUpdates` lets xcodebuild mint or fetch whatever the plist
 * does not pin, which is exactly what automatic signing needs and what the
 * archive step already passes. It needs App Store Connect authentication to
 * reach the portal, hence the pairing with `authenticationArgs`; a manual
 * export names every profile itself and stays offline without a key.
 *
 * @param {object} options
 * @param {string} options.archivePath path of the `.xcarchive` to export
 * @param {string} options.exportOptions path of the ExportOptions plist
 * @param {string} options.ipaDir directory to write the `.ipa` into
 * @param {boolean} options.hasApiKey whether an App Store Connect key is configured
 * @param {'manual'|'automatic'} [options.signingStyle] from {@link signingPlan}
 * @param {string[]} [options.authenticationArgs] App Store Connect auth flags
 * @returns {string[]} arguments for `xcodebuild`
 */
export function exportArchiveArgs({
  archivePath,
  exportOptions,
  ipaDir,
  hasApiKey,
  signingStyle,
  authenticationArgs = [],
}) {
  const canResolveProfiles = Boolean(hasApiKey) || signingStyle !== 'manual';
  return [
    '-exportArchive',
    '-archivePath',
    archivePath,
    '-exportOptionsPlist',
    exportOptions,
    '-exportPath',
    ipaDir,
    ...(canResolveProfiles ? ['-allowProvisioningUpdates', ...authenticationArgs] : []),
  ];
}
