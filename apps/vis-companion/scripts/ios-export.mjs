// Argument construction for `xcodebuild -exportArchive`.
//
// This lives apart from `ios-release.mjs` because that script is a top-level
// side-effecting release runner: importing it starts a release, so the one
// decision that has already broken a shipping build cannot be tested in place.

/**
 * Build the `xcodebuild -exportArchive` argument list.
 *
 * The subtlety is `-allowProvisioningUpdates`. With `signingStyle` = manual the
 * ExportOptions plist must name a profile for EVERY bundle inside the archive,
 * and the share extension carries its own bundle id (`<app>.share`) with no
 * profile secret of its own — so an export without this flag fails with
 * `"VisShare.appex" requires a provisioning profile` even though the archive
 * step, which always passes the flag, had just minted that profile itself.
 *
 * Passing it lets xcodebuild resolve whatever the plist does not pin, so the app
 * keeps its named profile and any further target we add resolves itself instead
 * of demanding a new secret. It needs App Store Connect authentication to reach
 * the portal, hence the pairing with `authenticationArgs`; automatic signing
 * (no named profile) relies on the Xcode-resident account instead.
 *
 * @param {object} options
 * @param {string} options.archivePath path of the `.xcarchive` to export
 * @param {string} options.exportOptions path of the ExportOptions plist
 * @param {string} options.ipaDir directory to write the `.ipa` into
 * @param {boolean} options.hasApiKey whether an App Store Connect key is configured
 * @param {string} [options.provisioningProfileName] pinned profile, when signing manually
 * @param {string[]} [options.authenticationArgs] App Store Connect auth flags
 * @returns {string[]} arguments for `xcodebuild`
 */
export function exportArchiveArgs({
  archivePath,
  exportOptions,
  ipaDir,
  hasApiKey,
  provisioningProfileName,
  authenticationArgs = [],
}) {
  const canResolveProfiles = Boolean(hasApiKey) || !provisioningProfileName;
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
