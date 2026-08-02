import { describe, expect, it } from 'vitest';

import { exportArchiveArgs } from './ios-export.mjs';

const base = {
  archivePath: '/build/ios/Vis-1.2.3-4.xcarchive',
  exportOptions: '/build/ios/ExportOptions.plist',
  ipaDir: '/build/ios/export-1.2.3-4',
};
const authenticationArgs = [
  '-authenticationKeyPath',
  '/tmp/key.p8',
  '-authenticationKeyID',
  'KEY123',
  '-authenticationKeyIssuerID',
  'ISSUER123',
];

describe('exportArchiveArgs', () => {
  it('always names the archive, the plist and the export directory', () => {
    const args = exportArchiveArgs({ ...base, hasApiKey: false });
    expect(args.slice(0, 7)).toEqual([
      '-exportArchive',
      '-archivePath',
      base.archivePath,
      '-exportOptionsPlist',
      base.exportOptions,
      '-exportPath',
      base.ipaDir,
    ]);
  });

  // The release that broke: a pinned profile made the export sign manually, the
  // ExportOptions plist named a profile only for the app, and the share extension
  // has its own bundle id — so xcodebuild refused with `"VisShare.appex" requires
  // a provisioning profile`. CI always has the key, so this pairing must survive.
  it('lets CI resolve profiles it has not pinned, so an extension cannot block the export', () => {
    const args = exportArchiveArgs({
      ...base,
      hasApiKey: true,
      provisioningProfileName: 'Vis App Store',
      authenticationArgs,
    });
    expect(args).toContain('-allowProvisioningUpdates');
    expect(args.slice(args.indexOf('-allowProvisioningUpdates') + 1)).toEqual(authenticationArgs);
  });

  it('resolves profiles under automatic signing, where nothing is pinned at all', () => {
    const args = exportArchiveArgs({ ...base, hasApiKey: false });
    expect(args).toContain('-allowProvisioningUpdates');
    expect(args.filter((arg) => arg.startsWith('-authentication'))).toEqual([]);
  });

  it('keeps a keyless manual export offline, since it cannot reach the portal', () => {
    const args = exportArchiveArgs({
      ...base,
      hasApiKey: false,
      provisioningProfileName: 'Vis App Store',
    });
    expect(args).not.toContain('-allowProvisioningUpdates');
  });
});
