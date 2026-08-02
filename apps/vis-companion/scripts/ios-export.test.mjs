import { describe, expect, it } from 'vitest';

import { exportArchiveArgs, exportOptionsPlist, signingPlan } from './ios-export.mjs';

const appId = 'com.blockether.viscompanion';
const shareId = `${appId}.share`;

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

describe('signingPlan', () => {
  // The release that broke: a pinned profile made the export sign manually, the
  // ExportOptions plist named a profile only for the app, and the share extension
  // has its own bundle id — so xcodebuild refused with `"VisShare.appex" requires
  // a provisioning profile`. Manual signing is all-or-nothing, so a half-pinned
  // archive must export the way it was signed: automatically.
  it('refuses to sign manually while a shipped bundle has no profile', () => {
    const plan = signingPlan({
      bundleIds: [appId, shareId],
      profileNames: { [appId]: 'Vis App Store' },
    });
    expect(plan.signingStyle).toBe('automatic');
    expect(plan.provisioningProfiles).toEqual({});
    expect(plan.unnamed).toEqual([shareId]);
  });

  it('pins every bundle once every bundle is named', () => {
    const plan = signingPlan({
      bundleIds: [appId, shareId],
      profileNames: { [appId]: 'Vis App Store', [shareId]: 'Vis Share App Store' },
    });
    expect(plan.signingStyle).toBe('manual');
    expect(plan.provisioningProfiles).toEqual({
      [appId]: 'Vis App Store',
      [shareId]: 'Vis Share App Store',
    });
    expect(plan.unnamed).toEqual([]);
  });

  it('treats a blank secret as no profile at all', () => {
    const plan = signingPlan({ bundleIds: [appId], profileNames: { [appId]: '  ' } });
    expect(plan.signingStyle).toBe('automatic');
    expect(plan.unnamed).toEqual([appId]);
  });

  it('signs automatically when nothing is pinned', () => {
    const plan = signingPlan({ bundleIds: [appId, shareId] });
    expect(plan.signingStyle).toBe('automatic');
    expect(plan.unnamed).toEqual([appId, shareId]);
  });
});

describe('exportOptionsPlist', () => {
  it('names a profile for every bundle of a manual export', () => {
    const plist = exportOptionsPlist({
      teamId: 'TEAM123',
      ...signingPlan({
        bundleIds: [appId, shareId],
        profileNames: { [appId]: 'Vis App Store', [shareId]: 'Vis Share App Store' },
      }),
    });
    expect(plist).toContain('<key>signingStyle</key>\n\t<string>manual</string>');
    expect(plist).toContain(`<key>${appId}</key>\n\t\t<string>Vis App Store</string>`);
    expect(plist).toContain(`<key>${shareId}</key>\n\t\t<string>Vis Share App Store</string>`);
    expect(plist).toContain('<key>teamID</key>\n\t<string>TEAM123</string>');
  });

  it('leaves an automatic export without a profile dictionary to fall short of', () => {
    const plist = exportOptionsPlist({
      teamId: 'TEAM123',
      ...signingPlan({ bundleIds: [appId, shareId], profileNames: { [appId]: 'Vis App Store' } }),
    });
    expect(plist).toContain('<key>signingStyle</key>\n\t<string>automatic</string>');
    expect(plist).not.toContain('provisioningProfiles');
    expect(plist).not.toContain('Vis App Store');
  });

  it('always exports for App Store Connect, and stays a parseable plist', () => {
    const plist = exportOptionsPlist({ teamId: 'TEAM123', signingStyle: 'automatic' });
    expect(plist.startsWith('<?xml version="1.0" encoding="UTF-8"?>\n')).toBe(true);
    expect(plist).toContain('<key>method</key>\n\t<string>app-store-connect</string>');
    expect(plist.trimEnd().endsWith('</plist>')).toBe(true);
  });

  it('escapes markup rather than emitting a broken plist', () => {
    const plist = exportOptionsPlist({
      teamId: 'TEAM123',
      signingStyle: 'manual',
      provisioningProfiles: { [appId]: 'Ben & Co <Store>' },
    });
    expect(plist).toContain('<string>Ben &amp; Co &lt;Store&gt;</string>');
  });
});

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

  // CI always has the key, and an automatic export has to reach the portal to
  // resolve the profiles the plist deliberately does not pin.
  it('lets an automatic export resolve profiles through App Store Connect', () => {
    const args = exportArchiveArgs({
      ...base,
      hasApiKey: true,
      signingStyle: 'automatic',
      authenticationArgs,
    });
    expect(args).toContain('-allowProvisioningUpdates');
    expect(args.slice(args.indexOf('-allowProvisioningUpdates') + 1)).toEqual(authenticationArgs);
  });

  it('resolves profiles with no key, where the Xcode-resident account signs', () => {
    const args = exportArchiveArgs({ ...base, hasApiKey: false, signingStyle: 'automatic' });
    expect(args).toContain('-allowProvisioningUpdates');
    expect(args.filter((arg) => arg.startsWith('-authentication'))).toEqual([]);
  });

  it('keeps a keyless manual export offline, since it names every profile itself', () => {
    const args = exportArchiveArgs({ ...base, hasApiKey: false, signingStyle: 'manual' });
    expect(args).not.toContain('-allowProvisioningUpdates');
  });
});
