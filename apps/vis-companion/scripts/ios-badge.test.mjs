import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// `ios-prepare.mjs` stamps the generated Xcode project the moment it is
// imported, so what it embeds is read as text instead.
const here = dirname(fileURLToPath(import.meta.url));
const prepare = readFileSync(join(here, 'ios-prepare.mjs'), 'utf8');
const release = readFileSync(join(here, 'ios-release.mjs'), 'utf8');
const between = (name) => prepare.split(`const ${name} = \``)[1]?.split('\n`;')[0] ?? '';
const service = between('notifyServiceSource');
const plist = between('notifyPlistSource');
const badge = between('badgeSource');

describe('VisNotify badge extension', () => {
  // iOS paints the badge from `aps.badge` alone, and that number is ABSOLUTE —
  // APNs never increments anything. This phone is paired with several gateways,
  // each knowing only its own sessions, so no server can supply the total: the
  // count is decided on the device, inside every arriving alert.
  it('counts the alerts still waiting, plus the one arriving', () => {
    expect(service).toContain('class NotificationService: UNNotificationServiceExtension');
    expect(service).toContain('getDeliveredNotifications');
    expect(service).toContain('content.badge = NSNumber(value: delivered.count + 1)');
  });

  // iOS gives a service extension seconds and then delivers whatever it holds.
  // Without this the alert itself would be lost, not just the count.
  it('still delivers the alert when the time runs out', () => {
    expect(service).toContain('override func serviceExtensionTimeWillExpire()');
    expect(service).toContain('handler(pending)');
  });

  it('registers as a notification service extension', () => {
    expect(plist).toContain('<string>com.apple.usernotifications.service</string>');
    expect(plist).toContain('$(PRODUCT_MODULE_NAME).NotificationService');
    expect(plist).toContain('<string>XPC!</string>');
  });

  // An extension only ever runs when the payload asks for it; `push.clj` and
  // `relay.clj` send `mutable-content`, and this is the target that answers.
  it('is stamped as a second target the app embeds', () => {
    expect(prepare).toContain("const notifyTarget = { name: 'VisNotify', bundleSuffix: 'notify' };");
    expect(prepare).toContain('VisNotify.appex in Embed Foundation Extensions');
    expect(prepare).toContain('extensionSettings(notifyTarget, notifyIds.debug');
    expect(prepare).toContain('extensionSettings(notifyTarget, notifyIds.release');
  });

  it('signs the extension along with the app', () => {
    expect(release).toContain('const notifyBundleId = `${appBundleId}.notify`;');
    expect(release.match(/bundleIds: \[appBundleId, shareBundleId, notifyBundleId\]/g)?.length).toBe(2);
  });
});

describe('VisBadge plugin', () => {
  // The app half: the only API that moves the icon badge from inside the app.
  it('sets an absolute count through UNUserNotificationCenter', () => {
    expect(badge).toContain('@objc(VisBadgePlugin)');
    expect(badge).toContain('public let jsName = "VisBadge"');
    expect(badge).toContain('UNUserNotificationCenter.current().setBadgeCount(count)');
    expect(badge).toContain('let count = max(0, call.getInt("count") ?? 0)');
  });

  // Capacitor only loads a plugin named in `packageClassList`; without this the
  // bridge answers "not implemented" and the app half silently does nothing.
  it('is registered with the Capacitor bridge', () => {
    expect(prepare).toContain("packageClassList = [...(capConfigJson.packageClassList ?? []), 'VisBadgePlugin']");
    expect(prepare).toContain("(capConfigJson.packageClassList ?? []).includes('VisBadgePlugin')");
  });

  // `--check` is what CI runs; a host missing the badge pieces must fail it,
  // exactly as a host missing the share extension does.
  it('is part of what --check refuses to pass without', () => {
    expect(prepare).toContain('delegateOk && boardOk && plistOk && appIconOk && shareOk && badgeOk');
    expect(prepare).toContain('const badgeOk = notifyFilesOk && notifyProjectOk && capConfigOk;');
  });
});
