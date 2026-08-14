// The blocked state's verb, and the one platform with a door to open.
//
// A permission turned off in the OS is the only notifications state this app
// cannot change from inside itself, so the panel offers the way out — but only
// where one exists. On iOS, Capacitor's navigation delegate hands a top-level
// navigation it cannot serve to `UIApplication.shared.open` (`@capacitor/ios`
// `WebViewDelegationHandler`), which lands on this app's own page in Settings.
// Android needs an Intent and has no URL for it, so nothing is offered there
// rather than a button that would quietly do nothing.
import { beforeEach, describe, expect, it, vi } from 'vitest';

import {
  canOpenSystemNotificationSettings,
  openSystemNotificationSettings,
} from './push';

let platform = 'ios';
let plugin = true;

vi.mock('@capacitor/core', () => ({
  Capacitor: {
    getPlatform: () => platform,
    isNativePlatform: () => platform !== 'web',
    isPluginAvailable: () => plugin,
  },
}));
vi.mock('@capacitor/push-notifications', () => ({ PushNotifications: {} }));

/** Stands in for the webview's own address bar. */
const location = { href: '' };

beforeEach(() => {
  platform = 'ios';
  plugin = true;
  location.href = '';
  vi.stubGlobal('window', { location });
});

describe('the door to system notification settings', () => {
  it('sends an iPhone to this app own page in Settings', () => {
    expect(canOpenSystemNotificationSettings()).toBe(true);
    openSystemNotificationSettings();
    expect(location.href).toBe('app-settings:');
  });

  it('offers nothing where the OS has no URL for it', () => {
    for (const other of ['android', 'web']) {
      platform = other;
      location.href = '';
      expect(canOpenSystemNotificationSettings()).toBe(false);
      openSystemNotificationSettings();
      expect(location.href).toBe('');
    }
  });

  it('offers nothing where push itself does not exist', () => {
    plugin = false;
    expect(canOpenSystemNotificationSettings()).toBe(false);
  });
});
