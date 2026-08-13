import { describe, expect, it } from 'vitest';
import { configureAndroidPushPlugin, PUSH_PLUGIN } from './android-push-plugin.mjs';

// Regression: Android builds without google-services.json exposed the push plugin,
// whose register() call crashed the process because Firebase was not initialized.
describe('Android push plugin availability', () => {
  const app = { pkg: '@capacitor/app', classpath: 'AppPlugin' };

  it('removes push when Firebase is absent and restores it when present', () => {
    expect(configureAndroidPushPlugin([app, PUSH_PLUGIN], false)).toEqual([app]);
    expect(configureAndroidPushPlugin([app], true)).toEqual([app, PUSH_PLUGIN]);
  });
});
