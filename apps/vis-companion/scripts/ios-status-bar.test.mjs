import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

const here = dirname(fileURLToPath(import.meta.url));
const prepare = readFileSync(join(here, 'ios-prepare.mjs'), 'utf8');

describe('iOS status bar', () => {
  // Regression: the clock/search strip remained above the app on iPad and iPhone.
  // ios/ is generated; its plist must be stamped on every fresh build and existing install.
  it('hides the status bar before the web view paints and keeps it hidden under Capacitor', () => {
    const entries = prepare.slice(
      prepare.indexOf('const plistEntries = ['),
      prepare.indexOf('const currentPlist ='),
    );
    const stamp = prepare.slice(
      prepare.indexOf('const currentPlist ='),
      prepare.indexOf('const plistOk ='),
    );
    expect(entries).toContain('<key>UIStatusBarHidden</key>\n\\t<true/>');
    expect(stamp).toContain('UIViewControllerBasedStatusBarAppearance');
    expect(stamp).toContain('<false/>');
    expect(stamp).toContain('preparedPlist');
  });
});
