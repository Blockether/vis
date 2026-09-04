import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

const here = dirname(fileURLToPath(import.meta.url));
const prepare = readFileSync(join(here, 'ios-prepare.mjs'), 'utf8');
const between = (name) => prepare.split(`const ${name} = \``)[1]?.split('\n`;')[0] ?? '';
const host = between('hostSource');

// Regression, user report (paraphrased: installed on a MacBook, tapping the input
// shows a grey field where the keyboard would be): nothing the web view can measure
// tells a Mac window from an iPad, so the iOS project has to say it itself.
describe('iOS host plugin', () => {
  it('answers the one question only native code can', () => {
    expect(host).toContain('@objc(VisHostPlugin)');
    expect(host).toContain('public let jsName = "VisHost"');
    expect(host).toContain('CAPPluginMethod(name: "info", returnType: CAPPluginReturnPromise)');
    expect(host).toContain('ProcessInfo.processInfo.isiOSAppOnMac');
    expect(host).toContain('call.resolve(["isMac": isMac])');
  });

  it('compiles and registers the generated plugin in the app target', () => {
    expect(prepare).toContain('VisHost.swift in Sources');
    expect(prepare).toContain("'VisHostPlugin'");
    expect(prepare).toContain('const hostOk = hostFileOk && hostProjectOk && hostConfigOk;');
    expect(prepare).toContain('badgeOk && speechOk && hostOk');
  });
});
