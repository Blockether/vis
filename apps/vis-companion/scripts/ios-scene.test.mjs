import { execFileSync, spawnSync } from 'node:child_process';
import { cpSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');

// iOS 27 stops the app at launch when it has no scene life cycle:
// "UIScene life cycle is required for apps built with this SDK".
describe('iOS scene life cycle preparation', () => {
  it('gives a fresh and an older project the scene delegate and manifest', () => {
    const temporary = mkdtempSync(join(tmpdir(), 'vis-ios-scene-'));
    const fixture = join(temporary, 'apps/vis-companion');
    const ios = join(fixture, 'ios');
    const app = join(ios, 'App/App');
    const script = join(fixture, 'scripts/ios-prepare.mjs');
    try {
      mkdirSync(dirname(script), { recursive: true });
      cpSync(join(root, 'scripts/ios-prepare.mjs'), script);
      for (const path of [
        'native-assets/ios', 'native/ios', 'native/oauth/Sources',
        'native/shortcuts/Sources', 'native/live-voice/Sources',
      ]) {
        mkdirSync(dirname(join(fixture, path)), { recursive: true });
        cpSync(join(root, path), join(fixture, path), { recursive: true });
      }
      const schema = 'packages/vis-contract/resources/vis-contract/schema/gateway.json';
      mkdirSync(dirname(join(temporary, schema)), { recursive: true });
      cpSync(resolve(root, '../..', schema), join(temporary, schema));
      writeFileSync(join(fixture, 'capacitor.config.json'), JSON.stringify({ appId: 'com.example.scene' }));
      mkdirSync(ios, { recursive: true });
      execFileSync('tar', ['-xzf', join(root, 'node_modules/@capacitor/cli/assets/ios-pods-template.tar.gz'), '-C', ios]);
      const run = (...args) => spawnSync(process.execPath, [script, ...args], { encoding: 'utf8' });
      const plistPath = join(app, 'Info.plist');
      const delegatePath = join(app, 'AppDelegate.swift');
      const projectPath = join(ios, 'App/App.xcodeproj/project.pbxproj');

      const prepared = run();
      expect(prepared.stderr).toBe('');
      expect(prepared.status).toBe(0);
      const plist = readFileSync(plistPath, 'utf8');
      const manifest = /<key>UIApplicationSceneManifest<\/key>\s*<dict>([\s\S]*?)<\/array>/.exec(plist);
      expect(manifest).not.toBeNull();
      expect(manifest[1]).toMatch(/<key>UISceneDelegateClassName<\/key>\s*<string>\$\(PRODUCT_MODULE_NAME\)\.SceneDelegate<\/string>/);
      expect(manifest[1]).toMatch(/<key>UISceneStoryboardFile<\/key>\s*<string>Main<\/string>/);
      expect(plist).not.toContain('UIMainStoryboardFile');
      expect(readFileSync(join(app, 'SceneDelegate.swift'), 'utf8')).toBe(
        readFileSync(join(root, 'native/ios/SceneDelegate.swift'), 'utf8'),
      );
      const project = readFileSync(projectPath, 'utf8');
      expect(project.match(/SceneDelegate\.swift in Sources \*\/,/g)).toHaveLength(1);
      expect(project.match(/path = SceneDelegate\.swift;/g)).toHaveLength(1);
      const delegate = readFileSync(delegatePath, 'utf8');
      expect(delegate).not.toContain('endEditing');
      expect(run('--check').status).toBe(0);

      // A scene manifest with another delegate or storyboard stops the app at launch too.
      writeFileSync(plistPath, plist
        .replace('$(PRODUCT_MODULE_NAME).SceneDelegate', '$(PRODUCT_MODULE_NAME).WrongDelegate')
        .replace(/(<key>UISceneStoryboardFile<\/key>\s*<string>)Main(<\/string>)/, '$1Missing$2'));
      expect(run('--check').status).not.toBe(0);
      expect(run().status).toBe(0);
      expect(readFileSync(plistPath, 'utf8')).toBe(plist);

      // A project made before the scene life cycle: no scene delegate in the target,
      // an app-level storyboard and the keyboard stamp in the app delegate.
      const objectId = (n) => `5A11E5${n.toString(16).toUpperCase().padStart(18, '0')}`;
      writeFileSync(projectPath, project.split('\n').filter((line) => !line.includes('SceneDelegate.swift')).join('\n'));
      rmSync(join(app, 'SceneDelegate.swift'));
      writeFileSync(plistPath, plist.replace(
        /\t<key>UIApplicationSceneManifest<\/key>\n\t<dict>[\s\S]*?\n\t<\/dict>\n/,
        '\t<key>UIMainStoryboardFile</key>\n\t<string>Main</string>\n',
      ));
      writeFileSync(delegatePath, delegate.replace(
        '    func applicationWillResignActive(_ application: UIApplication) {\n',
        "    func applicationWillResignActive(_ application: UIApplication) {\n        // Release WebKit's editor before UIKit starts the scene transition.\n        window?.endEditing(true)\n",
      ));
      expect(readFileSync(delegatePath, 'utf8')).toContain('endEditing');
      expect(run('--check').status).not.toBe(0);
      expect(run().status).toBe(0);
      expect(readFileSync(delegatePath, 'utf8')).toBe(delegate);
      expect(readFileSync(plistPath, 'utf8')).not.toContain('UIMainStoryboardFile');
      expect(readFileSync(plistPath, 'utf8')).toContain('$(PRODUCT_MODULE_NAME).SceneDelegate');
      const migrated = readFileSync(projectPath, 'utf8');
      expect(migrated).toContain(`${objectId(67)} /* SceneDelegate.swift in Sources */,`);
      expect(migrated.match(/SceneDelegate\.swift in Sources \*\/,/g)).toHaveLength(1);
      expect(readFileSync(join(app, 'SceneDelegate.swift'), 'utf8')).toBe(
        readFileSync(join(root, 'native/ios/SceneDelegate.swift'), 'utf8'),
      );
      expect(run().status).toBe(0);
      expect(readFileSync(projectPath, 'utf8')).toBe(migrated);
      expect(run('--check').status).toBe(0);
    } finally { rmSync(temporary, { recursive: true, force: true }); }
  });
});
