import { execFileSync, spawnSync } from 'node:child_process';
import { cpSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');

describe('iOS live voice preparation', () => {
  it.each(['fresh', 'legacy', 'duplicate'])('installs and repairs the %s project and plugin registry', (shape) => {
    const temporary = mkdtempSync(join(tmpdir(), 'vis-ios-live-voice-'));
    const fixture = join(temporary, 'apps/vis-companion');
    const ios = join(fixture, 'ios');
    const app = join(ios, 'App/App');
    const script = join(fixture, 'scripts/ios-prepare.mjs');
    const config = join(app, 'capacitor.config.json');
    try {
      mkdirSync(ios, { recursive: true });
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
      writeFileSync(join(fixture, 'capacitor.config.json'), JSON.stringify({ appId: 'com.example.voice' }));
      execFileSync('tar', ['-xzf', join(root, 'node_modules/@capacitor/cli/assets/ios-pods-template.tar.gz'), '-C', ios]);
      writeFileSync(config, JSON.stringify({ packageClassList: ['ExistingPlugin'] }));
      const run = (...args) => spawnSync(process.execPath, [script, ...args], { encoding: 'utf8' });
      const first = run();
      expect(first.stderr).toBe('');
      expect(first.status).toBe(0);
      const projectPath = join(ios, 'App/App.xcodeproj/project.pbxproj');
      let project = readFileSync(projectPath, 'utf8');
      const objectId = (n) => `5A11E5${n.toString(16).toUpperCase().padStart(18, '0')}`;
      if (shape !== 'fresh') {
        // Keep a user signing setting and all unrelated entries through migration.
        project = project.replace('PRODUCT_NAME = "$(TARGET_NAME)";',
          'PRODUCT_NAME = "$(TARGET_NAME)";\n\t\t\t\tDEVELOPMENT_TEAM = USERTEAM;');
        expect(project).toContain('DEVELOPMENT_TEAM = USERTEAM;');
        const liveLines = project.split('\n').filter((line) =>
          line.includes('LiveVoice.swift') &&
          (line.includes(objectId(44)) || line.includes(objectId(45))));
        expect(liveLines).toHaveLength(4);
        const oldProject = project.split('\n').filter((line) =>
          !line.includes('PasteMenu.swift')).flatMap((line) => {
          if (!liveLines.includes(line)) return [line];
          const legacy = line.replaceAll(objectId(44), objectId(42))
            .replaceAll(objectId(45), objectId(43));
          return shape === 'duplicate' ? [line, legacy] : [legacy];
        }).join('\n');
        writeFileSync(projectPath, oldProject);
        expect(run('--check').status).not.toBe(0);
        expect(readFileSync(projectPath, 'utf8')).toBe(oldProject);
        const migrated = run();
        expect(migrated.stderr).toBe('');
        expect(migrated.status).toBe(0);
        const migratedProject = readFileSync(projectPath, 'utf8');
        const unrelated = (value) => value.split('\n').filter((line) =>
          !line.includes('LiveVoice.swift') && !line.includes('PasteMenu.swift')).join('\n');
        expect(unrelated(migratedProject)).toBe(unrelated(project));
        project = migratedProject;
      }
      const definitions = [...project.matchAll(/^\s*([A-F0-9]{24}) \/\*[^\n]*?\*\/ = \{/gm)];
      expect(new Set(definitions.map((entry) => entry[1])).size).toBe(definitions.length);
      for (const [name, ref, build] of [['PasteMenu', 42, 43], ['LiveVoice', 44, 45]]) {
        const source = `${name}.swift`;
        const files = [...project.matchAll(new RegExp(`([A-F0-9]{24}) /\\* ${source.replace('.', '\\.')} \\*/ = \\{([^}]+)\\};`, 'g'))];
        expect(files).toHaveLength(1);
        expect(files[0][1]).toBe(objectId(ref));
        expect(files[0][2]).toContain(`path = ${source};`);
        expect(files[0][2]).toContain('isa = PBXFileReference;');
        expect(project).toContain(`${objectId(build)} /* ${source} in Sources */ = {isa = PBXBuildFile; fileRef = ${objectId(ref)} /* ${source} */; };`);
        expect(project.match(new RegExp(`${objectId(build)} /\\* ${source.replace('.', '\\.')} in Sources \\*/,`, 'g'))).toHaveLength(1);
        expect(project.match(new RegExp(`${objectId(ref)} /\\* ${source.replace('.', '\\.')} \\*/,`, 'g'))).toHaveLength(1);
      }
      const source = readFileSync(join(app, 'LiveVoice.swift'), 'utf8');
      expect(source).toBe(
        readFileSync(join(root, 'native/live-voice/Sources/LiveVoice/LiveVoiceState.swift'), 'utf8') +
        '\n' + readFileSync(join(root, 'native/ios/LiveVoicePlugin.swift'), 'utf8'),
      );
      expect(project.match(/LiveVoice.swift in Sources \*\/ =/g)).toHaveLength(1);
      expect(project).toContain('path = LiveVoice.swift;');
      const plugins = JSON.parse(readFileSync(config, 'utf8')).packageClassList;
      expect(plugins).toEqual(expect.arrayContaining(['ExistingPlugin', 'LiveVoicePlugin', 'OAuthLoopbackPlugin', 'PasteMenuPlugin']));
      expect(run().status).toBe(0);
      expect(readFileSync(projectPath, 'utf8')).toBe(project);
      expect(run('--check').status).toBe(0);
      writeFileSync(config, JSON.stringify({ packageClassList: ['ExistingPlugin'] }));
      expect(run('--check').status).not.toBe(0);
      expect(run().status).toBe(0);
      expect(run('--check').status).toBe(0);
      writeFileSync(join(app, 'LiveVoice.swift'), '// Outdated source.\n');
      expect(run('--check').status).not.toBe(0);
      expect(run().status).toBe(0);
      expect(readFileSync(join(app, 'LiveVoice.swift'), 'utf8')).toBe(source);
    } finally { rmSync(temporary, { recursive: true, force: true }); }
  });
});
