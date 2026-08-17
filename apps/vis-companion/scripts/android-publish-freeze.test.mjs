import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import {
  ANDROID_PUBLISH_FREEZE,
  androidPublishRefusal,
  assertAndroidPublishAllowed,
  freezeState,
} from './android-publish-freeze.mjs';

// The freeze is a switch that WILL be flipped, so the state itself is tested through explicit
// freeze objects: these cases keep meaning the same thing the day Android publishing resumes.
const appDir = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const repoRoot = resolve(appDir, '..', '..');
const read = (...parts) => readFileSync(join(...parts), 'utf8');
const script = (name) => join(appDir, 'scripts', name);

const frozen = { isFrozen: true, reason: 'the submitted build is with review', liftedBy: 'the release owner' };
const lifted = { ...frozen, isFrozen: false };

describe('the Android publish freeze', () => {
  it('refuses while frozen, naming the reason and who lifts it', () => {
    const refusal = androidPublishRefusal('publishing the app', frozen);
    expect(refusal).toContain('publishing the app is frozen');
    expect(refusal).toContain(frozen.reason);
    expect(refusal).toContain(frozen.liftedBy);
    expect(refusal).toContain('android-publish-freeze.mjs');
    expect(() => assertAndroidPublishAllowed('publishing the app', frozen)).toThrow(/is frozen/);
    expect(freezeState(frozen)).toBe('frozen');
  });

  it('permits everything once lifted', () => {
    expect(androidPublishRefusal('publishing the app', lifted)).toBeUndefined();
    expect(() => assertAndroidPublishAllowed('publishing the app', lifted)).not.toThrow();
    expect(freezeState(lifted)).toBe('allowed');
  });

  it('is ONE switch: the module state is the single word CI branches on', () => {
    expect(freezeState()).toBe(ANDROID_PUBLISH_FREEZE.isFrozen ? 'frozen' : 'allowed');
  });

  it('gates CI through a green step, never a red run', () => {
    const out = join(mkdtempSync(join(tmpdir(), 'android-freeze-')), 'github-output');
    writeFileSync(out, '');
    const res = spawnSync(process.execPath, [script('android-publish-freeze.mjs'), '--github-output'], {
      cwd: appDir,
      encoding: 'utf8',
      env: { ...process.env, GITHUB_OUTPUT: out, GITHUB_ACTIONS: 'true' },
    });
    expect(res.status).toBe(0);
    expect(readFileSync(out, 'utf8').trim()).toBe(`android=${freezeState()}`);
  });
});

describe('release:android:store', () => {
  it('asks the freeze before it builds anything', () => {
    const src = read(appDir, 'scripts', 'android-release.mjs');
    expect(src).toContain("from './android-publish-freeze.mjs'");
    expect(src.indexOf('androidPublishRefusal(')).toBeLessThan(src.indexOf("run('npm', ['run', 'build'])"));
  });

  it.runIf(ANDROID_PUBLISH_FREEZE.isFrozen)('refuses an upload run, and says how to build anyway', () => {
    const res = spawnSync(process.execPath, [script('android-release.mjs'), '--track', 'internal'], {
      cwd: appDir,
      encoding: 'utf8',
    });
    expect(res.status).toBe(1);
    expect(res.stderr).toMatch(/frozen/i);
    expect(res.stderr).toContain('--no-upload');
  });
});

describe('CI', () => {
  it('skips the whole Play job while frozen', () => {
    const workflow = read(repoRoot, '.github', 'workflows', 'mobile-release.yml');
    expect(workflow).toContain('node scripts/android-publish-freeze.mjs --github-output');
    expect(workflow).toContain('needs: android-gate');
    expect(workflow).toContain("needs.android-gate.outputs.android == 'allowed'");
  });

  it('skips Firebase App Distribution while frozen', () => {
    const workflow = read(repoRoot, '.github', 'workflows', 'android-companion.yml');
    expect(workflow).toContain('node scripts/android-publish-freeze.mjs --github-output');
    expect(workflow).toMatch(/Distribute to Firebase App Distribution[\s\S]*steps\.freeze\.outputs\.android == 'allowed'/);
  });
});
