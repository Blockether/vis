import { afterAll, beforeAll, beforeEach, expect, test } from 'vitest';
import { readFileSync } from 'node:fs';
import { runtimeFixture } from './test-support.js';
import { moderationStatements, publicationStatements } from './moderate.mjs';
import { identity, releaseVersion } from './github.js';

let fixture,
  serial = 0,
  id;
const repository_url = 'https://github.com/example/extensions',
  subdirectory = 'plugins/greeting';
const manifest = readFileSync('examples/vis-greeter/pyproject.toml', 'utf8');
beforeAll(async () => {
  fixture = await runtimeFixture();
  id = await identity(repository_url + '\n' + subdirectory);
});
afterAll(async () => {
  await fixture?.runtime.dispose();
});
beforeEach(async () => {
  Object.assign(fixture.controls, {
    github: 'ok',
    verification: 'ok',
    requests: [],
    releases: [],
    manifest,
  });
  fixture.controls.refs.clear();
  fixture.controls.manifests.clear();
  fixture.controls.tokens.clear();
  fixture.controls.contents.clear();
  await fixture.db.batch(
    ['DELETE FROM submissions', 'DELETE FROM releases', 'DELETE FROM extensions'].map((sql) =>
      fixture.db.prepare(sql),
    ),
  );
  await fixture.runtime.purgeCache();
  publish('1.0.0', 'a');
});
function publish(version, digit, extra = {}) {
  const revision = digit.repeat(40),
    release_tag = extra.tag_name || 'v' + version;
  fixture.controls.releases.unshift({
    id: ++serial,
    tag_name: release_tag,
    sha: revision,
    draft: false,
    prerelease: version.includes('rc'),
    published_at: '2026-02-11T12:00:00Z',
    ...extra,
  });
  fixture.controls.manifests.set(
    revision,
    manifest.replace('version = "1.0.0"', 'version = "' + version + '"'),
  );
  return { release_tag, revision };
}
async function post(path, source = {}) {
  const token = (path === '/api/preview' ? 'preview' : 'submit') + '-' + ++serial;
  return fixture.runtime.dispatchFetch('https://center.example.com' + path, {
    method: 'POST',
    headers: {
      Origin: 'https://center.example.com',
      'Content-Type': 'application/json',
      'CF-Connecting-IP': '10.2.0.' + ((serial % 250) + 1),
    },
    body: JSON.stringify({ repository_url, subdirectory, turnstile_token: token, ...source }),
  });
}
const detail = async (version) => {
  const response = await fixture.runtime.dispatchFetch(
    'https://center.example.com/api/extensions/' +
      id +
      (version ? '?version=' + encodeURIComponent(version) : ''),
  );
  return { status: response.status, data: await response.json() };
};
async function moderate(action, reference) {
  for (const sql of moderationStatements(action, reference)) await fixture.db.prepare(sql).run();
  await fixture.runtime.purgeCache();
}
async function firstApproval() {
  const response = await post('/api/submissions', {
    release_tag: 'v1.0.0',
    revision: 'a'.repeat(40),
  });
  expect(response.status).toBe(202);
  await moderate('approve', (await response.json()).id);
}
async function scheduled() {
  const worker = await fixture.runtime.getWorker();
  return worker.scheduled({ cron: '*/5 * * * *', scheduledTime: Date.now() });
}
const pending = async (version) =>
  fixture.db
    .prepare("SELECT id FROM submissions WHERE json_extract(metadata,'$.version')=?")
    .bind(version)
    .first();

test('scheduled GitHub releases remain private until approved, and every approved version stays selectable', async () => {
  await firstApproval();
  const added = (await detail()).data.added_at;
  publish('1.1.0', 'b');
  publish('2.0.0rc1', 'c');
  await scheduled();
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM submissions').first()).n).toBe(2);
  expect((await detail('1.1.0')).status).toBe(404);
  expect((await detail()).data.releases.map((release) => release.version)).toEqual(['1.0.0']);
  await moderate('approve', (await pending('2.0.0rc1')).id);
  expect((await detail()).data.version).toBe('1.0.0');
  await moderate('approve', (await pending('1.1.0')).id);
  const current = (await detail()).data;
  expect(current.version).toBe('1.1.0');
  expect(current.added_at).toBe(added);
  expect(current.releases.map((release) => release.version)).toEqual([
    '1.1.0',
    '1.0.0',
    '2.0.0rc1',
  ]);
  expect(current.releases.every((release) => !Object.hasOwn(release, 'readme'))).toBe(true);
  const previous = (await detail('1.0.0')).data;
  expect(previous.revision).toBe('a'.repeat(40));
  expect(previous.latest_version).toBe('1.1.0');
  expect(previous.manifest_url).toContain('/blob/' + 'a'.repeat(40) + '/');
  const page = await fixture.runtime.dispatchFetch(
    'https://center.example.com/extensions/' + id + '?version=1.0.0',
  );
  const html = await page.text();
  expect(page.status).toBe(200);
  expect(html).toContain('--version &#39;1.0.0&#39;');
  expect(html).toContain('id="release-version"');
  expect(html).toContain('Release notes');
  expect(html).toContain('Source</a>');
});

test('moved tags and conflicting approvals cannot replace an approved version, even after an approval race', async () => {
  await firstApproval();
  const before = (await detail('1.0.0')).data;
  fixture.controls.refs.set('v1.0.0', { type: 'commit', sha: 'b'.repeat(40) });
  expect(
    (await post('/api/submissions', { release_tag: 'v1.0.0', revision: 'a'.repeat(40) })).status,
  ).toBe(400);
  expect(
    (await post('/api/submissions', { release_tag: 'v1.0.0', revision: 'b'.repeat(40) })).status,
  ).toBe(409);
  const reference = 'f'.repeat(24),
    metadata = { ...before, revision: 'b'.repeat(40) };
  await fixture.db
    .prepare('INSERT INTO submissions VALUES (?,?,?,?,?)')
    .bind(reference, id, metadata.revision, JSON.stringify(metadata), '2026-02-12T00:00:00Z')
    .run();
  await expect(moderate('approve', reference)).rejects.toThrow(/constraint/i);
  expect((await detail('1.0.0')).data.revision).toBe(before.revision);
  await moderate('reject', reference);
  expect(
    await fixture.db.prepare('SELECT id FROM submissions WHERE id=?').bind(reference).first(),
  ).toBeNull();
  expect((await detail('1.0.0')).data.revision).toBe(before.revision);
});

test('rejection is permanent for discovery and does not remove the current approved release', async () => {
  await firstApproval();
  publish('1.1.0', 'b');
  await scheduled();
  await moderate('reject', (await pending('1.1.0')).id);
  fixture.controls.requests = [];
  await scheduled();
  expect(await pending('1.1.0')).toBeNull();
  expect((await detail('1.1.0')).status).toBe(404);
  expect((await detail()).data.version).toBe('1.0.0');
  expect(fixture.controls.requests.some((url) => url.includes('/releases/tags/v1.1.0'))).toBe(
    false,
  );
});

test('numeric release order keeps a newer stable release when a backport is approved later', async () => {
  await firstApproval();
  publish('1.10.0', 'b');
  publish('1.9.0', 'c');
  await scheduled();
  await moderate('approve', (await pending('1.10.0')).id);
  await moderate('approve', (await pending('1.9.0')).id);
  expect((await detail()).data.version).toBe('1.10.0');
});

test('drafts, missing published releases, invalid tags and manifest mismatches are not admitted', async () => {
  fixture.controls.releases = [];
  expect((await post('/api/preview')).status).toBe(400);
  publish('1.0.0', 'a', { draft: true });
  expect((await post('/api/preview', { release_tag: 'v1.0.0' })).status).toBe(400);
  fixture.controls.releases = [];
  publish('1.0.0', 'a', { tag_name: 'v1.1.0' });
  expect((await post('/api/preview', { release_tag: 'v1.1.0' })).status).toBe(400);
  expect((await post('/api/preview', { release_tag: '../other' })).status).toBe(400);
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM submissions').first()).n).toBe(0);
});

test('monorepo release tags and annotated tags resolve only to the checked commit', async () => {
  publish('1.0.0', 'a', { tag_name: 'vis-greeter/v1.0.0' });
  fixture.controls.refs.set('vis-greeter/v1.0.0', { type: 'tag', sha: 'd'.repeat(40) });
  fixture.controls.refs.set('d'.repeat(40), { type: 'commit', sha: 'a'.repeat(40) });
  const response = await post('/api/preview', { release_tag: 'vis-greeter/v1.0.0' });
  const data = await response.json();
  expect(response.status).toBe(200);
  expect(data.revision).toBe('a'.repeat(40));
  expect(data.release_tag).toBe('vis-greeter/v1.0.0');
  expect(data.release_url).toContain('vis-greeter%2Fv1.0.0');
  fixture.controls.refs.set('d'.repeat(40), { type: 'tag', sha: 'd'.repeat(40) });
  expect((await post('/api/preview', { release_tag: 'vis-greeter/v1.0.0' })).status).toBe(400);
});

test('discovery is bounded, resumes older pages and records failures privately without losing current metadata', async () => {
  await firstApproval();
  publish('1.1.0', 'b');
  fixture.controls.releases.unshift(
    ...Array.from({ length: 20 }, (_, index) => ({ tag_name: 'other/v' + index, draft: false })),
  );
  await scheduled();
  expect(await pending('1.1.0')).toBeNull();
  expect((await fixture.db.prepare('SELECT page FROM release_sync').first()).page).toBe(2);
  fixture.controls.github = 'rate';
  await scheduled();
  const failure = await fixture.db.prepare('SELECT page,error FROM release_sync').first();
  expect(failure.page).toBe(2);
  expect(failure.error).toContain('rate-limited');
  expect((await detail()).data.version).toBe('1.0.0');
  expect((await detail()).data.error).toBeUndefined();
  fixture.controls.github = 'ok';
  await scheduled();
  expect(await pending('1.1.0')).not.toBeNull();
  expect(await fixture.db.prepare('SELECT page,error FROM release_sync').first()).toEqual({
    page: 1,
    error: null,
  });
});

test('schema initialization preserves reviewed history without approving old source snapshots', async () => {
  await firstApproval();
  const approved = (await detail('1.0.0')).data;
  const statements = readFileSync('schema.sql', 'utf8')
    .split(';')
    .filter((sql) => sql.trim());
  for (let n = 0; n < 2; n++)
    await fixture.db.batch(statements.map((sql) => fixture.db.prepare(sql)));
  expect((await detail('1.0.0')).data).toEqual(approved);
  await fixture.db.prepare('DELETE FROM releases').run();
  await fixture.db.batch(statements.map((sql) => fixture.db.prepare(sql)));
  expect((await detail()).data.releases).toEqual([]);
});

test('discovery inspects at most five new releases per tick and resumes within a page', async () => {
  await firstApproval();
  for (let n = 1; n <= 7; n++) publish('1.' + n + '.0', n.toString(16));
  await scheduled();
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM submissions').first()).n).toBe(5);
  expect(await fixture.db.prepare('SELECT page,position FROM release_sync').first()).toEqual({
    page: 1,
    position: 5,
  });
  await scheduled();
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM submissions').first()).n).toBe(7);
  expect(await fixture.db.prepare('SELECT page,position FROM release_sync').first()).toEqual({
    page: 1,
    position: 0,
  });
});

test('approval records review time, not submission time, and retains it on retry', async () => {
  const response = await post('/api/submissions', {
    release_tag: 'v1.0.0',
    revision: 'a'.repeat(40),
  });
  const result = await response.json();
  expect(response.status, JSON.stringify(result)).toBe(202);
  const reference = result.id;
  const submitted = '2020-01-01T00:00:00Z';
  await fixture.db
    .prepare('UPDATE submissions SET submitted_at=? WHERE id=?')
    .bind(submitted, reference)
    .run();
  const statements = moderationStatements('approve', reference);
  await fixture.db.prepare(statements[0]).run();
  const reviewed = (await fixture.db.prepare('SELECT reviewed_at FROM releases').first())
    .reviewed_at;
  expect(Date.parse(reviewed)).toBeGreaterThan(Date.parse(submitted));
  await moderate('approve', reference);
  expect((await detail()).data.releases[0].approved_at).toBe(reviewed);
});

test('canonical release versions sort like their Python version selectors', () => {
  const ordered = ['0.9.0', '1.0.0a1', '1.0.0b1', '1.0.0rc1', '1.0.0', '1.9.0', '1.10.0'];
  expect(
    [...ordered]
      .reverse()
      .sort((a, b) => releaseVersion(a).version_key.localeCompare(releaseVersion(b).version_key)),
  ).toEqual(ordered);
  for (const version of ['1.0', '01.0.0', 'v1.0.0', '1.0.0-rc.1', 'not-a-version'])
    expect(() => releaseVersion(version)).toThrow();
});

test('authenticated publication uses reviewed identities, handles quotes and remains idempotent', async () => {
  const preview = await post('/api/preview', { release_tag: 'v1.0.0' });
  const metadata = await preview.json();
  expect(preview.status).toBe(200);
  metadata.readme = "# Author's README";
  const statements = await publicationStatements(metadata);
  await fixture.db.batch(statements.map((sql) => fixture.db.prepare(sql)));
  expect((await detail()).data.readme).toBe(metadata.readme);
  await fixture.db.batch(statements.map((sql) => fixture.db.prepare(sql)));
  expect((await detail()).data.releases).toHaveLength(1);
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM submissions').first()).n).toBe(0);
  for (const change of [
    { revision: 'b'.repeat(40) },
    { name: 'other-name', version: '1.1.0', revision: 'c'.repeat(40) },
  ]) {
    const conflicting = await publicationStatements({ ...metadata, ...change });
    await expect(
      fixture.db.batch(conflicting.map((sql) => fixture.db.prepare(sql))),
    ).rejects.toThrow();
    expect((await detail()).data.revision).toBe('a'.repeat(40));
  }
});
