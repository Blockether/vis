import { afterAll, beforeAll, beforeEach, expect, test } from 'vitest';
import { readFileSync } from 'node:fs';
import { runtimeFixture } from './test-support.js';

let fixture;
const sample = JSON.parse(readFileSync('web/catalog.fixture.json', 'utf8'))[0];
beforeAll(async () => {
  fixture = await runtimeFixture();
});
afterAll(async () => {
  await fixture?.runtime.dispose();
});
beforeEach(async () => {
  Object.assign(fixture.controls, { github: 'ok', stars: 37, releases: [], requests: [] });
  await fixture.db.batch(
    ['DELETE FROM releases', 'DELETE FROM extensions'].map((sql) => fixture.db.prepare(sql)),
  );
  await fixture.runtime.purgeCache();
});
async function listing(n, repository = 'example/stars-' + n) {
  const item = {
    ...sample,
    id: n.toString(16).padStart(24, '0'),
    repository_url: 'https://github.com/' + repository,
    repository,
    stars: 12,
  };
  await fixture.db
    .prepare('INSERT INTO extensions VALUES (?,?,?)')
    .bind(item.id, JSON.stringify(item), item.added_at)
    .run();
  await fixture.db
    .prepare('INSERT INTO releases VALUES (?,?,?,?,?,?)')
    .bind(item.id, item.version, item.revision, JSON.stringify(item), 'approved', item.added_at)
    .run();
  return item;
}
async function tick() {
  const worker = await fixture.runtime.getWorker();
  await worker.scheduled({ cron: '*/5 * * * *', scheduledTime: Date.now() });
  await fixture.runtime.purgeCache();
}
const get = async (path) =>
  (await fixture.runtime.dispatchFetch('https://center.example.com' + path)).json();
const repositoryRequests = () =>
  fixture.controls.requests.filter((url) =>
    /^https:\/\/api.github.com\/repos\/[^/]+\/[^/?]+$/.test(url),
  );

test('GitHub stars refresh without a release and overlay every public version, not reviewed metadata', async () => {
  const item = await listing(1);
  await tick();
  expect((await get('/api/extensions')).extensions[0].stars).toBe(37);
  const detail = await get('/api/extensions/' + item.id + '?version=' + item.version);
  expect(detail.stars).toBe(37);
  expect(detail.releases[0].stars).toBe(37);
  expect(Date.parse(detail.stars_checked_at)).not.toBeNaN();
  const reviewed = await fixture.db
    .prepare('SELECT metadata FROM releases WHERE extension_id=?')
    .bind(item.id)
    .first();
  expect(JSON.parse(reviewed.metadata).stars).toBe(12);
  fixture.controls.requests = [];
  await get('/api/extensions');
  await tick();
  expect(repositoryRequests()).toHaveLength(0);
});

test('one repository serves multiple package folders and zero stars is a real value', async () => {
  await listing(2, 'Example/shared');
  await listing(3, 'example/shared');
  fixture.controls.stars = 0;
  await tick();
  expect((await get('/api/extensions')).extensions.map((item) => item.stars)).toEqual([0, 0]);
  expect(repositoryRequests()).toHaveLength(1);
});

test('rate limits preserve the last successful snapshot and retries are bounded', async () => {
  const item = await listing(4);
  await tick();
  await fixture.db.prepare("UPDATE repository_stats SET attempted_at='2000-01-01T00:00:00Z'").run();
  fixture.controls.github = 'rate';
  await tick();
  expect((await get('/api/extensions/' + item.id)).stars).toBe(37);
  const failed = await fixture.db
    .prepare('SELECT error FROM repository_stats WHERE repository_url=?')
    .bind(item.repository_url)
    .first();
  expect(failed.error).toContain('rate-limited');
  fixture.controls.requests = [];
  await tick();
  expect(repositoryRequests()).toHaveLength(0);
});

test('refresh work is capped and oldest repositories rotate across ticks', async () => {
  for (let n = 10; n < 17; n++) await listing(n);
  await tick();
  expect(repositoryRequests()).toHaveLength(5);
  fixture.controls.requests = [];
  await tick();
  expect(repositoryRequests()).toHaveLength(2);
  expect((await get('/api/extensions')).extensions.every((item) => item.stars === 37)).toBe(true);
});

test.each(['private', 'invalid'])('%s GitHub responses never invent a count', async (mode) => {
  const item = await listing(mode === 'private' ? 20 : 21);
  if (mode === 'private') fixture.controls.github = 'private';
  else fixture.controls.stars = -1;
  await tick();
  expect((await get('/api/extensions/' + item.id)).stars).toBe(12);
  expect((await get('/api/extensions/' + item.id)).stars_checked_at).toBeNull();
});
