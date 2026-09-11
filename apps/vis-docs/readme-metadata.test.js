import { afterAll, beforeAll, beforeEach, expect, test } from 'vitest';
import { runtimeFixture } from './test-support.js';
import { moderationStatements } from './moderate.mjs';
let fixture,
  serial = 0;
beforeAll(async () => {
  fixture = await runtimeFixture();
});
afterAll(async () => {
  await fixture?.runtime.dispose();
});
beforeEach(async () => {
  fixture.controls.contents.clear();
  fixture.controls.requests = [];
  fixture.controls.tokens.clear();
  await fixture.db.batch(
    ['DELETE FROM submissions', 'DELETE FROM extensions'].map((sql) => fixture.db.prepare(sql)),
  );
  await fixture.runtime.purgeCache();
});
const post = (path) =>
  fixture.runtime.dispatchFetch('https://center.example.com' + path, {
    method: 'POST',
    headers: {
      Origin: 'https://center.example.com',
      'Content-Type': 'application/json',
      'CF-Connecting-IP': '10.8.0.' + ++serial,
    },
    body: JSON.stringify({
      repository_url: 'https://github.com/example/repository',
      subdirectory: 'plugins/greeting',
      revision: fixture.revision,
      turnstile_token: (path === '/api/preview' ? 'preview' : 'submit') + '-' + serial,
    }),
  });
test('approved README is pinned and rendered on details, not duplicated through catalog summaries', async () => {
  const pending = await (await post('/api/submissions')).json();
  for (const sql of moderationStatements('approve', pending.id))
    await fixture.db.prepare(sql).run();
  const summaries = await (
    await fixture.runtime.dispatchFetch('https://center.example.com/api/extensions')
  ).json();
  const id = summaries.extensions[0].id;
  expect(summaries.extensions[0].readme).toBeUndefined();
  expect(summaries.extensions[0].description).toBeTruthy();
  const detail = await (
    await fixture.runtime.dispatchFetch('https://center.example.com/api/extensions/' + id)
  ).json();
  expect(detail.readme).toContain('# Fixture README');
  expect(detail.readme_url).toContain(fixture.revision);
  const html = await (
    await fixture.runtime.dispatchFetch('https://center.example.com/extensions/' + id)
  ).text();
  expect(html).toContain('<h3>Fixture README</h3>');
  expect(html).toContain('id="feedback"');
  expect(
    fixture.controls.requests
      .filter((url) => url.includes('/contents/'))
      .every((url) => url.endsWith('?ref=' + fixture.revision)),
  ).toBe(true);
});
test.each([
  { type: 'symlink', encoding: 'base64', size: 1, content: 'eA==' },
  { type: 'file', encoding: 'base64', size: 131073, content: 'eA==' },
  { type: 'file', encoding: 'base64', size: 1, content: Buffer.alloc(131073).toString('base64') },
  { type: 'file', encoding: 'base64', size: 1, content: Buffer.from([255]).toString('base64') },
])('README must be a bounded regular UTF-8 file: case %#', async (file) => {
  fixture.controls.contents.set('plugins/greeting/README.md', file);
  const result = await post('/api/preview');
  expect(result.status).toBe(400);
  expect((await result.json()).error).toMatch(/README|UTF-8/);
});
