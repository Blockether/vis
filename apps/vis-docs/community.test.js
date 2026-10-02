import { afterAll, beforeAll, beforeEach, expect, test } from 'vitest';
import { runtimeFixture } from './test-support.js';
import { moderationStatements } from './moderate.mjs';

const id = '1'.repeat(24),
  other = '2'.repeat(24),
  origin = 'https://center.example.com';
let fixture,
  serial = 0,
  address = 0;
beforeAll(async () => {
  fixture = await runtimeFixture();
});
afterAll(async () => {
  await fixture?.runtime.dispose();
});
beforeEach(async () => {
  address++;
  fixture.controls.verification = 'ok';
  fixture.controls.tokens.clear();
  await fixture.db.batch(['DELETE FROM extensions'].map((sql) => fixture.db.prepare(sql)));
  for (const key of [id, other])
    await fixture.db
      .prepare('INSERT INTO extensions VALUES (?, ?, ?)')
      .bind(key, JSON.stringify({ id: key, name: 'fixture' }), new Date().toISOString())
      .run();
});
const ip = () => `10.2.${address}.5`;
const get = (key = id, query = '') =>
  fixture.runtime.dispatchFetch(`${origin}/api/extensions/${key}/community${query}`, {
    headers: { 'CF-Connecting-IP': ip() },
  });
const post = (suffix, body = {}, headers = {}) => {
  const action =
    suffix === 'comments'
      ? 'extension-comment'
      : suffix === 'vote'
        ? 'extension-vote'
        : 'comment-vote';
  return fixture.runtime.dispatchFetch(`${origin}/api/extensions/${id}/${suffix}`, {
    method: 'POST',
    headers: {
      Origin: origin,
      'Content-Type': 'application/json',
      'CF-Connecting-IP': ip(),
      ...headers,
    },
    body: JSON.stringify({ turnstile_token: action + '-' + ++serial, ...body }),
  });
};
const approve = async (reference) => {
  for (const sql of moderationStatements('approve-comment', String(reference)))
    await fixture.db.prepare(sql).run();
};
const download = (headers = {}, key = id) =>
  fixture.runtime.dispatchFetch(`${origin}/api/extensions/${key}/downloads`, {
    method: 'POST',
    headers: { 'User-Agent': 'Vis-Extension-Installer', 'CF-Connecting-IP': ip(), ...headers },
  });
const downloads = async (key = id) =>
  (
    await fixture.db
      .prepare('SELECT downloads FROM download_counts WHERE extension_id=?')
      .bind(key)
      .first()
  )?.downloads ?? 0;

test('community starts empty, private and bound to a listed package', async () => {
  const response = await get();
  expect(response.status).toBe(200);
  expect(response.headers.get('cache-control')).toBe('no-store');
  expect(await response.json()).toMatchObject({
    votes: { up: 0, down: 0, score: 0, own: 0 },
    comments: [],
    next: null,
  });
  expect((await get('3'.repeat(24))).status).toBe(404);
});
test('comments require moderation, carry no verified identity and omit private voter keys', async () => {
  const response = await post('comments', { name: ' Example ', body: ' Useful package. ' });
  expect(response.status).toBe(202);
  const comment = await response.json();
  expect(comment.status).toBe('pending');
  expect((await (await get()).json()).comments).toEqual([]);
  await approve(comment.id);
  const text = await (await get()).text(),
    data = JSON.parse(text);
  expect(data.comments).toHaveLength(1);
  expect(data.comments[0]).toMatchObject({
    name: 'Example',
    body: 'Useful package.',
    votes: { up: 0, down: 0, score: 0, own: 0 },
  });
  expect(text).not.toMatch(/voter|server-only|10\.2\.|verified/);
  expect((await (await get(other)).json()).comments).toEqual([]);
  for (const sql of moderationStatements('reject-comment', String(comment.id)))
    await fixture.db.prepare(sql).run();
  expect((await (await get()).json()).comments).toEqual([]);
});
test('package votes are one reversible choice per network address, not repeat increments', async () => {
  for (let n = 0; n < 2; n++) expect((await post('vote', { value: 1 })).status).toBe(200);
  expect((await (await get()).json()).votes).toEqual({ up: 1, down: 0, score: 1, own: 1 });
  await post('vote', { value: -1 });
  expect((await (await get()).json()).votes).toEqual({ up: 0, down: 1, score: -1, own: -1 });
  await post('vote', { value: 1 }, { 'CF-Connecting-IP': '10.5.0.5' });
  expect((await (await get()).json()).votes).toEqual({ up: 1, down: 1, score: 0, own: -1 });
  await post('vote', { value: 0 });
  expect((await (await get()).json()).votes).toEqual({ up: 1, down: 0, score: 1, own: 0 });
});
test('comment votes target only approved comments of that package', async () => {
  const comment = await (
    await post('comments', { name: 'Reader', body: 'Helpful documentation.' })
  ).json();
  expect((await post(`comments/${comment.id}/vote`, { value: 1 })).status).toBe(404);
  await approve(comment.id);
  expect((await post(`comments/${comment.id}/vote`, { value: 1 })).status).toBe(200);
  await post(`comments/${comment.id}/vote`, { value: 1 });
  expect((await (await get()).json()).comments[0].votes).toEqual({
    up: 1,
    down: 0,
    score: 1,
    own: 1,
  });
  await post(`comments/${comment.id}/vote`, { value: 0 });
  expect((await (await get()).json()).comments[0].votes.score).toBe(0);
  await fixture.db
    .prepare('UPDATE comments SET extension_id=? WHERE id=?')
    .bind(other, comment.id)
    .run();
  expect((await post(`comments/${comment.id}/vote`, { value: 1 })).status).toBe(404);
});
test('write guard rejects wrong origin, action, reused tokens and content before storing', async () => {
  expect((await post('vote', { value: 1 }, { Origin: 'https://other.example.com' })).status).toBe(
    403,
  );
  expect((await post('vote', { value: 1 }, { 'Content-Type': 'text/plain' })).status).toBe(415);
  expect(
    (await post('vote', { value: 1, turnstile_token: 'extension-comment-wrong' })).status,
  ).toBe(403);
  expect((await post('vote', { value: 1, turnstile_token: 'extension-vote-once' })).status).toBe(
    200,
  );
  expect((await post('vote', { value: 1, turnstile_token: 'extension-vote-once' })).status).toBe(
    403,
  );
  fixture.controls.verification = 'hostname';
  expect((await post('vote', { value: 1 })).status).toBe(403);
});
test('input is bounded and voting accepts only numeric -1, 0 or 1', async () => {
  for (const value of [2, '1', true, null])
    expect((await post('vote', { value })).status).toBe(400);
  expect((await post('comments', { name: '', body: 'text' })).status).toBe(400);
  expect((await post('comments', { name: 'Reader', body: 'x'.repeat(2001) })).status).toBe(400);
  expect((await post('comments', { name: 'Reader', body: 'x', status: 'approved' })).status).toBe(
    400,
  );
  expect((await post('comments', { name: 'Reader', body: 'x'.repeat(17000) })).status).toBe(413);
});
test('pending and rejected comments both count toward the daily anti-spam limit', async () => {
  for (let n = 0; n < 5; n++)
    expect((await post('comments', { name: 'Reader', body: 'Comment ' + n })).status).toBe(202);
  expect((await post('comments', { name: 'Reader', body: 'Too many' })).status).toBe(429);
});
test('pagination is bounded, stable and excludes pending rows', async () => {
  for (let n = 0; n < 52; n++)
    await fixture.db
      .prepare(
        "INSERT INTO comments (extension_id,voter,name,body,status,created_at) VALUES (?, 'fixture', 'Reader', ?, 'approved', ?)",
      )
      .bind(id, String(n), new Date().toISOString())
      .run();
  const first = await (await get()).json();
  expect(first.comments).toHaveLength(50);
  expect(first.next).toBeTruthy();
  const second = await (await get(id, '?before=' + first.next)).json();
  expect(second.comments).toHaveLength(2);
  expect(second.next).toBeNull();
  expect(new Set([...first.comments, ...second.comments].map((row) => row.id)).size).toBe(52);
  expect((await get(id, '?before=-1')).status).toBe(400);
});
test('deleting a listing removes its comments, votes and download counts', async () => {
  await post('vote', { value: 1 });
  await post('comments', { name: 'Reader', body: 'Feedback' });
  await download();
  await fixture.db.prepare('DELETE FROM extensions WHERE id=?').bind(id).run();
  for (const table of [
    'comments',
    'package_votes',
    'comment_votes',
    'download_counts',
    'download_marks',
  ])
    expect((await fixture.db.prepare(`SELECT COUNT(*) AS n FROM ${table}`).first()).n).toBe(0);
});
test('installer downloads count once per network and UTC day without storing addresses', async () => {
  const first = await download();
  expect(first.status).toBe(204);
  expect(first.headers.get('cache-control')).toBe('no-store');
  expect(await first.text()).toBe('');
  expect((await download()).status).toBe(204);
  expect(await downloads()).toBe(1);
  expect((await download({ 'CF-Connecting-IP': '10.9.9.9' })).status).toBe(204);
  expect(await downloads()).toBe(2);
  expect(await downloads(other)).toBe(0);
  await post('vote', { value: 1 });
  const voter = (await fixture.db.prepare('SELECT voter FROM package_votes').first()).voter;
  const { results } = await fixture.db.prepare('SELECT day,network FROM download_marks').all();
  expect(results).toHaveLength(2);
  for (const mark of results) {
    expect(mark.day).toBe(new Date().toISOString().slice(0, 10));
    expect(mark.network).toMatch(/^[0-9a-f]{64}$/);
    expect(mark.network).not.toBe(voter);
  }
  expect(JSON.stringify(results)).not.toContain('10.');
});
test('download reports refuse browsers and unlisted packages', async () => {
  expect((await download({ Origin: origin })).status).toBe(403);
  expect((await download({}, '3'.repeat(24))).status).toBe(404);
  expect(await downloads()).toBe(0);
});
