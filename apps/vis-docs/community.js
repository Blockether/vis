import { RequestError } from './github.js';
import { protectedBody } from './antispam.js';

// Network addresses never enter D1 or public responses. This is abuse mitigation,
// not person-level authentication: shared networks share a vote; addresses can change.
async function voterKey(request, env) {
  const address = request.headers.get('CF-Connecting-IP');
  if (!address || !env.TURNSTILE_SECRET_KEY) return '';
  const encoder = new TextEncoder();
  const key = await crypto.subtle.importKey(
    'raw',
    encoder.encode(env.TURNSTILE_SECRET_KEY),
    { name: 'HMAC', hash: 'SHA-256' },
    false,
    ['sign'],
  );
  const digest = await crypto.subtle.sign(
    'HMAC',
    key,
    encoder.encode('extension-community\n' + address),
  );
  return [...new Uint8Array(digest)].map((byte) => byte.toString(16).padStart(2, '0')).join('');
}
const votes = (row) => ({
  up: row.up || 0,
  down: row.down || 0,
  score: (row.up || 0) - (row.down || 0),
  own: row.own || 0,
});
const aggregate =
  'COALESCE(SUM(value=1),0) AS up, COALESCE(SUM(value=-1),0) AS down, COALESCE(MAX(CASE WHEN voter=? THEN value END),0) AS own';

export async function readCommunity(request, env, id) {
  if (!(await env.DB.prepare('SELECT id FROM extensions WHERE id=?').bind(id).first()))
    throw new RequestError('Repository not listed.', 404);
  const cursor = new URL(request.url).searchParams.get('before');
  if (
    cursor !== null &&
    (!/^[1-9][0-9]{0,15}$/.test(cursor) || !Number.isSafeInteger(Number(cursor)))
  )
    throw new RequestError('Invalid comment cursor.');
  const voter = await voterKey(request, env);
  const rating = await env.DB.prepare(`SELECT ${aggregate} FROM package_votes WHERE extension_id=?`)
    .bind(voter, id)
    .first();
  const { results } = await env.DB.prepare(
    `SELECT c.id,c.name,c.body,c.created_at,
    COALESCE(SUM(v.value=1),0) AS up,COALESCE(SUM(v.value=-1),0) AS down,
    COALESCE(MAX(CASE WHEN v.voter=? THEN v.value END),0) AS own
    FROM comments c LEFT JOIN comment_votes v ON v.comment_id=c.id
    WHERE c.extension_id=? AND c.status='approved' AND c.id<?
    GROUP BY c.id ORDER BY c.id DESC LIMIT 51`,
  )
    .bind(voter, id, cursor ? Number(cursor) : Number.MAX_SAFE_INTEGER)
    .all();
  const comments = results.slice(0, 50).map((row) => ({
    id: row.id,
    name: row.name,
    body: row.body,
    created_at: row.created_at,
    votes: votes(row),
  }));
  return { votes: votes(rating), comments, next: results.length > 50 ? comments.at(-1).id : null };
}

export async function writeCommunity(request, env, id, kind, commentId) {
  const commenting = kind === 'comments',
    action = commenting ? 'extension-comment' : commentId ? 'comment-vote' : 'extension-vote';
  const body = await protectedBody(
    request,
    env,
    action,
    commenting ? ['name', 'body'] : ['value'],
    16384,
  );
  if (!(await env.DB.prepare('SELECT id FROM extensions WHERE id=?').bind(id).first()))
    throw new RequestError('Repository not listed.', 404);
  const voter = await voterKey(request, env);
  if (commenting) {
    const name = typeof body.name === 'string' ? body.name.trim() : '',
      text = typeof body.body === 'string' ? body.body.trim() : '';
    const control = [...(name + text)].some(
      (char) =>
        (char.charCodeAt(0) < 32 && ![9, 10, 13].includes(char.charCodeAt(0))) ||
        char.charCodeAt(0) === 127,
    );
    if (!name || name.length > 60 || !text || text.length > 2000 || control)
      throw new RequestError('Use a name of 1–60 characters and a comment of 1–2,000 characters.');
    const now = new Date().toISOString(),
      cutoff = new Date(Date.now() - 86400000).toISOString();
    const result = await env.DB.prepare(
      `INSERT INTO comments (extension_id,voter,name,body,created_at)
      SELECT ?,?,?,?,? WHERE (SELECT COUNT(*) FROM comments WHERE voter=? AND created_at>=?)<5`,
    )
      .bind(id, voter, name, text, now, voter, cutoff)
      .run();
    if (!result.meta.changes)
      throw new RequestError(
        'Five comments per network address are allowed in 24 hours. Try again tomorrow.',
        429,
      );
    return { id: result.meta.last_row_id, status: 'pending' };
  }
  if (![-1, 0, 1].includes(body.value) || typeof body.value !== 'number')
    throw new RequestError('Vote must be -1, 0 or 1.');
  if (
    commentId &&
    (!Number.isSafeInteger(Number(commentId)) ||
      !(await env.DB.prepare(
        "SELECT id FROM comments WHERE id=? AND extension_id=? AND status='approved'",
      )
        .bind(Number(commentId), id)
        .first()))
  )
    throw new RequestError('Comment not found.', 404);
  const table = commentId ? 'comment_votes' : 'package_votes',
    column = commentId ? 'comment_id' : 'extension_id',
    target = commentId ? Number(commentId) : id;
  if (body.value === 0)
    await env.DB.prepare(`DELETE FROM ${table} WHERE ${column}=? AND voter=?`)
      .bind(target, voter)
      .run();
  else
    await env.DB.prepare(
      `INSERT INTO ${table} (${column},voter,value) VALUES (?,?,?) ON CONFLICT(${column},voter) DO UPDATE SET value=excluded.value`,
    )
      .bind(target, voter, body.value)
      .run();
  return {
    votes: votes(
      await env.DB.prepare(`SELECT ${aggregate} FROM ${table} WHERE ${column}=?`)
        .bind(voter, target)
        .first(),
    ),
  };
}
