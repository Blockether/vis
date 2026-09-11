/** Approved release history and bounded discovery. Never execute repository code. */
import {
  githubClient,
  identity,
  inspectRepository,
  RequestError,
  repositoryURL,
} from './github.js';

export async function queueRelease(env, metadata) {
  const existing = await env.DB.prepare(
    'SELECT revision,status FROM releases WHERE extension_id=? AND version=?',
  )
    .bind(metadata.id, metadata.version)
    .first();
  const id = await identity(metadata.id + '\n' + metadata.revision);
  if (existing) {
    if (existing.revision !== metadata.revision)
      throw new RequestError(
        'This version already identifies another reviewed commit. Publish a new version.',
        409,
      );
    if (existing.status === 'rejected')
      throw new RequestError('This version has already been reviewed. Publish a new version.', 409);
    return { id, status: 'approved' };
  }
  const listed = await env.DB.prepare('SELECT metadata FROM extensions WHERE id=?')
    .bind(metadata.id)
    .first();
  if (listed && JSON.parse(listed.metadata).name !== metadata.name)
    throw new RequestError('A release cannot change the extension package name.', 409);
  const now = new Date().toISOString();
  await env.DB.prepare(
    'INSERT INTO submissions (id, extension_id, revision, metadata, submitted_at) VALUES (?, ?, ?, ?, ?) ON CONFLICT(extension_id, revision) DO NOTHING',
  )
    .bind(id, metadata.id, metadata.revision, JSON.stringify({ ...metadata, checked_at: now }), now)
    .run();
  return { id, status: 'pending' };
}

export function withRepositoryStats(metadata, stats) {
  return {
    ...metadata,
    stars: stats.stars ?? metadata.stars,
    stars_checked_at: stats.checked_at ?? null,
  };
}

export async function extensionDetail(env, id, version = null) {
  const row = await env.DB.prepare(
    "SELECT e.metadata,e.added_at,s.stars,s.checked_at FROM extensions e LEFT JOIN repository_stats s ON s.repository_url=lower(json_extract(e.metadata,'$.repository_url')) WHERE e.id=?",
  )
    .bind(id)
    .first();
  if (!row) return null;
  const { results } = await env.DB.prepare(
    "SELECT json_remove(metadata,'$.readme','$.dependencies','$.source_paths','$.skills','$.topics') AS metadata,reviewed_at FROM releases WHERE extension_id=? AND status='approved' ORDER BY COALESCE(json_extract(metadata,'$.prerelease'),0),json_extract(metadata,'$.version_key') DESC,reviewed_at DESC",
  )
    .bind(id)
    .all();
  const releases = results.map((release) => ({
    ...withRepositoryStats(JSON.parse(release.metadata), row),
    approved_at: release.reviewed_at,
  }));
  const latest = JSON.parse(row.metadata);
  const selected =
    version === null
      ? row
      : await env.DB.prepare(
          "SELECT metadata FROM releases WHERE extension_id=? AND version=? AND status='approved'",
        )
          .bind(id, version)
          .first();
  if (!selected) return null;
  return {
    ...withRepositoryStats(JSON.parse(selected.metadata), row),
    added_at: row.added_at,
    latest_version: latest.version,
    releases,
  };
}

export async function discoverReleases(env) {
  // One listing, one page and at most five new inspections per tick; resume within a page.
  const row = await env.DB.prepare(
    "SELECT e.id,e.metadata,COALESCE(s.page,1) AS page,COALESCE(s.position,0) AS position FROM extensions e LEFT JOIN release_sync s ON s.extension_id=e.id ORDER BY COALESCE(s.checked_at,''),e.id LIMIT 1",
  ).first();
  if (!row) return { checked: 0, queued: 0, failed: 0 };
  const listing = JSON.parse(row.metadata),
    now = new Date().toISOString();
  let page = row.page,
    position = row.position,
    inspected = 0,
    queued = 0,
    failed = 0,
    error = null;
  try {
    const repository = repositoryURL(listing.repository_url).slice('https://github.com/'.length);
    const candidates = await githubClient(env)(
      '/repos/' + repository + '/releases?per_page=20&page=' + page,
    );
    if (!Array.isArray(candidates) || candidates.length > 20)
      throw new RequestError('Invalid GitHub releases response.');
    for (; position < candidates.length; position++) {
      const candidate = candidates[position];
      if (candidate.draft || typeof candidate.tag_name !== 'string') continue;
      if (
        !candidate.tag_name.startsWith('v') &&
        !candidate.tag_name.startsWith(listing.name + '/v')
      )
        continue;
      const known = await env.DB.prepare(
        "SELECT 1 FROM releases WHERE extension_id=? AND json_extract(metadata,'$.release_tag')=? UNION ALL SELECT 1 FROM submissions WHERE extension_id=? AND json_extract(metadata,'$.release_tag')=? LIMIT 1",
      )
        .bind(row.id, candidate.tag_name, row.id, candidate.tag_name)
        .first();
      if (known) continue;
      if (inspected === 5) break;
      inspected++;
      try {
        const metadata = await inspectRepository(
          {
            repository_url: listing.repository_url,
            subdirectory: listing.subdirectory,
            release_tag: candidate.tag_name,
          },
          env,
        );
        const result = await queueRelease(env, metadata);
        if (result.status === 'pending') queued++;
      } catch (cause) {
        failed++;
        error = cause instanceof RequestError ? cause.message : 'GitHub release inspection failed.';
      }
    }
    if (position >= candidates.length) {
      page = candidates.length === 20 ? page + 1 : 1;
      position = 0;
    }
  } catch (cause) {
    failed++;
    error = cause instanceof RequestError ? cause.message : 'GitHub release discovery failed.';
  }
  await env.DB.prepare(
    'INSERT INTO release_sync (extension_id,page,position,checked_at,error) VALUES (?,?,?,?,?) ON CONFLICT(extension_id) DO UPDATE SET page=excluded.page,position=excluded.position,checked_at=excluded.checked_at,error=excluded.error',
  )
    .bind(row.id, page, position, now, error)
    .run();
  return { checked: 1, queued, failed };
}

export async function refreshRepositoryStats(env) {
  // Deduplicate monorepo listings; check at most five oldest repositories per tick.
  // Failed attempts also wait an hour, preserving the last successful count.
  const now = new Date().toISOString(),
    due = new Date(Date.now() - 60 * 60 * 1000).toISOString();
  const { results } = await env.DB.prepare(
    "SELECT DISTINCT lower(json_extract(e.metadata,'$.repository_url')) AS repository_url,s.attempted_at FROM extensions e LEFT JOIN repository_stats s ON s.repository_url=lower(json_extract(e.metadata,'$.repository_url')) WHERE s.attempted_at IS NULL OR s.attempted_at<=? ORDER BY s.attempted_at,repository_url LIMIT 5",
  )
    .bind(due)
    .all();
  let failed = 0;
  for (const row of results) {
    let stars = null,
      error = null;
    try {
      const repository = repositoryURL(row.repository_url).slice('https://github.com/'.length);
      const repo = await githubClient(env)('/repos/' + repository);
      if (
        repo.private !== false ||
        !Number.isSafeInteger(repo.stargazers_count) ||
        repo.stargazers_count < 0
      )
        throw new RequestError('GitHub returned invalid public repository statistics.');
      stars = repo.stargazers_count;
    } catch (cause) {
      failed++;
      error =
        cause instanceof RequestError ? cause.message : 'GitHub repository statistics unavailable.';
    }
    await env.DB.prepare(
      'INSERT INTO repository_stats (repository_url,stars,checked_at,attempted_at,error) VALUES (?,?,?,?,?) ON CONFLICT(repository_url) DO UPDATE SET stars=COALESCE(excluded.stars,repository_stats.stars),checked_at=COALESCE(excluded.checked_at,repository_stats.checked_at),attempted_at=excluded.attempted_at,error=excluded.error',
    )
      .bind(row.repository_url, stars, stars === null ? null : now, now, error)
      .run();
  }
  return { checked: results.length, failed };
}
