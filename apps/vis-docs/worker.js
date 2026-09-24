import { renderPage } from './web/render.js';
import { security } from './headers.js';
import { sitemap, catalogText, extensionPath } from './web/discovery.js';
import { inspectRepository, RequestError } from './github.js';
import {
  discoverReleases,
  extensionDetail,
  queueRelease,
  refreshRepositoryStats,
  withRepositoryStats,
} from './releases.js';
import { protectedBody } from './antispam.js';
import { readCommunity, writeCommunity } from './community.js';

const reply = (body, status = 200, html = false, cache = 'no-store') =>
  new Response(html ? body : JSON.stringify(body), {
    status,
    headers: {
      ...security,
      'Content-Type': html ? 'text/html; charset=utf-8' : 'application/json; charset=utf-8',
      'Cache-Control': cache,
    },
  });

async function catalog(env, origin, ctx) {
  // Cache one canonical snapshot, not a new D1 scan for every filter/search URL.
  const cache = globalThis.caches?.default,
    key = new Request(origin + '/api/extensions');
  const saved = await cache?.match(key);
  if (saved) return saved.json();
  const { results } = await env.DB.prepare(
    "SELECT e.id,json_remove(e.metadata,'$.readme') AS metadata,e.added_at,s.stars,s.checked_at FROM extensions e LEFT JOIN repository_stats s ON s.repository_url=lower(json_extract(e.metadata,'$.repository_url')) ORDER BY e.id",
  ).all();
  const data = {
    extensions: results.map((row) => ({
      ...withRepositoryStats(JSON.parse(row.metadata), row),
      added_at: row.added_at,
    })),
  };
  if (cache) ctx.waitUntil(cache.put(key, reply(data, 200, false, 'public, max-age=60')));
  return data;
}

// One interactive inspection costs up to ~90 GitHub calls. They are charged against a shared
// hourly ceiling, in blocks, so submissions cannot spend the quota discovery depends on.
// Scheduled discovery is never charged.
const GITHUB_BUDGET_PER_HOUR = 1200,
  GITHUB_BUDGET_BLOCK = 16,
  CATALOG_VERSION = /^(0|[1-9]\d{0,8})\.(0|[1-9]\d{0,8})\.(0|[1-9]\d{0,8})((a|b|rc)(0|[1-9]\d{0,8}))?$/;

function inspectionBudget(env) {
  let remaining = 0;
  return async () => {
    if (remaining > 0) {
      remaining -= 1;
      return;
    }
    const hour = new Date().toISOString().slice(0, 13),
      charged = await env.DB.prepare(
        'INSERT INTO github_budget (hour, calls) VALUES (?, ?) ON CONFLICT(hour) DO UPDATE SET calls=calls+? WHERE calls+?<=? RETURNING calls',
      )
        .bind(
          hour,
          GITHUB_BUDGET_BLOCK,
          GITHUB_BUDGET_BLOCK,
          GITHUB_BUDGET_BLOCK,
          GITHUB_BUDGET_PER_HOUR,
        )
        .first();
    if (!charged)
      throw new RequestError('The catalog reached its hourly GitHub budget. Try again later.', 429);
    remaining = GITHUB_BUDGET_BLOCK - 1;
  };
}

async function detail(env, origin, id, version, ctx) {
  // Refuse an impossible version before D1 sees it, then cache what every page load repeats.
  if (version && !CATALOG_VERSION.test(version)) return null;
  const cache = globalThis.caches?.default,
    key = new Request(
      origin + '/api/extensions/' + id + (version ? '?version=' + encodeURIComponent(version) : ''),
    );
  const saved = await cache?.match(key);
  if (saved) return saved.json();
  const item = await extensionDetail(env, id, version);
  if (item && cache) ctx.waitUntil(cache.put(key, reply(item, 200, false, 'public, max-age=60')));
  return item;
}
async function protectedSource(request, env, path) {
  const action = path === '/api/preview' ? 'extension-preview' : 'extension-submit';
  const source = await protectedBody(request, env, action, [
    'repository_url',
    'subdirectory',
    'revision',
    'release_tag',
  ]);
  if (path === '/api/submissions' && !/^[0-9a-f]{40}$/.test(source.revision || ''))
    throw new RequestError('Review a pinned commit before submitting.');
  return source;
}
async function handle(request, env, ctx) {
  const url = new URL(request.url),
    path = url.pathname;
  const page = path.startsWith('/extensions/');
  if (['GET', 'HEAD'].includes(request.method)) {
    if (path === '/extensions')
      return new Response(null, {
        status: 308,
        headers: { ...security, Location: '/extensions/' + url.search },
      });
    if (path === '/extensions/sitemap.xml' || path === '/extensions/llms.txt') {
      const items = (await catalog(env, url.origin, ctx)).extensions;
      const xml = path.endsWith('.xml');
      return new Response(
        xml ? sitemap(['/extensions/', ...items.map(extensionPath)]) : catalogText(items),
        {
          headers: {
            ...security,
            'Content-Type': xml ? 'application/xml; charset=utf-8' : 'text/plain; charset=utf-8',
            'Cache-Control': 'public, max-age=60',
          },
        },
      );
    }
    if (page) {
      const data = { items: [], search: url.search, siteKey: env.TURNSTILE_SITE_KEY || '' };
      let status = 200;
      try {
        data.items = (await catalog(env, url.origin, ctx)).extensions;
        if (path !== '/extensions/') {
          const listing = data.items.find(
            (item) => path === extensionPath(item) || path === '/extensions/' + item.id,
          );
          if (listing && path !== extensionPath(listing))
            return new Response(null, {
              status: 308,
              headers: { ...security, Location: extensionPath(listing) + url.search },
            });
          data.item =
            listing &&
            (await detail(env, url.origin, listing.id, url.searchParams.get('version'), ctx));
          if (!data.item) {
            data.detailError = true;
            status = 404;
          }
        }
      } catch {
        data.error = 'Could not load the catalog. Try again later.';
        data.detailError = path !== '/extensions/';
        status = 503;
      }
      return reply(renderPage(data), status, true);
    }
    if (path === '/api/extensions')
      return reply(await catalog(env, url.origin, ctx), 200, false, 'public, max-age=60');
    const id = path.match(/^\/api\/extensions\/([0-9a-f]{24})$/);
    const community = path.match(/^\/api\/extensions\/([0-9a-f]{24})\/community$/);
    if (community) return reply(await readCommunity(request, env, community[1]));
    const slug = path.match(/^\/api\/extensions\/([^/]+)\/([^/]+)(\/.+)?$/);
    if (id || slug) {
      let extensionId = id?.[1];
      if (slug) {
        const canonical =
          '/extensions/' +
          slug[1].toLowerCase() +
          '/' +
          slug[2].toLowerCase() +
          (slug[3] || '');
        extensionId = (await catalog(env, url.origin, ctx)).extensions.find(
          (item) => extensionPath(item) === canonical,
        )?.id;
      }
      const item = extensionId
        ? await detail(env, url.origin, extensionId, url.searchParams.get('version'), ctx)
        : null;
      return item
        ? reply(item, 200, false, 'public, max-age=60')
        : reply({ error: 'Repository or approved version not listed.' }, 404);
    }
    if (path.startsWith('/api/')) return reply({ error: 'Not found.' }, 404);
    return env.ASSETS.fetch(request);
  }
  const feedback = path.match(
    /^\/api\/extensions\/([0-9a-f]{24})\/(?:(vote|comments)|comments\/([1-9][0-9]{0,15})\/vote)$/,
  );
  if (request.method === 'POST' && feedback)
    return reply(
      await writeCommunity(request, env, feedback[1], feedback[2], feedback[3]),
      feedback[2] === 'comments' ? 202 : 200,
    );
  if (request.method !== 'POST' || !['/api/preview', '/api/submissions'].includes(path))
    return reply({ error: 'Method not allowed.' }, 405);
  const source = await protectedSource(request, env, path);
  const metadata = await inspectRepository(source, env, inspectionBudget(env));
  if (path === '/api/preview') return reply(metadata);
  const submission = await queueRelease(env, metadata);
  return reply(submission, submission.status === 'pending' ? 202 : 200);
}
export default {
  async fetch(request, env, ctx) {
    let response;
    try {
      response = await handle(request, env, ctx);
    } catch (error) {
      response = reply(
        {
          error:
            error instanceof RequestError
              ? error.message
              : 'Catalog or GitHub is unavailable. Try again later.',
        },
        error instanceof RequestError ? error.status : 503,
      );
    }
    if (response.status === 429) response.headers.set('Retry-After', '60');
    if (new URL(request.url).pathname.startsWith('/api/') || response.status >= 400) {
      response = new Response(response.body, response);
      response.headers.set('X-Robots-Tag', 'noindex');
    }
    return request.method === 'HEAD' ? new Response(null, response) : response;
  },
  async scheduled(_controller, env, ctx) {
    // Discovery spends no interactive budget; it only drops windows nobody can charge again.
    ctx.waitUntil(
      Promise.all([
        discoverReleases(env),
        refreshRepositoryStats(env),
        env.DB.prepare('DELETE FROM github_budget WHERE hour < ?')
          .bind(new Date(Date.now() - 2 * 3600 * 1000).toISOString().slice(0, 13))
          .run(),
      ]),
    );
  },
};
