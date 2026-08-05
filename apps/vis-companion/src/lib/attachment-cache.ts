/**
 * THE ARTIFACT CACHE — a produced picture is downloaded ONCE, ever.
 *
 * An attachment is append-only and content-addressed by (gateway, session,
 * iteration, index): its URL IS its identity and its bytes can never change.
 * That is exactly the thing a cache is allowed to keep forever — and the app
 * was not keeping it at all. The client held a bounded map of `blob:` URLs in
 * MEMORY, so re-entering a session, re-opening the app, or simply scrolling far
 * enough re-downloaded the same figures over the same phone connection. On a
 * train that is a spinner where a picture used to be.
 *
 * So bytes land in the platform's own persistent store (`CacheStorage`), keyed
 * by the absolute attachment URL, and the network is asked only for artifacts
 * this device has never seen. Everything is best-effort: a browser with no
 * `caches`, a private window, a full disk — every failure path here degrades to
 * "fetch it again", never to a broken tile.
 *
 * A cache with no ceiling is a leak with good manners, and a phone answers a
 * leak by killing the webview. So both tiers — the object URLs in memory and
 * the bytes on disk — are bounded by SIZE and by NUMBER through the one pure
 * policy below, and the same policy is what the paging in the UI keeps in
 * budget. One artifact bigger than `maxEntryBytes` is never stored at all: a
 * 200 MB clip must not evict a whole session's figures on its way through.
 */

/** What a tier may hold. Bytes AND count, because either one alone lies. */
export interface CacheBudget {
  /** Ceiling on the total bytes the tier holds. */
  maxBytes: number;
  /** Ceiling on how MANY artifacts it holds. */
  maxEntries: number;
  /** One artifact larger than this is never admitted. */
  maxEntryBytes: number;
}

/** One thing a tier is holding, as the eviction policy sees it. */
export interface CacheEntry {
  /** Identity — the absolute attachment URL, or the memory tier's own key. */
  url: string;
  /** What it costs to keep. Unknown counts as 0, and the COUNT bound still applies. */
  bytes: number;
  /**
   * Recency, larger is newer. Entries from a previous run of the app have
   * nothing to say here and pass `-1`, which is what makes them go first.
   */
  used: number;
  /** On screen right now: it counts against the budget but is never evicted. */
  pinned?: boolean;
}

/** Disk. Big, because it is the whole point: what is here is not re-downloaded. */
export const ATTACHMENT_DISK_BUDGET: CacheBudget = {
  maxBytes: 96 * 1024 * 1024,
  maxEntries: 256,
  maxEntryBytes: 24 * 1024 * 1024,
};

/**
 * Memory. Small, because every live entry pins fully DECODED bytes for the
 * lifetime of the document — a long session of figures is precisely the memory
 * curve iOS answers by killing the webview. Evicting here is cheap now: the
 * bytes are still on disk, so a re-scroll costs a decode, not a download.
 */
export const ATTACHMENT_MEMORY_BUDGET: CacheBudget = {
  maxBytes: 48 * 1024 * 1024,
  maxEntries: 24,
  maxEntryBytes: Number.POSITIVE_INFINITY,
};

/**
 * WHO HAS TO GO, least recently used first — the whole eviction policy, pure.
 *
 * Both bounds are honoured together: a tier is over budget when it holds too
 * many bytes OR too many artifacts, and eviction stops the moment neither is
 * true. `pinned` entries are SKIPPED rather than deferred (a visible image
 * beats a freed URL), so a tier may legitimately sit over its bound while that
 * many artifacts are genuinely on screen.
 */
export function cacheVictims(
  entries: CacheEntry[],
  budget: CacheBudget,
): string[] {
  let bytes = entries.reduce((sum, entry) => sum + Math.max(0, entry.bytes), 0);
  let count = entries.length;
  if (bytes <= budget.maxBytes && count <= budget.maxEntries) return [];
  const order = entries
    .map((entry, at) => ({ entry, at }))
    .sort((a, b) => a.entry.used - b.entry.used || a.at - b.at);
  const victims: string[] = [];
  for (const { entry } of order) {
    if (bytes <= budget.maxBytes && count <= budget.maxEntries) break;
    if (entry.pinned) continue;
    victims.push(entry.url);
    bytes -= Math.max(0, entry.bytes);
    count -= 1;
  }
  return victims;
}

const CACHE_NAME = 'vis-attachments-v1';
/**
 * What the entry cost, written when it was stored. `Content-Length` is not
 * dependable on a synthetic `Response`, and reading `.blob()` to find out would
 * pull every cached artifact off disk just to decide which one to drop.
 */
const BYTES_HEADER = 'x-vis-bytes';

/**
 * Recency for THIS run only. Nothing is persisted: an entry a previous run
 * stored reads as `-1` and is evicted before anything this run has touched,
 * which is the honest ordering when the alternative is rewriting a cached
 * response (all of its bytes) on every cache hit just to stamp a timestamp.
 */
const used = new Map<string, number>();
let tick = 0;

function touch(url: string): void {
  tick += 1;
  used.set(url, tick);
}

/** The platform store, or `null` where there is none (SSR, private mode, tests). */
async function openCache(): Promise<Cache | null> {
  try {
    if (typeof caches === 'undefined') return null;
    return await caches.open(CACHE_NAME);
  } catch {
    return null;
  }
}

/**
 * The bytes this device already has for `url`, or `null` — the ONE call that
 * decides whether an artifact is downloaded again.
 */
export async function readCachedAttachment(url: string): Promise<Blob | null> {
  const cache = await openCache();
  if (!cache) return null;
  try {
    const hit = await cache.match(url);
    if (!hit) return null;
    const blob = await hit.blob();
    if (!blob.size) return null;
    touch(url);
    return blob;
  } catch {
    return null;
  }
}

/**
 * Keep these bytes for next time, then bring the store back inside its budget.
 *
 * Storing is never load-bearing: the caller already has the blob it needs, so a
 * quota error, a disabled store or an eviction race is swallowed on purpose.
 */
export async function writeCachedAttachment(
  url: string,
  blob: Blob,
  budget: CacheBudget = ATTACHMENT_DISK_BUDGET,
): Promise<void> {
  if (!blob.size || blob.size > budget.maxEntryBytes) return;
  const cache = await openCache();
  if (!cache) return;
  try {
    await cache.put(
      url,
      new Response(blob, {
        headers: {
          'Content-Type': blob.type || 'application/octet-stream',
          [BYTES_HEADER]: String(blob.size),
        },
      }),
    );
    touch(url);
    await evictCachedAttachments(cache, budget);
  } catch {
    // No room, no store, or the entry vanished under us — the picture still paints.
  }
}

/** What the store is holding, headers only: no artifact's body is read here. */
async function cachedEntries(cache: Cache): Promise<CacheEntry[]> {
  const keys = await cache.keys();
  const entries = await Promise.all(
    keys.map(async (request) => {
      const hit = await cache.match(request);
      const declared = Number(hit?.headers.get(BYTES_HEADER) ?? 0);
      return {
        url: request.url,
        bytes: Number.isFinite(declared) ? declared : 0,
        used: used.get(request.url) ?? -1,
      };
    }),
  );
  return entries;
}

async function evictCachedAttachments(
  cache: Cache,
  budget: CacheBudget,
): Promise<void> {
  const victims = cacheVictims(await cachedEntries(cache), budget);
  for (const url of victims) {
    used.delete(url);
    await cache.delete(url);
  }
}

/** What the device is holding — the honest number behind "cached". */
export async function attachmentCacheStats(): Promise<{
  entries: number;
  bytes: number;
}> {
  const cache = await openCache();
  if (!cache) return { entries: 0, bytes: 0 };
  try {
    const entries = await cachedEntries(cache);
    return {
      entries: entries.length,
      bytes: entries.reduce((sum, entry) => sum + entry.bytes, 0),
    };
  } catch {
    return { entries: 0, bytes: 0 };
  }
}

/** Drop everything, e.g. when a gateway is unpaired. Best-effort, like the rest. */
export async function clearAttachmentCache(): Promise<void> {
  used.clear();
  try {
    if (typeof caches !== 'undefined') await caches.delete(CACHE_NAME);
  } catch {
    // Nothing to clear.
  }
}
