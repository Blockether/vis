/**
 * The outbox for dictated audio that has not been transcribed yet.
 *
 * A dictation is finished LOCALLY — the microphone, the resampler and the WAV
 * encoder need nothing but the device. Transcription is the only part that
 * needs the gateway, and it is exactly the part that fails when the phone is
 * offline, on a dead link, or frozen by iOS mid-request. Holding the audio in a
 * React ref made that failure fatal in the one case it matters: the webview is
 * torn down (OS reclaim, crash, reload) and the words spoken into a stopped
 * recorder are gone with it.
 *
 * So the WAV lands here the moment it exists, and only a transcript removes it.
 * Speaking offline, tapping the mic off, killing the app and coming back is a
 * supported sequence: the next live moment picks the audio up and drains it
 * into the composer.
 *
 * IndexedDB, not Preferences/localStorage: a 15-minute dictation is ~29 MB of
 * PCM, which no string-keyed key/value store will take. The payload is stored
 * as an ArrayBuffer, never a Blob — WebKit has shipped several versions that
 * lose or corrupt Blobs round-tripped through IndexedDB.
 */

const DB_NAME = 'vis.voice';
const DB_VERSION = 1;
const STORE = 'pending';

/** Audio nobody came back for. Long enough to survive a weekend offline. */
const MAX_AGE_MS = 7 * 24 * 60 * 60 * 1000;

/** Sessions that may hold undelivered audio at once. Oldest entries drop first. */
const MAX_ENTRIES = 8;

interface PendingRow {
  key: string;
  at: number;
  type: string;
  bytes: ArrayBuffer;
}

let dbPromise: Promise<IDBDatabase | null> | null = null;

function openDb(): Promise<IDBDatabase | null> {
  if (dbPromise) return dbPromise;
  dbPromise = new Promise((resolve) => {
    // Private modes and locked-down webviews expose the API and then refuse to
    // open. A missing outbox must degrade to "in-memory only", never throw into
    // the dictation path.
    let request: IDBOpenDBRequest;
    try {
      if (typeof indexedDB === 'undefined') {
        resolve(null);
        return;
      }
      request = indexedDB.open(DB_NAME, DB_VERSION);
    } catch {
      resolve(null);
      return;
    }
    request.onupgradeneeded = () => {
      const db = request.result;
      if (!db.objectStoreNames.contains(STORE)) db.createObjectStore(STORE, { keyPath: 'key' });
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => resolve(null);
    request.onblocked = () => resolve(null);
  });
  return dbPromise;
}

function run<T>(
  mode: IDBTransactionMode,
  body: (store: IDBObjectStore) => IDBRequest<T> | null,
): Promise<T | null> {
  return openDb().then(
    (db) =>
      new Promise<T | null>((resolve) => {
        if (!db) {
          resolve(null);
          return;
        }
        let request: IDBRequest<T> | null;
        try {
          request = body(db.transaction(STORE, mode).objectStore(STORE));
        } catch {
          resolve(null);
          return;
        }
        if (!request) {
          resolve(null);
          return;
        }
        request.onsuccess = () => resolve(request.result ?? null);
        request.onerror = () => resolve(null);
      }),
  );
}

/**
 * Hold this session's audio until a transcript comes back. Replaces whatever
 * was held for the same key: a newer dictation always wins over an older one
 * that never made it out.
 */
export async function savePendingVoice(key: string, wav: Blob): Promise<void> {
  let bytes: ArrayBuffer;
  try {
    bytes = await wav.arrayBuffer();
  } catch {
    return;
  }
  const row: PendingRow = { key, at: Date.now(), type: wav.type || 'audio/wav', bytes };
  await run('readwrite', (store) => store.put(row) as IDBRequest<IDBValidKey>);
  void prune();
}

/** The audio still owed a transcript for this session, if any. */
export async function readPendingVoice(key: string): Promise<Blob | null> {
  const row = await run<PendingRow>('readonly', (store) => store.get(key) as IDBRequest<PendingRow>);
  if (!row?.bytes) return null;
  if (Date.now() - (row.at ?? 0) > MAX_AGE_MS) {
    void clearPendingVoice(key);
    return null;
  }
  return new Blob([row.bytes], { type: row.type || 'audio/wav' });
}

/** The words landed in the composer (or the user threw them away). */
export async function clearPendingVoice(key: string): Promise<void> {
  await run('readwrite', (store) => store.delete(key) as unknown as IDBRequest<undefined>);
}

/** Drop stale and surplus audio, newest kept. Best effort, never awaited by callers. */
async function prune(): Promise<void> {
  const rows = await run<PendingRow[]>(
    'readonly',
    (store) => store.getAll() as IDBRequest<PendingRow[]>,
  );
  if (!rows?.length) return;
  const cutoff = Date.now() - MAX_AGE_MS;
  const doomed = rows
    .filter((row) => (row.at ?? 0) < cutoff)
    .map((row) => row.key)
    .concat(
      [...rows]
        .filter((row) => (row.at ?? 0) >= cutoff)
        .sort((a, b) => (b.at ?? 0) - (a.at ?? 0))
        .slice(MAX_ENTRIES)
        .map((row) => row.key),
    );
  for (const key of doomed) await clearPendingVoice(key);
}
