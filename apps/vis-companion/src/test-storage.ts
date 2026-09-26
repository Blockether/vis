// Every test file's own web storage: empty when the file starts, gone when it ends.
//
// Without it a file starts from whatever an earlier file left. A `node` unit file
// reaches the PROCESS's store (below), and story files play in iframes of one origin,
// so they share the browser's: a story file hydrated the gateway snapshots another
// file had flushed (see `lib/snapshot-store`) and painted that file's sessions.

/** A Storage that is nothing but a Map: no file, no process, no other test file. */
function inMemoryStorage(): Storage {
  const store = new Map<string, string>();
  return {
    get length() {
      return store.size;
    },
    clear: () => store.clear(),
    getItem: (key) => store.get(key) ?? null,
    key: (index) => [...store.keys()][index] ?? null,
    removeItem: (key) => void store.delete(key),
    setItem: (key, value) => void store.set(key, String(value)),
  };
}

// EVERY environment gets that Map, DOM or not. Node ships web storage of its
// own (on by default since Node 25), so a `node` test reading
// `globalThis.localStorage?` used to reach the PROCESS-global store: the first
// operation printed `Warning: --localstorage-file was provided without a valid
// path`, and from then on every file that worker ran shared one set of keys —
// a persisted store deciding what a unit test observes. It also wins over
// jsdom's, so a DOM test importing the app's snapshot cache threw at import
// time. Installed by DESCRIPTOR, never by reading the global first, because
// reading is what makes Node create it; `writable` so a test can still hand
// over its own (`globalThis.localStorage = …`, `vi.stubGlobal`).
for (const name of ['localStorage', 'sessionStorage'] as const) {
  Object.defineProperty(globalThis, name, {
    configurable: true,
    writable: true,
    value: inMemoryStorage(),
  });
}
