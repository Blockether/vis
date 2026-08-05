/**
 * The browser bits the attachment cache stands on, faked — TEST ONLY, imported
 * by no shipped module.
 *
 * Node has neither `CacheStorage` nor object URLs, and the app must not grow a
 * seam just to be testable: the store is installed on `globalThis` exactly the
 * way a browser hands it over, so the code under test takes the real path.
 */

/** A `Cache` that is a `Map` of URL → `Response`. `match` clones, as the real one does. */
export class FakeCache {
  readonly store = new Map<string, Response>();

  async put(url: string, response: Response): Promise<void> {
    this.store.set(url, response);
  }

  async match(
    request: string | { url: string },
  ): Promise<Response | undefined> {
    const key = typeof request === 'string' ? request : request.url;
    return this.store.get(key)?.clone();
  }

  async keys(): Promise<{ url: string }[]> {
    return [...this.store.keys()].map((url) => ({ url }));
  }

  async delete(url: string): Promise<boolean> {
    return this.store.delete(url);
  }
}

/** Give this run a store. Returns it, so a test can look at what survived. */
export function installFakeCacheStorage(): FakeCache {
  const cache = new FakeCache();
  (globalThis as { caches?: unknown }).caches = {
    open: async () => cache,
    delete: async () => {
      cache.store.clear();
      return true;
    },
  };
  return cache;
}

export function uninstallFakeCacheStorage(): void {
  delete (globalThis as { caches?: unknown }).caches;
}

/** Object URLs, recorded: `made` in order, `revoked` when the tier let one go. */
export interface FakeObjectUrls {
  made: string[];
  revoked: string[];
  sizeOf: (url: string) => number;
}

export function installFakeObjectUrls(): FakeObjectUrls {
  const made: string[] = [];
  const revoked: string[] = [];
  const sizes = new Map<string, number>();
  URL.createObjectURL = (blob: Blob | MediaSource) => {
    const url = `blob:vis/${made.length}`;
    made.push(url);
    sizes.set(url, 'size' in blob ? blob.size : 0);
    return url;
  };
  URL.revokeObjectURL = (url: string) => void revoked.push(url);
  return { made, revoked, sizeOf: (url) => sizes.get(url) ?? 0 };
}
