// What every DOM test gets for free: jest-dom's matchers, and an unmount
// between tests.
//
// The suite is deliberately MIXED — most files are pure logic in the `node`
// environment and must not pay for a DOM — so this setup asks the environment
// what it is instead of assuming a document exists. Every file gets the storage
// stand-in below (a `node` test reads `localStorage` too); only a file that
// opted into `// @vitest-environment jsdom` pays for Testing Library and the
// layout stubs.
//
// Rendering is how a component is tested here. Reading a component's SOURCE
// with `?raw` and matching class strings asserts what the file says, not what
// the screen does: it passes for a control that never renders, and it fails for
// a refactor that changed nothing a user can see. Use `render` + a role/label
// query + `userEvent`; keep a source scan only for a rule about the source
// itself (an import that must not exist, a call site that must be used).
import { afterEach } from "vitest";

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
for (const name of ["localStorage", "sessionStorage"] as const) {
  Object.defineProperty(globalThis, name, {
    configurable: true,
    writable: true,
    value: inMemoryStorage(),
  });
}

if (typeof document !== "undefined") {
  // jsdom lays nothing out and implements neither observer, so a screen that
  // measures itself would throw before it rendered. These are the smallest
  // stand-ins that let the real component mount; a test that needs a FIGURE
  // hands over its own geometry.
  const observer = class {
    observe() {}
    unobserve() {}
    disconnect() {}
    takeRecords() {
      return [];
    }
  };
  globalThis.ResizeObserver ??= observer as never;
  globalThis.IntersectionObserver ??= observer as never;
  Element.prototype.scrollTo ??= function scrollTo() {};
  Element.prototype.scrollIntoView ??= function scrollIntoView() {};
  window.matchMedia ??= ((query: string) => ({
    matches: false,
    media: query,
    onchange: null,
    addListener: () => {},
    removeListener: () => {},
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => false,
  })) as never;

  await import("@testing-library/jest-dom/vitest");
  const { cleanup } = await import("@testing-library/react");
  afterEach(cleanup);
}
