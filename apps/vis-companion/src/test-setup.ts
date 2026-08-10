// What every DOM test gets for free: jest-dom's matchers, and an unmount
// between tests.
//
// The suite is deliberately MIXED — most files are pure logic in the `node`
// environment and must not pay for a DOM — so this setup asks the environment
// what it is instead of assuming a document exists. In `node` it does nothing
// at all; in a file that opted into `// @vitest-environment jsdom` it wires up
// Testing Library.
//
// Rendering is how a component is tested here. Reading a component's SOURCE
// with `?raw` and matching class strings asserts what the file says, not what
// the screen does: it passes for a control that never renders, and it fails for
// a refactor that changed nothing a user can see. Use `render` + a role/label
// query + `userEvent`; keep a source scan only for a rule about the source
// itself (an import that must not exist, a call site that must be used).
import { afterEach } from "vitest";

if (typeof document !== "undefined") {
  // Node 22 puts its own experimental `localStorage` on `globalThis`, and
  // without `--localstorage-file` it is a stub with no `getItem`. It wins over
  // jsdom's, so the app's snapshot cache throws at import time. A DOM test gets
  // a real one.
  const store = new Map<string, string>();
  const local: Storage = {
    get length() {
      return store.size;
    },
    clear: () => store.clear(),
    getItem: (key) => store.get(key) ?? null,
    key: (index) => [...store.keys()][index] ?? null,
    removeItem: (key) => void store.delete(key),
    setItem: (key, value) => void store.set(key, String(value)),
  };
  Object.defineProperty(globalThis, "localStorage", {
    configurable: true,
    value: local,
  });

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
