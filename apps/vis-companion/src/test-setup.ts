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
  await import("@testing-library/jest-dom/vitest");
  const { cleanup } = await import("@testing-library/react");
  afterEach(cleanup);
}
