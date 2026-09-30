// What every DOM test gets for free: jest-dom's matchers, and an unmount
// between tests.
//
// The suite is deliberately MIXED — most files are pure logic in the `node`
// environment and must not pay for a DOM — so this setup asks the environment
// what it is instead of assuming a document exists. Every file gets the storage
// stand-in of `test-storage.ts` (a `node` test reads `localStorage` too); only a
// file that opted into `// @vitest-environment jsdom` pays for Testing Library
// and the layout stubs.
//
// Rendering is how a component is tested here. Reading a component's SOURCE
// with `?raw` and matching class strings asserts what the file says, not what
// the screen does: it passes for a control that never renders, and it fails for
// a refactor that changed nothing a user can see. Use `render` + a role/label
// query + `userEvent`; keep a source scan only for a rule about the source
// itself (an import that must not exist, a call site that must be used).
import type { AxeResults } from 'axe-core';
import { afterEach } from 'vitest';

import './test-storage';

if (typeof document !== 'undefined') {
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
  // Pointer capture is a browser layout API used by the shared Select primitive.
  Element.prototype.hasPointerCapture ??= () => false;
  Element.prototype.setPointerCapture ??= function setPointerCapture() {};
  Element.prototype.releasePointerCapture ??= function releasePointerCapture() {};
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
  // jsdom has no canvas and computes no pseudo-element styles. It answers as a
  // browser without them would (no context, the element's own style) but logs a
  // line per call, and the accessibility check makes hundreds of those calls.
  HTMLCanvasElement.prototype.getContext = (() => null) as never;
  const computedStyle = window.getComputedStyle.bind(window);
  window.getComputedStyle = (element: Element) => computedStyle(element);

  await import('@testing-library/jest-dom/vitest');
  const { cleanup, configure } = await import('@testing-library/react');
  // The addon reports failures but does not always throw under vmForks.
  // Consume its existing scan so a failed accessibility report fails the test.
  afterEach(({ task }) => {
    const reports = (task.meta as {
      reports?: {
        type: string;
        status: string;
        result: AxeResults | { error: unknown };
      }[];
    }).reports ?? [];
    const failures = reports.filter((report) => report.type === 'a11y' && report.status === 'failed');
    if (failures.length === 0) return;
    const details = failures.map(({ result }) => {
      if ('error' in result) return `Accessibility scanner error: ${String(result.error)}`;
      return result.violations.map(({ id, help, nodes }) =>
        `${id}: ${help}\n${nodes.map((node) => `${node.target.join(', ')}\n${node.failureSummary ?? ''}`).join('\n')}`,
      ).join('\n');
    });
    throw new Error(`Accessibility checks failed:\n${details.join('\n')}`);
  });
  afterEach(cleanup);
  // A whole screen on a busy machine can take longer than Testing Library's
  // one-second default to settle, so every `findBy*` and `waitFor` waits five
  // seconds, as the story plays do (`.storybook/preview.tsx`).
  configure({ asyncUtilTimeout: 5_000 });
}
