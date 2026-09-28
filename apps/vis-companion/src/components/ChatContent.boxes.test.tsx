// @vitest-environment jsdom
import v8 from 'node:v8';
import vm from 'node:vm';
import { afterEach, expect, it, vi } from 'vitest';

import { observeBox } from './ChatContent';

v8.setFlagsFromString('--expose-gc');
const collectGarbage = vm.runInNewContext('gc') as () => void;

afterEach(() => {
  vi.unstubAllGlobals();
});

// Regression: the shared ResizeObserver and its rotation hook live as long as the page.
// Created inside `observeBox`, they shared its closure context and kept the first box
// ever observed — and through it that session's whole detached screen — after the
// screen was gone: a heap snapshot of the perf build traced a closed session's detached
// screen to these two closures.
it('lets every box go once its screen stops observing it', async () => {
  vi.stubGlobal(
    'ResizeObserver',
    class {
      observe() {}
      unobserve() {}
      disconnect() {}
    },
  );
  const boxes = (() =>
    ['first', 'second'].map((name) => {
      const box = document.createElement('article');
      box.dataset.name = name;
      observeBox(box, () => undefined)();
      return new WeakRef(box);
    }))();

  for (let round = 0; round < 5 && boxes.some((box) => box.deref()); round += 1) {
    await new Promise((resolve) => setTimeout(resolve, 0));
    collectGarbage();
  }

  expect(boxes.map((box) => box.deref()?.dataset.name ?? 'collected')).toEqual(['collected', 'collected']);
});
