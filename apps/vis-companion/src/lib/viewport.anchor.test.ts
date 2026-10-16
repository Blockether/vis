import { describe, expect, it } from 'vitest';

import { applyScrollAnchorGrowth, scrollAnchorFor } from './viewport';

// One transcript in content coordinates: the view starts at 100 px on screen, and an
// element at content position `position` paints at `100 + position - scrollTop`.
function scene(scrollTop: number, position: number) {
  const viewport = { scrollTop, getBoundingClientRect: () => ({ top: 100, bottom: 700 }) };
  const line = {
    isConnected: true,
    position,
    children: [] as Element[],
    getBoundingClientRect() {
      const top = 100 + line.position - viewport.scrollTop;
      return { top, bottom: top + 20 };
    },
  };
  const transcript = {
    isConnected: true,
    children: [line as unknown as Element],
    getBoundingClientRect: () => ({ top: 100 - viewport.scrollTop, bottom: 9_999 }),
  };
  return { viewport, line, anchor: scrollAnchorFor(viewport, transcript)! };
}

// Regression, user report: on the web, a wheel scroll up through a long session jumped
// by a whole turn when that turn's earlier steps landed above the reader mid-gesture.
describe('applyScrollAnchorGrowth', () => {
  it('bills the history that landed above the reader and keeps the wheel move', () => {
    const { viewport, line, anchor } = scene(1_000, 1_040);
    viewport.scrollTop = 940;
    line.position += 2_651;

    expect(applyScrollAnchorGrowth(viewport, anchor)).toBe(true);

    expect(viewport.scrollTop).toBe(940 + 2_651);
    expect(line.getBoundingClientRect().top - 100).toBe(40 + 60);
  });

  it('leaves a scroll without growth where the reader put it', () => {
    const { viewport, anchor } = scene(1_000, 1_040);
    viewport.scrollTop = 940;

    applyScrollAnchorGrowth(viewport, anchor);

    expect(viewport.scrollTop).toBe(940);
  });

  it('reports an unmounted anchor and moves nothing', () => {
    const { viewport, line, anchor } = scene(1_000, 1_040);
    line.isConnected = false;
    line.position += 2_651;

    expect(applyScrollAnchorGrowth(viewport, anchor)).toBe(false);
    expect(viewport.scrollTop).toBe(1_000);
  });
});
