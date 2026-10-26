import { afterEach, describe, expect, it, vi } from 'vitest';

import { applyScrollAnchor, applyScrollAnchorGrowth, scrollAnchorFor } from './viewport';

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

// Two stacked segments: the reader reads `first`, and `second` starts below it.
function stack(scrollTop: number) {
  const viewport = { scrollTop, getBoundingClientRect: () => ({ top: 100, bottom: 700 }) };
  const part = (position: number, height: number) => {
    const node = {
      isConnected: true,
      position,
      children: [] as Element[],
      nextElementSibling: null as Element | null,
      getBoundingClientRect() {
        const top = 100 + node.position - viewport.scrollTop;
        return { top, bottom: top + height };
      },
    };
    return node;
  };
  const first = part(900, 400);
  const second = part(1_300, 400);
  first.nextElementSibling = second as unknown as Element;
  const transcript = {
    isConnected: true,
    children: [first, second] as unknown as Element[],
    getBoundingClientRect: () => ({ top: 100 - viewport.scrollTop, bottom: 9_999 }),
  };
  return { viewport, first, second, anchor: scrollAnchorFor(viewport, transcript)! };
}

// Regression, user report: the web transcript sometimes jumped by a whole history page.
// The page merged the first loaded step into an earlier segment under a new key, so
// the segment that the reader was reading unmounted while 8 623 px landed above.
describe('a replaced anchor', () => {
  it('holds the line by the sibling below it', () => {
    const { viewport, first, second, anchor } = stack(1_000);
    first.isConnected = false;
    second.position += 8_623;

    expect(applyScrollAnchor(viewport, anchor)).toBe(true);

    expect(viewport.scrollTop).toBe(1_000 + 8_623);
    expect(second.getBoundingClientRect().top - 100).toBe(300);
  });

  it('bills the growth by the sibling below it under a wheel move', () => {
    const { viewport, first, second, anchor } = stack(1_000);
    viewport.scrollTop = 940;
    first.isConnected = false;
    second.position += 8_623;

    expect(applyScrollAnchorGrowth(viewport, anchor)).toBe(true);

    expect(viewport.scrollTop).toBe(940 + 8_623);
  });
});

const IPHONE = 'Mozilla/5.0 (iPhone; CPU iPhone OS 26_5 like Mac OS X)';
const MAC = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)';

// Regression, user report: on an iPhone, a short scroll up in a long turn put the
// reader near the start of the turn. The momentum of the flick undid each correction
// for the history that landed above the reader.
describe('a correction during iOS momentum', () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  // The scene with an inline style, and the overflow that each scroll write met.
  function styled(userAgent: string, maxTouchPoints: number) {
    vi.stubGlobal('navigator', { ...navigator, userAgent, maxTouchPoints });
    const { viewport, line, anchor } = scene(1_000, 1_040);
    const style = { overflowY: '' };
    const writes: string[] = [];
    let top = viewport.scrollTop;
    Object.defineProperty(viewport, 'scrollTop', {
      get: () => top,
      set: (value: number) => {
        writes.push(style.overflowY);
        top = value;
      },
    });
    Object.assign(viewport, { style });
    return { viewport, line, anchor, style, writes };
  }

  it('stops the momentum for the write on iOS and restores the overflow', () => {
    const { viewport, line, anchor, style, writes } = styled(IPHONE, 5);
    line.position += 4_331;

    expect(applyScrollAnchor(viewport, anchor)).toBe(true);

    expect(viewport.scrollTop).toBe(1_000 + 4_331);
    expect(writes).toEqual(['hidden']);
    expect(style.overflowY).toBe('');
  });

  it('bills growth under a hand the same way', () => {
    const { viewport, line, anchor, writes } = styled(IPHONE, 5);
    viewport.scrollTop = 940;
    line.position += 4_331;

    expect(applyScrollAnchorGrowth(viewport, anchor)).toBe(true);

    expect(viewport.scrollTop).toBe(940 + 4_331);
    expect(writes).toEqual(['', 'hidden']);
  });

  it('keeps the plain write on a desktop browser', () => {
    const { viewport, line, anchor, writes } = styled(MAC, 0);
    line.position += 4_331;

    expect(applyScrollAnchor(viewport, anchor)).toBe(true);

    expect(viewport.scrollTop).toBe(1_000 + 4_331);
    expect(writes).toEqual(['']);
  });
});

/** A box at content `position`, painted at `100 + position - scrollTop` like `scene`. */
function box(viewport: { scrollTop: number }, position: number, height: number, extra = {}) {
  const node = {
    isConnected: true,
    position,
    children: [] as Element[],
    nextElementSibling: null as Element | null,
    getBoundingClientRect() {
      const top = 100 + node.position - viewport.scrollTop;
      return { top, bottom: top + height };
    },
    ...extra,
  };
  return node;
}

function column(viewport: { scrollTop: number }, children: object[], marks: object[] = []) {
  children.forEach((child, index) => {
    (child as { nextElementSibling: unknown }).nextElementSibling = children[index + 1] ?? null;
  });
  return {
    isConnected: true,
    children: children as unknown as Element[],
    getBoundingClientRect: () => ({ top: 100 - viewport.scrollTop, bottom: 9_999 }),
    querySelectorAll: () => marks as unknown as Element[],
  };
}

// Regression, user report: on iOS Safari, a scroll up through a long session jumped by
// a few pixels at a time. The anchor was an absolute `sr-only` label that kept its top
// while the transcript scrolled, so each scroll step was billed as growth.
describe('an out-of-flow box', () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it('never anchors the reader', () => {
    vi.stubGlobal('getComputedStyle', (node: { cssPosition?: string }) => ({
      position: node.cssPosition ?? 'static',
    }));
    const viewport = { scrollTop: 1_000, getBoundingClientRect: () => ({ top: 100, bottom: 700 }) };
    const above = box(viewport, 900, 50);
    const label = {
      isConnected: true,
      cssPosition: 'absolute',
      children: [] as Element[],
      getBoundingClientRect: () => ({ top: 4_593, bottom: 4_594 }),
    };
    const reading = box(viewport, 1_000, 400);
    const anchor = scrollAnchorFor(viewport, column(viewport, [above, label, reading]))!;

    expect(anchor.el).toBe(reading);
    viewport.scrollTop = 988;
    applyScrollAnchorGrowth(viewport, anchor);
    expect(viewport.scrollTop).toBe(988);
  });
});

// Regression, user report: after an earlier-steps page landed at rest, the next scroll
// moved the reader by the same page again.
describe('a correction', () => {
  it('bills each growth once', () => {
    const { viewport, line, anchor } = scene(1_000, 1_040);
    line.position += 1_351;

    expect(applyScrollAnchor(viewport, anchor)).toBe(true);
    expect(viewport.scrollTop).toBe(2_351);

    viewport.scrollTop = 2_339;
    applyScrollAnchorGrowth(viewport, anchor);
    expect(viewport.scrollTop).toBe(2_339);
  });
});

// Regression, user report: an earlier-steps page that landed with its mark in view
// pushed the steps that the reader was reading off the screen.
describe('a skip mark in view', () => {
  function turn(markAt: number) {
    const viewport = { scrollTop: 1_000, getBoundingClientRect: () => ({ top: 100, bottom: 700 }) };
    const header = box(viewport, 980, markAt - 980);
    const mark = box(viewport, markAt, 1, { dataset: { anchor: 'skip' } });
    const steps = box(viewport, markAt + 1, 2_000);
    const transcript = column(viewport, [header, mark, steps], [mark]);
    return { viewport, header, steps, anchor: scrollAnchorFor(viewport, transcript)! };
  }

  it('in the upper half holds the steps below it', () => {
    const { viewport, steps, anchor } = turn(1_020);
    expect(anchor.el).toBe(steps);

    steps.position += 1_351;
    applyScrollAnchor(viewport, anchor);

    expect(viewport.scrollTop).toBe(2_351);
    expect(steps.getBoundingClientRect().top).toBe(121);
  });

  it('in the lower half keeps the fold', () => {
    const { header, anchor } = turn(1_400);

    expect(anchor.el).toBe(header);
  });
});
