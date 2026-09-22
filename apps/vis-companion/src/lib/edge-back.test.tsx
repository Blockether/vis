// @vitest-environment jsdom
import { act, render } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

// jsdom cannot build a `Touch`, so the same dispatch helpers the pull gesture
// uses carry these fingers too.
import { drag, fireTouch } from './pull-to-search.fixture';
import {
  EDGE_BACK_PX,
  EDGE_EASING,
  EDGE_PARALLAX,
  EDGE_SETTLE_MS,
  EDGE_UNDER_DIM,
  EDGE_ZONE_PX,
  dismissTopLayer,
  edgeIsFree,
  edgeMove,
  edgeStart,
  useBackLayer,
  useEdgeBack,
  type EdgePhase,
  type EdgePoint,
} from './edge-back';

const PANE = { left: 0 };
const AT: EdgePoint = { x: 6, y: 300 };
const watching = { from: AT, phase: 'none' as EdgePhase, across: 0 };
const across = (by: number) => ({ x: AT.x + by, y: AT.y });

describe('reading a swipe in from the pane edge', () => {
  it('watches a finger that lands on the edge strip', () => {
    expect(edgeStart(PANE, 1, AT)).toEqual(watching);
  });

  it('measures the strip from the pane itself, not from the window', () => {
    const sidebar = { left: 320 };
    expect(edgeStart(sidebar, 1, { x: 326, y: 300 })).toEqual({
      from: { x: 326, y: 300 },
      phase: 'none',
      across: 0,
    });
    expect(edgeStart(sidebar, 1, AT)).toBeNull();
  });

  it('refuses a finger that lands inside the transcript, which is reading', () => {
    expect(edgeStart(PANE, 1, { x: EDGE_ZONE_PX + 1, y: 300 })).toBeNull();
  });

  it('refuses a second finger, because a pinch is not a way out', () => {
    expect(edgeStart(PANE, 2, AT)).toBeNull();
  });

  it('arms only once a lift would be a deliberate navigation', () => {
    expect(edgeMove(watching, 1, across(EDGE_BACK_PX - 1))?.phase).toBe('none');
    expect(edgeMove(watching, 1, across(EDGE_BACK_PX))?.phase).toBe('armed');
  });

  it('disarms when the finger is carried home again, so the lift can be taken back', () => {
    const armed = edgeMove(watching, 1, across(EDGE_BACK_PX))!;
    expect(armed.phase).toBe('armed');
    expect(edgeMove(armed, 1, across(EDGE_BACK_PX / 2))?.phase).toBe('none');
    expect(edgeMove(armed, 1, across(-24))).toBeNull();
  });

  it('gives an up-and-down drag back to the transcript it is scrolling', () => {
    expect(edgeMove(watching, 1, { x: AT.x + 6, y: AT.y - 90 })).toBeNull();
  });

  it('keeps a swipe that merely wanders, since no thumb travels straight', () => {
    expect(edgeMove(watching, 1, { x: AT.x + EDGE_BACK_PX, y: AT.y + 10 })?.phase).toBe('armed');
  });
});

/** A pane with something in it, so the walk up the tree has a tree to walk. */
function paneWith(inner: HTMLElement): HTMLElement {
  const pane = document.createElement('div');
  pane.append(inner);
  document.body.append(pane);
  return pane;
}

describe('what the edge strip is standing on', () => {
  it('is free over ordinary transcript content', () => {
    const line = document.createElement('p');
    expect(edgeIsFree(line, paneWith(line))).toBe(true);
  });

  it('gives the drag to a block the reader can scroll sideways', () => {
    const code = document.createElement('pre');
    code.style.overflowX = 'auto';
    Object.defineProperty(code, 'scrollWidth', { value: 900 });
    Object.defineProperty(code, 'clientWidth', { value: 360 });
    expect(edgeIsFree(code, paneWith(code))).toBe(false);
  });

  it('keeps the stroke over a block whose content already fits', () => {
    const code = document.createElement('pre');
    code.style.overflowX = 'auto';
    Object.defineProperty(code, 'scrollWidth', { value: 360 });
    Object.defineProperty(code, 'clientWidth', { value: 360 });
    expect(edgeIsFree(code, paneWith(code))).toBe(true);
  });

  it('stands down while a dialog is up, since that is what owns the screen', () => {
    const line = document.createElement('p');
    const pane = paneWith(line);
    const dialog = document.createElement('div');
    dialog.setAttribute('role', 'dialog');
    dialog.setAttribute('aria-modal', 'true');
    document.body.append(dialog);

    expect(edgeIsFree(line, pane)).toBe(false);

    dialog.remove();
    expect(edgeIsFree(line, pane)).toBe(true);
  });

  it('keeps the stroke for the dialog that is dragging ITSELF out', () => {
    const line = document.createElement('p');
    const pane = paneWith(line);
    const dialog = document.createElement('div');
    dialog.setAttribute('role', 'dialog');
    dialog.setAttribute('aria-modal', 'true');
    pane.append(dialog);

    expect(edgeIsFree(line, pane)).toBe(false);
    expect(edgeIsFree(line, pane, true)).toBe(true);

    dialog.remove();
  });
});

function OpenSession({
  onBack,
  open = true,
  enabled = true,
}: {
  onBack: () => void;
  open?: boolean;
  enabled?: boolean;
}) {
  const { pane, under } = useEdgeBack(enabled ? onBack : null);
  // The same two panes `App` mounts inside the shell: the list is up behind the
  // transcript the whole time it is open, which is what the stroke uncovers.
  return (
    <div data-viewport-shell>
      <div ref={under} data-testid="list">
        sessions
      </div>
      {open && (
        <div ref={pane} data-testid="pane">
          <p data-testid="line">transcript</p>
        </div>
      )}
    </div>
  );
}

/** Swipe in from the edge of the pane by `distance` and leave. */
function swipeIn(element: Element, distance: number, ending: 'lift' | 'cancel' = 'lift'): void {
  const steps = [Math.round(distance / 2), distance].map((by) => ({ x: AT.x + by, y: AT.y }));
  drag(element, AT, steps, ending);
}

/** Let the pop finish itself, however fast the finger was going. */
function settle(): void {
  act(() => {
    vi.advanceTimersByTime(EDGE_SETTLE_MS);
  });
}

/** Where a pane has been carried, in px. */
const xOf = (element: HTMLElement): number =>
  Number(/translateX\((-?[\d.]+)px\)/.exec(element.style.transform)?.[1] ?? NaN);

describe('swiping back to the session list', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it('leaves the session on the lift that ends an armed swipe', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20));
    settle();

    expect(onBack).toHaveBeenCalledTimes(1);
  });

  it('leaves nothing for a swipe that never reached the threshold', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX - 20));
    settle();

    expect(onBack).not.toHaveBeenCalled();
  });

  it('leaves nothing when the browser takes the drag away instead of releasing it', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20, 'cancel'));
    settle();

    expect(onBack).not.toHaveBeenCalled();
  });

  it('ignores a drag that began away from the edge, which is the transcript being read', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);
    const from = { x: 200, y: 300 };

    act(() => drag(getByTestId('line'), from, [{ x: from.x + 200, y: from.y }]));
    settle();

    expect(onBack).not.toHaveBeenCalled();
  });

  it('says nothing at all where the list already stands beside the transcript', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} enabled={false} />);

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20));
    settle();

    expect(onBack).not.toHaveBeenCalled();
  });

  it('watches the pane that arrives after the session has opened', () => {
    const onBack = vi.fn();
    const { getByTestId, rerender } = render(<OpenSession onBack={onBack} open={false} />);
    expect(getByTestId('list')).toBeInTheDocument();

    rerender(<OpenSession onBack={onBack} open />);
    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20));
    settle();

    expect(onBack).toHaveBeenCalledTimes(1);
  });

  it('stops watching when the session closes', () => {
    const onBack = vi.fn();
    const { getByTestId, rerender } = render(<OpenSession onBack={onBack} />);
    const line = getByTestId('line');

    rerender(<OpenSession onBack={onBack} enabled={false} />);
    act(() => {
      fireTouch(line, 'touchstart', [AT]);
      fireTouch(line, 'touchmove', [across(EDGE_BACK_PX + 20)]);
      fireTouch(line, 'touchend', []);
    });
    settle();

    expect(onBack).not.toHaveBeenCalled();
  });
});

// Reported as "the swipe back has no iOS animation at all": the stroke used to
// read the finger and do nothing until the lift, so the transcript stood still
// and the list appeared in one jump. The way back is now the pop iOS
// draws — the transcript rides the finger, the list waits a third of the width
// behind it, dimmed, and the lift carries both the rest of the way.
describe('the pop the stroke draws', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it('drags the transcript with the finger and stands the list behind it', () => {
    const { getByTestId } = render(<OpenSession onBack={vi.fn()} />);
    const pane = getByTestId('pane');
    const list = getByTestId('list');

    act(() => {
      fireTouch(getByTestId('line'), 'touchstart', [AT]);
      fireTouch(getByTestId('line'), 'touchmove', [across(60)]);
    });

    // The transcript is lifted onto the shell so the bar coming back cannot push
    // it down, and it follows the finger one for one.
    expect(pane.style.position).toBe('absolute');
    expect(xOf(pane)).toBe(60);
    // The list waits a third of the width back, dimmed, and has already begun to
    // come home.
    const parked = window.innerWidth * EDGE_PARALLAX;
    expect(xOf(list)).toBeGreaterThan(-parked);
    expect(xOf(list)).toBeLessThan(0);
    expect(Number(list.style.opacity)).toBeGreaterThan(EDGE_UNDER_DIM);
    expect(Number(list.style.opacity)).toBeLessThan(1);
  });

  it("carries the rest of the way on iOS' own curve, and only then takes the step", () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);
    const pane = getByTestId('pane');
    const list = getByTestId('list');

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20));

    expect(pane.style.transition).toContain(EDGE_EASING);
    expect(xOf(pane)).toBe(window.innerWidth);
    expect(xOf(list)).toBe(0);
    expect(list.style.opacity).toBe('1');
    // The transcript is off the glass BEFORE it is off the shell, or the list
    // would appear under a pane that is still sliding.
    expect(onBack).not.toHaveBeenCalled();

    settle();
    expect(onBack).toHaveBeenCalledTimes(1);
  });

  it('carries the transcript home again when the stroke is taken back', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);
    const pane = getByTestId('pane');
    const list = getByTestId('list');

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX - 30));

    expect(xOf(pane)).toBe(0);
    expect(xOf(list)).toBe(-window.innerWidth * EDGE_PARALLAX);
    expect(list.style.opacity).toBe(`${EDGE_UNDER_DIM}`);

    settle();
    expect(onBack).not.toHaveBeenCalled();
  });

  it('keeps the plain step for a reader who asked for less motion', () => {
    const onBack = vi.fn();
    const asked = window.matchMedia;
    window.matchMedia = ((query: string) => ({
      ...asked(query),
      matches: query.includes('prefers-reduced-motion'),
    })) as typeof window.matchMedia;

    try {
      const { getByTestId } = render(<OpenSession onBack={onBack} />);

      act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20));

      expect(getByTestId('pane').style.transform).toBe('');
      expect(onBack).toHaveBeenCalledTimes(1);
    } finally {
      window.matchMedia = asked;
    }
  });
});

/** A dialog standing over the open session, mounted the way `Modal` mounts one. */
function OpenDialog({ onBack }: { onBack: () => void }) {
  const { pane } = useEdgeBack(onBack, { isLayer: true });
  return (
    <div data-viewport-shell>
      <div data-testid="list">sessions</div>
      <div ref={pane} data-testid="box">
        <section role="dialog" aria-modal="true" aria-label="Build" data-testid="run">
          the live view
        </section>
      </div>
    </div>
  );
}

// Reported: the stroke that leaves a session did nothing inside an opened live run, where
// the ✕ was the only way back to the transcript. A dialog is the layer on top, so it takes
// the stroke itself and leaves ITSELF.
describe('a dialog leaving by the same stroke', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it('drags the layer where it stands and dismisses it on the lift', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenDialog onBack={onBack} />);
    const box = getByTestId('box');
    const run = getByTestId('run');

    act(() => {
      fireTouch(run, 'touchstart', [AT]);
      fireTouch(run, 'touchmove', [across(60)]);
    });

    // A layer is in nobody's flow, so it is never put on the shell: it follows the
    // finger from exactly where it already stands.
    expect(box.style.position).toBe('');
    expect(xOf(box)).toBe(60);

    act(() => {
      fireTouch(run, 'touchmove', [across(EDGE_BACK_PX + 20)]);
      fireTouch(run, 'touchend', []);
    });
    settle();

    expect(onBack).toHaveBeenCalledTimes(1);
  });
});

/** Something standing over the application: only its being up matters here. */
function Layer({ onDismiss }: { onDismiss: () => void }) {
  useBackLayer(onDismiss);
  return null;
}

// Reported after the stroke was fixed: on Android the system back never reaches this
// file as touches — it arrives as ONE event for the shell — so a reader inside an
// opened run was carried out of the session instead of out of the run.
describe('the phone answering back while a layer is up', () => {
  it('takes down the layer on top, then the one that was under it', () => {
    const sheet = vi.fn();
    const run = vi.fn();
    const under = render(<Layer onDismiss={sheet} />);
    const over = render(<Layer onDismiss={run} />);

    expect(dismissTopLayer()).toBe(true);
    expect(run).toHaveBeenCalledTimes(1);
    expect(sheet).not.toHaveBeenCalled();

    over.unmount();
    expect(dismissTopLayer()).toBe(true);
    expect(sheet).toHaveBeenCalledTimes(1);

    under.unmount();
    expect(dismissTopLayer()).toBe(false);
  });

  it('leaves back unspent when nothing stands over the application', () => {
    expect(dismissTopLayer()).toBe(false);
  });

  // The dialog that takes the edge stroke is the same layer the button takes down:
  // one statement, whichever way the phone says "back".
  it('answers for a dialog that took the stroke', () => {
    const onBack = vi.fn();
    const dialog = render(<OpenDialog onBack={onBack} />);

    expect(dismissTopLayer()).toBe(true);
    expect(onBack).toHaveBeenCalledTimes(1);

    dialog.unmount();
    expect(dismissTopLayer()).toBe(false);
  });
});
