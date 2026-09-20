// @vitest-environment jsdom
import { act, render } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

// jsdom cannot build a `Touch`, so the same dispatch helpers the pull gesture
// uses carry these fingers too.
import { drag, fireTouch } from './pull-to-search.fixture';
import {
  EDGE_BACK_PX,
  EDGE_ZONE_PX,
  edgeIsFree,
  edgeMove,
  edgeStart,
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
  const pane = useEdgeBack(enabled ? onBack : null);
  if (!open) return <p data-testid="list">sessions</p>;
  return (
    <div ref={pane} data-testid="pane">
      <p data-testid="line">transcript</p>
    </div>
  );
}

/** Swipe in from the edge of the pane by `distance` and leave. */
function swipeIn(element: Element, distance: number, ending: 'lift' | 'cancel' = 'lift'): void {
  const steps = [Math.round(distance / 2), distance].map((by) => ({ x: AT.x + by, y: AT.y }));
  drag(element, AT, steps, ending);
}

describe('swiping back to the session list', () => {
  it('leaves the session on the lift that ends an armed swipe', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20));

    expect(onBack).toHaveBeenCalledTimes(1);
  });

  it('leaves nothing for a swipe that never reached the threshold', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX - 20));

    expect(onBack).not.toHaveBeenCalled();
  });

  it('leaves nothing when the browser takes the drag away instead of releasing it', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20, 'cancel'));

    expect(onBack).not.toHaveBeenCalled();
  });

  it('ignores a drag that began away from the edge, which is the transcript being read', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} />);
    const from = { x: 200, y: 300 };

    act(() => drag(getByTestId('line'), from, [{ x: from.x + 200, y: from.y }]));

    expect(onBack).not.toHaveBeenCalled();
  });

  it('says nothing at all where the list already stands beside the transcript', () => {
    const onBack = vi.fn();
    const { getByTestId } = render(<OpenSession onBack={onBack} enabled={false} />);

    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20));

    expect(onBack).not.toHaveBeenCalled();
  });

  it('watches the pane that arrives after the session has opened', () => {
    const onBack = vi.fn();
    const { getByTestId, rerender } = render(<OpenSession onBack={onBack} open={false} />);
    expect(getByTestId('list')).toBeInTheDocument();

    rerender(<OpenSession onBack={onBack} open />);
    act(() => swipeIn(getByTestId('line'), EDGE_BACK_PX + 20));

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

    expect(onBack).not.toHaveBeenCalled();
  });
});
