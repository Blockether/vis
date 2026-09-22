// @vitest-environment jsdom
import { useRef } from 'react';
import { act, cleanup, render } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { fireTouch } from './pull-to-search.fixture';
import {
  DROP_TARGET_ATTRIBUTE,
  EDGE_PULL_PX,
  EDGE_PULL_SPEED,
  LIFT_DELAY_MS,
  LIFT_SLOP_PX,
  beginLift,
  cancelLift,
  carryOver,
  dropTargetAt,
  edgePull,
  liftedSession,
  movedBeyond,
  offerLift,
  releaseLift,
  useSessionDropTarget,
  useSessionLift,
} from './session-drag';

/** jsdom lays nothing out, so every box in these tests is stated outright. */
interface Box {
  left: number;
  top: number;
  width: number;
  height: number;
}

function stand(element: Element, box: Box): void {
  vi.spyOn(element, 'getBoundingClientRect').mockReturnValue({
    ...box,
    right: box.left + box.width,
    bottom: box.top + box.height,
    x: box.left,
    y: box.top,
    toJSON: () => ({}),
  } as DOMRect);
}

function placeAt(key: string, box: Box): Element {
  const element = document.createElement('div');
  element.setAttribute(DROP_TARGET_ATTRIBUTE, key);
  stand(element, box);
  return element;
}

describe('reading a finger that is picking a session up', () => {
  it('holds still while the finger stays where it landed', () => {
    const from = { x: 100, y: 200 };
    expect(movedBeyond(from, { x: 100 + LIFT_SLOP_PX, y: 200 })).toBe(false);
    expect(movedBeyond(from, { x: 100, y: 200 + LIFT_SLOP_PX + 1 })).toBe(true);
  });

  it('answers the innermost place under the finger, because a band stands in a set', () => {
    const places = [
      placeAt('sessions', { left: 0, top: 0, width: 390, height: 600 }),
      placeAt('group', { left: 0, top: 200, width: 390, height: 220 }),
    ];
    expect(dropTargetAt({ x: 100, y: 260 }, places)).toBe('group');
    expect(dropTargetAt({ x: 100, y: 100 }, places)).toBe('sessions');
  });

  it('answers nothing where no place is, so a release there files nothing', () => {
    const places = [placeAt('group', { left: 0, top: 200, width: 390, height: 220 })];
    expect(dropTargetAt({ x: 100, y: 700 }, places)).toBeNull();
  });

  it('pulls the list along only within reach of an edge', () => {
    const view = { top: 0, bottom: 800 };
    expect(edgePull(400, view)).toBe(0);
    expect(edgePull(EDGE_PULL_PX, view)).toBe(0);
    expect(edgePull(0, view)).toBe(-EDGE_PULL_SPEED);
    expect(edgePull(800, view)).toBe(EDGE_PULL_SPEED);
    expect(edgePull(-40, view)).toBe(-EDGE_PULL_SPEED);
  });
});

describe('carrying one session between places', () => {
  afterEach(() => {
    cancelLift();
  });

  it('hands the session to the place it was released over', () => {
    const take = vi.fn();
    const withdraw = offerLift('group', take);
    beginLift('ses-1');
    carryOver('group');
    expect(liftedSession()).toEqual({ sessionId: 'ses-1', over: 'group' });
    expect(releaseLift()).toBe(true);
    expect(take).toHaveBeenCalledWith('ses-1');
    expect(liftedSession()).toBeNull();
    withdraw();
  });

  it('files nothing when the session is released over no place at all', () => {
    const take = vi.fn();
    const withdraw = offerLift('group', take);
    beginLift('ses-1');
    expect(releaseLift()).toBe(false);
    expect(take).toHaveBeenCalledTimes(0);
    withdraw();
  });

  it('files nothing when the browser takes the gesture away', () => {
    const take = vi.fn();
    const withdraw = offerLift('group', take);
    beginLift('ses-1');
    carryOver('group');
    cancelLift();
    expect(liftedSession()).toBeNull();
    expect(take).toHaveBeenCalledTimes(0);
    withdraw();
  });
});

const SESSION = 'ses-1';

/** The set of ungrouped sessions, one group's band, and a row standing in that band. */
function Board({
  toSessions,
  toGroup,
  canFile = true,
}: {
  toSessions?: (sid: string) => void;
  toGroup?: (sid: string) => void;
  canFile?: boolean;
}) {
  const row = useRef<HTMLDivElement>(null);
  const isCarried = useSessionLift(SESSION, row, canFile);
  const sessions = useSessionDropTarget(toSessions);
  const group = useSessionDropTarget(toGroup);
  return (
    <div>
      <div
        data-testid="sessions"
        data-over={sessions.isOver ? 'yes' : 'no'}
        {...sessions.targetProps}
      />
      <div data-testid="group" data-over={group.isOver ? 'yes' : 'no'} {...group.targetProps}>
        <div ref={row} data-testid="row" data-carried={isCarried ? 'yes' : 'no'}>
          A session
        </div>
      </div>
    </div>
  );
}

const SESSIONS_BOX: Box = { left: 0, top: 100, width: 390, height: 44 };
const GROUP_BOX: Box = { left: 0, top: 200, width: 390, height: 220 };
const ROW_BOX: Box = { left: 0, top: 240, width: 390, height: 60 };

/** On the row, inside its group's band. */
const ON_ROW = { x: 100, y: 260 };
/** Over the set of ungrouped sessions, far from either edge of the view. */
const ON_SESSIONS = { x: 100, y: 120 };
/** Over the page, and over no place that takes a session. */
const NOWHERE = { x: 100, y: 600 };

function board(props: Parameters<typeof Board>[0]) {
  const view = render(<Board {...props} />);
  const row = view.getByTestId('row');
  stand(row, ROW_BOX);
  stand(view.getByTestId('sessions'), SESSIONS_BOX);
  stand(view.getByTestId('group'), GROUP_BOX);
  return { view, row };
}

/** How many copies of a row are travelling above the page right now. */
function copiesInTheAir(): number {
  return document.querySelectorAll('[aria-hidden="true"]').length;
}

describe('picking a session row up with a finger', () => {
  afterEach(() => {
    cleanup();
    vi.useRealTimers();
  });

  it('picks the row up once the finger has rested on it', () => {
    vi.useFakeTimers();
    const { row } = board({ toSessions: vi.fn(), toGroup: vi.fn() });

    fireTouch(row, 'touchstart', [ON_ROW]);
    expect(row.dataset.carried).toBe('no');

    act(() => {
      vi.advanceTimersByTime(LIFT_DELAY_MS);
    });
    expect(row.dataset.carried).toBe('yes');
    expect(copiesInTheAir()).toBe(1);

    act(() => {
      fireTouch(row, 'touchend', []);
    });
    expect(copiesInTheAir()).toBe(0);
  });

  it('leaves a finger that moves on before the row is up to the list it is scrolling', () => {
    vi.useFakeTimers();
    const toSessions = vi.fn();
    const { row } = board({ toSessions, toGroup: vi.fn() });

    fireTouch(row, 'touchstart', [ON_ROW]);
    fireTouch(row, 'touchmove', [{ x: ON_ROW.x, y: ON_ROW.y - LIFT_SLOP_PX - 12 }]);
    act(() => {
      vi.advanceTimersByTime(LIFT_DELAY_MS);
    });

    expect(row.dataset.carried).toBe('no');
    expect(copiesInTheAir()).toBe(0);
    fireTouch(row, 'touchend', []);
    expect(toSessions).toHaveBeenCalledTimes(0);
  });

  it('files the session into the set the finger carried it to', () => {
    vi.useFakeTimers();
    const toSessions = vi.fn();
    const toGroup = vi.fn();
    const { view, row } = board({ toSessions, toGroup });

    fireTouch(row, 'touchstart', [ON_ROW]);
    act(() => {
      vi.advanceTimersByTime(LIFT_DELAY_MS);
    });
    // It starts over the band it is standing in, and that band says so.
    expect(view.getByTestId('group').dataset.over).toBe('yes');

    act(() => {
      fireTouch(row, 'touchmove', [ON_SESSIONS]);
    });
    expect(view.getByTestId('sessions').dataset.over).toBe('yes');
    expect(view.getByTestId('group').dataset.over).toBe('no');

    act(() => {
      fireTouch(row, 'touchend', []);
    });
    expect(toSessions).toHaveBeenCalledWith(SESSION);
    expect(toGroup).toHaveBeenCalledTimes(0);
    expect(view.getByTestId('sessions').dataset.over).toBe('no');
  });

  it('keeps the list and the row swipe still while a row is being carried', () => {
    vi.useFakeTimers();
    const { row } = board({ toSessions: vi.fn(), toGroup: vi.fn() });

    fireTouch(row, 'touchstart', [ON_ROW]);
    const scrolling = fireTouch(row, 'touchmove', [{ x: ON_ROW.x + 2, y: ON_ROW.y }]);
    expect(scrolling.defaultPrevented).toBe(false);

    act(() => {
      vi.advanceTimersByTime(LIFT_DELAY_MS);
    });
    const carrying = fireTouch(row, 'touchmove', [ON_SESSIONS]);
    expect(carrying.defaultPrevented).toBe(true);

    act(() => {
      fireTouch(row, 'touchcancel', []);
    });
  });

  it('puts the row back when it is released over nothing', () => {
    vi.useFakeTimers();
    const toSessions = vi.fn();
    const toGroup = vi.fn();
    const { row } = board({ toSessions, toGroup });

    fireTouch(row, 'touchstart', [ON_ROW]);
    act(() => {
      vi.advanceTimersByTime(LIFT_DELAY_MS);
    });
    act(() => {
      fireTouch(row, 'touchmove', [NOWHERE]);
      fireTouch(row, 'touchend', []);
    });

    expect(toSessions).toHaveBeenCalledTimes(0);
    expect(toGroup).toHaveBeenCalledTimes(0);
    expect(row.dataset.carried).toBe('no');
    expect(copiesInTheAir()).toBe(0);
  });

  it('files nothing when the browser takes the carry away mid-gesture', () => {
    vi.useFakeTimers();
    const toGroup = vi.fn();
    const { row } = board({ toSessions: vi.fn(), toGroup });

    fireTouch(row, 'touchstart', [ON_ROW]);
    act(() => {
      vi.advanceTimersByTime(LIFT_DELAY_MS);
    });
    act(() => {
      fireTouch(row, 'touchcancel', []);
    });

    expect(toGroup).toHaveBeenCalledTimes(0);
    expect(row.dataset.carried).toBe('no');
    expect(copiesInTheAir()).toBe(0);
  });

  it('leaves a row alone in a list with nowhere to file it', () => {
    vi.useFakeTimers();
    const { row } = board({ toSessions: undefined, toGroup: undefined, canFile: false });

    fireTouch(row, 'touchstart', [ON_ROW]);
    act(() => {
      vi.advanceTimersByTime(LIFT_DELAY_MS);
    });

    expect(row.dataset.carried).toBe('no');
    expect(copiesInTheAir()).toBe(0);
    expect(document.querySelectorAll(`[${DROP_TARGET_ATTRIBUTE}]`).length).toBe(0);
  });
});
