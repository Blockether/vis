// @vitest-environment jsdom
import { fireEvent, render } from '@testing-library/react';
import { useRef } from 'react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { forgetListScroll, parkedListScroll, topVisibleRow, useListScrollPark } from './list-scroll';

const ROWS_PER_GROUP = 32;

/** Expanded groups keep their sessions in the same vertical order as the navigator. */
function List({ count = 1024, isVisible = true }: { count?: number; isVisible?: boolean }) {
  const ref = useRef<HTMLDivElement | null>(null);
  useListScrollPark(ref, () => {}, isVisible);
  return (
    <div ref={ref} data-testid="list">
      {Array.from({ length: Math.ceil(count / ROWS_PER_GROUP) }, (_, group) => (
        <section key={group}>
          <h2>Group {group}</h2>
          {Array.from(
            { length: Math.min(ROWS_PER_GROUP, count - group * ROWS_PER_GROUP) },
            (_, row) => {
              const id = `s${group * ROWS_PER_GROUP + row}`;
              return (
                <article key={id} data-session-id={id}>
                  {id}
                </article>
              );
            },
          )}
        </section>
      ))}
    </div>
  );
}

/** jsdom has no layout; model a phone's rows and the gaps occupied by group headings. */
function layout(list: HTMLElement, heights?: number[]) {
  const measured = vi.fn();
  list.getBoundingClientRect = () => new DOMRect(0, 100, 390, 600);
  let offset = 0;
  const starts: number[] = [];
  const rows = list.querySelectorAll<HTMLElement>('[data-session-id]');
  for (const [index, row] of Array.from(rows).entries()) {
    if (index % ROWS_PER_GROUP === 0) offset += 30;
    const top = offset;
    const height = heights?.[index] ?? 50;
    starts.push(top);
    row.getBoundingClientRect = () => {
      measured(row.dataset.sessionId);
      return new DOMRect(0, 100 + top - list.scrollTop, 390, height);
    };
    offset += height;
  }
  return { measured, starts, end: offset };
}

beforeEach(() => forgetListScroll());
afterEach(() => forgetListScroll());

// Regression, user report: scrolling with every group expanded slowed down as the reader
// moved down the list. Each scroll measured every preceding session to remember its anchor.
describe('scrolling a long expanded sessions list', () => {
  it('finds the row near the end without measuring every preceding session', () => {
    const { getByTestId, rerender } = render(<List />);
    const list = getByTestId('list');
    const { measured, starts } = layout(list);
    list.scrollTop = starts[960] + 10;

    fireEvent.scroll(list);

    expect(measured.mock.calls.length).toBeLessThanOrEqual(11);
    rerender(<List isVisible={false} />);
    expect(parkedListScroll()).toEqual({
      top: starts[960] + 10,
      anchor: { id: 's960', offset: -10 },
    });
  });

  it('does not measure rows when scrolling back to the top', () => {
    const { getByTestId, rerender } = render(<List />);
    const list = getByTestId('list');
    const { measured } = layout(list);
    list.scrollTop = 0;

    fireEvent.scroll(list);

    expect(measured).not.toHaveBeenCalled();
    rerender(<List isVisible={false} />);
    expect(parkedListScroll()).toBeNull();
  });
});

describe('the first session below the list edge', () => {
  it('keeps a partly visible row and uses its actual height', () => {
    const { getByTestId } = render(<List count={65} />);
    const list = getByTestId('list');
    const heights = Array.from({ length: 65 }, (_, index) => index === 32 ? 140 : 46 + index % 3);
    const { starts } = layout(list, heights);
    list.scrollTop = starts[32] + 139;

    expect(topVisibleRow(list)).toEqual({ id: 's32', offset: -139 });
  });

  it('anchors on the next session when a group heading occupies the top edge', () => {
    const { getByTestId } = render(<List count={65} />);
    const list = getByTestId('list');
    const { starts } = layout(list);
    list.scrollTop = starts[32] - 10;

    expect(topVisibleRow(list)).toEqual({ id: 's32', offset: 10 });
  });

  it('excludes a row whose bottom is exactly at the list edge', () => {
    const { getByTestId } = render(<List count={3} />);
    const list = getByTestId('list');
    const { starts } = layout(list);
    list.scrollTop = starts[1];

    expect(topVisibleRow(list)).toEqual({ id: 's1', offset: 0 });
  });

  it('returns no anchor when the list is absent, empty or past its final row', () => {
    expect(topVisibleRow(null)).toBeNull();
    const { getByTestId, rerender } = render(<List count={0} />);
    const list = getByTestId('list');
    layout(list);
    expect(topVisibleRow(list)).toBeNull();

    rerender(<List count={1} />);
    const { end } = layout(list);
    list.scrollTop = end;
    expect(topVisibleRow(list)).toBeNull();
  });
});
