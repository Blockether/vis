// @vitest-environment jsdom
import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { useRef } from 'react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { NARROW_SCREEN, RAIL_SHOW_MS, type OutlineEntry } from '../lib/transcript-outline';
import { TranscriptOutline } from './TranscriptOutline';

const LONG_ANSWER =
  'The gateway kept both images. The tile showed only the file name, because the gateway sent the attachments without their step, so the app could not fetch them.';

const ENTRIES: OutlineEntry[] = [
  { id: 't1', label: 'Where did the images go?', answer: LONG_ANSWER, status: 'failed', isCouncil: false },
  { id: 't2', label: 'Is it fixed now?', answer: 'Yes.', status: 'done', isCouncil: false },
];

function Outline() {
  const scroller = useRef<HTMLDivElement>(null);
  const column = useRef<HTMLDivElement>(null);
  return (
    <div ref={scroller}>
      <div ref={column}>
        {ENTRIES.map((entry) => (
          <div key={entry.id} data-turn-id={entry.id}>
            {entry.label}
          </div>
        ))}
      </div>
      <TranscriptOutline entries={ENTRIES} total={ENTRIES.length} scroller={scroller} column={column} onJump={() => {}} />
    </div>
  );
}

/** Lays the long answer out in ten lines of 18px, as a browser does, while its preview shows six. */
function overflowLongAnswer() {
  const height = (element: Element, lines: number) => (element.textContent === LONG_ANSWER ? lines * 18 : 0);
  vi.spyOn(Element.prototype, 'scrollHeight', 'get').mockImplementation(function (this: Element) {
    return height(this, 10);
  });
  vi.spyOn(Element.prototype, 'clientHeight', 'get').mockImplementation(function (this: Element) {
    return height(this, 6);
  });
}

/** Puts the outline card at the right edge of a wide screen, so its preview has room on the left. */
function placeCardRight() {
  const rect = Element.prototype.getBoundingClientRect;
  const card = { x: 1100, y: 12, left: 1100, top: 12, right: 1388, bottom: 432, width: 288, height: 420 };
  vi.spyOn(Element.prototype, 'getBoundingClientRect').mockImplementation(function (this: Element) {
    return this.getAttribute('role') === 'dialog' ? ({ ...card, toJSON: () => card } as DOMRect) : rect.call(this);
  });
}

describe('TranscriptOutline preview', () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it('names the state of the turn and fades the end of a cut answer', async () => {
    overflowLongAnswer();
    placeCardRight();
    const user = userEvent.setup();
    render(<Outline />);
    await user.click(screen.getByRole('button', { name: 'Jump to a message' }));
    const panel = await screen.findByRole('dialog', { name: 'Jump to a message' });
    const [cut, whole] = within(panel).getAllByRole('button');

    await user.hover(cut);
    const preview = await screen.findByRole('tooltip');
    expect(within(preview).getByText('Failed')).toHaveClass('text-err');
    await waitFor(() => expect(within(preview).getByText(LONG_ANSWER).className).toContain('mask-image'));

    await user.hover(whole);
    await waitFor(() => expect(within(screen.getByRole('tooltip')).getByText('Done')).toHaveClass('text-ok'));
    expect(within(screen.getByRole('tooltip')).getByText('Yes.').className).not.toContain('mask-image');
  });
});

/** Makes the screen as narrow as a phone. */
function narrowScreen() {
  vi.spyOn(window, 'matchMedia').mockImplementation(
    (query: string) =>
      ({
        matches: query === NARROW_SCREEN,
        media: query,
        onchange: null,
        addListener: () => {},
        removeListener: () => {},
        addEventListener: () => {},
        removeEventListener: () => {},
        dispatchEvent: () => false,
      }) as MediaQueryList,
  );
}

/** A finger taps the rail. */
function tap(rail: HTMLElement) {
  const finger = { pointerType: 'touch', isPrimary: true, pointerId: 1 };
  fireEvent.pointerDown(rail, finger);
  fireEvent.pointerUp(rail, finger);
  fireEvent.click(rail, { detail: 1 });
}

describe('TranscriptOutline rail on a narrow screen', () => {
  afterEach(() => {
    vi.useRealTimers();
    vi.restoreAllMocks();
  });

  it('shows the hidden rail while the reader scrolls, and hides it again after', () => {
    vi.useFakeTimers();
    narrowScreen();
    const { container } = render(<Outline />);
    const scroller = container.firstElementChild as HTMLElement;
    const rail = screen.getByRole('button', { name: 'Jump to a message' });
    expect(rail).toHaveClass('max-sm:opacity-0');

    fireEvent.touchMove(scroller);
    expect(rail).not.toHaveClass('max-sm:opacity-0');
    act(() => vi.advanceTimersByTime(RAIL_SHOW_MS - 1));
    // Momentum scrolls on after the finger lifts and keeps the rail on screen.
    fireEvent.scroll(scroller);
    act(() => vi.advanceTimersByTime(RAIL_SHOW_MS - 1));
    expect(rail).not.toHaveClass('max-sm:opacity-0');
    act(() => vi.advanceTimersByTime(1));
    expect(rail).toHaveClass('max-sm:opacity-0');
  });

  it('only shows the hidden rail on the first tap, and opens the card on the next', () => {
    narrowScreen();
    render(<Outline />);
    const rail = screen.getByRole('button', { name: 'Jump to a message' });

    tap(rail);
    expect(rail).not.toHaveClass('max-sm:opacity-0');
    expect(screen.queryByRole('dialog', { name: 'Jump to a message' })).not.toBeInTheDocument();

    tap(rail);
    expect(screen.getByRole('dialog', { name: 'Jump to a message' })).toBeInTheDocument();
  });

  it('paints the line of the turn on screen longer than the others', async () => {
    render(<Outline />);
    const rail = screen.getByRole('button', { name: 'Jump to a message' });
    const active = await waitFor(() => {
      const line = rail.querySelector('[data-active]');
      expect(line).not.toBeNull();
      return line;
    });
    expect(active).toHaveClass('w-5');
    for (const line of rail.querySelectorAll('span:not([data-active])')) expect(line).toHaveClass('w-4');
  });
});
