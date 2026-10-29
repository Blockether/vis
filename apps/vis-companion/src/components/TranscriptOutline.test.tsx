// @vitest-environment jsdom
import { act, fireEvent, render, screen } from '@testing-library/react';
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

});
