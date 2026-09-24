// @vitest-environment jsdom
import { createEvent, fireEvent, render } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { STORY_GATEWAYS, STORY_SESSION_ROW } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import type { Session } from '../lib/types';
import { SessionRow } from './SessionList';

const NEIGHBOUR: Session = { ...STORY_SESSION_ROW, id: 'neighbour', title: 'Neighbour session' };

function mount(selected: readonly string[] = []) {
  const commands = {
    open: vi.fn(),
    rename: vi.fn(async () => {}),
    requestDelete: vi.fn(),
    toggleStar: vi.fn(),
  };
  render(
    <>
      {[STORY_SESSION_ROW, NEIGHBOUR].map((session) => (
        <SessionRow
          key={session.id}
          session={session}
          group={null}
          draft={EMPTY_DRAFT_MESSAGE}
          conn={STORY_GATEWAYS[0]}
          match={null}
          needle=""
          commands={commands}
          deletion={null}
          isSelected={selected.includes(session.id)}
          dragIds={selected}
          isDraggable
        />
      ))}
    </>,
  );
  return (sid: string) => document.querySelector(`[data-session-row="${sid}"]`) as HTMLElement;
}

function carrier() {
  return { setData: vi.fn(), setDragImage: vi.fn(), effectAllowed: '' };
}

// A pointer takes hold of a row AT A POINT. jsdom has no `DragEvent`, so the stand-in it
// builds carries no coordinates of its own and has to be told where the hand closed.
function pickUp(row: HTMLElement, dataTransfer: ReturnType<typeof carrier>, x: number, y: number) {
  const event = createEvent.dragStart(row, { dataTransfer });
  Object.defineProperty(event, 'clientX', { value: x });
  Object.defineProperty(event, 'clientY', { value: y });
  fireEvent(row, event);
}

// Regression, user report with a screenshot of the desktop window (paraphrased: taking one
// session often looks like taking several). Left to itself WebKit paints its drag picture
// from the layer the row stands in rather than from the row, so the ghost under the cursor
// carried a copy of every row below the one that was picked up.
describe('a session picked up by the pointer', () => {
  it('hands the browser a picture of the one row it took', () => {
    const row = mount();
    const dataTransfer = carrier();

    pickUp(row(STORY_SESSION_ROW.id), dataTransfer, 40, 12);

    expect(dataTransfer.setData).toHaveBeenCalledWith('text/plain', STORY_SESSION_ROW.id);
    // Held where it was grabbed: jsdom lays every box at the origin, so the offsets into
    // the picture are the pointer's own coordinates.
    expect(dataTransfer.setDragImage).toHaveBeenCalledWith(expect.any(HTMLElement), 40, 12);

    const picture = dataTransfer.setDragImage.mock.calls[0][0] as HTMLElement;
    expect(picture.querySelectorAll('[data-row-surface]')).toHaveLength(1);
    expect(picture.textContent).toContain(String(STORY_SESSION_ROW.title));
    expect(picture.textContent).not.toContain(String(NEIGHBOUR.title));
    // A see-through picture shows the very rows it stands over, which is the report.
    expect(picture.className).toContain('bg-panel');
    // This row answers its width to the LIST it stands in, and the picture stands outside
    // that list: without a container of its own the copy loses the row's second line and
    // its group rail stops short of the card's end (reported from the desktop app).
    expect(picture.className).toContain('@container');
    // The copy is a picture, not a second row: nothing that looks for this session finds it.
    expect(
      document.querySelectorAll(`[data-session-id="${String(STORY_SESSION_ROW.id)}"]`),
    ).toHaveLength(1);
  });

  it('shows every selected row in the drag picture, with the picked row under the pointer', () => {
    const row = mount([STORY_SESSION_ROW.id, NEIGHBOUR.id]);
    const dataTransfer = carrier();
    vi.spyOn(row(NEIGHBOUR.id), 'getBoundingClientRect').mockReturnValue(
      { top: 40, left: 5, width: 280, height: 36 } as DOMRect,
    );
    vi.spyOn(row(STORY_SESSION_ROW.id), 'getBoundingClientRect').mockReturnValue(
      { top: 0, left: 5, width: 280, height: 36 } as DOMRect,
    );

    pickUp(row(NEIGHBOUR.id), dataTransfer, 18, 47);

    const picture = dataTransfer.setDragImage.mock.calls[0][0] as HTMLElement;
    expect(dataTransfer.setDragImage).toHaveBeenCalledWith(picture, 13, 7);
    expect(picture.style.height).toBe('72px');
    expect(Array.from(picture.children).slice(0, 2).map((card) => (card as HTMLElement).style.height)).toEqual([
      '36px', '36px',
    ]);
    expect(dataTransfer.setData).toHaveBeenCalledWith(
      'application/vnd.vis.sessions+json', JSON.stringify([STORY_SESSION_ROW.id, NEIGHBOUR.id]),
    );
    const surfaces = Array.from(
      picture.querySelectorAll('[data-row-surface]'),
      (surface) => surface.textContent,
    );
    expect(surfaces).toEqual([
      expect.stringContaining(String(NEIGHBOUR.title)),
      expect.stringContaining(String(STORY_SESSION_ROW.title)),
    ]);
    expect(picture.querySelectorAll('[data-session-row], [data-session-id]')).toHaveLength(0);
    expect(picture).toHaveAttribute('aria-hidden', 'true');
  });

  it('takes the copy back out of the page once the drag is under way', async () => {
    const row = mount();
    const dataTransfer = carrier();

    pickUp(row(NEIGHBOUR.id), dataTransfer, 8, 8);

    const picture = dataTransfer.setDragImage.mock.calls[0][0] as HTMLElement;
    expect(document.body.contains(picture)).toBe(true);
    await new Promise((settle) => setTimeout(settle, 0));
    expect(document.body.contains(picture)).toBe(false);
    expect(document.querySelectorAll('[data-row-surface]')).toHaveLength(2);
  });
});
