// @vitest-environment jsdom
import { fireEvent, render } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { STORY_GATEWAYS, STORY_SESSION_ROW } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import { SessionRow, type SessionRowCommands } from './SessionList';

// Regression, this Vis session (paraphrased: "switching between sessions on the web is
// very slow"): the transcript read began only after the click had mounted the screen.
// A row the reader reaches for starts that read while the click is still on its way.
describe('reaching for a session row', () => {
  afterEach(() => vi.useRealTimers());

  const row = (isOpen = false) => {
    const commands: SessionRowCommands = {
      open: vi.fn(),
      rename: vi.fn(async () => {}),
      requestDelete: vi.fn(),
      toggleStar: vi.fn(),
      warm: vi.fn(),
    };
    const view = render(
      <SessionRow
        session={STORY_SESSION_ROW}
        group={null}
        draft={EMPTY_DRAFT_MESSAGE}
        conn={STORY_GATEWAYS[0]}
        needle=""
        commands={commands}
        deletion={null}
        isOpen={isOpen}
      />,
    );
    const surface = view.container.querySelector<HTMLElement>('[data-row-surface]');
    if (!surface) throw new Error('The row has no pressable surface.');
    return { commands, surface };
  };

  it('reads the transcript once a mouse pointer rests on the row', () => {
    vi.useFakeTimers();
    const { commands, surface } = row();

    fireEvent.pointerEnter(surface, { pointerType: 'mouse' });
    expect(commands.warm).not.toHaveBeenCalled();
    vi.advanceTimersByTime(100);

    expect(commands.warm).toHaveBeenCalledTimes(1);
    expect(commands.warm).toHaveBeenCalledWith(STORY_GATEWAYS[0], STORY_SESSION_ROW);
  });

  it('leaves alone a row the pointer only crosses', () => {
    vi.useFakeTimers();
    const { commands, surface } = row();

    fireEvent.pointerEnter(surface, { pointerType: 'mouse' });
    fireEvent.pointerLeave(surface, { pointerType: 'mouse' });
    vi.advanceTimersByTime(1000);

    expect(commands.warm).not.toHaveBeenCalled();
  });

  it('reads the transcript as soon as the row is pressed', () => {
    const { commands, surface } = row();

    fireEvent.pointerDown(surface, { button: 0, pointerType: 'touch' });

    expect(commands.warm).toHaveBeenCalledTimes(1);
  });

  it('does not read again for the session already open beside the list', () => {
    vi.useFakeTimers();
    const { commands, surface } = row(true);

    fireEvent.pointerEnter(surface, { pointerType: 'mouse' });
    vi.advanceTimersByTime(100);
    fireEvent.pointerDown(surface, { button: 0, pointerType: 'mouse' });

    expect(commands.warm).not.toHaveBeenCalled();
  });
});
