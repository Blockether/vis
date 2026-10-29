// @vitest-environment jsdom
import { fireEvent, render } from '@testing-library/react';
import type { ComponentProps } from 'react';
import { describe, expect, it, vi } from 'vitest';

import { STORY_GATEWAYS, STORY_SESSION_ROW } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import { SessionRow } from './SessionList';

const SKIPPED_OFF_SCREEN = 'in-data-rows-settled:[content-visibility:auto]';

function renderRow(props: Partial<ComponentProps<typeof SessionRow>> = {}) {
  const commands = {
    open: vi.fn(),
    rename: vi.fn(async () => {}),
    requestDelete: vi.fn(),
    toggleStar: vi.fn(),
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
      {...props}
    />,
  );
  const row = view.container.querySelector('[data-session-row]') as HTMLElement;
  return { row, commands, surface: row.querySelector('[data-session-id]') as HTMLElement };
}

// Regression, user report: going back from a session to a long list lagged while Chromium
// sorted a compositor layer for every row's swipe track.
describe('a session row out of sight', () => {
  it('stays drawn while it asks to confirm a delete', () => {
    const { row } = renderRow({
      deletion: { isBusy: false, error: null, confirm: vi.fn(), cancel: vi.fn() },
    });
    expect(row).not.toHaveClass(SKIPPED_OFF_SCREEN);
  });
});

// The list hands every row one selection handler, so a selection change does not render
// every memoized row again.
describe('a session row in a selection', () => {
  it('hands the shared handler its own id instead of opening', () => {
    const onSelectionClick = vi.fn(() => true);
    const { surface, commands } = renderRow({ onSelectionClick });
    fireEvent.click(surface);
    expect(onSelectionClick).toHaveBeenCalledWith(
      STORY_SESSION_ROW.id,
      expect.objectContaining({ type: 'click' }),
    );
    expect(commands.open).not.toHaveBeenCalled();
  });
});
