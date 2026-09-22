// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { STORY_GATEWAYS, STORY_SESSION_ROW } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import type { Session } from '../lib/types';
import { SessionRow } from './SessionList';

// Putting a session away is a decision about the conversation, and the GATEWAY holds it,
// so every row that paints that session — the list, a project's reveal, a search result —
// reads the same stamp. The status cell is the one place a row says what it is, and for
// an archived session that is what it says.
describe('an archived session row', () => {
  const row = (session: Partial<Session>) =>
    render(
      <SessionRow
        session={{ ...STORY_SESSION_ROW, ...session }}
        group={null}
        draft={EMPTY_DRAFT_MESSAGE}
        conn={STORY_GATEWAYS[0]}
        match={null}
        needle=""
        commands={{
          open: vi.fn(),
          rename: vi.fn(async () => {}),
          requestDelete: vi.fn(),
          toggleStar: vi.fn(),
        }}
        deletion={null}
      />,
    ).container;

  const idle = {
    status: 'idle',
    live: false,
    current_turn_id: null,
    is_awaiting_input: false,
    was_interrupted: false,
    is_unread: false,
    unread_answers: 0,
  } satisfies Partial<Session>;

  const statusCell = (session: Partial<Session>) =>
    row(session).querySelector('[data-session-status]')!;

  it('says ARCHIVED where a working row says what it is doing', () => {
    const container = row({ ...idle, archived_at: 1717 });
    const cell = container.querySelector('[data-session-status]')!;
    expect(cell.textContent).toContain('ARCHIVED');
    expect(cell.className).toContain('text-muted');
    // Filled and dimmed: put away is a state the row IS in, not the absence of one, so
    // it takes a solid mark rather than IDLE's hollow square.
    const dot = container.querySelector('[data-session-status-dot]')!;
    expect(dot.className).toContain('bg-muted');
    expect(dot.className).not.toContain('animate-pulse');
  });

  it('keeps its mark at narrow widths, where IDLE hides its own', () => {
    expect(statusCell({ ...idle, archived_at: 1717 }).className).not.toContain('hidden');
    expect(statusCell({ ...idle, archived_at: null }).className).toContain('hidden @sm:inline-flex');
  });

  it('outranks whatever the row was doing when it was filed', () => {
    // A row left open on another screen still carries the flags of the turn it was
    // watching. The archive is the newer fact about that session, and the gateway
    // refuses work on it either way, so the mark cannot keep saying it is waiting.
    expect(statusCell({ archived_at: 1717 }).textContent).toContain('ARCHIVED');
    expect(screen.queryByText('INPUT NEEDED')).not.toBeInTheDocument();
  });
});
