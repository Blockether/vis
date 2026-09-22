// @vitest-environment jsdom
import { render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

import { STORY_GATEWAYS, STORY_SESSION_ROW } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import { GatewayError } from '../lib/gateway';
import type { GatewayConn, Session } from '../lib/types';
import { SessionRow, type SessionRowCommands } from './SessionList';

// A conversation that has stopped. The archive is for sessions nobody is waiting on, so
// every row that is put away here starts from this, not from the story's running turn.
const idle = {
  status: 'idle',
  live: false,
  current_turn_id: null,
  is_awaiting_input: false,
  was_interrupted: false,
  is_unread: false,
  unread_answers: 0,
} satisfies Partial<Session>;

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

// THE VERB THAT MOVES A SESSION ACROSS THE ARCHIVE is the row's own, in the same drawer as
// Rename and Delete, and it reads whichever way it is pointing. The gateway holds the stamp,
// so the row asks and takes the answer back; the one thing it settles by itself is a session
// still working, which it refuses in the words the gateway refuses it with.
describe('the archive verb on a session row', () => {
  const STILL_WORKING = 'This session is still active. Archive it once its turn is done.';

  const drawer = (session: Partial<Session>, archive?: SessionRowCommands['archive']) => {
    const commands: SessionRowCommands = {
      open: vi.fn(),
      rename: vi.fn(async () => {}),
      requestDelete: vi.fn(),
      toggleStar: vi.fn(),
      archive,
    };
    render(
      <SessionRow
        session={{ ...STORY_SESSION_ROW, ...session }}
        group={null}
        draft={EMPTY_DRAFT_MESSAGE}
        conn={STORY_GATEWAYS[0]}
        match={null}
        needle=""
        commands={commands}
        deletion={null}
      />,
    );
    return commands;
  };

  const verbs = () =>
    within(screen.getByRole('group', { name: `${STORY_SESSION_ROW.title} actions` }));

  it('puts the session away once, on the machine the row belongs to', async () => {
    const archive = vi.fn(async (session: Session, _conn: GatewayConn, _away: boolean) => ({
      ...session,
      archived_at: 1717,
    }));
    drawer(idle, archive);
    await userEvent.click(verbs().getByRole('button', { name: 'Archive' }));
    await waitFor(() => expect(archive).toHaveBeenCalledTimes(1));
    expect(archive.mock.calls[0][1]).toBe(STORY_GATEWAYS[0]);
    expect(archive.mock.calls[0][2]).toBe(true);
  });

  it('reads Unarchive on a row the gateway has already stamped', async () => {
    const archive = vi.fn(async (session: Session, _conn: GatewayConn, _away: boolean) => ({
      ...session,
      archived_at: null,
    }));
    drawer({ ...idle, archived_at: 1717 }, archive);
    expect(verbs().queryByRole('button', { name: 'Archive' })).not.toBeInTheDocument();
    await userEvent.click(verbs().getByRole('button', { name: 'Unarchive' }));
    await waitFor(() => expect(archive).toHaveBeenCalledTimes(1));
    expect(archive.mock.calls[0][2]).toBe(false);
  });

  it('keeps a session that is still working, and sends nothing', async () => {
    // The story row is live and parked on a human: archiving it would bury the very turn
    // that is waiting for an answer.
    const archive = vi.fn(async (session: Session) => session);
    drawer({}, archive);
    await userEvent.click(verbs().getByRole('button', { name: 'Archive' }));
    expect(await screen.findByText(STILL_WORKING)).toBeInTheDocument();
    expect(archive).not.toHaveBeenCalled();
  });

  it('reads a gateway that refuses a turn started meanwhile the same way', async () => {
    const archive = vi.fn(async () => {
      throw new GatewayError(409, 'session is still working', {
        error: {
          type: 'session-busy',
          message: 'this session is still working',
          session_id: STORY_SESSION_ROW.id,
        },
      });
    });
    drawer(idle, archive);
    await userEvent.click(verbs().getByRole('button', { name: 'Archive' }));
    expect(await screen.findByText(STILL_WORKING)).toBeInTheDocument();
  });

  it('offers nothing to a row standing only because its group was put away', () => {
    // The session itself was never archived; the band around it was. There is nothing of
    // its own to take back, so the drawer carries no archive verb at all.
    drawer(idle);
    expect(verbs().queryByRole('button', { name: 'Archive' })).not.toBeInTheDocument();
    expect(verbs().queryByRole('button', { name: 'Unarchive' })).not.toBeInTheDocument();
  });
});
