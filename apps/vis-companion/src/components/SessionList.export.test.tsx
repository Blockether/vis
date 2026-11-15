// @vitest-environment jsdom
import { render, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

import { STORY_GATEWAYS, STORY_SESSION_ROW } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import type { Session } from '../lib/types';
import { SessionRow, type SessionRowCommands } from './SessionList';

// The export of a session is a row action, in the same drawer as Fork and Delete. The pointer
// menu and the touch tray both read this list of actions.
describe('the export action of a session row', () => {
  const drawer = (session: Partial<Session>) => {
    const exportSession = vi.fn<NonNullable<SessionRowCommands['exportSession']>>();
    render(
      <SessionRow
        session={{ ...STORY_SESSION_ROW, ...session }}
        group={null}
        draft={EMPTY_DRAFT_MESSAGE}
        conn={STORY_GATEWAYS[0]}
        needle=""
        commands={{
          open: vi.fn(),
          rename: vi.fn(async () => {}),
          requestDelete: vi.fn(),
          toggleStar: vi.fn(),
          exportSession,
        }}
        deletion={null}
      />,
    );
    return exportSession;
  };
  const actions = () =>
    within(screen.getByRole('group', { name: `${STORY_SESSION_ROW.title} actions` }));

  it('asks for the formats of this session, under the control that the person pressed', async () => {
    const exportSession = drawer({ turn_count: 3 });
    const control = actions().getByRole('button', { name: 'Export' });
    await userEvent.click(control);
    expect(exportSession).toHaveBeenCalledOnce();
    const [session, conn, anchor] = exportSession.mock.calls[0];
    expect(session.id).toBe(STORY_SESSION_ROW.id);
    expect(conn).toBe(STORY_GATEWAYS[0]);
    expect(anchor).toBeInstanceOf(HTMLElement);
  });

  it('is absent for a session without turns', () => {
    drawer({ turn_count: 0 });
    expect(actions().queryByRole('button', { name: 'Export' })).toBeNull();
  });
});
