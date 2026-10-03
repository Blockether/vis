// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { STORY_FLEET_CONNS, STORY_NEWER_PROJECT } from '../../dev/story-data';
import { machineKey, sessionRowKey } from '../../lib/fleet';
import type { GatewayClient } from '../../lib/gateway';
import { groupFoldKey, writeProjectFold } from '../../lib/project-fold';
import type { Session } from '../../lib/types';
import { ProjectGroup, type SessionRowsContext } from './SessionProjectGroups';

const conn = STORY_FLEET_CONNS[0];
const ROOT = STORY_NEWER_PROJECT.root;
const GROUP = 'status-group';
const NAME = 'Work needing attention';

function row(id: string, changes: Partial<Session> = {}): Session {
  return {
    ...STORY_NEWER_PROJECT.rows[0],
    id, title: id, group_id: GROUP, live: false, is_awaiting_input: false,
    awaiting_input_count: 0, is_unread: false, unread_answers: 0,
    answer_count: 6, archived_at: null, ...changes,
  };
}

function statusRows(): Session[] {
  return [
    row('input-a', { live: true, is_awaiting_input: true, awaiting_input_count: 3 }),
    row('input-b', { live: true, is_awaiting_input: true, awaiting_input_count: 1 }),
    row('running', { live: true }),
    row('new-a', { is_unread: true, unread_answers: 5 }),
    row('new-b', { is_unread: true, unread_answers: 1 }),
    ...Array.from({ length: 9 }, (_, index) => row(`idle-${index}`)),
    row('other-running', { group_id: 'other-group', live: true }),
  ];
}

function mount(initialRows = statusRows()) {
  const server = { rows: initialRows };
  const loose = Array.from({ length: 21 }, (_, index) => row(`loose-${index}`, {
    group_id: null, is_unread: true, unread_answers: 1,
  }));
  const client = {
    base: conn.url,
    heldProjectPage: () => null,
    heldSessionGroups: () => null,
    isSessionDeleted: () => false,
    listProjectPage: vi.fn(async (_root: string, limit: number, after: string) => {
      const start = after ? Number(after.replace('after-', '')) : 0;
      return {
        rows: loose.slice(start, start + limit), total: loose.length,
        // The same HITL row can occur in both sidecars. Count that session once.
        awaiting: server.rows.filter((session) => session.is_awaiting_input),
        grouped: after ? [] : server.rows,
        nextCursor: start + limit < loose.length ? `after-${start + limit}` : '',
      };
    }),
    listSessionGroups: vi.fn(async () => ({
      groups: [
        { id: GROUP, name: NAME, color: 'blue', position: 0,
          session_count: server.rows.filter((session) => session.group_id === GROUP).length },
        { id: 'other-group', name: 'Other work', color: null, position: 1, session_count: 1 },
        { id: 'empty-group', name: 'Empty work', color: null, position: 2, session_count: 0 },
      ],
      total: 3, session_total: server.rows.length, offset: 0, limit: null, has_more: false,
    })),
    prefetchTranscript: vi.fn(),
  };
  let context: SessionRowsContext = {
    getClient: () => client as unknown as GatewayClient,
    drafts: {}, needle: '', openRow: null, previewId: null, preview: null,
    actions: {
      commands: { open: vi.fn(), rename: vi.fn(async () => {}), requestDelete: vi.fn(), toggleStar: vi.fn() },
      deletion: { target: null, isBusy: false, error: null, confirm: vi.fn(), cancel: vi.fn() },
    },
  };
  let revision = 0;
  const project = () => (
    <ProjectGroup
      group={{ root: ROOT, label: 'Project', projectId: STORY_NEWER_PROJECT.projectId,
        sessions: [], tally: { count: loose.length, live: 99, awaiting: 99, unread: 99 } }}
      machine={{ conn, sessions: [] }}
      context={context}
      reading={{ pageSize: 10, isVisible: true, revision }}
      creation={{ state: null, start: vi.fn(async () => {}) }}
      initiallyOpen
    />
  );
  for (const id of [GROUP, 'other-group', 'empty-group']) {
    writeProjectFold(groupFoldKey(machineKey(conn), ROOT, id), false);
  }
  const view = render(project());
  return {
    server, client,
    update(changes: Partial<SessionRowsContext> = {}, refresh = false) {
      context = { ...context, ...changes };
      if (refresh) revision += 1;
      view.rerender(project());
    },
  };
}

function heading(name = NAME, expanded = false) {
  return screen.getByRole('button', { name: `${expanded ? 'Collapse' : 'Expand'} ${name}` });
}

function expectCounts(input: number, live: number, unread: number, name = NAME, expanded = false) {
  const scope = within(heading(name, expanded));
  for (const [label, count] of [['HITL', input], ['LIVE', live], ['NEW', unread]] as const) {
    if (count > 0) expect(scope.getByText(`${count} ${label}`)).toBeVisible();
    else expect(scope.queryByText(new RegExp(`^[0-9]+ ${label}$`))).toBeNull();
  }
}

beforeEach(() => { localStorage.clear(); });
afterEach(() => { cleanup(); vi.restoreAllMocks(); });

// Regression: folding a group hid every HITL, LIVE and NEW indication inside it.
describe('session group status counts', () => {
  it('uses the same status colors in group headers and session rows', async () => {
    mount();
    await waitFor(() => expectCounts(2, 1, 2));
    fireEvent.click(heading());
    for (const [id, label, countLabel, tone] of [
      ['input-a', 'HITL ×3', '2 HITL', 'text-warn'],
      ['running', 'LIVE', '1 LIVE', 'text-ok'],
      ['new-a', 'NEW ×5', '2 NEW', 'text-accent-ink'],
    ]) {
      const row = document.querySelector<HTMLElement>(`[data-session-id="${id}"]`)!;
      expect(within(row).getByText(label).parentElement).toHaveClass(tone);
      expect(within(heading(NAME, true)).getByText(countLabel)).toHaveClass(tone);
    }
  });

  it('counts the complete group once while collapsed, without reading conversations', async () => {
    const { client } = mount();
    await waitFor(() => expectCounts(2, 1, 2));
    expectCounts(0, 1, 0, 'Other work');
    expectCounts(0, 0, 0, 'Empty work');
    expect(document.querySelector('[data-session-id="input-a"]')).toBeNull();
    expect(heading()).toHaveTextContent(/2 HITL.*1 LIVE.*2 NEW/);
    expect(heading()).toHaveAccessibleDescription('2 sessions need input. 1 live session. 2 sessions with new answers.');
    expect(client.prefetchTranscript).not.toHaveBeenCalled();

    fireEvent.click(heading());
    expectCounts(2, 1, 2, NAME, true);
    expect(document.querySelector('[data-session-id="input-a"]')).toHaveAttribute('data-session-id', 'input-a');
    fireEvent.click(heading(NAME, true));
    expectCounts(2, 1, 2);
  });

  it('keeps complete counts on a later loose page and refreshes HITL after an answer', async () => {
    const { server, update } = mount();
    await waitFor(() => expectCounts(2, 1, 2));
    const pages = screen.getByRole('navigation', { name: 'Pages of Project sessions' });
    fireEvent.click(within(pages).getByRole('button', { name: 'Next page' }));
    await within(pages).findByText('Page 2 of 3');
    expectCounts(2, 1, 2);

    // One answered prompt still leaves this session waiting on two more prompts.
    server.rows = server.rows.map((session) => session.id === 'input-a'
      ? { ...session, awaiting_input_count: 2 } : session);
    update({}, true);
    await waitFor(() => expectCounts(2, 1, 2));
    server.rows = server.rows.map((session) => session.id === 'input-a'
      ? { ...session, is_awaiting_input: false, awaiting_input_count: 0 } : session);
    update({}, true);
    await waitFor(() => expectCounts(1, 2, 2));
    expect(within(pages).getByText('Page 2 of 3')).toBeVisible();
  });

  it('clears visited NEW locally, keeps HITL, and counts a later answer again', async () => {
    const { server, update } = mount();
    await waitFor(() => expectCounts(2, 1, 2));
    update({ openRow: sessionRowKey(conn, 'new-a') });
    expectCounts(2, 1, 1);
    update({ openRow: sessionRowKey(conn, 'input-a'),
      readFloors: new Map([[sessionRowKey(conn, 'new-a'), 6]]) });
    expectCounts(2, 1, 1);
    update({ openRow: null });
    expectCounts(2, 1, 1);

    server.rows = server.rows.map((session) => session.id === 'new-a'
      ? { ...session, answer_count: 7, unread_answers: 6 } : session);
    update({}, true);
    await waitFor(() => expectCounts(2, 1, 2));
    server.rows = server.rows.map((session) => session.id === 'new-b'
      ? { ...session, is_unread: false, unread_answers: 0 } : session);
    update({}, true);
    await waitFor(() => expectCounts(2, 1, 1));
  });

  it('removes zero badges after sessions move to another group', async () => {
    const { server, update } = mount([row('input', { live: true, is_awaiting_input: true })]);
    await waitFor(() => expectCounts(1, 0, 0));
    server.rows = server.rows.map((session) => ({ ...session, group_id: 'other-group' }));
    update({}, true);
    await waitFor(() => {
      expectCounts(0, 0, 0);
      expectCounts(1, 0, 0, 'Other work');
    });
    expect(heading()).not.toHaveAttribute('aria-describedby');
  });
});
