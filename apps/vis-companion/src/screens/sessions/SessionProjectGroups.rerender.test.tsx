// @vitest-environment jsdom
import { act, cleanup, render, waitFor } from '@testing-library/react';
import { memo, type ComponentProps } from 'react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { STORY_FLEET_CONNS, STORY_NEWER_PROJECT } from '../../dev/story-data';
import type { GatewayClient } from '../../lib/gateway';
import type { Session, SessionGroup } from '../../lib/types';
import { ProjectGroup, type SessionRowsContext } from './SessionProjectGroups';

const renders = vi.hoisted(() => new Map<string, number>());

// Count the renders of each row behind the same memo as the real row.
vi.mock('../../components/SessionList', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../../components/SessionList')>();
  const Counted = memo((props: ComponentProps<typeof actual.SessionRow>) => {
    renders.set(props.session.id, (renders.get(props.session.id) ?? 0) + 1);
    return <actual.SessionRow {...props} />;
  });
  return { ...actual, SessionRow: Counted };
});

const conn = STORY_FLEET_CONNS[0];
const ROOT = STORY_NEWER_PROJECT.root;
const GROUP = 'busy-group';

function row(id: string): Session {
  return {
    ...STORY_NEWER_PROJECT.rows[0],
    id, title: id, group_id: GROUP, live: false, is_awaiting_input: false,
    awaiting_input_count: 0, is_unread: false, unread_answers: 0, archived_at: null,
  };
}

function mount() {
  const server = { rows: Array.from({ length: 12 }, (_, index) => row(`filed-${index}`)) };
  const group: SessionGroup = {
    id: GROUP, project_id: STORY_NEWER_PROJECT.projectId, name: 'Busy work', color: 'blue',
    session_count: server.rows.length,
  };
  // Each read answers with fresh copies, as the gateway does.
  const client = {
    base: conn.url,
    heldProjectPage: () => null,
    heldSessionGroups: () => null,
    isSessionDeleted: () => false,
    listProjectPage: vi.fn(async () => ({
      rows: [], total: 0, awaiting: [], grouped: structuredClone(server.rows), nextCursor: '',
    })),
    listSessionGroups: vi.fn(async () => ({
      groups: [structuredClone(group)], total: 1, session_total: server.rows.length,
      offset: 0, limit: null, has_more: false,
    })),
    prefetchTranscript: vi.fn(),
  };
  const context: SessionRowsContext = {
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
        sessions: [], tally: { count: 0, live: 0, awaiting: 0, unread: 0 } }}
      machine={{ conn, sessions: [] }}
      context={context}
      reading={{ pageSize: 10, isVisible: true, revision, reads: revision }}
      creation={{ state: null, start: vi.fn(async () => {}) }}
      initiallyOpen
    />
  );
  const view = render(project());
  return {
    server, client,
    /** A fleet read moved this machine: the page and the groups are read again. */
    async refresh() {
      const pages = client.listProjectPage.mock.calls.length;
      const walls = client.listSessionGroups.mock.calls.length;
      revision += 1;
      view.rerender(project());
      await waitFor(() => {
        expect(client.listProjectPage.mock.calls.length).toBeGreaterThan(pages);
        expect(client.listSessionGroups.mock.calls.length).toBeGreaterThan(walls);
      });
      await act(async () => {});
    },
  };
}

beforeEach(() => { localStorage.clear(); renders.clear(); });
afterEach(() => { cleanup(); vi.restoreAllMocks(); });

// Regression: each revalidation re-rendered every row of a band and blocked a fast scroll.
describe('session group revalidation', () => {
  it('keeps unchanged rows of an open group from rendering again', async () => {
    const { server, refresh } = mount();
    await waitFor(() => expect(document.querySelectorAll('[data-session-id]')).toHaveLength(12));
    await act(async () => {});
    const before = new Map(renders);

    await refresh();
    expect(renders).toEqual(before);

    server.rows = server.rows.map((session) =>
      session.id === 'filed-3' ? { ...session, title: 'Renamed elsewhere' } : session);
    await refresh();
    for (const [id, count] of renders) {
      expect([id, count]).toEqual([id, (before.get(id) ?? 0) + (id === 'filed-3' ? 1 : 0)]);
    }
  });
});
