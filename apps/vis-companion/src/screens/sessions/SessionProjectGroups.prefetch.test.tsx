// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { GatewayClient, SESSION_CACHE_LIMIT } from '../../lib/gateway';
import { machineKey } from '../../lib/fleet';
import { groupFoldKey, writeProjectFold } from '../../lib/project-fold';
import type { Session } from '../../lib/types';
import { ProjectGroup } from './SessionProjectGroups';

const ROOT = '/project';
const GROUP = 'ready-group';
const conn = { url: 'http://gateway.example.com' };
const turns = [{
  turn_id: 'answer', request: 'A question', status: 'completed', iterations: [],
  content: [{ id: 'prose', type: 'prose', markdown: 'The grouped answer is ready.' }],
}];

class RowObserver {
  static instances: RowObserver[] = [];
  targets = new Set<Element>();
  readonly callback: IntersectionObserverCallback;
  constructor(callback: IntersectionObserverCallback) {
    this.callback = callback;
    RowObserver.instances.push(this);
  }
  observe = (target: Element) => { this.targets.add(target); };
  unobserve = (target: Element) => { this.targets.delete(target); };
  disconnect = vi.fn(() => { this.targets.clear(); });
}

function nearRow(id: string, isIntersecting: boolean) {
  act(() => {
    for (const observer of RowObserver.instances) {
      const entries = [...observer.targets]
        .filter((target) => target.getAttribute('data-session-row') === id)
        .map((target) => ({ target, isIntersecting }) as IntersectionObserverEntry);
      if (entries.length) observer.callback(entries, observer as unknown as IntersectionObserver);
    }
  });
}

beforeEach(() => {
  localStorage.clear();
  RowObserver.instances = [];
  vi.stubGlobal('IntersectionObserver', RowObserver);
});

afterEach(() => {
  cleanup();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

async function project(name: string) {
  const client = new GatewayClient({ url: `${conn.url}/${name}` });
  const row = (id: string, group_id: string | null): Session => ({
    id, title: id, live: false, turn_count: 1, answer_count: 1,
    is_unread: true, unread_answers: 1, modified_at: 'ready', group_id,
    workspace: { root: ROOT },
  } as Session);
  const loose = Array.from({ length: SESSION_CACHE_LIMIT }, (_, i) => row(`loose-${i}`, null));
  const grouped = Array.from({ length: 14 }, (_, i) => row(`grouped-${i}`, GROUP));
  const bodies: string[] = [];
  vi.stubGlobal('fetch', vi.fn(async (input: string) => {
    const path = new URL(String(input)).pathname;
    if (path.endsWith('/transcript')) {
      bodies.push(path);
      return new Response(JSON.stringify({ turns, total: 1, offset: 0, has_more: false }));
    }
    if (path.endsWith('/session-groups')) return new Response(JSON.stringify({
      groups: [{ id: GROUP, name: 'Ready group', color: 'blue', position: 0, session_count: grouped.length }],
      total: 1, session_total: grouped.length, offset: 0, limit: null, has_more: false,
    }));
    return new Response(JSON.stringify({ sessions: loose, grouped, total: loose.length }));
  }));
  writeProjectFold(groupFoldKey(machineKey(conn), ROOT, GROUP), false);
  const view = render(
    <ProjectGroup
      group={{ root: ROOT, label: 'Project', projectId: 'project', sessions: [],
        tally: { count: 24, live: 0, awaiting: 0, unread: 24 } }}
      machine={{ conn, sessions: [] }}
      context={{ getClient: () => client, drafts: {}, needle: '', openRow: null,
        previewId: null, preview: null, actions: {
          commands: { open: vi.fn(), rename: vi.fn(async () => {}), requestDelete: vi.fn(), toggleStar: vi.fn() },
          deletion: { target: null, isBusy: false, error: null, confirm: vi.fn(), cancel: vi.fn() },
        } }}
      reading={{ pageSize: 10, isVisible: true }}
      creation={{ state: null, start: vi.fn(async () => {}) }}
      initiallyOpen
    />,
  );
  await screen.findByRole('button', { name: 'Expand Ready group' });
  await waitFor(() => expect(client.cachedTranscript(loose[9].id)).toEqual(turns));
  const poll = () => client.listProjectPage(ROOT, 10, '', new Map(), undefined, true, 'exclude', undefined, true);
  return { client, loose, grouped, bodies, poll, ...view };
}

// Regression: loose NEW rows used the cache budget; expanding or scrolling a group did not warm its rows.
describe('NEW answers inside session groups', () => {
  it('warms expanded and scrolled rows beyond the initial ten without a press', async () => {
    const { client, grouped, bodies } = await project('group-scroll');
    expect(client.cachedTranscript(grouped[0].id)).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Expand Ready group' }));
    nearRow(grouped[0].id, true);
    await waitFor(() => expect(client.cachedTranscript(grouped[0].id)).toEqual(turns));
    expect(client.cachedTranscript(grouped[13].id)).toBeNull();

    nearRow(grouped[0].id, false);
    nearRow(grouped[13].id, true);
    await waitFor(() => expect(client.cachedTranscript(grouped[13].id)).toEqual(turns));
    await expect(client.transcriptIfMoved(grouped[13].id, grouped[13])).resolves.toBeNull();
    expect(bodies.filter((path) => path.includes(`/${grouped[13].id}/`))).toHaveLength(1);
    const rowObservers = RowObserver.instances.filter((observer) =>
      [...observer.targets].some((target) => target.hasAttribute('data-session-row')),
    );
    expect(rowObservers).toHaveLength(1);
  });

  it('keeps visible answers through list polls and releases them when the group folds', async () => {
    const { client, grouped, loose, bodies, poll, unmount } = await project('group-retain');
    fireEvent.click(screen.getByRole('button', { name: 'Expand Ready group' }));
    nearRow(grouped[13].id, true);
    await waitFor(() => expect(client.cachedTranscript(grouped[13].id)).toEqual(turns));
    await act(async () => { await poll(); await poll(); });
    expect(client.cachedTranscript(grouped[13].id)).toEqual(turns);
    expect(bodies.filter((path) => path.includes(`/${grouped[13].id}/`))).toHaveLength(1);
    expect([...loose, ...grouped].filter((row) => client.cachedTranscript(row.id))).toHaveLength(SESSION_CACHE_LIMIT);

    fireEvent.click(screen.getByRole('button', { name: 'Collapse Ready group' }));
    await act(async () => { await poll(); });
    expect(client.cachedTranscript(grouped[13].id)).toBeNull();
    unmount();
    expect(RowObserver.instances.every((observer) => observer.targets.size === 0)).toBe(true);
    expect(RowObserver.instances.every((observer) => observer.disconnect.mock.calls.length > 0)).toBe(true);
  });
});
