// @vitest-environment jsdom
import { act, cleanup, renderHook, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient } from '../../lib/gateway';
import { useSessionSearchScope } from './SessionSearchScopes';

const conn = { url: 'https://catalog.example.com' };
const conns = [conn];

// Search used to fetch groups for every project after fetching the projects.
it('shows archived and empty group choices from one catalog request per machine', async () => {
  const projects = [
    { id: 'p1', name: 'Workbench', workspace_root: '/work' },
    { id: 'p2', name: 'Archives', workspace_root: '/archives', archived_at: 1 },
  ];
  const groups = [
    { id: 'g1', project_id: 'p1', name: 'Other project', color: 'red', session_count: 1 },
    { id: 'g2', project_id: 'p2', name: 'Filed away', color: 'blue', session_count: 2, archived_at: 1 },
    { id: 'g3', project_id: 'p2', name: 'Empty group', color: 'blue', session_count: 0 },
  ];
  const fetches = vi.fn(async (url: string) => new Response(JSON.stringify(
    new URL(url).pathname === '/v1/projects' ? { projects, session_groups: groups } : { groups },
  )));
  vi.stubGlobal('fetch', fetches);
  const client = new GatewayClient(conn);
  const getClient = () => client;
  const { result } = renderHook(() => useSessionSearchScope(conns, true, null, getClient));
  await waitFor(() => expect(result.current.projects).toHaveLength(2));
  act(() => result.current.setProject(`${conn.url}\u0000p2`));
  expect(result.current.options.map((option) => option.value.split('\u0000')[1])).toEqual(['g2', 'g3']);
  const requests = fetches.mock.calls.map(([url]) => new URL(url));
  expect(requests.map((url) => url.pathname)).toEqual(['/v1/projects']);
  expect(requests[0].searchParams.get('include')).toBe('groups');
  expect(requests[0].searchParams.get('archived')).toBe('include');
});

afterEach(() => { cleanup(); vi.unstubAllGlobals(); });
