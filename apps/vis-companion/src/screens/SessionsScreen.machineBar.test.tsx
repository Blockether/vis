// @vitest-environment jsdom
import { screen, within } from '@testing-library/react';
import { afterEach, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

// Regression, visual review: the machine strip is a single row of machine names
// and an icon-only project action. Address choices remain in Machines settings.
it('shows a compact machine strip without current or alternate addresses', async () => {
  const conn = {
    url: 'http://10.0.0.5:7890',
    token: 't',
    label: 'tower',
    alts: ['https://gateway.example.com'],
  };
  const view = renderSessionsScreen({
    machines: [{ label: 'tower', sessions: [listSession()] }],
    at: [conn],
  });
  restore = view.restore;
  await screen.findByText('A session');

  const strip = screen.getByRole('group', { name: 'Machines' });
  expect(within(strip).getByRole('button', { name: 'tower' })).toHaveAttribute('aria-pressed', 'true');
  expect(screen.queryByRole('group', { name: 'Addresses on tower' })).toBeNull();
  expect(screen.queryByText('10.0.0.5:7890')).toBeNull();
  expect(screen.queryByText('gateway.example.com')).toBeNull();
  const projects = screen.getByRole('button', { name: 'New project on tower' });
  expect(projects).not.toHaveTextContent('New project');
  expect(projects.querySelector('svg')).toBeInTheDocument();
});

it('keeps retry actions without showing routes', async () => {
  const conn = {
    url: 'http://10.0.0.5:7890', token: 't', label: 'tower',
    alts: ['https://gateway.example.com'],
  };
  const down = { url: 'http://10.0.0.9:7890', token: 't', label: 'mini' };
  const view = renderSessionsScreen({
    machines: [{ label: 'tower', sessions: [listSession()] }, { label: 'mini', down: true }],
    at: [conn, down],
  });
  restore = view.restore;
  await screen.findByText('A session');
  const retry = await screen.findByRole('button', { name: 'Reconnect to mini' });
  expect(retry).toHaveClass('text-err-ink');
  expect(screen.queryByRole('group', { name: /Addresses on/ })).toBeNull();
  expect(screen.getByRole('button', { name: 'New project on tower' })).toBeVisible();
});

// Option C: each machine tab counts its unread conversations and says when it last moved.
it('counts unread conversations on a machine tab beside the time of its newest activity', async () => {
  const fresh = new Date(Date.now() - 5 * 60_000).toISOString();
  const view = renderSessionsScreen({
    machines: [
      { label: 'alpha', sessions: [listSession()] },
      {
        label: 'beta',
        sessions: [
          listSession({ id: 'b1', title: 'First', is_unread: true, modified_at: fresh }),
          listSession({ id: 'b2', title: 'Second', is_unread: true, modified_at: fresh }),
        ],
      },
    ],
  });
  restore = view.restore;
  await screen.findByText('A session');

  const strip = screen.getByRole('group', { name: 'Machines' });
  const beta = await within(strip).findByRole('button', { name: /^beta\s*2 unread$/ });
  expect(beta.querySelector('.bg-accent')).toHaveTextContent(/^2$/);
  expect(beta).toHaveTextContent('5 min');
  expect(within(strip).getByRole('button', { name: 'alpha' })).toHaveTextContent(/\d+ d$/);
});
