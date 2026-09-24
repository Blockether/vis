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
  const projects = screen.getByRole('button', { name: 'Projects on tower' });
  expect(projects).not.toHaveTextContent('Projects');
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
  expect(screen.getByRole('button', { name: 'Projects on tower' })).toBeVisible();
});
