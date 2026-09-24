// @vitest-environment jsdom
import { screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, expect, it, vi } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

// Regression, user visual review: active and alternate routes should appear beside
// the machine and named Projects action, instead of disappearing into Settings.
it('binds an alternate on the scoped machine without changing the machine choice', async () => {
  const conn = {
    url: 'http://10.0.0.5:7890',
    token: 't',
    label: 'tower',
    alts: ['https://gateway.example.com'],
  };
  const onSelectAddress = vi.fn();
  const view = renderSessionsScreen({
    machines: [{ label: 'tower', sessions: [listSession()] }],
    at: [conn],
    onSelectAddress,
  });
  restore = view.restore;
  await screen.findByText('A session');

  const strip = screen.getByRole('group', { name: 'Machines' });
  expect(within(strip).getByRole('button', { name: 'tower' })).toHaveAttribute('aria-pressed', 'true');
  const addresses = within(screen.getByRole('group', { name: 'Addresses on tower' }));
  expect(addresses.getByRole('button', { name: 'Using 10.0.0.5:7890 on tower' })).toHaveAttribute(
    'aria-pressed', 'true',
  );
  expect(screen.getByRole('button', { name: 'Projects on tower' })).toHaveTextContent('Projects');
  await userEvent.click(addresses.getByRole('button', { name: 'Use gateway.example.com on tower' }));
  expect(onSelectAddress).toHaveBeenCalledExactlyOnceWith(conn, 'https://gateway.example.com', true);
  expect(within(strip).getByRole('button', { name: 'tower' })).toHaveAttribute('aria-pressed', 'true');
});

it('shows only the selected machine’s addresses and keeps failures on retry tiles', async () => {
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
  expect(screen.getByRole('group', { name: 'Addresses on tower' })).toBeVisible();
  expect(screen.queryByRole('group', { name: 'Addresses on mini' })).toBeNull();
  expect(screen.getByRole('button', { name: 'Projects on tower' })).toBeVisible();
});
