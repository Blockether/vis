// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

const links = vi.hoisted(() => ({ receive: (_url: string) => {} }));
vi.mock('./lib/deeplink', () => ({
  onPairingLink: async (handler: (url: string) => void) => {
    links.receive = handler;
    return () => {};
  },
}));

import { renderApp } from './app-harness';
import { peekPendingShare, receiveSharedText, resetShareIntakeForTests } from './lib/share-intake';
import { listSession } from './screens/sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
  resetShareIntakeForTests();
  vi.restoreAllMocks();
  window.history.replaceState(null, '', '/');
});

const settle = (ms = 0) => new Promise((resolve) => setTimeout(resolve, ms));

const fleet = () => [
  { label: 'laptop', sessions: [listSession({ id: 's1', title: 'Session one' })] },
  { label: 'server', sessions: [listSession({ id: 's2', title: 'Session two' })] },
];

// Regression, cross-validated on the Android emulator: a voice memo shared from
// the system sheet went straight into whichever session happened to be open, and
// the list never got to offer the choice. The screen that is mounted is not a
// destination — only a human naming one is.
describe('a share that arrives with no destination', () => {
  it('does not fall into the session that happens to be open', async () => {
    const view = renderApp({ machines: fleet() });
    restore = view.restore;

    fireEvent.click(await screen.findByText('Session one'));
    const composer = (await screen.findByLabelText('Message Vis')) as HTMLTextAreaElement;
    await settle(50);

    receiveSharedText({ text: 'look at this' });
    await settle(50);

    expect(composer.value).toBe('');
    expect(peekPendingShare()?.text).toBe('look at this');
    await waitFor(() => expect(screen.queryByLabelText('Message Vis')).not.toBeInTheDocument());
    expect(await screen.findByText('Sharing')).toBeVisible();
    // The source machine is not the destination. Switching machines keeps the share.
    fireEvent.click(within(screen.getByRole('group', { name: 'Machines' })).getByText('server'));
    fireEvent.click(await screen.findByText('Session two'));
    await waitFor(() => expect(screen.getByLabelText('Message Vis')).toHaveValue('look at this'));
    expect(peekPendingShare()).toBeNull();
    view.unmount();
  });

  it('lands in the session the human then picks', async () => {
    const view = renderApp({ machines: fleet() });
    restore = view.restore;

    await screen.findByText('Session one');
    receiveSharedText({ text: 'look at this' });
    // The list is the chooser, and it says what it is holding.
    expect(await screen.findByText('Sharing')).toBeInTheDocument();

    fireEvent.click(screen.getByText('Session one'));
    const composer = (await screen.findByLabelText('Message Vis')) as HTMLTextAreaElement;
    await settle(50);

    expect(composer.value).toContain('look at this');
    expect(peekPendingShare()).toBeNull();
    view.unmount();
  });

  it('closes diagnostics settings when the native share sheet returns logs to Vis', async () => {
    window.history.replaceState(null, '', '/');
    const view = renderApp({ machines: fleet() });
    restore = view.restore;
    fireEvent.click(await screen.findByRole('button', { name: 'Open preferences' }));
    expect(await screen.findByRole('dialog')).toBeVisible();

    await act(async () => {
      links.receive(
        'vis://share?file=file:///cache/vis-diagnostics.jsonl.gz&name=vis-diagnostics.jsonl.gz&type=application/gzip&at=1',
      );
    });

    await waitFor(() => expect(screen.queryByRole('dialog')).not.toBeInTheDocument());
    expect(await screen.findByText('Sharing')).toBeVisible();
    expect(peekPendingShare()?.files?.[0].name).toBe('vis-diagnostics.jsonl.gz');
    fireEvent.click(screen.getByLabelText('Discard the share'));
    await waitFor(() => expect(screen.queryByText('Sharing')).not.toBeInTheDocument());
    expect(peekPendingShare()).toBeNull();
    view.unmount();
  });

  it('reveals the desktop chooser for PLAN.md even when the sidebar was hidden', async () => {
    const matchMedia = window.matchMedia;
    vi.spyOn(window, 'matchMedia').mockImplementation((query) => ({
      ...matchMedia(query),
      matches: query.includes('pointer: fine'),
    }));
    localStorage.setItem('vis.sidebar', 'hidden');
    const view = renderApp({ machines: fleet() });
    restore = view.restore;
    await screen.findByRole('button', { name: 'Show the session list' });

    await act(async () => {
      links.receive('vis://share?file=file:///cache/PLAN.md&name=PLAN.md&type=text/markdown&at=2');
    });

    expect(await screen.findByRole('button', { name: 'Hide the session list' })).toBeVisible();
    const list = screen.getByRole('region', { name: 'Sessions' });
    expect(list.parentElement).not.toHaveClass('hidden');
    expect(within(list).getByText('Sharing')).toBeVisible();
    expect(peekPendingShare()?.files?.[0].name).toBe('PLAN.md');
    view.unmount();
  });
});
