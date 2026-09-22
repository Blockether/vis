// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { useState } from 'react';
import { afterEach, expect, it, vi } from 'vitest';
import readFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-reads.json';
import { activityProjectionFromWire } from '../lib/activity';
import type { GatewayClient } from '../lib/gateway';
import { OpenPathContext } from '../lib/open-path';
import { ActivityPanel } from './ActivityPanel';
import { FilePreview } from './FilePreview';

afterEach(cleanup);

const PATH = '/Users/ana/vis/src/app.ts';

function gateway(overrides: Record<string, unknown> = {}): GatewayClient {
  return {
    readPath: vi.fn().mockResolvedValue({
      path: PATH,
      line: 12,
      first_line: 10,
      lines: ['ten', 'eleven', 'twelve', 'thirteen'],
      is_truncated: false,
      size_bytes: 40,
    }),
    openPath: vi.fn().mockResolvedValue({ path: PATH, is_open: true }),
    ...overrides,
  } as unknown as GatewayClient;
}

function show(client: GatewayClient, line?: number) {
  render(<FilePreview client={client} sid="session-1" path={PATH} line={line} onClose={vi.fn()} />);
}

function ReadActivityPreview({ client }: { client: GatewayClient }) {
  const [path, setPath] = useState<string | null>(null);
  return (
    <OpenPathContext.Provider value={setPath}>
      <ActivityPanel activity={activityProjectionFromWire(readFixture)!} />
      {path && (
        <FilePreview client={client} sid="session-1" path={path} onClose={() => setPath(null)} />
      )}
    </OpenPathContext.Provider>
  );
}

it('opens the simplified preview from a filename-only read activity', async () => {
  const client = gateway();
  render(<ReadActivityPreview client={client} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: /Read ×2/ }));
  const row = document.querySelector<HTMLElement>('[data-activity-row]')!;
  expect(within(row).queryByText('Read')).toBeNull();
  fireEvent.click(within(row).getByRole('button', { name: 'Open ~/vis/PLAN.md' }));

  const dialog = screen.getByRole('dialog', { name: 'PLAN.md' });
  await waitFor(() => expect(within(dialog).getByText('twelve')).toBeVisible());
  expect(client.readPath).toHaveBeenCalledWith(
    'session-1',
    '~/vis/PLAN.md',
    undefined,
    expect.anything(),
  );
  expect(dialog.querySelector('header h2')).toHaveClass('sr-only');
  expect(within(dialog).getByText('~/vis/PLAN.md')).toBeVisible();
  const editor = within(dialog).getByRole('button', { name: 'Open in editor' });
  expect(editor).toHaveAttribute('title', 'Open in editor');
  expect(editor.textContent).toBe('');
  expect(editor.querySelector('svg')).toBeInTheDocument();
  expect(editor.className).not.toMatch(/\bborder-l(?:\s|$)/);
  fireEvent.click(editor);
  expect(client.openPath).toHaveBeenCalledWith('session-1', '~/vis/PLAN.md');
  fireEvent.click(within(dialog).getByRole('button', { name: 'Close PLAN.md' }));
  expect(screen.queryByRole('dialog')).toBeNull();
  expect(row).toBeVisible();
});

it('shows only the file path, without the duplicate headline', async () => {
  show(gateway());

  await waitFor(() => expect(screen.getByText('twelve')).toBeVisible());
  const dialog = screen.getByRole('dialog', { name: 'app.ts' });
  const header = dialog.querySelector('header')!;
  expect(header.querySelector('h2')).toHaveClass('sr-only');
  expect(screen.getByText(PATH)).toBeVisible();
  expect(header.querySelectorAll('p')).toHaveLength(1);
});

it('uses an accessible editor icon without a left divider', async () => {
  show(gateway());

  await waitFor(() => expect(screen.getByText('twelve')).toBeVisible());
  const open = screen.getByRole('button', { name: 'Open in editor' });
  expect(open).toHaveAttribute('title', 'Open in editor');
  expect(open.textContent).toBe('');
  expect(open.querySelector('svg')).toBeInTheDocument();
  expect(open.className).not.toMatch(/\bborder-l(?:\s|$)/);
});

it('reads the file where the session is being read, standing at the pressed line', async () => {
  const client = gateway();
  show(client, 12);

  await waitFor(() => expect(screen.getByText('twelve')).toBeVisible());
  expect(client.readPath).toHaveBeenCalledWith('session-1', PATH, 12, expect.anything());
  expect(screen.getByText('ten').closest('[data-line]')).toHaveAttribute('data-line', '10');
  const anchored = [...document.querySelectorAll('[data-anchor="true"]')];
  expect(anchored).toHaveLength(1);
  expect(anchored[0].textContent).toContain('twelve');
});

it('marks no line when the press named none', async () => {
  show(gateway(), undefined);

  await waitFor(() => expect(screen.getByText('twelve')).toBeVisible());
  expect(document.querySelector('[data-anchor="true"]')).toBeNull();
});

it('hands the file to the editor on the machine that holds it', async () => {
  const client = gateway();
  show(client, 12);

  await waitFor(() => expect(screen.getByText('twelve')).toBeVisible());
  fireEvent.click(screen.getByRole('button', { name: 'Open in editor' }));
  expect(client.openPath).toHaveBeenCalledWith('session-1', PATH);
});

it('says what the gateway refused, rather than showing an empty file', async () => {
  show(gateway({ readPath: vi.fn().mockRejectedValue(new Error('that file is not text')) }), 1);

  await waitFor(() => expect(screen.getByText('that file is not text')).toBeVisible());
});

it('says when the file runs past what a preview reads', async () => {
  show(
    gateway({
      readPath: vi.fn().mockResolvedValue({
        path: PATH,
        line: 1,
        first_line: 1,
        lines: ['one'],
        is_truncated: true,
        size_bytes: 9_000_000,
      }),
    }),
  );

  await waitFor(() => expect(screen.getByText(/past what a preview reads/)).toBeVisible());
});
