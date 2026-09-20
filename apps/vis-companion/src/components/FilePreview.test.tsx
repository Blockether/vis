// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import type { GatewayClient } from '../lib/gateway';
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
