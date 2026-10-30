// @vitest-environment jsdom
import { cleanup, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import type { GatewayClient } from '../lib/gateway';
import { FilePreview } from './FilePreview';

afterEach(cleanup);

const PATH = '/Users/ana/vis/src/app.ts';

function gateway(overrides: Record<string, unknown> = {}): GatewayClient {
  return {
    readPath: vi.fn().mockResolvedValue({
      kind: 'text',
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

it('says when the file runs past what a preview reads', async () => {
  show(
    gateway({
      readPath: vi.fn().mockResolvedValue({
        kind: 'text',
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

// #432: a PNG linked from Activity opened a text sheet that said "that file is not text".
it('shows a picture in the image viewer when the file is an image', async () => {
  const createObjectURL = vi.fn((_: Blob) => 'blob:shot');
  const revokeObjectURL = vi.fn();
  vi.stubGlobal('URL', Object.assign(URL, { createObjectURL, revokeObjectURL }));
  const client = gateway({
    readPath: vi.fn().mockResolvedValue({
      kind: 'image',
      path: '/tmp/shot.png',
      media_type: 'image/png',
      data: btoa('png'),
      size_bytes: 3,
    }),
  });
  render(<FilePreview client={client} sid="session-1" path="/tmp/shot.png" onClose={vi.fn()} />);

  const picture = await screen.findByRole('img', { name: 'shot.png' });
  expect(picture).toHaveAttribute('src', 'blob:shot');
  const blob = createObjectURL.mock.calls[0][0];
  expect(blob.type).toBe('image/png');
  expect(await blob.text()).toBe('png');
  cleanup();
  expect(revokeObjectURL).toHaveBeenCalledWith('blob:shot');
  vi.unstubAllGlobals();
});

it('offers the editor for a binary file that is not a picture', async () => {
  const client = gateway({
    readPath: vi.fn().mockResolvedValue({ kind: 'binary', path: '/tmp/app.zip', size_bytes: 8 }),
  });
  render(<FilePreview client={client} sid="session-1" path="/tmp/app.zip" onClose={vi.fn()} />);

  await waitFor(() => expect(screen.getByText(/no preview for this kind of file/)).toBeVisible());
  expect(screen.getByRole('button', { name: 'Open in editor' })).toBeVisible();
});
