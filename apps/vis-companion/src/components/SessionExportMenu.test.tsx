// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { GatewayError, type GatewayClient } from '../lib/gateway';
import { useNotices } from '../lib/use-notices';
import { NoticeToast } from './NoticeToast';
import { useSessionExport } from './SessionExportMenu';

const session = { id: '123e4567-e89b-12d3-a456-426614174000', title: 'Stalled stream' };

/** One screen that offers the export: a control, the formats and the toast. */
function Screen({ client }: { client: GatewayClient }) {
  const notices = useNotices();
  const sessionExport = useSessionExport(notices);
  return (
    <>
      <button
        type="button"
        onClick={(event) => sessionExport.open(client, session, event.currentTarget.getBoundingClientRect())}
      >
        Export
      </button>
      {sessionExport.menu}
      <NoticeToast notices={notices} />
    </>
  );
}

const formats = () => within(screen.getByRole('dialog', { name: 'Export Stalled stream' }));

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe('the export choice of a session', () => {
  it('expands the three formats, and a pick saves that file and says where it is', async () => {
    const path = '/home/ada/Downloads/Vis Artifacts/1/stalled-stream-123e4567.md';
    const invoke = vi.fn(async () => ({ revealed: true, path }));
    vi.stubGlobal('__TAURI__', { core: { invoke } });
    const client = { transcriptMarkdown: vi.fn(async () => new Blob(['# Turn 1'])) } as unknown as GatewayClient;
    render(<Screen client={client} />);
    fireEvent.click(screen.getByRole('button', { name: 'Export' }));

    expect(formats().getAllByRole('button').map((row) => row.textContent)).toEqual([
      expect.stringContaining('HTML'),
      expect.stringContaining('Markdown'),
      expect.stringContaining('Snapshot (.vissnap)'),
    ]);
    fireEvent.click(formats().getByRole('button', { name: /^Markdown/ }));

    expect(screen.queryByRole('dialog', { name: 'Export Stalled stream' })).toBeNull();
    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent(`Markdown file saved to ${path}.`));
    expect(invoke).toHaveBeenCalledExactlyOnceWith(
      'reveal_artifact',
      expect.objectContaining({ filename: 'stalled-stream-123e4567.md' }),
    );
  });

  it('shows why an export failed', async () => {
    const client = {
      sessionSnapshot: vi.fn(async () => {
        throw new GatewayError(413, 'The session snapshot exceeds the size limit');
      }),
    } as unknown as GatewayClient;
    render(<Screen client={client} />);
    fireEvent.click(screen.getByRole('button', { name: 'Export' }));
    fireEvent.click(formats().getByRole('button', { name: /^Snapshot/ }));
    await waitFor(() =>
      expect(screen.getByRole('status')).toHaveTextContent(
        'The export failed: The session snapshot exceeds the size limit',
      ),
    );
  });
});
