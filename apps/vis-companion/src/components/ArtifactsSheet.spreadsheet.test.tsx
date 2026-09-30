// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { XLSX_MEDIA } from '../lib/artifacts';
import { ArtifactsSheet } from './ArtifactsSheet';
import type { SessionArtifact } from '../lib/artifacts';
import type { GatewayClient } from '../lib/gateway';

const workbook: SessionArtifact = {
  key: 'i1:0',
  kind: 'file',
  name: 'Q3 report.xlsx',
  media: 'XLSX',
  mediaType: XLSX_MEDIA,
  sizeLabel: '4B',
  turn: 1,
  iterationId: 'i1',
  index: 0,
  version: 1,
};
const original = new Blob([new Uint8Array([80, 75, 3, 4])], { type: workbook.mediaType });
const client = {
  attachmentUrl: vi.fn(async () => 'blob:workbook'),
  attachmentBlob: vi.fn(async () => original),
  retainAttachment: () => () => {},
} as unknown as GatewayClient;

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe('spreadsheet artifacts', () => {
  it('hands the original XLSX to the desktop application instead of a web viewer', async () => {
    const invoke = vi.fn(async () => ({ opened: true }));
    vi.stubGlobal('__TAURI__', { core: { invoke } });
    const user = userEvent.setup();
    render(<ArtifactsSheet client={client} sid="s1" artifacts={[workbook]} onClose={() => {}} />);

    await user.click(screen.getByRole('button', { name: /^Open Q3 report.xlsx,/ }));
    await user.click(await screen.findByRole('button', { name: 'Open in app' }));

    expect(invoke).toHaveBeenCalledWith('open_xlsx', {
      filename: 'Q3-report.xlsx',
      data: [80, 75, 3, 4],
    });
    expect(globalThis.document.querySelector('iframe')).toBeNull();
    expect(await screen.findByText('Workbook opened in your spreadsheet app.')).toBeInTheDocument();
  });

  it('offers Download on the web and preserves the original workbook bytes', async () => {
    const user = userEvent.setup();
    const createUrl = vi.spyOn(URL, 'createObjectURL').mockReturnValue('blob:download');
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => {});
    render(<ArtifactsSheet client={client} sid="s1" artifacts={[workbook]} onClose={() => {}} />);
    await user.click(screen.getByRole('button', { name: /^Open Q3 report.xlsx,/ }));
    await user.click(await screen.findByRole('button', { name: 'Download' }));
    expect(createUrl).toHaveBeenCalledWith(original);
    expect((click.mock.instances[0] as HTMLAnchorElement).download).toBe('Q3-report.xlsx');
    expect(await screen.findByText('Workbook downloaded.')).toBeInTheDocument();
  });

  it('shows a saved-copy fallback rather than claiming an app opened', async () => {
    vi.stubGlobal('__TAURI__', { core: { invoke: vi.fn(async () => ({ opened: false })) } });
    const user = userEvent.setup();
    render(<ArtifactsSheet client={client} sid="s1" artifacts={[workbook]} onClose={() => {}} />);
    await user.click(screen.getByRole('button', { name: /^Open Q3 report.xlsx,/ }));
    await user.click(await screen.findByRole('button', { name: 'Open in app' }));
    expect(await screen.findByText(/Workbook saved in Downloads\/Vis Artifacts/)).toBeInTheDocument();
  });

  it('keeps a mislabeled workbook out of the document viewer', async () => {
    const user = userEvent.setup();
    render(
      <ArtifactsSheet
        client={client}
        sid="s1"
        artifacts={[{ ...workbook, kind: 'doc', mediaType: 'text/plain' }]}
        onClose={() => {}}
      />,
    );
    await user.click(screen.getByRole('button', { name: /^Open Q3 report.xlsx,/ }));
    expect(await screen.findByRole('button', { name: 'Download' })).toBeInTheDocument();
    expect(globalThis.document.querySelector('iframe')).toBeNull();
    expect(screen.queryByText('A read that works.')).not.toBeInTheDocument();
  });

  it('reports host errors and allows another attempt', async () => {
    const invoke = vi.fn(async () => {
      throw new Error('Disk is full');
    });
    vi.stubGlobal('__TAURI__', { core: { invoke } });
    const user = userEvent.setup();
    render(<ArtifactsSheet client={client} sid="s1" artifacts={[workbook]} onClose={() => {}} />);
    await user.click(screen.getByRole('button', { name: /^Open Q3 report.xlsx,/ }));
    await user.click(await screen.findByRole('button', { name: 'Open in app' }));
    expect(await screen.findByText('Could not open workbook. Try again.')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Open in app' })).toBeEnabled();
  });
});
