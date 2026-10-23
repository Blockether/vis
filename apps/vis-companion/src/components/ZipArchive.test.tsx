// @vitest-environment jsdom
import { fireEvent, render, waitFor } from '@testing-library/react';
import { CompressionStream, DecompressionStream } from 'node:stream/web';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { buildZip } from '../lib/zip-harness';
import { ZipArchive, zipEntryMediaType } from './ZipArchive';

const bytes = (value: string) => new TextEncoder().encode(value);

// jsdom has no compression streams; the browser and the webviews do.
beforeEach(() => {
  vi.stubGlobal('CompressionStream', CompressionStream);
  vi.stubGlobal('DecompressionStream', DecompressionStream);
});
afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe('ZIP archive view', () => {
  it('lists files, previews text and saves one entry', async () => {
    const buffer = await buildZip([
      { name: 'docs/', data: new Uint8Array() },
      { name: 'docs/readme.md', data: bytes('# Hello from the archive'), deflate: true },
      { name: 'data.bin', data: bytes('raw') },
    ]);
    vi.spyOn(URL, 'createObjectURL').mockReturnValue('blob:entry');
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => {});
    const view = render(<ZipArchive blob={new Blob([buffer], { type: 'application/zip' })} name="bundle.zip" />);

    expect(await view.findByText('2 files, 27B unpacked')).toBeVisible();
    const list = view.getByRole('list', { name: 'Files in bundle.zip' });
    expect(Array.from(list.querySelectorAll('li')).map((row) => row.textContent)).toEqual([
      'docs/readme.md24B',
      'data.bin3B',
    ]);

    fireEvent.click(view.getByText('docs/readme.md'));
    expect(await view.findByRole('heading', { name: 'Hello from the archive' })).toBeVisible();
    expect(view.queryByText('# Hello from the archive')).toBeNull();

    fireEvent.click(view.getByText('data.bin'));
    expect(await view.findByText('No preview for this file type. Save it to open it.')).toBeVisible();

    fireEvent.click(view.getByRole('button', { name: 'Save readme.md' }));
    expect(await view.findByText('File downloaded.')).toBeVisible();
    expect((click.mock.instances[0] as HTMLAnchorElement).download).toBe('readme.md');
    view.unmount();
  });

  it('shows a CSV entry as a table', async () => {
    const buffer = await buildZip([{ name: 'coverage.csv', data: bytes('file,lines\ncore.clj,92\n') }]);
    const view = render(<ZipArchive blob={new Blob([buffer])} name="results.zip" />);

    fireEvent.click(await view.findByText('coverage.csv'));
    await waitFor(() => expect(view.getByRole('grid')).toHaveTextContent('core.clj'));
    view.unmount();
  });

  it('says when the bytes are not a ZIP archive', async () => {
    const view = render(<ZipArchive blob={new Blob(['not an archive at all, really'])} name="broken.zip" />);
    expect(await view.findByRole('alert')).toHaveTextContent(
      'broken.zip could not be read: Not a ZIP archive. Save the archive to open it in another app.',
    );
    view.unmount();
  });

  it.each([
    ['photo.PNG', 'image/png'],
    ['src/main.clj', 'text/plain'],
    ['docs/README.md', 'text/markdown'],
    ['coverage.csv', 'text/csv'],
    ['inner.zip', 'application/zip'],
    ['tool.exe', 'application/octet-stream'],
  ])('types the entry %s as %s', (name, media) => {
    expect(zipEntryMediaType(name)).toBe(media);
  });
});
