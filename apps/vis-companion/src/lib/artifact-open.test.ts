// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const native = vi.hoisted(() => ({ value: false }));
const writeFile = vi.hoisted(() => vi.fn(async () => undefined));
const getUri = vi.hoisted(() => vi.fn(async () => ({ uri: 'file:///cache/report.xlsx' })));
const deleteFile = vi.hoisted(() => vi.fn(async () => undefined));
const share = vi.hoisted(() => vi.fn(async () => ({})));
vi.mock('@capacitor/core', () => ({ Capacitor: { isNativePlatform: () => native.value } }));
vi.mock('@capacitor/filesystem', () => ({
  Directory: { Cache: 'CACHE' },
  Filesystem: { writeFile, getUri, deleteFile },
}));
vi.mock('@capacitor/share', () => ({ Share: { share } }));

import { openSpreadsheet, spreadsheetOpenVerb } from './artifact-open';
import {
  artifactKind,
  artifactMedia,
  attachmentIsDoc,
  isDocMedia,
  isXlsxMedia,
  XLSX_MEDIA,
} from './artifacts';
import type { IterationAttachment } from './types';

const data = new Uint8Array([80, 75, 3, 4, 0, 255]);
const workbook = () => new Blob([data], { type: XLSX_MEDIA });

beforeEach(() => {
  native.value = false;
  vi.clearAllMocks();
  share.mockResolvedValue({});
});
afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  vi.useRealTimers();
});

describe('XLSX classification', () => {
  it.each([
    [XLSX_MEDIA, 'report'],
    [XLSX_MEDIA.toUpperCase() + '; charset=binary', 'report'],
    ['application/octet-stream', 'REPORT.XLSX'],
    ['application/zip', 'report.xlsx'],
    ['text/plain', 'report.xlsx'],
    [undefined, 'report.xlsx'],
  ])('recognizes %s / %s without an in-app document reader', (media, filename) => {
    const attachment = { kind: 'doc', filename, media_type: media } as IterationAttachment;
    expect(isXlsxMedia(media, filename)).toBe(true);
    expect(isDocMedia(media, filename)).toBe(false);
    expect(attachmentIsDoc(attachment)).toBe(false);
    expect(artifactKind(attachment)).toBe('file');
    expect(artifactMedia(attachment)).toBe('XLSX');
  });

  it.each(['report.xlsx.exe', 'report.xls', 'report.csv', 'report.xlsx.zip'])(
    'does not treat %s as XLSX',
    (filename) => {
      expect(isXlsxMedia('application/octet-stream', filename)).toBe(false);
    },
  );
});

describe('spreadsheet file hand-off', () => {
  it('sends original bytes, never a gateway URL or token, to the desktop host', async () => {
    const invoke = vi.fn(async () => ({ opened: true }));
    vi.stubGlobal('__TAURI__', { core: { invoke } });
    expect(spreadsheetOpenVerb()).toBe('Open in app');
    expect(await openSpreadsheet(workbook(), '../Q3 report.xlsx', XLSX_MEDIA)).toBe(
      'Workbook opened in your spreadsheet app.',
    );
    expect(invoke).toHaveBeenCalledExactlyOnceWith('open_xlsx', {
      filename: 'Q3-report.xlsx',
      data: Array.from(data),
    });
    expect(share).not.toHaveBeenCalled();
  });

  it('adds the safe extension for a MIME-only workbook', async () => {
    const invoke = vi.fn(async () => ({ opened: true }));
    vi.stubGlobal('__TAURI__', { core: { invoke } });
    await openSpreadsheet(workbook(), 'report.exe', XLSX_MEDIA);
    expect(invoke.mock.calls[0]).toEqual([
      'open_xlsx',
      { filename: 'report.exe.xlsx', data: Array.from(data) },
    ]);
  });

  it('reports the retained download when no desktop app is associated', async () => {
    vi.stubGlobal('__TAURI__', { core: { invoke: vi.fn(async () => ({ opened: false })) } });
    expect(await openSpreadsheet(workbook(), 'report.xlsx')).toBe(
      'Workbook saved in Downloads/Vis Artifacts. Install a spreadsheet app to open it.',
    );
  });

  it.each(['iOS', 'Android'])('uses the %s app chooser with an XLSX cache file', async () => {
    native.value = true;
    expect(spreadsheetOpenVerb()).toBe('Open in app');
    expect(await openSpreadsheet(workbook(), 'Q3 report.xlsx', XLSX_MEDIA)).toBe('Workbook shared.');
    expect(writeFile).toHaveBeenCalledWith(
      expect.objectContaining({
        path: expect.stringMatching(/^shared\/\d+-Q3-report\.xlsx$/),
        directory: 'CACHE',
        data: 'UEsDBAD/',
      }),
    );
    expect(share).toHaveBeenCalledWith({
      title: 'Q3 report.xlsx',
      files: ['file:///cache/report.xlsx'],
      dialogTitle: 'Open workbook in an app',
    });
    // A receiving app may read the shared URI after the chooser has returned.
    expect(deleteFile).not.toHaveBeenCalled();
  });

  it('treats a dismissed native app chooser as cancellation', async () => {
    native.value = true;
    share.mockRejectedValueOnce(new Error('Share canceled'));
    expect(await openSpreadsheet(workbook(), 'report.xlsx')).toBe('');
    expect(deleteFile).toHaveBeenCalledOnce();
  });

  it('downloads in a browser even when Web Share is available', async () => {
    vi.useFakeTimers();
    const webShare = vi.fn();
    vi.stubGlobal('navigator', { share: webShare, canShare: () => true });
    vi.spyOn(URL, 'createObjectURL').mockReturnValue('blob:workbook');
    const revoke = vi.spyOn(URL, 'revokeObjectURL');
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => {});
    expect(spreadsheetOpenVerb()).toBe('Download');
    expect(await openSpreadsheet(workbook(), 'Q3 report.xlsx')).toBe('Workbook downloaded.');
    expect(URL.createObjectURL).toHaveBeenCalledWith(expect.any(Blob));
    const link = click.mock.instances[0] as HTMLAnchorElement;
    expect(link.href).toBe('blob:workbook');
    expect(link.download).toBe('Q3-report.xlsx');
    expect(webShare).not.toHaveBeenCalled();
    expect(share).not.toHaveBeenCalled();
    expect(revoke).not.toHaveBeenCalled();
    vi.runAllTimers();
    expect(revoke).toHaveBeenCalledWith('blob:workbook');
  });

  it('does not open unrelated files or hide real host failures', async () => {
    const invoke = vi.fn(async () => {
      throw new Error('Disk is full');
    });
    vi.stubGlobal('__TAURI__', { core: { invoke } });
    await expect(openSpreadsheet(new Blob(['other']), 'file.exe')).rejects.toThrow('Not an XLSX');
    expect(invoke).not.toHaveBeenCalled();
    await expect(openSpreadsheet(workbook(), 'report.xlsx')).rejects.toThrow('Disk is full');
  });

  it('cleans up a staged native file when the receiver fails', async () => {
    native.value = true;
    share.mockRejectedValueOnce(new Error('App hand-off failed'));
    await expect(openSpreadsheet(workbook(), 'report.xlsx')).rejects.toThrow('App hand-off failed');
    expect(deleteFile).toHaveBeenCalledOnce();
  });
});
