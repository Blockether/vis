// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const native = vi.hoisted(() => ({ value: false }));
const writeFile = vi.hoisted(() => vi.fn(async () => undefined));
const getUri = vi.hoisted(() => vi.fn(async () => ({ uri: 'file:///cache/export' })));
const rmdir = vi.hoisted(() => vi.fn(async () => undefined));
const share = vi.hoisted(() => vi.fn(async () => ({})));
vi.mock('@capacitor/core', () => ({ Capacitor: { isNativePlatform: () => native.value } }));
vi.mock('@capacitor/filesystem', () => ({
  Directory: { Cache: 'CACHE' },
  Filesystem: { writeFile, getUri, rmdir },
}));
vi.mock('@capacitor/share', () => ({ Share: { share } }));

import { GatewayError, type GatewayClient } from './gateway';
import { exportFailure, exportSession, sessionExportFilename } from './session-export';

const session = { id: '123e4567-e89b-12d3-a456-426614174000', title: 'Fix the Stalled  Stream!' };
const bytes = new Uint8Array([86, 73, 83, 0, 255]);

/** A gateway that answers each export route with its own body. */
const gateway = () =>
  ({
    transcriptHtml: vi.fn(async () => new Blob(['<html></html>'], { type: 'text/html' })),
    transcriptMarkdown: vi.fn(async () => new Blob(['# Turn 1'], { type: 'text/markdown' })),
    sessionSnapshot: vi.fn(async () => new Blob([bytes])),
  }) as unknown as GatewayClient;

beforeEach(() => {
  native.value = false;
  vi.clearAllMocks();
  share.mockResolvedValue({});
});
afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe('the name of an exported session file', () => {
  it.each([
    [session.title, 'html', 'fix-the-stalled-stream-123e4567.html'],
    ['  ', 'md', 'vis-session-123e4567.md'],
    ['Zażółć', 'vissnap', 'za-123e4567.vissnap'],
    [`${'a'.repeat(47)} b c`, 'md', `${'a'.repeat(47)}-123e4567.md`],
  ] as const)('names %j as %s: %s', (title, format, name) => {
    expect(sessionExportFilename({ ...session, title }, format)).toBe(name);
  });
});

describe('a session export on each host', () => {
  it('saves the snapshot bytes through the desktop host and names the saved path', async () => {
    const path = '/home/ada/Downloads/Vis Artifacts/1/fix-the-stalled-stream-123e4567.vissnap';
    const invoke = vi.fn(async () => ({ revealed: true, path }));
    vi.stubGlobal('__TAURI__', { core: { invoke } });
    expect(await exportSession(gateway(), session, 'vissnap')).toBe(`Snapshot saved to ${path}.`);
    expect(invoke).toHaveBeenCalledExactlyOnceWith('reveal_artifact', {
      filename: 'fix-the-stalled-stream-123e4567.vissnap',
      data: Array.from(bytes),
    });
  });

  it.each(['iOS', 'Android'])('opens the %s share sheet with the Markdown file', async () => {
    native.value = true;
    expect(await exportSession(gateway(), session, 'md')).toBe('Markdown file shared.');
    expect(writeFile.mock.calls[0]).toEqual([
      expect.objectContaining({
        path: expect.stringMatching(/^shared\/\d+\/fix-the-stalled-stream-123e4567\.md$/),
        data: btoa('# Turn 1'),
      }),
    ]);
    expect(share).toHaveBeenCalledExactlyOnceWith(
      expect.objectContaining({ files: ['file:///cache/export'] }),
    );
  });

  it('says nothing when the person closes the share sheet', async () => {
    native.value = true;
    share.mockRejectedValue(new Error('Share canceled'));
    expect(await exportSession(gateway(), session, 'html')).toBe('');
  });

  it('downloads the HTML page in a browser', async () => {
    vi.spyOn(URL, 'createObjectURL').mockReturnValue('blob:export');
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => {});
    expect(await exportSession(gateway(), session, 'html')).toBe('HTML page downloaded.');
    expect((click.mock.instances[0] as HTMLAnchorElement).download).toBe(
      'fix-the-stalled-stream-123e4567.html',
    );
  });
});

describe('the words of a failed export', () => {
  it('says that a gateway without a web app cannot make the HTML page', () => {
    const refusal = new GatewayError(503, 'transcript export template missing');
    expect(exportFailure(refusal, 'html')).toMatch(/cannot export HTML/);
  });

  it('keeps the reason that the gateway gave', () => {
    const refusal = new GatewayError(413, 'The session snapshot exceeds the size limit');
    expect(exportFailure(refusal, 'vissnap')).toBe(
      'The export failed: The session snapshot exceeds the size limit',
    );
  });
});
