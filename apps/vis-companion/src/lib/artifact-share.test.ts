// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';

const native = vi.hoisted(() => ({ value: true }));
const writeFile = vi.hoisted(() => vi.fn(async () => undefined));
const getUri = vi.hoisted(() => vi.fn(async () => ({ uri: 'file:///cache/shared/report.pdf' })));
const rmdir = vi.hoisted(() => vi.fn(async () => undefined));
const readdir = vi.hoisted(() => vi.fn(async () => ({ files: [] as { name: string }[] })));
const share = vi.hoisted(() => vi.fn(async () => ({})));

vi.mock('@capacitor/core', () => ({
  Capacitor: { isNativePlatform: () => native.value },
}));
vi.mock('@capacitor/filesystem', () => ({
  Directory: { Cache: 'CACHE' },
  Filesystem: { writeFile, getUri, rmdir, readdir },
}));
vi.mock('@capacitor/share', () => ({ Share: { share } }));

import { shareArtifact } from './artifact-share';

describe('artifact sharing', () => {
  beforeEach(() => {
    native.value = true;
    writeFile.mockClear();
    getUri.mockClear();
    rmdir.mockClear();
    readdir.mockReset();
    readdir.mockResolvedValue({ files: [] });
    share.mockClear();
  });

  it('hands the original artifact to the native share sheet as a named file', async () => {
    const result = await shareArtifact(
      new Blob(['report'], { type: 'application/pdf' }),
      '../Q3 report.pdf',
      'application/pdf',
    );

    expect(result).toBe('Artifact shared.');
    expect(writeFile).toHaveBeenCalledWith(
      expect.objectContaining({
        path: expect.stringMatching(/^shared\/\d+\/Q3-report\.pdf$/),
        directory: 'CACHE',
        recursive: true,
      }),
    );
    expect(share).toHaveBeenCalledWith({
      title: '../Q3 report.pdf',
      files: ['file:///cache/shared/report.pdf'],
      dialogTitle: 'Share artifact',
    });
    expect(rmdir).toHaveBeenCalledWith({
      path: expect.stringMatching(/^shared\/\d+$/),
      directory: 'CACHE',
      recursive: true,
    });
  });

  // Regression, verifier report on iOS: each completed share left its file in the cache.
  // After three exports the cache held 5.4 MB that no later share removed.
  it('removes the kept files of earlier shares after a day, and keeps the newer ones', async () => {
    const now = Date.now();
    const old = String(now - 25 * 60 * 60 * 1000);
    const fresh = String(now - 60 * 1000);
    readdir.mockResolvedValue({ files: [{ name: old }, { name: fresh }, { name: 'notes' }] });

    const result = await shareArtifact(new Blob(['zip']), 'bundle.zip', 'application/zip', {
      retainCacheFile: true,
    });

    expect(result).toBe('Artifact shared.');
    expect(readdir).toHaveBeenCalledExactlyOnceWith({ path: 'shared', directory: 'CACHE' });
    // The file of this share stays for its receiver; only the old folder goes.
    expect(rmdir.mock.calls).toEqual([[{ path: `shared/${old}`, directory: 'CACHE', recursive: true }]]);
  });

  it('shares the file also when the cache has no folder of earlier shares', async () => {
    readdir.mockRejectedValue(new Error('Folder does not exist'));
    await expect(shareArtifact(new Blob(['zip']), 'bundle.zip')).resolves.toBe('Artifact shared.');
    expect(share).toHaveBeenCalledOnce();
  });

  it('uses the browser file share contract when it is available', async () => {
    native.value = false;
    const webShare = vi.fn(async (_data: ShareData) => undefined);
    Object.defineProperty(navigator, 'share', { configurable: true, value: webShare });
    Object.defineProperty(navigator, 'canShare', {
      configurable: true,
      value: vi.fn(() => true),
    });

    await shareArtifact(new Blob(['a,b'], { type: 'text/csv' }), 'jobs.csv', 'text/csv');

    expect(webShare).toHaveBeenCalledOnce();
    const file = webShare.mock.calls[0]?.[0].files?.[0];
    expect(file).toMatchObject({ name: 'jobs.csv', type: 'text/csv' });
  });

  it('hands the browser share sheet only the file, without a title as text', async () => {
    // WebKit adds a title as a text item beside the files; Telegram then posted
    // only the filename instead of the shared video.
    native.value = false;
    const webShare = vi.fn(async (_data: ShareData) => undefined);
    Object.defineProperty(navigator, 'share', { configurable: true, value: webShare });
    Object.defineProperty(navigator, 'canShare', {
      configurable: true,
      value: vi.fn(() => true),
    });

    await shareArtifact(new Blob(['clip'], { type: 'video/mp4' }), 'brag-long.mp4', 'video/mp4', {
      title: 'brag-long.mp4',
    });

    const data = webShare.mock.calls[0]?.[0];
    expect(Object.keys(data ?? {})).toEqual(['files']);
    expect(data?.files?.[0]).toMatchObject({ name: 'brag-long.mp4', type: 'video/mp4' });
  });
});
