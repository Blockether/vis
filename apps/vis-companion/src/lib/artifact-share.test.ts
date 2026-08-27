// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';

const native = vi.hoisted(() => ({ value: true }));
const writeFile = vi.hoisted(() => vi.fn(async () => undefined));
const getUri = vi.hoisted(() => vi.fn(async () => ({ uri: 'file:///cache/shared/report.pdf' })));
const deleteFile = vi.hoisted(() => vi.fn(async () => undefined));
const share = vi.hoisted(() => vi.fn(async () => ({})));

vi.mock('@capacitor/core', () => ({
  Capacitor: { isNativePlatform: () => native.value },
}));
vi.mock('@capacitor/filesystem', () => ({
  Directory: { Cache: 'CACHE' },
  Filesystem: { writeFile, getUri, deleteFile },
}));
vi.mock('@capacitor/share', () => ({ Share: { share } }));

import { shareArtifact } from './artifact-share';

describe('artifact sharing', () => {
  beforeEach(() => {
    native.value = true;
    writeFile.mockClear();
    getUri.mockClear();
    deleteFile.mockClear();
    share.mockClear();
  });

  it('hands the original artifact to the native share sheet as a named file', async () => {
    const result = await shareArtifact(
      new Blob(['report'], { type: 'application/pdf' }),
      '../Q3 report.pdf',
      'application/pdf',
    );

    expect(result).toBe('Artifact shared.');
    expect(writeFile).toHaveBeenCalledWith(expect.objectContaining({
      path: expect.stringMatching(/^shared\/\d+-Q3-report\.pdf$/),
      directory: 'CACHE',
      recursive: true,
    }));
    expect(share).toHaveBeenCalledWith({
      title: '../Q3 report.pdf',
      files: ['file:///cache/shared/report.pdf'],
      dialogTitle: 'Share artifact',
    });
    expect(deleteFile).toHaveBeenCalledWith(expect.objectContaining({ directory: 'CACHE' }));
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
});
