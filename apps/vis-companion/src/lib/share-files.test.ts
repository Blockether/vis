// @vitest-environment jsdom
// What the SYSTEM share sheet hands over, on its way into the composer.
//
// The native side has already staged a copy (Android cache dir, iOS App Group
// container) and named it in the deep link; everything below is the app reading
// that copy, gating it exactly like a picked file, and deleting it afterwards.

import { beforeEach, describe, expect, it, vi } from 'vitest';

const readFile = vi.fn();
const deleteFile = vi.fn();

vi.mock('@capacitor/filesystem', () => ({
  Filesystem: {
    readFile: (options: { path: string }) => readFile(options),
    deleteFile: (options: { path: string }) => deleteFile(options),
  },
}));

const { attachmentsFromSharedFiles, discardSharedFiles, readSharedFiles } = await import(
  './share-files'
);

const staged = (name: string, type?: string) => ({
  path: `/tmp/vis-share/0/${name}`,
  name,
  type,
});

// A four-byte payload is enough: this module is about routing, not decoding.
const base64 = btoa('vis!');

beforeEach(() => {
  readFile.mockReset();
  deleteFile.mockReset();
  readFile.mockResolvedValue({ data: base64 });
  deleteFile.mockResolvedValue(undefined);
});

describe('readSharedFiles', () => {
  it('reads every staged copy in share order', async () => {
    const files = await readSharedFiles([staged('memo.m4a', 'audio/mp4'), staged('shot.png', 'image/png')]);

    expect(files.map((file) => file.name)).toEqual(['memo.m4a', 'shot.png']);
    expect(readFile).toHaveBeenCalledTimes(2);
    expect(readFile).toHaveBeenNthCalledWith(1, { path: '/tmp/vis-share/0/memo.m4a' });
  });

  // Android's document provider calls a voice memo `application/octet-stream`,
  // and a blob typed that way plays in nothing.
  it('types a file by its extension when the platform makes no claim', async () => {
    const [file] = await readSharedFiles([staged('memo.m4a', 'application/octet-stream')]);

    expect(file.type).toBe('audio/mp4');
  });

  // One unreadable item must not cost the others their share.
  it('skips a file it cannot read', async () => {
    readFile.mockRejectedValueOnce(new Error('gone'));

    const files = await readSharedFiles([staged('memo.m4a', 'audio/mp4'), staged('shot.png', 'image/png')]);

    expect(files.map((file) => file.name)).toEqual(['shot.png']);
  });
});

describe('attachmentsFromSharedFiles', () => {
  it('hands a shared recording to the composer as an attachment', async () => {
    const result = await attachmentsFromSharedFiles([staged('memo.m4a', 'audio/mp4')]);

    expect(result.rejected).toEqual([]);
    expect(result.attachments).toHaveLength(1);
    expect(result.attachments[0]?.media_type).toBe('audio/mp4');
    expect(result.attachments[0]?.filename).toBe('memo.m4a');
  });

  it('reports the files it could not read', async () => {
    readFile.mockRejectedValueOnce(new Error('gone'));

    const result = await attachmentsFromSharedFiles([staged('memo.m4a', 'audio/mp4')]);

    expect(result.attachments).toEqual([]);
    expect(result.rejected).toContain('1 shared file could not be read');
  });

  // Megabytes of audio the user never asked us to keep: the staged copy is the
  // native side's temporary, and the app is the only one who knows it is spent.
  it('deletes the staged copies', async () => {
    await attachmentsFromSharedFiles([staged('memo.m4a', 'audio/mp4')]);
    await discardSharedFiles([staged('shot.png', 'image/png')]);

    expect(deleteFile.mock.calls.map(([options]) => options.path)).toContain(
      '/tmp/vis-share/0/shot.png',
    );
  });
});
