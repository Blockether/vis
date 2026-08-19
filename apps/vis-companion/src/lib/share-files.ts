// Files the SYSTEM share sheet handed over, on their way into the composer.
//
// A share is bytes on disk, not bytes in the URL: `vis://share?file=…` names a
// copy the native side staged (Android cache dir, iOS App Group container)
// precisely because the original — a `content://` URI, a share extension's own
// temp file — is unreadable from this process. So the app's job is: read the
// staged copy, hand it to the same validation every picked file goes through,
// and DELETE the copy, because a voice memo left behind is megabytes the user
// never asked us to keep.

import { Filesystem } from '@capacitor/filesystem';
import {
  attachmentsFromFiles,
  candidateMediaType,
  type AttachmentLimits,
  type PickAttachmentResult,
} from './attachments';
import { bridged } from './bridge';
import type { SharedFile } from './share-intake';

/**
 * Reading 30 MB of audio off disk and base64-encoding it across the bridge is
 * not a 2-second call, so this is not the default bridge deadline. It is still
 * A deadline: a wedged bridge must read as "could not be read", never as a
 * composer that never comes back.
 */
const READ_TIMEOUT_MS = 30_000;

function bytesFromBase64(base64: string): Uint8Array {
  const payload = base64.startsWith('data:')
    ? base64.slice(base64.indexOf(',') + 1)
    : base64;
  const binary = atob(payload);
  const bytes = new Uint8Array(binary.length);
  for (let index = 0; index < binary.length; index += 1) {
    bytes[index] = binary.charCodeAt(index);
  }
  return bytes;
}

/** Drop the staged copy. Best effort: the share is already in the composer. */
export async function discardSharedFiles(files: SharedFile[]): Promise<void> {
  for (const file of files) {
    await bridged(
      () => Filesystem.deleteFile({ path: file.path }),
      () => undefined,
    );
  }
}

/**
 * The staged copies as `File` objects, in share order. A file that cannot be
 * read is SKIPPED rather than fatal — one unreadable item must not cost the
 * others, and the caller reports the shortfall.
 */
export async function readSharedFiles(files: SharedFile[]): Promise<File[]> {
  const read: File[] = [];
  for (const file of files) {
    const data = await bridged<string | Blob | null>(
      async () => (await Filesystem.readFile({ path: file.path })).data,
      () => null,
      READ_TIMEOUT_MS,
    );
    if (!data) continue;
    // The platform's claim is often `application/octet-stream` (Android's
    // document provider says that about any voice memo), so the extension gets
    // the same second word it gets in the picker.
    const type = candidateMediaType(file.name, file.type);
    const bytes: BlobPart =
      typeof data === 'string' ? bytesFromBase64(data).buffer as ArrayBuffer : data;
    read.push(new File([bytes], file.name, { type }));
  }
  return read;
}

/**
 * Shared files as composer attachments — the same gate the `+` uses, so a
 * shared clip is refused for the same reasons a picked one is, in the same
 * words.
 */
export async function attachmentsFromSharedFiles(
  files: SharedFile[],
  limits: AttachmentLimits = {},
): Promise<PickAttachmentResult> {
  const read = await readSharedFiles(files);
  const result = await attachmentsFromFiles(read, limits);
  void discardSharedFiles(files);
  const unread = files.length - read.length;
  if (unread <= 0) return result;
  return {
    attachments: result.attachments,
    rejected: [
      ...result.rejected,
      `${unread} shared ${unread === 1 ? 'file' : 'files'} could not be read`,
    ],
  };
}
