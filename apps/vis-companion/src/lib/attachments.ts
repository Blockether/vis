import { FilePicker, type PickedFile } from '@capawesome/capacitor-file-picker';
import { Capacitor } from '@capacitor/core';
import { MAX_DECODE_BYTES, optimizeImage, retargetFilename } from './image-optimize';
import type { GatewayAttachment } from './types';

export interface PendingAttachment extends GatewayAttachment {
  id: string;
  size: number;
  previewUrl: string;
}

export interface PickAttachmentResult {
  attachments: PendingAttachment[];
  rejected: string[];
}

const DEFAULT_MEDIA_TYPES = [
  'image/jpeg',
  'image/png',
  'image/gif',
  'image/webp',
  'image/bmp',
];

function blobAsDataUrl(blob: Blob): Promise<string> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onerror = () => reject(reader.error ?? new Error('Could not read image'));
    reader.onload = () => resolve(String(reader.result));
    reader.readAsDataURL(blob);
  });
}

function base64AsBlob(base64: string, mimeType: string): Blob {
  const payload = base64.startsWith('data:') ? base64.slice(base64.indexOf(',') + 1) : base64;
  const binary = atob(payload);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) bytes[i] = binary.charCodeAt(i);
  return new Blob([bytes], { type: mimeType });
}

// The picker hands back whichever of data/blob/path the platform had; the rest
// of this module only ever wants a Blob, because that is what decodes.
async function pickedFileBlob(file: PickedFile): Promise<Blob> {
  if (file.blob) return file.blob;
  if (file.data) return base64AsBlob(file.data, file.mimeType || 'application/octet-stream');
  if (file.path) {
    const response = await fetch(file.path);
    if (!response.ok) throw new Error(`Could not read ${file.name}`);
    return response.blob();
  }
  throw new Error(`Could not read ${file.name}`);
}

interface PreparedImage {
  filename: string;
  mediaType: string;
  dataUrl: string;
  size: number;
}

/**
 * Blob -> the exact envelope the gateway is given, SHRUNK first.
 *
 * The size limit is checked against the OPTIMIZED bytes, never the original:
 * a 6 MB phone photo is a perfectly ordinary thing to send and lands well under
 * the cap once its long edge is bounded, so rejecting it up front (what this
 * did before) only ever punished the good case. What genuinely cannot be shrunk
 * under the cap is still refused, with a message that says so.
 */
async function prepareImage(
  blob: Blob,
  name: string,
  mimeType: string,
  maxFileBytes: number,
): Promise<PreparedImage> {
  if (blob.size > MAX_DECODE_BYTES) {
    throw new Error(`too large to process (${Math.round(blob.size / 1024 / 1024)} MB)`);
  }
  const optimized = await optimizeImage(blob, mimeType);
  const payload = optimized?.blob ?? blob;
  const mediaType = optimized?.mediaType ?? mimeType;
  if (payload.size > maxFileBytes) {
    throw new Error(`larger than ${Math.round(maxFileBytes / 1024 / 1024)} MB even after shrinking`);
  }
  return {
    filename: optimized ? retargetFilename(name, mediaType) : name,
    mediaType,
    dataUrl: await blobAsDataUrl(payload),
    size: payload.size,
  };
}

export async function pickImageAttachments({
  maxFiles = 8,
  maxFileBytes = 5 * 1024 * 1024,
  mediaTypes = DEFAULT_MEDIA_TYPES,
}: {
  maxFiles?: number;
  maxFileBytes?: number;
  mediaTypes?: string[];
} = {}): Promise<PickAttachmentResult> {
  const result = Capacitor.isNativePlatform()
    ? await FilePicker.pickImages({
        readData: true,
        skipTranscoding: false,
        ordered: true,
      })
    : await FilePicker.pickFiles({ types: mediaTypes, readData: true });

  const attachments: PendingAttachment[] = [];
  const rejected: string[] = [];
  for (const file of result.files) {
    if (attachments.length >= maxFiles) {
      rejected.push(`${file.name}: limit of ${maxFiles} images reached`);
      continue;
    }
    if (!mediaTypes.includes(file.mimeType)) {
      rejected.push(`${file.name}: unsupported image format`);
      continue;
    }

    try {
      const prepared = await prepareImage(
        await pickedFileBlob(file),
        file.name,
        file.mimeType,
        maxFileBytes,
      );
      attachments.push({
        id: crypto.randomUUID(),
        filename: prepared.filename,
        media_type: prepared.mediaType,
        base64: prepared.dataUrl,
        previewUrl: prepared.dataUrl,
        size: prepared.size,
      });
    } catch (cause) {
      rejected.push(`${file.name}: ${(cause as Error).message}`);
    }
  }
  return { attachments, rejected };
}

// Build attachments from raw File/Blob objects — the clipboard-paste and
// drag-drop path (web + iOS/Android WKWebView), reusing the same validation,
// shrinking and data-URL encoding as the native file picker above.
export async function attachmentsFromFiles(
  files: File[],
  {
    maxFiles = 8,
    maxFileBytes = 5 * 1024 * 1024,
    mediaTypes = DEFAULT_MEDIA_TYPES,
  }: { maxFiles?: number; maxFileBytes?: number; mediaTypes?: string[] } = {},
): Promise<PickAttachmentResult> {
  const attachments: PendingAttachment[] = [];
  const rejected: string[] = [];
  for (const file of files) {
    const name = file.name || 'pasted-image';
    if (attachments.length >= maxFiles) {
      rejected.push(`${name}: limit of ${maxFiles} images reached`);
      continue;
    }
    if (!mediaTypes.includes(file.type)) {
      rejected.push(`${name}: unsupported image format`);
      continue;
    }
    try {
      const prepared = await prepareImage(file, name, file.type, maxFileBytes);
      attachments.push({
        id: crypto.randomUUID(),
        filename: prepared.filename,
        media_type: prepared.mediaType,
        base64: prepared.dataUrl,
        previewUrl: prepared.dataUrl,
        size: prepared.size,
      });
    } catch (cause) {
      rejected.push(`${name}: ${(cause as Error).message}`);
    }
  }
  return { attachments, rejected };
}
