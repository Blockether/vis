import { FilePicker, type PickedFile } from '@capawesome/capacitor-file-picker';
import { Capacitor } from '@capacitor/core';
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

async function pickedFileData(file: PickedFile): Promise<string> {
  if (file.data) {
    return file.data.startsWith('data:')
      ? file.data
      : `data:${file.mimeType || 'application/octet-stream'};base64,${file.data}`;
  }
  if (file.blob) return blobAsDataUrl(file.blob);
  if (file.path) {
    const response = await fetch(file.path);
    if (!response.ok) throw new Error(`Could not read ${file.name}`);
    return blobAsDataUrl(await response.blob());
  }
  throw new Error(`Could not read ${file.name}`);
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
    if (file.size > maxFileBytes) {
      rejected.push(`${file.name}: larger than ${Math.round(maxFileBytes / 1024 / 1024)} MB`);
      continue;
    }

    try {
      const previewUrl = await pickedFileData(file);
      attachments.push({
        id: crypto.randomUUID(),
        filename: file.name,
        media_type: file.mimeType,
        base64: previewUrl,
        previewUrl,
        size: file.size,
      });
    } catch (cause) {
      rejected.push(`${file.name}: ${(cause as Error).message}`);
    }
  }
  return { attachments, rejected };
}
