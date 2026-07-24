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

// Build attachments from raw File/Blob objects — the clipboard-paste and
// drag-drop path (web + iOS/Android WKWebView), reusing the same validation and
// data-URL encoding as the native file picker above.
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
    if (file.size > maxFileBytes) {
      rejected.push(`${name}: larger than ${Math.round(maxFileBytes / 1024 / 1024)} MB`);
      continue;
    }
    try {
      const previewUrl = await blobAsDataUrl(file);
      attachments.push({
        id: crypto.randomUUID(),
        filename: name,
        media_type: file.type,
        base64: previewUrl,
        previewUrl,
        size: file.size,
      });
    } catch (cause) {
      rejected.push(`${name}: ${(cause as Error).message}`);
    }
  }
  return { attachments, rejected };
}

// Native camera capture (iOS/Android) via the official @capacitor/camera plugin.
// This is SPM-compatible, so it links into the Capacitor 8 iOS project cleanly —
// unlike the ML Kit barcode scanner, whose GoogleMLKit dependency is CocoaPods-only.
export async function captureCameraAttachment({
  maxFileBytes = 5 * 1024 * 1024,
}: { maxFileBytes?: number } = {}): Promise<PendingAttachment | null> {
  const { Camera, CameraResultType, CameraSource } = await import('@capacitor/camera');
  const permission = await Camera.requestPermissions({ permissions: ['camera'] });
  if (permission.camera === 'denied') {
    throw new Error('Camera access was denied — enable it in Settings');
  }
  const photo = await Camera.getPhoto({
    quality: 82,
    allowEditing: false,
    resultType: CameraResultType.Base64,
    source: CameraSource.Camera,
    saveToGallery: false,
  });
  if (!photo.base64String) return null;
  const format = (photo.format || 'jpeg').toLowerCase();
  const media_type = `image/${format === 'jpg' ? 'jpeg' : format}`;
  const previewUrl = `data:${media_type};base64,${photo.base64String}`;
  // base64String is raw (no data: prefix); size approximates decoded bytes.
  const size = Math.floor((photo.base64String.length * 3) / 4);
  if (size > maxFileBytes) {
    throw new Error(`Photo is larger than ${Math.round(maxFileBytes / 1024 / 1024)} MB`);
  }
  return {
    id: crypto.randomUUID(),
    filename: `camera-${Date.now()}.${format === 'jpeg' ? 'jpg' : format}`,
    media_type,
    base64: previewUrl,
    previewUrl,
    size,
  };
}
