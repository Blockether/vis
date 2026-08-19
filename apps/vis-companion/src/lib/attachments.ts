import { FilePicker, type PickedFile } from '@capawesome/capacitor-file-picker';
import { Capacitor } from '@capacitor/core';
import { blobAsDataUrl } from './image-file';
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

const DEFAULT_IMAGE_MEDIA_TYPES = [
  'image/jpeg',
  'image/png',
  'image/gif',
  'image/webp',
  'image/bmp',
];

// A clip attaches everywhere a picture does. The engine is what makes that
// honest: `image-convert/video->wire-gif` samples the video into an animated
// GIF for the provider, so the model sees MOTION rather than a filename, while
// the session DB keeps the original bytes for replay. The app therefore only
// has to admit the type, size it, and play it back.
const DEFAULT_VIDEO_MEDIA_TYPES = ['video/mp4', 'video/quicktime'];

// A recording attaches the same way, and the engine is honest about it in the
// other direction: no multimodal wire has a block for audio, so the gateway
// stores the bytes for the HUMAN and NAMES the file to the model. The app owes
// it the same three things — admit the type, size it, play it back.
const DEFAULT_AUDIO_MEDIA_TYPES = [
  'audio/mpeg',
  'audio/aac',
  'audio/mp4',
  'audio/wav',
  'audio/aiff',
  'audio/x-caf',
  'audio/amr',
  'audio/ogg',
  'audio/flac',
];

const DEFAULT_MEDIA_TYPES = [
  ...DEFAULT_IMAGE_MEDIA_TYPES,
  ...DEFAULT_VIDEO_MEDIA_TYPES,
  ...DEFAULT_AUDIO_MEDIA_TYPES,
];

// Intake ceiling, not the provider's: the gateway shrinks an oversize still on
// the way out, so a phone photo is worth uploading. `max_file_bytes` from the
// handshake wins; this only backstops an older gateway.
const DEFAULT_MAX_FILE_BYTES = 25 * 1024 * 1024;
// A clip is megabytes where a screenshot is kilobytes; the gateway advertises
// its own ceiling (`max_video_bytes`) and this only backstops an older one.
const DEFAULT_MAX_VIDEO_BYTES = 32 * 1024 * 1024;
// A recording answers to the clip's ceiling rather than the still's: minutes of
// speech weigh what a screen recording does, and neither ever ships verbatim.
const DEFAULT_MAX_AUDIO_BYTES = 32 * 1024 * 1024;

/** Playable-not-still: the ONE test both the size limit and the preview key off. */
export function isVideoMediaType(media: string | null | undefined): boolean {
  return !!media && media.startsWith('video/');
}

/** Audible-not-visible: a recording has no thumbnail, only a player. */
export function isAudioMediaType(media: string | null | undefined): boolean {
  return !!media && media.startsWith('audio/');
}

export interface AttachmentLimits {
  maxFiles?: number;
  maxFileBytes?: number;
  maxVideoBytes?: number;
  maxAudioBytes?: number;
  mediaTypes?: string[];
}

function base64AsBlob(base64: string, mimeType: string): Blob {
  const payload = base64.startsWith('data:')
    ? base64.slice(base64.indexOf(',') + 1)
    : base64;
  const binary = atob(payload);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) bytes[i] = binary.charCodeAt(i);
  return new Blob([bytes], { type: mimeType });
}

// A blob whose `type` is the media type WE resolved. It routinely is not the
// platform's: Android hands a voice memo over as `application/octet-stream`, and
// the data URL built from such a blob will not play in an <audio> element even
// though the bytes are a perfect recording.
function retyped(blob: Blob, mediaType: string): Blob {
  return blob.type === mediaType ? blob : new Blob([blob], { type: mediaType });
}

// The picker hands back whichever of data/blob/path the platform had; the rest
// of this module only ever wants a Blob, because that is what decodes.
async function pickedFileBlob(
  file: PickedFile,
  mediaType: string,
): Promise<Blob> {
  if (file.blob) return retyped(file.blob, mediaType);
  if (file.data) return base64AsBlob(file.data, mediaType);
  if (file.path) {
    const response = await fetch(file.path);
    if (!response.ok) throw new Error(`Could not read ${file.name}`);
    return retyped(await response.blob(), mediaType);
  }
  throw new Error(`Could not read ${file.name}`);
}

// ONE candidate, however it was chosen: picker entry, pasted clipboard item or
// dropped File. Bytes stay behind a thunk so a rejected candidate is never read.
interface MediaCandidate {
  name: string;
  mimeType: string;
  blob: () => Promise<Blob>;
}

interface PreparedAttachment {
  filename: string;
  mediaType: string;
  dataUrl: string;
  size: number;
}

/**
 * Blob -> the exact envelope the gateway is given.
 *
 * NO OPTIMIZATION, here or anywhere: vis sends, stores and replays exactly the
 * bytes the user picked. A payload the gateway would refuse in one request is
 * REJECTED with a reason the user can act on, rather than silently resampled
 * behind their back — image optimization is a real problem with real tradeoffs
 * and vis does not pretend to solve it by shrinking things in the dark. The
 * same holds for a clip: transcoding for the provider is the ENGINE's job, on
 * a copy, and never costs the user the original.
 */
async function prepareAttachment(
  blob: Blob,
  name: string,
  mimeType: string,
  maxBytes: number,
): Promise<PreparedAttachment> {
  if (blob.size > maxBytes) {
    throw new Error(`larger than ${Math.round(maxBytes / 1024 / 1024)} MB`);
  }
  return {
    filename: name,
    mediaType: mimeType,
    dataUrl: await blobAsDataUrl(blob),
    size: blob.size,
  };
}

// The single admission gate: what the gateway accepts, and how many bytes of it.
// A clip and a still have different ceilings, so the limit is per candidate.
function attachmentGate({
  maxFileBytes = DEFAULT_MAX_FILE_BYTES,
  maxVideoBytes = DEFAULT_MAX_VIDEO_BYTES,
  maxAudioBytes = DEFAULT_MAX_AUDIO_BYTES,
  mediaTypes = DEFAULT_MEDIA_TYPES,
}: AttachmentLimits) {
  return {
    accepts: (mimeType: string) => mediaTypes.includes(mimeType),
    limitFor: (mimeType: string) => {
      if (isAudioMediaType(mimeType)) return maxAudioBytes;
      if (isVideoMediaType(mimeType)) return maxVideoBytes;
      return maxFileBytes;
    },
  };
}

async function collectAttachments(
  candidates: MediaCandidate[],
  limits: AttachmentLimits,
): Promise<PickAttachmentResult> {
  const { maxFiles = 8 } = limits;
  const gate = attachmentGate(limits);
  const attachments: PendingAttachment[] = [];
  const rejected: string[] = [];
  for (const candidate of candidates) {
    const { name, mimeType } = candidate;
    if (attachments.length >= maxFiles) {
      rejected.push(`${name}: limit of ${maxFiles} attachments reached`);
      continue;
    }
    if (!gate.accepts(mimeType)) {
      rejected.push(`${name}: unsupported media format`);
      continue;
    }
    try {
      const prepared = await prepareAttachment(
        await candidate.blob(),
        name,
        mimeType,
        gate.limitFor(mimeType),
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
      rejected.push(`${name}: ${(cause as Error).message}`);
    }
  }
  return { attachments, rejected };
}

// The document browser is the one picker that hands back a file the platform
// could not name: an iCloud Drive item and a voice memo routinely arrive with an
// empty `mimeType`. The extension is then the only claim there is — and it stays
// a claim: the gateway sniffs every payload from magic bytes and never trusts
// the label, so a wrong guess is refused there instead of believed here.
const EXTENSION_MEDIA_TYPES: Record<string, string> = {
  jpg: 'image/jpeg',
  jpeg: 'image/jpeg',
  png: 'image/png',
  gif: 'image/gif',
  webp: 'image/webp',
  bmp: 'image/bmp',
  mp4: 'video/mp4',
  m4v: 'video/mp4',
  mov: 'video/quicktime',
  mp3: 'audio/mpeg',
  m4a: 'audio/mp4',
  wav: 'audio/wav',
  ogg: 'audio/ogg',
  oga: 'audio/ogg',
  flac: 'audio/flac',
  aac: 'audio/aac',
  m4b: 'audio/mp4',
  aif: 'audio/aiff',
  aiff: 'audio/aiff',
  aifc: 'audio/aiff',
  caf: 'audio/x-caf',
  amr: 'audio/amr',
  opus: 'audio/ogg',
};

// A claim of "some bytes" is no claim at all. Android's document provider
// answers `application/octet-stream` for anything its MIME table does not know —
// a voice memo, a recording synced from a desktop — and believing that word
// costs the user the file, since the gate then refuses a format the gateway
// takes. The extension is the better guess, and the gateway's magic-byte sniff
// is the verdict either way.
const UNNAMED_MEDIA_TYPES = ['application/octet-stream', 'binary/octet-stream'];

/** What a picked file CLAIMS to be: the platform's word, else its extension. */
export function candidateMediaType(
  name: string,
  declared: string | null | undefined,
): string {
  const claim = (declared ?? '').trim().toLowerCase();
  if (claim && !UNNAMED_MEDIA_TYPES.includes(claim)) return claim;
  const extension = (name.split('.').pop() ?? '').toLowerCase();
  return EXTENSION_MEDIA_TYPES[extension] ?? claim;
}

/** Picker entries as candidates — the one shape every chooser funnels into. */
function pickedCandidates(files: PickedFile[]): MediaCandidate[] {
  return files.map((file) => {
    const mimeType = candidateMediaType(file.name, file.mimeType);
    return {
      name: file.name,
      mimeType,
      blob: () => pickedFileBlob(file, mimeType),
    };
  });
}

export async function pickMediaAttachments(
  limits: AttachmentLimits = {},
): Promise<PickAttachmentResult> {
  const mediaTypes = limits.mediaTypes ?? DEFAULT_MEDIA_TYPES;
  // Native gets the OS gallery sheet: `pickMedia` when the gateway takes clips,
  // `pickImages` when it does not, so a video the server would refuse is never
  // even offered. Web falls back to the typed file dialog.
  const wantsVideo = mediaTypes.some(isVideoMediaType);
  const result = Capacitor.isNativePlatform()
    ? await (wantsVideo ? FilePicker.pickMedia : FilePicker.pickImages)({
        readData: true,
        skipTranscoding: false,
        ordered: true,
      })
    : await FilePicker.pickFiles({ types: mediaTypes, readData: true });

  return collectAttachments(pickedCandidates(result.files), limits);
}

/**
 * The FILES door: the platform's own document browser, not its photo gallery.
 *
 * The gallery sheet can only offer what the camera roll holds. A voice memo, a
 * clip that arrived in a chat, a recording synced from a desktop and a picture
 * that was saved to Files instead of Photos all live somewhere the gallery
 * cannot see — so on a phone the `+` could reach none of them, however many
 * media types the gateway advertised. `pickFiles` is the same call the web
 * dialog already makes, and the same gate then judges the candidate: one
 * chooser more, no second idea of what is acceptable.
 */
export async function pickDocumentAttachments(
  limits: AttachmentLimits = {},
): Promise<PickAttachmentResult> {
  const types = limits.mediaTypes ?? DEFAULT_MEDIA_TYPES;
  const result = await FilePicker.pickFiles({ types, readData: true });
  return collectAttachments(pickedCandidates(result.files), limits);
}

/** Capture one new photo with the native camera, then apply the normal attachment gate. */
export async function capturePhotoAttachment(
  limits: AttachmentLimits = {},
): Promise<PickAttachmentResult> {
  const { Camera, CameraResultType, CameraSource } =
    await import('@capacitor/camera');
  const permission = await Camera.requestPermissions({
    permissions: ['camera'],
  });
  if (permission.camera === 'denied') {
    throw new Error('Camera access was denied — enable it in Settings');
  }

  const photo = await Camera.getPhoto({
    quality: 100,
    allowEditing: false,
    correctOrientation: true,
    resultType: CameraResultType.Base64,
    saveToGallery: false,
    source: CameraSource.Camera,
  });
  if (!photo.base64String) throw new Error('The camera did not return a photo');

  const format = photo.format.toLowerCase();
  const extension = format === 'jpeg' ? 'jpg' : format;
  const mimeType =
    format === 'jpg' || format === 'jpeg' ? 'image/jpeg' : `image/${format}`;
  const timestamp = new Date().toISOString().replace(/[:.]/gu, '-');
  const blob = base64AsBlob(photo.base64String, mimeType);

  return collectAttachments(
    [
      {
        name: `photo-${timestamp}.${extension}`,
        mimeType,
        blob: async () => blob,
      },
    ],
    { ...limits, maxFiles: 1 },
  );
}

// Build attachments from raw File/Blob objects — the clipboard-paste and
// drag-drop path (web + iOS/Android WKWebView), reusing the same validation
// and data-URL encoding as the native picker above.
export async function attachmentsFromFiles(
  files: File[],
  limits: AttachmentLimits = {},
): Promise<PickAttachmentResult> {
  return collectAttachments(
    files.map((file) => {
      const mimeType = candidateMediaType(file.name, file.type);
      return {
        name:
          file.name ||
          (isVideoMediaType(mimeType)
            ? 'pasted-clip'
            : isAudioMediaType(mimeType)
              ? 'pasted-recording'
              : 'pasted-image'),
        mimeType,
        blob: async () => retyped(file, mimeType),
      };
    }),
    limits,
  );
}

/** A canvas edit is PNG bytes; the name has to follow, or the extension lies. */
function pngFilename(name: string): string {
  const base = name.replace(/\.[^./\\]+$/u, '');
  return `${base || 'image'}.png`;
}

/**
 * Swap one pending attachment's bytes for an edited copy, in its own slot.
 *
 * The id and the position in the composer survive, so annotating a screenshot
 * before sending reads as "the same picture, drawn on" instead of arriving as a
 * second attachment next to the untouched one. Nothing is optimised on the way
 * through — the gateway's ceiling is re-checked here only because burning
 * strokes into a picture can grow the payload, never shrink it.
 */
export async function editedAttachment(
  previous: PendingAttachment,
  edited: Blob,
  limits: AttachmentLimits = {},
): Promise<PendingAttachment> {
  const mediaType = edited.type || 'image/png';
  const gate = attachmentGate(limits);
  if (!gate.accepts(mediaType))
    throw new Error(`${mediaType} is not accepted here`);
  const prepared = await prepareAttachment(
    edited,
    pngFilename(previous.filename),
    mediaType,
    gate.limitFor(mediaType),
  );
  return {
    ...previous,
    filename: prepared.filename,
    media_type: prepared.mediaType,
    base64: prepared.dataUrl,
    previewUrl: prepared.dataUrl,
    size: prepared.size,
  };
}
