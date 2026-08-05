/**
 * WHAT A SESSION PRODUCED, as one list.
 *
 * The gateway never ships bytes with a transcript: every figure, clip, PDF and
 * recorded file arrives as an `IterationAttachment` descriptor nested three deep
 * (turn → iteration → attachments). Painted where it was made, that is exactly
 * right; asked "show me that chart again", it is a scroll hunt through forty
 * turns.
 *
 * This module is the flattening, and it is the ONE place the app decides what
 * KIND of thing an attachment is. The classification used to live twice — the
 * transcript's private `attachmentIs*` helpers in `ChatContent`, the document
 * predicates in `DocArtifact` — so a new document media type had to be taught to
 * two components that disagree in the meantime.
 *
 * Nothing here touches React or the DOM: it is descriptors in, descriptors out,
 * which is why the whole vocabulary is testable without rendering a session.
 */

import type { IterationAttachment, TranscriptTurn } from './types';

/** Media types that ride the transcript as a document, never as model input. */
const DOC_MEDIA = new Set([
  'application/pdf',
  'text/html',
  'application/xhtml+xml',
]);

export function isDocMedia(mime: string | undefined): boolean {
  return DOC_MEDIA.has((mime ?? '').split(';')[0].trim().toLowerCase());
}

export function isPdfMedia(mime: string | undefined): boolean {
  return (mime ?? '').toLowerCase().includes('pdf');
}

/** The chip beside the name: what KIND of document this is. */
export function docKindLabel(mime: string | undefined): string {
  if (isPdfMedia(mime)) return 'PDF';
  if (isDocMedia(mime)) return 'HTML';
  return 'DOC';
}

export function attachmentIsImage(attachment: IterationAttachment): boolean {
  const media = attachment.media_type ?? '';
  return media ? media.startsWith('image/') : attachment.kind === 'image';
}

export function attachmentIsVideo(attachment: IterationAttachment): boolean {
  return (attachment.media_type ?? '').startsWith('video/');
}

// A PDF or an HTML page is a DOCUMENT: `vis_attach` clamps it to
// `audience: "user"`, so its bytes never reach the model and the app owes the
// human a reader for them instead of one more line in the recorded-files row.
export function attachmentIsDoc(attachment: IterationAttachment): boolean {
  return isDocMedia(attachment.media_type) || attachment.kind === 'doc';
}

// A still and a clip belong to the SAME rail: both are something the user asked
// to SEE, so both paint where they were made. Everything else is a recorded file.
export function attachmentIsPlayable(attachment: IterationAttachment): boolean {
  return attachmentIsImage(attachment) || attachmentIsVideo(attachment);
}

export function attachmentBytes(bytes?: number): string {
  if (typeof bytes !== 'number' || !Number.isFinite(bytes) || bytes < 0)
    return '';
  if (bytes < 1024) return `${bytes}B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)}KB`;
  return `${(bytes / 1024 / 1024).toFixed(1)}MB`;
}

/**
 * The four things an artifact can BE, in the order of how much the app can do
 * with one: a picture it can zoom and draw on, a clip it can play, a document it
 * can read in a sandboxed frame, and a file it can only name.
 */
export type ArtifactKind = 'image' | 'video' | 'doc' | 'file';

export function artifactKind(attachment: IterationAttachment): ArtifactKind {
  if (attachmentIsImage(attachment)) return 'image';
  if (attachmentIsVideo(attachment)) return 'video';
  if (attachmentIsDoc(attachment)) return 'doc';
  return 'file';
}

/**
 * `PNG`, `MP4`, `CSV` — one word for the format. The FILENAME is asked first
 * because it is what the human named the thing: an artifact written as
 * `coverage.json` is served as `application/json` and reads better as `JSON`
 * than as its subtype, and a generic `application/octet-stream` carries no word
 * at all.
 */
export function artifactMedia(attachment: IterationAttachment): string {
  const extension = (attachment.filename ?? '').split('.').pop() ?? '';
  if (extension && /^[a-z0-9]{1,5}$/i.test(extension))
    return extension.toUpperCase();
  const subtype = (attachment.media_type ?? '')
    .split(';')[0]
    .split('/')
    .pop()
    ?.split('+')
    .pop();
  return (subtype || 'FILE').toUpperCase();
}

/** One produced artifact, flattened out of the turn that made it. */
export interface SessionArtifact {
  /** Stable across re-renders: the iteration owns the index. */
  key: string;
  kind: ArtifactKind;
  name: string;
  media: string;
  mediaType: string;
  size?: number;
  sizeLabel: string;
  /** Which turn produced it, counted from the start of the session. */
  turn: number;
  /** The tool call inside that turn. Provenance is data. */
  tool: string;
  iterationId: string;
  index: number;
}

/**
 * Every artifact the held transcript knows about, NEWEST FIRST — the order the
 * question "what did it just make?" is asked in.
 *
 * `earlier` is `client.transcriptWindow(sid).offset`: the turns that exist on
 * the gateway before the window we hold. A turn has no ordinal of its own, so
 * the count only means "turn 6" when the pages before it are added back.
 */
export function collectArtifacts(
  turns: TranscriptTurn[],
  earlier = 0,
): SessionArtifact[] {
  const list: SessionArtifact[] = [];
  turns.forEach((turn, position) => {
    const ordinal = earlier + position + 1;
    for (const iteration of turn.iterations ?? []) {
      const iterationId = iteration.id ?? '';
      // `TranscriptIteration` carries the tool name only through its index
      // signature, so it arrives as `unknown` and is asked, never asserted.
      const tool =
        typeof iteration.tool_name === 'string' ? iteration.tool_name : '';
      for (const attachment of iteration.attachments ?? []) {
        const index = attachment.index ?? 0;
        list.push({
          key: `${iterationId}:${index}`,
          kind: artifactKind(attachment),
          name: attachment.filename || 'attachment',
          media: artifactMedia(attachment),
          mediaType: attachment.media_type ?? '',
          size: attachment.size,
          sizeLabel: attachmentBytes(attachment.size),
          turn: ordinal,
          tool,
          iterationId,
          index,
        });
      }
    }
  });
  return list.reverse();
}

/**
 * The kind filter. `All` lists every kind rather than skipping the check, so a
 * kind that is added to `ArtifactKind` and forgotten here disappears from the
 * sheet loudly instead of only from one chip.
 */
export const ARTIFACT_FILTERS: { label: string; kinds: ArtifactKind[] }[] = [
  { label: 'All', kinds: ['image', 'video', 'doc', 'file'] },
  { label: 'Pictures', kinds: ['image', 'video'] },
  { label: 'Documents', kinds: ['doc'] },
  { label: 'Files', kinds: ['file'] },
];

/** How much of it there is, added up. Empty when nothing declared a size. */
export function artifactTotalLabel(list: SessionArtifact[]): string {
  const known = list.filter((entry) => typeof entry.size === 'number');
  if (!known.length) return '';
  return attachmentBytes(
    known.reduce((sum, entry) => sum + (entry.size ?? 0), 0),
  );
}

/**
 * HOW MUCH OF A GALLERY IS FETCHED AT ONCE — paging by NUMBER and by SIZE.
 *
 * Every tile that reaches the screen asks the gateway for its bytes, so an
 * iteration that produced forty figures, or a session sheet with two hundred
 * artifacts in it, is forty or two hundred downloads started in one tick — on
 * whatever connection the phone happens to have. Counting tiles alone is not
 * enough either: six 4K screenshots are not six thumbnails.
 *
 * So a page ends at whichever bound comes first, and "show more" buys exactly
 * one more page of both. The first item is ALWAYS shown, however big it is: a
 * budget that can hide the only artifact there is would be a broken screen
 * rather than a thrifty one.
 */
export interface AttachmentPageLimits {
  /** How many artifacts one page may paint. */
  items: number;
  /** How many of their bytes one page may pull. */
  bytes: number;
}

/** In the transcript, under the tool call that made them: full-width pictures. */
export const RAIL_PAGE: AttachmentPageLimits = {
  items: 6,
  bytes: 8 * 1024 * 1024,
};

/** In the artifacts sheet: a grid of thumbnails, so more of them fit a page. */
export const SHEET_PAGE: AttachmentPageLimits = {
  items: 12,
  bytes: 16 * 1024 * 1024,
};

export interface AttachmentPage<T> {
  /** What is painted — and therefore what is downloaded. */
  shown: T[];
  /** What is not, yet. */
  rest: T[];
  restBytes: number;
  /** `18 more · 24.1MB` — what the control that reveals them says, or ''. */
  restLabel: string;
}

/** One page of `items`, per [[AttachmentPageLimits]]; `pages` counts reveals. */
export function pageBySize<T>(
  items: T[],
  sizeOf: (item: T) => number | undefined,
  pages: number,
  limits: AttachmentPageLimits,
): AttachmentPage<T> {
  const reveals = Math.max(1, Math.floor(pages) || 1);
  const maxItems = Math.max(1, limits.items * reveals);
  const maxBytes = limits.bytes * reveals;
  const size = (item: T) => Math.max(0, sizeOf(item) ?? 0);
  let bytes = 0;
  let at = 0;
  while (at < items.length && at < maxItems) {
    const next = size(items[at]);
    if (at > 0 && bytes + next > maxBytes) break;
    bytes += next;
    at += 1;
  }
  const rest = items.slice(at);
  const restBytes = rest.reduce((sum, item) => sum + size(item), 0);
  const weight = restBytes > 0 ? attachmentBytes(restBytes) : '';
  return {
    shown: items.slice(0, at),
    rest,
    restBytes,
    restLabel: rest.length
      ? `${rest.length} more${weight ? ` · ${weight}` : ''}`
      : '',
  };
}
