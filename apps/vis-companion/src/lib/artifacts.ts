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
