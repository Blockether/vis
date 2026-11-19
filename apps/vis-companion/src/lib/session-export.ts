import { keepFile, type KeptFile } from './artifact-open';
import { fileHost } from './artifact-save';
import { canShareArtifactFiles, shareArtifact } from './artifact-share';
import { GatewayError, type GatewayClient } from './gateway';

/**
 * EXPORT ONE SESSION AS A FILE.
 *
 * The gateway makes the file. This host keeps it: the desktop app saves it and shows it in the
 * file manager, a phone opens the share sheet, a browser downloads it. A desktop app or a
 * browser that can share a file lets the person choose between that and the share sheet. The
 * TUI offers the same formats and gives the file the same name.
 */
export type SessionExportFormat = 'html' | 'md' | 'vissnap' | 'atif';

export const SESSION_EXPORT_FORMATS: readonly {
  id: SessionExportFormat;
  extension: string;
  title: string;
  hint: string;
  mediaType: string;
  /** What a message calls the file. */
  noun: string;
}[] = [
  { id: 'html', extension: 'html', title: 'HTML', hint: 'One page that opens in a browser', mediaType: 'text/html', noun: 'HTML page' },
  { id: 'md', extension: 'md', title: 'Markdown', hint: 'The text of each turn', mediaType: 'text/markdown', noun: 'Markdown file' },
  {
    id: 'vissnap',
    extension: 'vissnap',
    title: 'Snapshot (.vissnap)',
    hint: 'A file that Vis imports as a new session',
    mediaType: 'application/octet-stream',
    noun: 'Snapshot',
  },
  {
    id: 'atif',
    extension: 'json',
    title: 'ATIF (.json)',
    hint: 'A trajectory that Vis and Harbor read',
    mediaType: 'application/json',
    noun: 'ATIF file',
  },
];

/** The session that an export reads. */
export type ExportedSession = { id: string; title: string };

/** `<slug of the title>-<first 8 characters of the id>.<extension>`. */
export function sessionExportFilename(session: ExportedSession, format: SessionExportFormat): string {
  const slug = session.title
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '')
    .slice(0, 48)
    .replace(/-+$/, '');
  const { extension } = SESSION_EXPORT_FORMATS.find((row) => row.id === format)!;
  return `${slug || 'vis-session'}-${session.id.slice(0, 8)}.${extension}`;
}

/** The bytes of `session` in `format`, from the gateway. */
export function readExport(client: GatewayClient, session: ExportedSession, format: SessionExportFormat): Promise<Blob> {
  if (format === 'html') return client.transcriptHtml(session.id);
  if (format === 'vissnap') return client.sessionSnapshot(session.id);
  if (format === 'atif') return client.sessionTrajectory(session.id);
  return client.transcriptMarkdown(session.id);
}

/** One way to keep an export on this host. */
export type ExportWay = 'Save' | 'Download' | 'Share';

/**
 * The ways this host can keep an export; the first one is the usual way. A phone has only
 * its share sheet, which holds Save to Files.
 */
export function exportWays(session: ExportedSession, format: SessionExportFormat): ExportWay[] {
  const host = fileHost();
  if (host === 'phone') return ['Share'];
  const keep: ExportWay = host === 'desktop' ? 'Save' : 'Download';
  const { mediaType } = SESSION_EXPORT_FORMATS.find((row) => row.id === format)!;
  return canShareArtifactFiles(sessionExportFilename(session, format), mediaType) ? [keep, 'Share'] : [keep];
}

/**
 * Keep the exported bytes of `session` in the chosen way. The answer says what happened, and
 * its text is '' when the person closed the share sheet.
 */
export async function keepExport(
  blob: Blob,
  session: ExportedSession,
  format: SessionExportFormat,
  way: ExportWay,
): Promise<KeptFile> {
  const { mediaType, noun, title } = SESSION_EXPORT_FORMATS.find((row) => row.id === format)!;
  const name = sessionExportFilename(session, format);
  if (way === 'Share' && fileHost() !== 'phone')
    return { text: await shareArtifact(blob, name, mediaType, { dialogTitle: `Share the ${title} export`, noun }) };
  return keepFile(blob, name, mediaType, { noun, dialogTitle: `Save the ${title} export` });
}

/** The words of a failed export, for a toast. */
export function exportFailure(cause: unknown, format: SessionExportFormat): string {
  // The gateway makes the HTML page from its web app. A gateway without one cannot make it.
  if (format === 'html' && cause instanceof GatewayError && cause.status === 503)
    return 'This gateway cannot export HTML, because it has no web app. Export Markdown or a snapshot.';
  const reason = cause instanceof Error && cause.message ? cause.message : String(cause);
  return `The export failed: ${reason}`;
}
