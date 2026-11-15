import { keepFile, type KeptFile } from './artifact-open';
import { GatewayError, type GatewayClient } from './gateway';

/**
 * EXPORT ONE SESSION AS A FILE.
 *
 * The gateway makes the file. This host keeps it: the desktop app saves it and shows it in the
 * file manager, a phone opens the share sheet, a browser downloads it. The TUI offers the same
 * three formats and gives the file the same name.
 */
export type SessionExportFormat = 'html' | 'md' | 'vissnap';

export const SESSION_EXPORT_FORMATS: readonly {
  /** The format, which is also the file extension. */
  id: SessionExportFormat;
  title: string;
  hint: string;
  mediaType: string;
  /** What a message calls the file. */
  noun: string;
}[] = [
  { id: 'html', title: 'HTML', hint: 'One page that opens in a browser', mediaType: 'text/html', noun: 'HTML page' },
  { id: 'md', title: 'Markdown', hint: 'The text of each turn', mediaType: 'text/markdown', noun: 'Markdown file' },
  {
    id: 'vissnap',
    title: 'Snapshot (.vissnap)',
    hint: 'A file that Vis imports as a new session',
    mediaType: 'application/octet-stream',
    noun: 'Snapshot',
  },
];

/** The session that an export reads. */
export type ExportedSession = { id: string; title: string };

/** `<slug of the title>-<first 8 characters of the id>.<format>`. */
export function sessionExportFilename(session: ExportedSession, format: SessionExportFormat): string {
  const slug = session.title
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '')
    .slice(0, 48)
    .replace(/-+$/, '');
  return `${slug || 'vis-session'}-${session.id.slice(0, 8)}.${format}`;
}

async function exportBytes(client: GatewayClient, sid: string, format: SessionExportFormat): Promise<Blob> {
  if (format === 'html') return client.transcriptHtml(sid);
  if (format === 'vissnap') return client.sessionSnapshot(sid);
  return client.transcriptMarkdown(sid);
}

/**
 * Export `session` and keep the file on this host. The answer says what happened, and its
 * text is '' when the person closed the share sheet.
 */
export async function exportSession(
  client: GatewayClient,
  session: ExportedSession,
  format: SessionExportFormat,
): Promise<KeptFile> {
  const { mediaType, noun, title } = SESSION_EXPORT_FORMATS.find((row) => row.id === format)!;
  const blob = await exportBytes(client, session.id, format);
  return keepFile(blob, sessionExportFilename(session, format), mediaType, {
    noun,
    dialogTitle: `Save the ${title} export`,
  });
}

/** The words of a failed export, for a toast. */
export function exportFailure(cause: unknown, format: SessionExportFormat): string {
  // The gateway makes the HTML page from its web app. A gateway without one cannot make it.
  if (format === 'html' && cause instanceof GatewayError && cause.status === 503)
    return 'This gateway cannot export HTML, because it has no web app. Export Markdown or a snapshot.';
  const reason = cause instanceof Error && cause.message ? cause.message : String(cause);
  return `The export failed: ${reason}`;
}
