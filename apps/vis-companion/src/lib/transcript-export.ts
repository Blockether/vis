import type { Session, Subagent, TranscriptTurn } from './types';

/**
 * WHAT AN HTML EXPORT CARRIES.
 *
 * The engine writes one JSON document into the export page (`transcript-export.html`). The
 * gateway's own transcript rows go in unchanged, so the export draws them with the same
 * components as the session screen. `child_session_ids` names this session's subagents, whose
 * wakes the screen hides too. `agents` holds their team rows, which each turn that started them
 * shows. `attachments` holds the bytes of each artifact that the steps
 * name, keyed by `<iteration id>/<index>`, so the page shows them without a gateway.
 */
export interface TranscriptExport {
  session: Pick<Session, 'id' | 'title' | 'agent_name' | 'workspace'>;
  turns: TranscriptTurn[];
  child_session_ids: string[];
  agents: Subagent[];
  attachments: Record<string, ExportAttachment>;
}

/** The stored bytes of one artifact, in base64. */
export interface ExportAttachment {
  media_type: string;
  base64: string;
}

/** The element of the export page that holds the document. */
export const TRANSCRIPT_EXPORT_DATA_ID = 'vis-transcript-data';

/**
 * Parse the document of an export page. Throws an error that names the defect, because a page
 * without a transcript has nothing else to show.
 */
export function parseTranscriptExport(text: string | null | undefined): TranscriptExport {
  const value: unknown = JSON.parse(text?.trim() || 'null');
  if (!value || typeof value !== 'object') throw new Error('This page holds no transcript.');
  const doc = value as Partial<TranscriptExport>;
  if (!doc.session || typeof doc.session.id !== 'string') throw new Error('The transcript names no session.');
  if (!Array.isArray(doc.turns)) throw new Error('The transcript has no turns.');
  return {
    session: doc.session,
    turns: doc.turns,
    child_session_ids: Array.isArray(doc.child_session_ids) ? doc.child_session_ids : [],
    agents: Array.isArray(doc.agents) ? doc.agents : [],
    attachments: doc.attachments && typeof doc.attachments === 'object' ? doc.attachments : {},
  };
}

/**
 * The gateway address of an export page. No gateway answers it: `snapshotFetch` answers each
 * request to it from the export, so the web app's own client reads the stored artifacts.
 */
export const SNAPSHOT_GATEWAY = 'https://transcript-export.invalid';

const ATTACHMENT_PATH = /^\/v1\/sessions\/[^/]+\/iterations\/([^/]+)\/attachments\/(\d+)$/;

/**
 * The answer of the export to one request for `url`: the stored bytes of an artifact, else a
 * 404, as a gateway answers an unknown artifact. A request to another address gets `null`.
 */
export function snapshotResponse(data: TranscriptExport, url: string): Response | null {
  if (!url.startsWith(`${SNAPSHOT_GATEWAY}/`)) return null;
  const match = ATTACHMENT_PATH.exec(new URL(url).pathname);
  const stored = match ? data.attachments[`${decodeURIComponent(match[1])}/${match[2]}`] : undefined;
  if (!stored) {
    return new Response(JSON.stringify({ error: 'not_in_export' }), {
      status: 404,
      headers: { 'Content-Type': 'application/json' },
    });
  }
  const bytes = Uint8Array.from(atob(stored.base64), (char) => char.charCodeAt(0));
  return new Response(bytes, { status: 200, headers: { 'Content-Type': stored.media_type } });
}

/** `fetch` that answers requests to `SNAPSHOT_GATEWAY` from the export and passes on the rest. */
export function snapshotFetch(data: TranscriptExport, next: typeof fetch): typeof fetch {
  return (input, init) => {
    const url = input instanceof Request ? input.url : String(input);
    const answer = snapshotResponse(data, url);
    return answer ? Promise.resolve(answer) : next(input, init);
  };
}
