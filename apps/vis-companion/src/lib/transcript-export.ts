import { inflateSync, strFromU8 } from 'fflate';
import type { Session, SessionUsage, Subagent, TranscriptAttachment, TranscriptIteration, TranscriptTurn } from './types';

/**
 * WHAT AN HTML EXPORT CARRIES FOR ONE SESSION.
 *
 * The gateway's own transcript rows go in unchanged, so the export draws them with the same
 * components as the session screen. `child_session_ids` names this session's subagents, whose
 * wakes the screen hides too. `agents` holds their team rows, which each turn that started them
 * shows. `attachments` holds the bytes of each artifact that the steps
 * name, keyed by `<iteration id>/<index>`, so the page shows them without a gateway. `usage` is
 * the session's usage rollup, as `GET /v1/sessions/:sid/usage` returns it, or `null` before the
 * first measured call. `failed_tools` holds the rows of the Failed tools list, because the steps
 * of a turn can stay in their chunk until the reader comes near the turn.
 */
export interface SessionExport {
  session: Pick<Session, 'id' | 'title' | 'agent_name' | 'workspace' | 'model' | 'model_pref'>;
  usage: SessionUsage | null;
  turns: ExportTurn[];
  child_session_ids: string[];
  agents: Subagent[];
  attachments: Record<string, ExportAttachment>;
  failed_tools?: WireFailedToolCall[];
}

/**
 * One turn of an export. The engine moves the steps of a turn to a chunk element of the page
 * and names it in `iterations_chunk`: the steps are most of an export, and the page reads them
 * only when the reader comes near the turn. A turn with `iterations` holds them inline. The bytes
 * of a request image are in the chunk element that its `chunk` names, else in its `base64`.
 */
export type ExportTurn = Omit<TranscriptTurn, 'attachments'> & {
  iterations_chunk?: string;
  attachments?: (TranscriptAttachment & { chunk?: string })[];
};

/** One row of `failed_tools`, as the engine writes it. */
export interface WireFailedToolCall {
  turn_id: string;
  scope: string;
  code: string;
  language: string;
  message: string;
}

/**
 * WHAT AN HTML EXPORT CARRIES.
 *
 * The engine writes one JSON document into the export page (`transcript-export.html`): the
 * document of the exported session, and under `descendants` the document of each subagent
 * session below it, at any depth, keyed by session id. A row of a subagent opens its document.
 * An export from before `descendants` holds only the exported session.
 */
export interface TranscriptExport extends SessionExport {
  descendants: Record<string, SessionExport>;
}

/** The stored bytes of one artifact, in base64: inline, or in the chunk element `chunk`. */
export interface ExportAttachment {
  media_type: string;
  base64?: string;
  chunk?: string;
}

/** The element of the export page that holds the document. */
export const TRANSCRIPT_EXPORT_DATA_ID = 'vis-transcript-data';

/**
 * Parse the document of an export page. Throws an error that names the defect, because a page
 * without a transcript has nothing else to show.
 */
export function parseTranscriptExport(text: string | null | undefined): TranscriptExport {
  const value: unknown = JSON.parse(text?.trim() || 'null');
  const root = sessionExport(value);
  const descendants = (value as Partial<TranscriptExport>).descendants;
  return {
    ...root,
    descendants:
      descendants && typeof descendants === 'object'
        ? Object.fromEntries(Object.entries(descendants).map(([sid, doc]) => [sid, sessionExport(doc)]))
        : {},
  };
}

/** The document of one session from `value`, with each absent list or map empty. */
function sessionExport(value: unknown): SessionExport {
  if (!value || typeof value !== 'object') throw new Error('This page holds no transcript.');
  const doc = value as Partial<SessionExport>;
  if (!doc.session || typeof doc.session.id !== 'string') throw new Error('The transcript names no session.');
  if (!Array.isArray(doc.turns)) throw new Error('The transcript has no turns.');
  return {
    session: doc.session,
    usage: doc.usage && typeof doc.usage === 'object' ? doc.usage : null,
    turns: doc.turns,
    child_session_ids: Array.isArray(doc.child_session_ids) ? doc.child_session_ids : [],
    agents: Array.isArray(doc.agents) ? doc.agents : [],
    attachments: doc.attachments && typeof doc.attachments === 'object' ? doc.attachments : {},
    ...(Array.isArray(doc.failed_tools) ? { failed_tools: doc.failed_tools } : {}),
  };
}

/** The text of the chunk element `id` of the export page. Throws when the page does not hold it. */
function chunkText(id: string): string {
  const text = document.getElementById(id)?.textContent;
  if (text == null) throw new Error(`The export page holds no chunk ${id}.`);
  return text;
}

const fromBase64 = (Uint8Array as { fromBase64?: (text: string) => Uint8Array<ArrayBuffer> }).fromBase64;

/** The bytes of base64 `text`. */
function base64Bytes(text: string): Uint8Array<ArrayBuffer> {
  if (fromBase64) return fromBase64(text);
  const binary = atob(text);
  const bytes = new Uint8Array(binary.length);
  for (let at = 0; at < binary.length; at++) bytes[at] = binary.charCodeAt(at);
  return bytes;
}

const heldSteps = new Map<string, TranscriptIteration[]>();

/**
 * The steps of `turn` when they are inline or already read, else `undefined`. A turn that the
 * reader saw draws whole at once when its transcript opens again, so a kept offset still holds.
 */
export function heldTurnSteps(turn: ExportTurn): TranscriptIteration[] | undefined {
  return turn.iterations_chunk ? heldSteps.get(turn.iterations_chunk) : (turn.iterations ?? []);
}

/**
 * The steps of `turn`: inline, else read from its chunk element, which holds them as raw
 * DEFLATE in base64. The read is synchronous, so a turn that comes into view draws before the
 * next frame shows it. A chunk is read one time; each later call gets the same answer.
 */
export function turnSteps(turn: ExportTurn): TranscriptIteration[] {
  const id = turn.iterations_chunk;
  if (!id) return turn.iterations ?? [];
  let steps = heldSteps.get(id);
  if (!steps) {
    steps = JSON.parse(strFromU8(inflateSync(base64Bytes(chunkText(id))))) as TranscriptIteration[];
    heldSteps.set(id, steps);
  }
  return steps;
}

const readingSteps = new Map<string, Promise<void>>();

/**
 * Reads the steps of `turn` ahead of its draw: the browser's stream inflates them, and the parse
 * runs in a task of its own, so neither adds to the frame that draws the turn. Each later
 * `turnSteps` call answers at once. A defect shows when `turnSteps` reads the chunk again.
 */
export function readTurnSteps(turn: ExportTurn): Promise<void> {
  const id = turn.iterations_chunk;
  if (!id || heldSteps.has(id) || typeof DecompressionStream === 'undefined') return Promise.resolve();
  let reading = readingSteps.get(id);
  if (!reading) {
    reading = (async () => {
      const inflated = new Response(base64Bytes(chunkText(id))).body!.pipeThrough(new DecompressionStream('deflate-raw'));
      const text = await new Response(inflated).text();
      if (!heldSteps.has(id)) heldSteps.set(id, JSON.parse(text) as TranscriptIteration[]);
    })().catch(() => {});
    readingSteps.set(id, reading);
  }
  return reading;
}

/** The request images of `turn`, each with its bytes in `base64`, as the session screen draws them. */
export function turnAttachments(turn: ExportTurn): TranscriptAttachment[] | undefined {
  return turn.attachments?.map(({ chunk, ...attachment }) =>
    chunk && !attachment.base64 ? { ...attachment, base64: chunkText(chunk) } : attachment,
  );
}

/** One tool call of the export that failed, as the Failed tools list shows it. */
export interface FailedToolCall {
  turnId: string;
  /** The step that ran the call, for example `t1/i65`, else the turn, for example `t2`. */
  scope: string;
  /** The code of the call, as the session screen shows it under CODE. */
  code: string;
  /** The highlighting language of `code`. */
  language: string;
  /** The first line of the error that the call returned. */
  message: string;
}

/**
 * The failed tool calls of `turns`, in order, each with its code: the rows that the engine `listed`, else the rows of
 * the inline steps. A call fails when it names a tool and returns an error, which is the rule of
 * `tool_failure_count`, so the list and the count agree.
 */
export function failedToolCalls(turns: TranscriptTurn[], listed?: readonly WireFailedToolCall[]): FailedToolCall[] {
  if (listed) {
    return listed.map((row) => ({
      turnId: row.turn_id,
      scope: row.scope,
      code: row.code,
      language: row.language,
      message: row.message,
    }));
  }
  return turns.flatMap((turn, index) =>
    (turn.iterations ?? []).flatMap((iteration) =>
      (iteration.forms ?? []).flatMap((form) => {
        const tool = typeof form.tool_name === 'string' ? form.tool_name.trim() : '';
        if (!tool || form.error == null) return [];
        const error = form.error;
        const text =
          typeof error === 'string'
            ? error
            : typeof error === 'object' && !Array.isArray(error) && typeof error.message === 'string'
              ? error.message
              : JSON.stringify(error);
        const source = form.display_code ?? form.code ?? form.source ?? form.src;
        return [
          {
            turnId: turn.turn_id,
            scope: form.scope || `t${index + 1}`,
            code: typeof source === 'string' ? source.trim() : '',
            language: form.display_language?.trim() || 'python',
            message: text.split('\n').find((line) => line.trim() !== '')?.trim() ?? '',
          },
        ];
      }),
    ),
  );
}

/**
 * The gateway address of an export page. No gateway answers it: `snapshotFetch` answers each
 * request to it from the export, so the web app's own client reads the stored artifacts.
 */
export const SNAPSHOT_GATEWAY = 'https://transcript-export.invalid';

const ATTACHMENT_PATH = /^\/v1\/sessions\/[^/]+\/iterations\/([^/]+)\/attachments\/(\d+)$/;

/**
 * The answer of the export to one request for `url`: the stored bytes of an artifact, else a
 * 404, as a gateway answers an unknown artifact. A request to another address gets `null`. An
 * iteration id names one step of one session, so the session that holds the artifact answers.
 */
export function snapshotResponse(data: TranscriptExport, url: string): Response | null {
  if (!url.startsWith(`${SNAPSHOT_GATEWAY}/`)) return null;
  const match = ATTACHMENT_PATH.exec(new URL(url).pathname);
  const key = match ? `${decodeURIComponent(match[1])}/${match[2]}` : undefined;
  const stored = key
    ? [data, ...Object.values(data.descendants)].map((doc) => doc.attachments[key]).find(Boolean)
    : undefined;
  if (!stored) {
    return new Response(JSON.stringify({ error: 'not_in_export' }), {
      status: 404,
      headers: { 'Content-Type': 'application/json' },
    });
  }
  const bytes = base64Bytes(stored.base64 ?? chunkText(stored.chunk ?? ''));
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
