/**
 * ACTIVITY — WHAT ONE FORM DID, as data.
 *
 * A form's execution leaves two different traces, and this module owns the
 * second one. `result` and `stdout` are what the block RETURNED and PRINTED,
 * read by the model; Activity is the bounded, human-facing chronology of the
 * tool calls that produced them, and never enters the model's context.
 *
 * It is NOT a Live View. Protocol 7 stopped shipping it as a classified view
 * addressed from a distance by an anchor: it belongs to the form that produced
 * it, arrives whole on the transient `block.activity` frame and settles on the
 * terminal block event, so it is parsed here and painted by `ActivityPanel` —
 * neither of which the Live View rail knows about. The two surfaces share a
 * transport, nothing else.
 *
 * The snapshot is ALREADY bounded when it arrives (128 rows, 64 KiB); this
 * parser re-checks both, because a payload that broke the engine's own bound
 * is a contract violation, not a bigger picture to render.
 */

function record(value: unknown): Record<string, unknown> | null {
  return value !== null && typeof value === 'object' && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : null;
}

function text(value: unknown): string {
  return typeof value === 'string' ? value : '';
}

function optionalText(value: unknown): string | undefined {
  const trimmed = text(value).trim();
  return trimmed === '' ? undefined : trimmed;
}

function optionalNumber(value: unknown): number | undefined {
  return typeof value === 'number' && Number.isFinite(value) ? value : undefined;
}

export const ACTIVITY_PRESENTERS = [
  'generic',
  'shell',
  'tests',
  'patch',
  'observation',
  'lint',
  'repl',
  'format',
  'list',
] as const;
export const ACTIVITY_SIGNALS = ['generic', 'observation', 'mutation', 'verification'] as const;
export const ACTIVITY_STATES = ['idle', 'running', 'succeeded', 'failed', 'cancelled'] as const;

export type ActivityPresenter = (typeof ACTIVITY_PRESENTERS)[number];
export type ActivitySignal = (typeof ACTIVITY_SIGNALS)[number];
export type ActivityState = (typeof ACTIVITY_STATES)[number];

export interface ActivityResource {
  type: string;
  id: string;
}

export interface ActivityTextEvidence {
  kind: 'arguments' | 'result' | 'error';
  text: string;
}

export interface ActivityDiffLine {
  kind: 'header' | 'hunk' | 'context' | 'addition' | 'deletion';
  text: string;
  is_redacted?: true;
}

export interface ActivityDiffEvidence {
  kind: 'diff';
  text: string;
  lines: ActivityDiffLine[];
  additions: number;
  deletions: number;
  modifications: number;
  omitted_lines: number;
  is_truncated: boolean;
  is_redacted: boolean;
}

export type ActivityEvidence = ActivityTextEvidence | ActivityDiffEvidence;

export interface ActivityRow {
  id: string;
  sequence: number;
  operation: string;
  presenter: ActivityPresenter;
  signal: ActivitySignal;
  state: ActivityState;
  summary: string;
  group_token?: string;
  duration_ms?: number;
  result_summary?: string;
  error_summary?: string;
  resources: ActivityResource[];
  evidence: ActivityEvidence[];
  children?: ActivityRow[];
  is_truncated?: boolean;
}

/**
 * One form's bounded execution picture. Protocol 7 carries no `schema_version`
 * and no `anchor`: the wire protocol number already gates the shape — that is
 * what the compatibility handshake is for — and a snapshot that lives ON its
 * form has nothing left to point at.
 */
export interface ActivityProjection {
  state: ActivityState;
  counts: Record<'running' | 'succeeded' | 'failed' | 'cancelled', number>;
  rows: ActivityRow[];
  omitted: {
    rows: number;
    by_classification: Record<string, number>;
  };
}

function activityEnum<T extends string>(value: unknown, values: readonly T[]): T | null {
  const candidate = text(value);
  return values.includes(candidate as T) ? (candidate as T) : null;
}

function activityCount(value: unknown): number | null {
  return typeof value === 'number' && Number.isInteger(value) && value >= 0 ? value : null;
}

function activityResourceFromWire(value: unknown): ActivityResource | null {
  const raw = record(value);
  if (!raw) return null;
  const type = text(raw.type).trim();
  const id = text(raw.id).trim();
  return type && id ? { type, id } : null;
}

function activityEvidenceFromWire(value: unknown): ActivityEvidence | null {
  const raw = record(value);
  if (!raw) return null;
  const kind = activityEnum(raw.kind, ['arguments', 'result', 'error', 'diff'] as const);
  if (!kind || typeof raw.text !== 'string') return null;
  const evidenceText = raw.text;
  if (kind !== 'diff') return { kind, text: evidenceText };

  const rawLines = Array.isArray(raw.lines) ? raw.lines : null;
  const additions = activityCount(raw.additions);
  const deletions = activityCount(raw.deletions);
  const modifications = activityCount(raw.modifications);
  const omittedLines = activityCount(raw.omitted_lines);
  if (
    !rawLines ||
    additions === null ||
    deletions === null ||
    modifications === null ||
    omittedLines === null ||
    typeof raw.is_truncated !== 'boolean' ||
    typeof raw.is_redacted !== 'boolean'
  )
    return null;
  const parsedLines = rawLines.map((line): ActivityDiffLine | null => {
    const entry = record(line);
    if (!entry) return null;
    const lineKind = activityEnum(
      entry.kind,
      ['header', 'hunk', 'context', 'addition', 'deletion'] as const,
    );
    if (
      !lineKind ||
      typeof entry.text !== 'string' ||
      (entry.is_redacted !== undefined && entry.is_redacted !== true)
    )
      return null;
    return {
      kind: lineKind,
      text: entry.text,
      ...(entry.is_redacted === true ? { is_redacted: true as const } : {}),
    };
  });
  if (parsedLines.some((line) => line === null)) return null;
  return {
    kind,
    text: evidenceText,
    lines: parsedLines as ActivityDiffLine[],
    additions,
    deletions,
    modifications,
    omitted_lines: omittedLines,
    is_truncated: raw.is_truncated,
    is_redacted: raw.is_redacted,
  };
}

function activityRowFromWire(value: unknown, depth = 0): ActivityRow | null {
  if (depth > 2) return null;
  const raw = record(value);
  if (!raw) return null;
  const id = text(raw.id).trim();
  const sequence = activityCount(raw.sequence);
  const operation = text(raw.operation).trim();
  const presenter = activityEnum(raw.presenter, ACTIVITY_PRESENTERS);
  const signal = activityEnum(raw.signal, ACTIVITY_SIGNALS);
  const state = activityEnum(raw.state, ACTIVITY_STATES);
  const resourcesRaw = Array.isArray(raw.resources) ? raw.resources : null;
  const evidenceRaw = Array.isArray(raw.evidence) ? raw.evidence : null;
  const resources = resourcesRaw
    ? resourcesRaw.map(activityResourceFromWire).filter((item): item is ActivityResource => item !== null)
    : null;
  const evidence = evidenceRaw
    ? evidenceRaw.map(activityEvidenceFromWire).filter((item): item is ActivityEvidence => item !== null)
    : null;
  if (
    !id ||
    sequence === null ||
    !operation ||
    !presenter ||
    !signal ||
    !state ||
    resources === null ||
    resources.length !== resourcesRaw!.length ||
    resources.length > 8 ||
    evidence === null ||
    evidence.length !== evidenceRaw!.length
  ) {
    return null;
  }
  const children = Array.isArray(raw.children)
    ? raw.children.map((child) => activityRowFromWire(child, depth + 1))
    : undefined;
  if (children?.some((child) => child === null)) return null;
  return {
    id,
    sequence,
    operation,
    presenter,
    signal,
    state,
    summary: text(raw.summary),
    resources,
    evidence,
    ...(optionalText(raw.group_token) ? { group_token: optionalText(raw.group_token) } : {}),
    ...(optionalNumber(raw.duration_ms) !== undefined
      ? { duration_ms: Math.max(0, Math.trunc(optionalNumber(raw.duration_ms)!)) }
      : {}),
    ...(optionalText(raw.result_summary) ? { result_summary: optionalText(raw.result_summary) } : {}),
    ...(optionalText(raw.error_summary) ? { error_summary: optionalText(raw.error_summary) } : {}),
    ...(children ? { children: children as ActivityRow[] } : {}),
    ...(raw.is_truncated === true ? { is_truncated: true } : {}),
  };
}

export function activityProjectionFromWire(value: unknown): ActivityProjection | null {
  const raw = record(value);
  if (!raw) return null;
  // The retired vocabulary is refused rather than ignored: a payload still
  // wearing it came from a gateway the compatibility gate should already have
  // turned away.
  if (raw.schema_version !== undefined || raw.anchor !== undefined) return null;
  const state = activityEnum(raw.state, ACTIVITY_STATES);
  const countsRaw = record(raw.counts);
  const omittedRaw = record(raw.omitted);
  const omittedBy = record(omittedRaw?.by_classification);
  if (!state || !countsRaw || !omittedRaw || !omittedBy || !Array.isArray(raw.rows)) return null;
  const counts = {
    running: activityCount(countsRaw.running),
    succeeded: activityCount(countsRaw.succeeded),
    failed: activityCount(countsRaw.failed),
    cancelled: activityCount(countsRaw.cancelled),
  };
  const omittedRows = activityCount(omittedRaw.rows);
  const byClassification = Object.fromEntries(
    Object.entries(omittedBy).filter(([, amount]) => activityCount(amount) !== null),
  ) as Record<string, number>;
  const parsedRows = raw.rows.map((row) => activityRowFromWire(row));
  if (
    Object.values(counts).some((amount) => amount === null) ||
    omittedRows === null ||
    Object.keys(byClassification).length !== Object.keys(omittedBy).length ||
    parsedRows.some((row) => row === null) ||
    parsedRows.length > 128 ||
    new TextEncoder().encode(JSON.stringify(raw)).length > 64 * 1024
  ) {
    return null;
  }
  return {
    state,
    counts: counts as ActivityProjection['counts'],
    rows: parsedRows as ActivityRow[],
    omitted: { rows: omittedRows, by_classification: byClassification },
  };
}
