/**
 * ACTIVITY — WHAT ONE FORM DID, as data.
 *
 * A form's execution leaves two different traces, and this module owns the
 * second one. `result` and `stdout` are what the block RETURNED and PRINTED,
 * read by the model; Activity is the bounded, human-facing chronology of the
 * tool calls that produced them, and never enters the model's context.
 *
 * It is NOT a Live View. Protocol 8 stopped shipping it as a classified view
 * addressed from a distance by an anchor. Protocol 9 gives both running and settled
 * revisions the one `block.activity` event type; the settled revision is durable. It
 * is parsed here and painted by `ActivityPanel` —
 * neither of which the Live View rail knows about. The two surfaces share a
 * transport, nothing else.
 *
 * History-backed snapshots are bounded windows, not complete histories. Their
 * durable identity and cursor let the clients retrieve every retained operation.
 */
import activitySchema from '../../../../packages/vis-contract/resources/vis-contract/schema/activity.json';

const PAGE_ROW_LIMIT = activitySchema.$defs.projection.properties.history['x-vis-max-page-rows'];
const PAGE_BYTE_TARGET = activitySchema.$defs.projection.properties.history['x-vis-page-target-bytes'];
const RESOURCE_LIMIT = activitySchema.$defs.row.properties.resources.maxItems;
const SUMMARY_BYTE_LIMIT = activitySchema.$defs.section.properties.summary['x-vis-max-bytes'];
const HANDLE_LIMIT = activitySchema.$defs.handle_id;

type RowSchema = {
  properties: Record<string, { items?: { $ref: string } }>;
  required: string[];
};

function rowSchema(depth: number): RowSchema | null {
  const definitions = activitySchema.$defs as unknown as Record<string, RowSchema>;
  let shape = definitions.row;
  for (let level = 0; level < depth; level++) {
    const ref = shape.properties.children?.items?.$ref;
    if (!ref) return null;
    shape = definitions[ref.slice('#/$defs/'.length)];
  }
  return shape;
}

export type OperationGroup = {
  id: string;
  label: string;
  rows: ActivityRow[];
};

export type ArgumentGroup = Pick<OperationGroup, 'id' | 'rows'>;

function firstInvocationId(row: ActivityRow): string {
  return (row.children?.length && (row.handle_id !== undefined || row.operation === 'shell')
    ? row.children[0]
    : row
  ).id;
}

/** Exact operation/argument pairs, within one block. Unknown keys never collapse. */
export function argumentGroups(rows: readonly ActivityRow[]): ArgumentGroup[] {
  const groups: ArgumentGroup[] = [];
  const byArguments = new Map<string, ArgumentGroup>();
  for (const row of [...rows].sort((a, b) => a.sequence - b.sequence)) {
    const key = row.argument_key ? JSON.stringify([row.operation, row.argument_key]) : undefined;
    const existing = key === undefined ? undefined : byArguments.get(key);
    if (existing) existing.rows.push(row);
    else {
      const group = { id: firstInvocationId(row), rows: [row] };
      groups.push(group);
      if (key !== undefined) byArguments.set(key, group);
    }
  }
  return groups;
}

/**
 * Group exact operations in invocation order, preserving shell evidence and identity.
 * Canonical labels win; otherwise use the first nonblank presentation headline,
 * falling back to the operation name when no member has one.
 */
export function operationGroups(rows: readonly ActivityRow[]): OperationGroup[] {
  const labels: Readonly<Record<string, string>> =
    activitySchema.$defs.row.properties.operation['x-vis-group-labels'];
  const byOperation = new Map<string, ActivityRow[]>();
  for (const row of [...rows].sort((a, b) => a.sequence - b.sequence)) {
    const members = byOperation.get(row.operation);
    if (members) members.push(row);
    else byOperation.set(row.operation, [row]);
  }
  return [...byOperation].map(([operation, members]) => ({
    id: firstInvocationId(members[0]),
    label: Object.hasOwn(labels, operation)
      ? labels[operation]
      : (members.find((row) => optionalText(row.presentation?.headline))?.presentation?.headline ??
        operation),
    rows: members,
  }));
}
function record(value: unknown): Record<string, unknown> | null {
  return value !== null && typeof value === 'object' && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : null;
}

function hasExactKeys(
  value: Record<string, unknown>,
  required: readonly string[],
  optional: readonly string[] = [],
): boolean {
  const allowed = new Set([...required, ...optional]);
  return (
    required.every((key) => Object.prototype.hasOwnProperty.call(value, key)) &&
    Object.keys(value).every((key) => allowed.has(key))
  );
}

function text(value: unknown): string {
  return typeof value === 'string' ? value : '';
}

function optionalText(value: unknown): string | undefined {
  return typeof value === 'string' && value.trim() !== '' ? value : undefined;
}

export const ACTIVITY_PRESENTERS = activitySchema.$defs.presenter.enum as readonly ActivityPresenter[];
export const ACTIVITY_SIGNALS = activitySchema.$defs.signal.enum as readonly ActivitySignal[];
export const ACTIVITY_STATES = activitySchema.$defs.state.enum as readonly ActivityState[];
/**
 * How a row's own words are to be READ. Absent means literal: a path, a glob or a command
 * must never be re-read as markup, so the engine DECLARES the format per field and the
 * renderer never guesses it from the characters.
 */
export const ACTIVITY_TEXT_FORMATS = activitySchema.$defs.text_format
  .enum as readonly ActivityTextFormat[];

export type ActivityPresenter =
  | 'generic'
  | 'shell'
  | 'tests'
  | 'patch'
  | 'observation'
  | 'lint'
  | 'repl'
  | 'format'
  | 'list';
export type ActivityTextFormat = 'inline' | 'markdown';
export type ActivitySignal = 'generic' | 'observation' | 'mutation' | 'verification';
export type ActivityState = 'idle' | 'running' | 'succeeded' | 'failed' | 'cancelled';

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
  is_truncated: boolean;
  is_redacted: boolean;
}

export type ActivityEvidence = ActivityTextEvidence | ActivityDiffEvidence;

export type ActivityContent =
  | { type: 'heading' | 'text' | 'markdown'; text: string }
  | { type: 'code' | 'diff'; text: string; language?: string }
  | { type: 'table'; columns: string[]; rows: string[][]; paths?: string[] }
  | {
      type: 'image' | 'video' | 'audio' | 'file';
      attachment_id: string;
      label: string;
    }
  | { type: 'progress'; label: string; value?: number; total?: number };

/** Closed, lossless content grammar shared with activity.json. Never accept markup as HTML. */
function activityContentFromWire(value: unknown): ActivityContent[] | null {
  if (!Array.isArray(value)) return null;
  const string = (v: unknown, max = 256): v is string =>
    typeof v === 'string' && [...v].length <= max;
  for (const item of value) {
    const b = record(item);
    if (!b) return null;
    switch (b.type) {
      case 'heading':
      case 'text':
      case 'markdown':
        if (!hasExactKeys(b, ['type', 'text']) || typeof b.text !== 'string') return null;
        break;
      case 'code':
      case 'diff':
        if (
          !hasExactKeys(b, ['type', 'text'], ['language']) ||
          typeof b.text !== 'string' ||
          (b.language !== undefined && !string(b.language))
        )
          return null;
        break;
      case 'table':
        if (
          !hasExactKeys(b, ['type', 'columns', 'rows'], ['paths']) ||
          !Array.isArray(b.columns) ||
          !b.columns.length ||
          !b.columns.every((c) => typeof c === 'string') ||
          !Array.isArray(b.rows)
        )
          return null;
        {
          const width = b.columns.length;
          if (
            !b.rows.every(
              (row) =>
                Array.isArray(row) &&
                row.length === width &&
                row.every((c) => typeof c === 'string'),
            )
          )
            return null;
          if (
            b.paths !== undefined &&
            (!Array.isArray(b.paths) ||
              b.paths.length !== b.rows.length ||
              !b.paths.every((p) => typeof p === 'string'))
          )
            return null;
        }
        break;
      case 'image':
      case 'video':
      case 'audio':
      case 'file':
        if (
          !hasExactKeys(b, ['type', 'attachment_id', 'label']) ||
          !string(b.attachment_id) ||
          !b.attachment_id.trim() ||
          !string(b.label)
        )
          return null;
        break;
      case 'progress':
        if (!hasExactKeys(b, ['type', 'label'], ['value', 'total']) || !string(b.label))
          return null;
        if (b.value !== undefined || b.total !== undefined) {
          if (
            typeof b.value !== 'number' ||
            !Number.isFinite(b.value) ||
            typeof b.total !== 'number' ||
            !Number.isFinite(b.total) ||
            b.value < 0 ||
            b.total <= 0 ||
            b.value > b.total
          )
            return null;
        }
        break;
      default:
        return null;
    }
  }
  return value as ActivityContent[];
}

export interface ActivitySection {
  headline: string;
  summary: string;
  summary_format?: ActivityTextFormat;
  content: ActivityContent[];
}

export interface ActivityPresentation extends ActivitySection {
  sections?: ActivitySection[];
  handle_id?: string;
}

function validHandleId(value: unknown): value is string {
  if (typeof value !== 'string' || value.trim() === '') return false;
  return (
    Array.from(value).length <= HANDLE_LIMIT.maxLength &&
    new TextEncoder().encode(value).length <= HANDLE_LIMIT['x-vis-max-bytes'] &&
    !/[\x00-\x1f\x7f\u2028\u2029]/u.test(value)
  );
}

function activityPresentationFromWire(value: unknown): ActivityPresentation | null {
  const raw = record(value);
  if (
    !raw ||
    !hasExactKeys(raw, ['headline', 'summary', 'content'], [
      'sections',
      'summary_format',
      'handle_id',
    ]) ||
    (raw.handle_id !== undefined && !validHandleId(raw.handle_id))
  )
    return null;
  const sections = raw.sections === undefined ? [] : raw.sections;
  if (!Array.isArray(sections)) return null;
  const bytes = (s: string) => new TextEncoder().encode(s).length;
  for (const [index, candidate] of [raw, ...sections].entries()) {
    const section = record(candidate);
    if (
      !section ||
      !hasExactKeys(
        section,
        ['headline', 'summary', 'content'],
        index === 0 ? ['sections', 'summary_format', 'handle_id'] : ['summary_format'],
      )
    )
      return null;
    for (const key of ['headline', 'summary']) {
      const line = section[key];
      if (
        typeof line !== 'string' ||
        bytes(line) > SUMMARY_BYTE_LIMIT ||
        /[\u0000-\u001f\u007f\u2028\u2029]/.test(line)
      )
        return null;
    }
    if (
      section.summary_format !== undefined &&
      !ACTIVITY_TEXT_FORMATS.includes(section.summary_format as ActivityTextFormat)
    )
      return null;
    if (!section.headline || !activityContentFromWire(section.content)) return null;
  }
  return value as ActivityPresentation;
}

export interface ActivityRow {
  id: string;
  sequence: number;
  operation: string;
  presenter: ActivityPresenter;
  signal: ActivitySignal;
  state: ActivityState;
  summary: string;
  summary_format?: ActivityTextFormat;
  argument_key?: string;
  handle_id?: string;
  read_key?: string;
  group_token?: string;
  duration_ms?: number;
  result_summary?: string;
  result_format?: ActivityTextFormat;
  error_summary?: string;
  resources: ActivityResource[];
  evidence: ActivityEvidence[];
  children?: ActivityRow[];
  is_truncated?: boolean;
  presentation?: ActivityPresentation;
}

export interface ActivityHistory {
  id: string;
  revision: number;
  total: number;
  after: number;
  next_after: number | null;
}

/** One form's current window, or a complete inline historical receipt. */
export interface ActivityProjection {
  history?: ActivityHistory;
  state: ActivityState;
  counts: Record<'running' | 'succeeded' | 'failed' | 'cancelled', number>;
  rows: ActivityRow[];
  omitted: {
    rows: number;
    by_classification: Record<string, number>;
  };
}

/** Merge display receipts without replacing their source histories or mutating wire data. */
export function mergeActivity(activities: readonly ActivityProjection[]): ActivityProjection {
  // Scope even the first receipt: appending another form must not change its row IDs.
  const counts = { running: 0, succeeded: 0, failed: 0, cancelled: 0 };
  const omitted: ActivityProjection['omitted'] = { rows: 0, by_classification: {} };
  const rows: ActivityRow[] = [];
  const scopedRow = (row: ActivityRow, scope: number): ActivityRow => ({
    ...row,
    id: `${scope}:${row.id}`,
    ...(row.children ? { children: row.children.map((child) => scopedRow(child, scope)) } : {}),
  });
  activities.forEach((activity, index) => {
    for (const key of Object.keys(counts) as Array<keyof typeof counts>)
      counts[key] += activity.counts[key];
    omitted.rows += activity.omitted.rows;
    for (const [signal, count] of Object.entries(activity.omitted.by_classification)) {
      omitted.by_classification[signal] = (omitted.by_classification[signal] ?? 0) + count;
    }
    for (const row of [...activity.rows].sort((a, b) => a.sequence - b.sequence)) {
      rows.push({ ...scopedRow(row, index), sequence: rows.length });
    }
  });
  const state =
    (['running', 'failed', 'cancelled', 'succeeded'] as const).find((state) =>
      activities.some((activity) => activity.state === state),
    ) ?? 'idle';
  return { state, counts, rows, omitted };
}

/** Copy retained invocations, not the visible grouping or viewport. Never include identity keys. */
export function activityCopyText(activity: ActivityProjection): string {
  const contentText = (block: ActivityContent): string => {
    if ('text' in block) return block.text;
    if (block.type === 'table')
      return [block.columns, ...block.rows].map((row) => row.join('\t')).join('\n');
    if (block.type === 'progress')
      return `${block.label}${block.value === undefined ? '' : `: ${block.value}/${block.total}`}`;
    return `${block.type}: ${block.label} (${block.attachment_id})`;
  };
  const rowText = (row: ActivityRow, depth: number): string => {
    const indent = '  '.repeat(depth);
    const lines = [
      `${indent}${row.operation} [${row.state}]${row.duration_ms === undefined ? '' : ` (${row.duration_ms}ms)`}`,
    ];
    const add = (text?: string) => {
      if (text?.trim()) lines.push(...text.split('\n').map((line) => `${indent}  ${line}`));
    };
    add(row.summary);
    if (row.result_summary) add(`Result: ${row.result_summary}`);
    if (row.error_summary) add(`Error: ${row.error_summary}`);
    for (const resource of row.resources) add(`${resource.type}: ${resource.id}`);
    for (const evidence of row.evidence) {
      const parts = [`${evidence.kind}:\n${evidence.text}`];
      if (evidence.kind === 'diff') {
        for (const line of evidence.lines)
          parts.push(
            `${line.kind === 'addition' ? '+' : line.kind === 'deletion' ? '-' : line.kind === 'context' ? ' ' : ''}${line.text}`,
          );
        if (evidence.is_truncated) parts.push('Diff truncated');
        if (evidence.is_redacted) parts.push('Diff redacted');
      }
      add(parts.join('\n'));
    }
    if (row.presentation) {
      for (const section of [row.presentation, ...(row.presentation.sections ?? [])]) {
        add(section.headline);
        add(section.summary);
        section.content.forEach((block) => add(contentText(block)));
      }
    }
    if (row.is_truncated) add('Details truncated');
    for (const child of [...(row.children ?? [])].sort((a, b) => a.sequence - b.sequence))
      lines.push('', rowText(child, depth + 1));
    return lines.join('\n');
  };
  const blocks = [
    'ACTIVITY',
    ...[...activity.rows].sort((a, b) => a.sequence - b.sequence).map((row) => rowText(row, 0)),
  ];
  if (activity.omitted.rows)
    blocks.push(
      `${activity.omitted.rows} ${activity.omitted.rows === 1 ? 'step' : 'steps'} omitted · Activity limit`,
    );
  return blocks.join('\n\n');
}

/** Copy complete retained histories without changing their bounded display windows. */
export async function activityHistoryCopyText(
  activities: readonly ActivityProjection[],
  load: (
    id: string,
    after: number,
    query: string,
    signal: AbortSignal,
  ) => Promise<ActivityProjection>,
  signal: AbortSignal,
): Promise<string> {
  signal.throwIfAborted();
  const complete: ActivityProjection[] = [];
  for (const activity of activities) {
    const history = activity.history;
    if (!history || (history.after === 0 && history.next_after === null)) {
      complete.push(activity);
      continue;
    }
    const rows: ActivityRow[] = [];
    let after = 0;
    for (;;) {
      signal.throwIfAborted();
      const page = await load(history.id, after, '', signal);
      signal.throwIfAborted();
      const next = page.history;
      if (
        !next ||
        next.id !== history.id ||
        next.revision !== history.revision ||
        next.total !== history.total ||
        next.after !== after ||
        (history.total > 0 && page.rows.length === 0) ||
        (next.next_after !== null &&
          (!Number.isInteger(next.next_after) || next.next_after <= after))
      ) {
        throw new Error('Activity changed. Copy again to use its latest history.');
      }
      rows.push(...page.rows);
      if (next.next_after === null) break;
      after = next.next_after;
    }
    complete.push({ ...activity, rows });
  }
  signal.throwIfAborted();
  return activityCopyText(mergeActivity(complete));
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
  if (!raw || !hasExactKeys(raw, ['type', 'id'])) return null;
  const type = optionalText(raw.type);
  const id = optionalText(raw.id);
  return type && id ? { type, id } : null;
}

function activityEvidenceFromWire(value: unknown): ActivityEvidence | null {
  const raw = record(value);
  if (!raw) return null;
  const kind = activityEnum(raw.kind, ['arguments', 'result', 'error', 'diff'] as const);
  if (!kind || typeof raw.text !== 'string') return null;
  const evidenceText = raw.text;
  if (kind !== 'diff') {
    return hasExactKeys(raw, ['kind', 'text']) ? { kind, text: evidenceText } : null;
  }
  if (
    !hasExactKeys(raw, [
      'kind',
      'text',
      'lines',
      'additions',
      'deletions',
      'modifications',
      'is_truncated',
      'is_redacted',
    ])
  ) {
    return null;
  }

  const rawLines = Array.isArray(raw.lines) ? raw.lines : null;
  const additions = activityCount(raw.additions);
  const deletions = activityCount(raw.deletions);
  const modifications = activityCount(raw.modifications);
  if (
    !rawLines ||
    additions === null ||
    deletions === null ||
    modifications === null ||
    typeof raw.is_truncated !== 'boolean' ||
    typeof raw.is_redacted !== 'boolean'
  )
    return null;
  const parsedLines = rawLines.map((line): ActivityDiffLine | null => {
    const entry = record(line);
    if (!entry || !hasExactKeys(entry, ['kind', 'text'], ['is_redacted'])) {
      return null;
    }
    const lineKind = activityEnum(entry.kind, [
      'header',
      'hunk',
      'context',
      'addition',
      'deletion',
    ] as const);
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
    is_truncated: raw.is_truncated,
    is_redacted: raw.is_redacted,
  };
}

function activityRowFromWire(value: unknown, depth = 0): ActivityRow | null {
  const shape = rowSchema(depth);
  const raw = record(value);
  if (!shape || !raw || !hasExactKeys(raw, shape.required, Object.keys(shape.properties))) {
    return null;
  }
  const id = optionalText(raw.id);
  const sequence = activityCount(raw.sequence);
  const operation = optionalText(raw.operation);
  const presenter = activityEnum(raw.presenter, ACTIVITY_PRESENTERS);
  const signal = activityEnum(raw.signal, ACTIVITY_SIGNALS);
  const state = activityEnum(raw.state, ACTIVITY_STATES);
  const resourcesRaw = Array.isArray(raw.resources) ? raw.resources : null;
  const evidenceRaw = Array.isArray(raw.evidence) ? raw.evidence : null;
  const resources = resourcesRaw
    ? resourcesRaw
        .map(activityResourceFromWire)
        .filter((item): item is ActivityResource => item !== null)
    : null;
  const evidence = evidenceRaw
    ? evidenceRaw
        .map(activityEvidenceFromWire)
        .filter((item): item is ActivityEvidence => item !== null)
    : null;
  const handleId = raw.handle_id === undefined ? undefined : optionalText(raw.handle_id);
  const groupToken = raw.group_token === undefined ? undefined : optionalText(raw.group_token);
  const argumentKey =
    typeof raw.argument_key === 'string' &&
    raw.argument_key.length === 64 &&
    /^[0-9a-f]{64}$/.test(raw.argument_key)
      ? raw.argument_key
      : undefined;
  const readKey =
    typeof raw.read_key === 'string' &&
    raw.read_key.length === 64 &&
    /^[0-9a-f]{64}$/.test(raw.read_key)
      ? raw.read_key
      : undefined;
  const duration = raw.duration_ms === undefined ? undefined : activityCount(raw.duration_ms);
  const resultSummary = typeof raw.result_summary === 'string' ? raw.result_summary : undefined;
  const errorSummary = typeof raw.error_summary === 'string' ? raw.error_summary : undefined;
  const summaryFormat =
    raw.summary_format === undefined
      ? undefined
      : activityEnum(raw.summary_format, ACTIVITY_TEXT_FORMATS);
  const resultFormat =
    raw.result_format === undefined
      ? undefined
      : activityEnum(raw.result_format, ACTIVITY_TEXT_FORMATS);
  const childrenRaw =
    raw.children === undefined
      ? undefined
      : Array.isArray(raw.children) && raw.children.length > 0
        ? raw.children
        : null;
  const children = childrenRaw?.map((child) => activityRowFromWire(child, depth + 1));
  const presentation =
    raw.presentation === undefined ? undefined : activityPresentationFromWire(raw.presentation);
  if (
    !id ||
    sequence === null ||
    !operation ||
    !presenter ||
    !signal ||
    !state ||
    typeof raw.summary !== 'string' ||
    resources === null ||
    resources.length !== resourcesRaw!.length ||
    resources.length > RESOURCE_LIMIT ||
    evidence === null ||
    evidence.length !== evidenceRaw!.length ||
    (raw.handle_id !== undefined && !validHandleId(handleId)) ||
    (raw.group_token !== undefined && groupToken === undefined) ||
    (raw.argument_key !== undefined && argumentKey === undefined) ||
    (raw.read_key !== undefined && readKey === undefined) ||
    duration === null ||
    (raw.result_summary !== undefined && resultSummary === undefined) ||
    (raw.error_summary !== undefined && errorSummary === undefined) ||
    (raw.summary_format !== undefined && !summaryFormat) ||
    (raw.result_format !== undefined && !resultFormat) ||
    presentation === null ||
    childrenRaw === null ||
    children?.some((child) => child === null) ||
    (raw.is_truncated !== undefined && raw.is_truncated !== true)
  ) {
    return null;
  }
  return {
    id,
    sequence,
    operation,
    presenter,
    signal,
    state,
    summary: raw.summary,
    resources,
    evidence,
    ...(handleId !== undefined ? { handle_id: handleId } : {}),
    ...(groupToken !== undefined ? { group_token: groupToken } : {}),
    ...(argumentKey !== undefined ? { argument_key: argumentKey } : {}),
    ...(readKey !== undefined ? { read_key: readKey } : {}),
    ...(duration !== undefined ? { duration_ms: duration } : {}),
    ...(resultSummary !== undefined ? { result_summary: resultSummary } : {}),
    ...(errorSummary !== undefined ? { error_summary: errorSummary } : {}),
    ...(summaryFormat ? { summary_format: summaryFormat } : {}),
    ...(resultFormat ? { result_format: resultFormat } : {}),
    ...(children ? { children: children as ActivityRow[] } : {}),
    ...(presentation ? { presentation } : {}),
    ...(raw.is_truncated === true ? { is_truncated: true } : {}),
  };
}

function activityRowIds(rows: readonly ActivityRow[]): string[] {
  return rows.flatMap((row) => [row.id, ...(row.children ? activityRowIds(row.children) : [])]);
}

function activityLeafCount(rows: readonly ActivityRow[]): number {
  return rows.reduce(
    (count, row) => count + (row.children?.length ? activityLeafCount(row.children) : 1),
    0,
  );
}

export function activityProjectionFromWire(value: unknown): ActivityProjection | null {
  const raw = record(value);
  if (!raw || !hasExactKeys(raw, ['state', 'counts', 'rows', 'omitted'], ['history'])) return null;
  const history = raw.history === undefined ? undefined : record(raw.history);
  if (
    raw.history !== undefined &&
    (!history ||
      !hasExactKeys(history, ['id', 'revision', 'total', 'after', 'next_after']) ||
      typeof history.id !== 'string' ||
      !/^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(history.id) ||
      [history.revision, history.total, history.after].some(
        (value) => activityCount(value) === null,
      ) ||
      (history.next_after !== null &&
        (activityCount(history.next_after) === null ||
          Number(history.next_after) <= Number(history.after))))
  )
    return null;
  const state = activityEnum(raw.state, ACTIVITY_STATES);
  const countsRaw = record(raw.counts);
  const omittedRaw = record(raw.omitted);
  const omittedBy = record(omittedRaw?.by_classification);
  if (
    !state ||
    !countsRaw ||
    !hasExactKeys(countsRaw, ['running', 'succeeded', 'failed', 'cancelled']) ||
    !omittedRaw ||
    !hasExactKeys(omittedRaw, ['rows', 'by_classification']) ||
    !omittedBy ||
    !Array.isArray(raw.rows)
  ) {
    return null;
  }
  const counts = {
    running: activityCount(countsRaw.running),
    succeeded: activityCount(countsRaw.succeeded),
    failed: activityCount(countsRaw.failed),
    cancelled: activityCount(countsRaw.cancelled),
  };
  const omittedRows = activityCount(omittedRaw.rows);
  const omittedEntries = Object.entries(omittedBy);
  const parsedRows = raw.rows.map((row) => activityRowFromWire(row));
  const rows = parsedRows as ActivityRow[];
  const ids = parsedRows.some((row) => row === null) ? [] : activityRowIds(rows);
  if (
    Object.values(counts).some((amount) => amount === null) ||
    omittedRows === null ||
    omittedEntries.some(
      ([classification, amount]) =>
        !ACTIVITY_SIGNALS.includes(classification as ActivitySignal) ||
        activityCount(amount) === null,
    ) ||
    parsedRows.some((row) => row === null) ||
    new Set(ids).size !== ids.length ||
    (history !== undefined &&
      (activityLeafCount(rows) > PAGE_ROW_LIMIT ||
        (activityLeafCount(rows) > 1 &&
          new TextEncoder().encode(JSON.stringify(raw)).length > PAGE_BYTE_TARGET)))
  ) {
    return null;
  }
  return {
    ...(history ? { history: history as unknown as ActivityHistory } : {}),
    state,
    counts: counts as ActivityProjection['counts'],
    rows,
    omitted: {
      rows: omittedRows,
      by_classification: Object.fromEntries(omittedEntries) as Record<string, number>,
    },
  };
}
