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
 * The snapshot is ALREADY bounded when it arrives (128 rows, 64 KiB); this
 * parser re-checks both, because a payload that broke the engine's own bound
 * is a contract violation, not a bigger picture to render.
 */
import activityContract from "../../../../packages/vis-contract/resources/vis-contract/activity.json";
const ACTIVITY_LIMITS = activityContract.limits;

export type OperationGroup = {
  id: string;
  label: string;
  rows: ActivityRow[];
};

export type ArgumentGroup = Pick<OperationGroup, "id" | "rows">;

function firstInvocationId(row: ActivityRow): string {
  return (row.operation === "shell" ? (row.children?.[0] ?? row) : row).id;
}

/** Exact operation/argument pairs, within one block. Unknown keys never collapse. */
export function argumentGroups(rows: readonly ActivityRow[]): ArgumentGroup[] {
  const groups: ArgumentGroup[] = [];
  const byArguments = new Map<string, ArgumentGroup>();
  for (const row of [...rows].sort((a, b) => a.sequence - b.sequence)) {
    const key = row.argument_key
      ? JSON.stringify([row.operation, row.argument_key])
      : undefined;
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

/** One group per operation across the block, ordered by first entry. Shell evidence stays intact. */
export function operationGroups(
  rows: readonly ActivityRow[],
): OperationGroup[] {
  const labels: Readonly<Record<string, string>> =
    activityContract.operation_groups;
  const groups: OperationGroup[] = [];
  const byOperation = new Map<string, OperationGroup>();
  for (const row of [...rows].sort((a, b) => a.sequence - b.sequence)) {
    const existing = byOperation.get(row.operation);
    if (existing) existing.rows.push(row);
    else {
      const label = Object.hasOwn(labels, row.operation)
        ? labels[row.operation]
        : row.operation;
      const group = { id: firstInvocationId(row), label, rows: [row] };
      groups.push(group);
      byOperation.set(row.operation, group);
    }
  }
  return groups;
}
function record(value: unknown): Record<string, unknown> | null {
  return value !== null && typeof value === "object" && !Array.isArray(value)
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
  return typeof value === "string" ? value : "";
}

function optionalText(value: unknown): string | undefined {
  return typeof value === "string" && value.trim() !== "" ? value : undefined;
}

export const ACTIVITY_PRESENTERS = [
  "generic",
  "shell",
  "tests",
  "patch",
  "observation",
  "lint",
  "repl",
  "format",
  "list",
] as const;
export const ACTIVITY_SIGNALS = [
  "generic",
  "observation",
  "mutation",
  "verification",
] as const;
export const ACTIVITY_STATES = [
  "idle",
  "running",
  "succeeded",
  "failed",
  "cancelled",
] as const;
/**
 * How a row's own words are to be READ. Absent means literal: a path, a glob or a command
 * must never be re-read as markup, so the engine DECLARES the format per field and the
 * renderer never guesses it from the characters.
 */
export const ACTIVITY_TEXT_FORMATS = ["inline", "markdown"] as const;

export type ActivityPresenter = (typeof ACTIVITY_PRESENTERS)[number];
export type ActivityTextFormat = (typeof ACTIVITY_TEXT_FORMATS)[number];
export type ActivitySignal = (typeof ACTIVITY_SIGNALS)[number];
export type ActivityState = (typeof ACTIVITY_STATES)[number];

export interface ActivityResource {
  type: string;
  id: string;
}

export interface ActivityTextEvidence {
  kind: "arguments" | "result" | "error";
  text: string;
}

export interface ActivityDiffLine {
  kind: "header" | "hunk" | "context" | "addition" | "deletion";
  text: string;
  is_redacted?: true;
}

export interface ActivityDiffEvidence {
  kind: "diff";
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
  | { type: "heading" | "text" | "markdown"; text: string }
  | { type: "code" | "diff"; text: string; language?: string }
  | { type: "table"; columns: string[]; rows: string[][] }
  | {
      type: "image" | "video" | "audio" | "file";
      attachment_id: string;
      label: string;
    }
  | { type: "progress"; label: string; value?: number; total?: number };

/** Closed, bounded content grammar shared with activity.json. Never accept markup as HTML. */
function activityContentFromWire(value: unknown): ActivityContent[] | null {
  if (
    !Array.isArray(value) ||
    value.length > 32 ||
    new TextEncoder().encode(JSON.stringify(value)).length > 32768
  )
    return null;
  const string = (v: unknown, max = 256): v is string =>
    typeof v === "string" && [...v].length <= max;
  for (const item of value) {
    const b = record(item);
    if (!b) return null;
    switch (b.type) {
      case "heading":
      case "text":
      case "markdown":
        if (!hasExactKeys(b, ["type", "text"]) || !string(b.text, 16384))
          return null;
        break;
      case "code":
      case "diff":
        if (
          !hasExactKeys(b, ["type", "text"], ["language"]) ||
          !string(b.text, 16384) ||
          (b.language !== undefined && !string(b.language))
        )
          return null;
        break;
      case "table":
        if (
          !hasExactKeys(b, ["type", "columns", "rows"]) ||
          !Array.isArray(b.columns) ||
          !b.columns.length ||
          b.columns.length > 16 ||
          !b.columns.every((c) => string(c)) ||
          !Array.isArray(b.rows) ||
          b.rows.length > 200
        )
          return null;
        {
          const width = b.columns.length;
          if (
            !b.rows.every(
              (row) =>
                Array.isArray(row) &&
                row.length === width &&
                row.every((c) => string(c)),
            )
          )
            return null;
        }
        break;
      case "image":
      case "video":
      case "audio":
      case "file":
        if (
          !hasExactKeys(b, ["type", "attachment_id", "label"]) ||
          !string(b.attachment_id) ||
          !b.attachment_id.trim() ||
          !string(b.label)
        )
          return null;
        break;
      case "progress":
        if (
          !hasExactKeys(b, ["type", "label"], ["value", "total"]) ||
          !string(b.label)
        )
          return null;
        if (b.value !== undefined || b.total !== undefined) {
          if (
            typeof b.value !== "number" ||
            !Number.isFinite(b.value) ||
            typeof b.total !== "number" ||
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
  content: ActivityContent[];
}

export interface ActivityPresentation extends ActivitySection {
  sections?: ActivitySection[];
}

function activityPresentationFromWire(
  value: unknown,
): ActivityPresentation | null {
  const raw = record(value);
  if (
    !raw ||
    !hasExactKeys(raw, ["headline", "summary", "content"], ["sections"])
  )
    return null;
  const sections = raw.sections === undefined ? [] : raw.sections;
  if (!Array.isArray(sections) || sections.length > 8) return null;
  const bytes = (s: string) => new TextEncoder().encode(s).length;
  if (bytes(JSON.stringify(value)) > 32768) return null;
  let blockCount = 0;
  for (const [index, candidate] of [raw, ...sections].entries()) {
    const section = record(candidate);
    if (
      !section ||
      !hasExactKeys(
        section,
        ["headline", "summary", "content"],
        index === 0 ? ["sections"] : [],
      )
    )
      return null;
    for (const key of ["headline", "summary"]) {
      const line = section[key];
      if (
        typeof line !== "string" ||
        bytes(line) > 512 ||
        /[\u0000-\u001f\u007f\u2028\u2029]/.test(line)
      )
        return null;
    }
    if (!section.headline || !activityContentFromWire(section.content))
      return null;
    blockCount += (section.content as ActivityContent[]).length;
  }
  return blockCount <= 32 ? (value as ActivityPresentation) : null;
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

/**
 * One form's bounded execution picture. Protocol 8 carries no `schema_version`
 * and no `anchor`: the wire protocol number already gates the shape — that is
 * what the compatibility handshake is for — and a snapshot that lives ON its
 * form has nothing left to point at.
 */
export interface ActivityProjection {
  state: ActivityState;
  counts: Record<"running" | "succeeded" | "failed" | "cancelled", number>;
  rows: ActivityRow[];
  omitted: {
    rows: number;
    by_classification: Record<string, number>;
  };
}

function activityEnum<T extends string>(
  value: unknown,
  values: readonly T[],
): T | null {
  const candidate = text(value);
  return values.includes(candidate as T) ? (candidate as T) : null;
}

function activityCount(value: unknown): number | null {
  return typeof value === "number" && Number.isInteger(value) && value >= 0
    ? value
    : null;
}

function activityResourceFromWire(value: unknown): ActivityResource | null {
  const raw = record(value);
  if (!raw || !hasExactKeys(raw, ["type", "id"])) return null;
  const type = optionalText(raw.type);
  const id = optionalText(raw.id);
  return type && id ? { type, id } : null;
}

function activityEvidenceFromWire(value: unknown): ActivityEvidence | null {
  const raw = record(value);
  if (!raw) return null;
  const kind = activityEnum(raw.kind, [
    "arguments",
    "result",
    "error",
    "diff",
  ] as const);
  if (!kind || typeof raw.text !== "string") return null;
  const evidenceText = raw.text;
  if (kind !== "diff") {
    return hasExactKeys(raw, ["kind", "text"])
      ? { kind, text: evidenceText }
      : null;
  }
  if (
    !hasExactKeys(raw, [
      "kind",
      "text",
      "lines",
      "additions",
      "deletions",
      "modifications",
      "is_truncated",
      "is_redacted",
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
    typeof raw.is_truncated !== "boolean" ||
    typeof raw.is_redacted !== "boolean"
  )
    return null;
  const parsedLines = rawLines.map((line): ActivityDiffLine | null => {
    const entry = record(line);
    if (!entry || !hasExactKeys(entry, ["kind", "text"], ["is_redacted"])) {
      return null;
    }
    const lineKind = activityEnum(entry.kind, [
      "header",
      "hunk",
      "context",
      "addition",
      "deletion",
    ] as const);
    if (
      !lineKind ||
      typeof entry.text !== "string" ||
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
  if (depth > ACTIVITY_LIMITS.max_depth) return null;
  const raw = record(value);
  if (
    !raw ||
    !hasExactKeys(
      raw,
      [
        "id",
        "sequence",
        "operation",
        "presenter",
        "signal",
        "state",
        "summary",
        "resources",
        "evidence",
      ],
      [
        "argument_key",
        "group_token",
        "duration_ms",
        "result_summary",
        "error_summary",
        "children",
        "is_truncated",
        "summary_format",
        "result_format",
        "presentation",
      ],
    )
  ) {
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
  const groupToken =
    raw.group_token === undefined ? undefined : optionalText(raw.group_token);
  const argumentKey =
    typeof raw.argument_key === "string" &&
    raw.argument_key.length === 64 &&
    /^[0-9a-f]{64}$/.test(raw.argument_key)
      ? raw.argument_key
      : undefined;
  const duration =
    raw.duration_ms === undefined ? undefined : activityCount(raw.duration_ms);
  const resultSummary =
    typeof raw.result_summary === "string" ? raw.result_summary : undefined;
  const errorSummary =
    typeof raw.error_summary === "string" ? raw.error_summary : undefined;
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
  const children = childrenRaw?.map((child) =>
    activityRowFromWire(child, depth + 1),
  );
  const presentation =
    raw.presentation === undefined
      ? undefined
      : activityPresentationFromWire(raw.presentation);
  if (
    !id ||
    sequence === null ||
    !operation ||
    !presenter ||
    !signal ||
    !state ||
    typeof raw.summary !== "string" ||
    resources === null ||
    resources.length !== resourcesRaw!.length ||
    resources.length > ACTIVITY_LIMITS.max_resources ||
    evidence === null ||
    evidence.length !== evidenceRaw!.length ||
    (raw.group_token !== undefined && groupToken === undefined) ||
    (raw.argument_key !== undefined && argumentKey === undefined) ||
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
    ...(groupToken !== undefined ? { group_token: groupToken } : {}),
    ...(argumentKey !== undefined ? { argument_key: argumentKey } : {}),
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
  return rows.flatMap((row) => [
    row.id,
    ...(row.children ? activityRowIds(row.children) : []),
  ]);
}

export function activityProjectionFromWire(
  value: unknown,
): ActivityProjection | null {
  const raw = record(value);
  if (!raw || !hasExactKeys(raw, ["state", "counts", "rows", "omitted"]))
    return null;
  const state = activityEnum(raw.state, ACTIVITY_STATES);
  const countsRaw = record(raw.counts);
  const omittedRaw = record(raw.omitted);
  const omittedBy = record(omittedRaw?.by_classification);
  if (
    !state ||
    !countsRaw ||
    !hasExactKeys(countsRaw, ["running", "succeeded", "failed", "cancelled"]) ||
    !omittedRaw ||
    !hasExactKeys(omittedRaw, ["rows", "by_classification"]) ||
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
  const ids = parsedRows.some((row) => row === null)
    ? []
    : activityRowIds(rows);
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
    parsedRows.length > ACTIVITY_LIMITS.max_rows ||
    new TextEncoder().encode(JSON.stringify(raw)).length >
      ACTIVITY_LIMITS.max_receipt_bytes
  ) {
    return null;
  }
  return {
    state,
    counts: counts as ActivityProjection["counts"],
    rows,
    omitted: {
      rows: omittedRows,
      by_classification: Object.fromEntries(omittedEntries) as Record<
        string,
        number
      >,
    },
  };
}
