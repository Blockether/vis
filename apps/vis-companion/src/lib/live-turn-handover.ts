import type { TranscriptTurn } from "./types";

function rowId(turn: TranscriptTurn): string {
  return turn.id ?? turn.turn_id ?? "";
}

function rowRequest(turn: TranscriptTurn): string {
  return turn.user_request ?? turn.request ?? "";
}

function isSettledRow(turn: TranscriptTurn): boolean {
  return turn.status !== "running" && turn.status !== "queued";
}

function rowCarriesProse(turn: TranscriptTurn): boolean {
  return Boolean(
    turn.content?.some(
      (block) => block.type === "prose" && Boolean(block.markdown?.trim()),
    ) ||
      turn.iterations?.some((iteration) =>
        Boolean(iteration.assistant_prose?.trim()),
      ),
  );
}

function rowCarriesOutput(turn: TranscriptTurn): boolean {
  if (
    turn.content?.some((block) => {
      if (block.type === "prose") return Boolean(block.markdown?.trim());
      // A spoken projection is secondary metadata. It cannot replace prose or
      // tool output the user has already seen in the live bubble.
      if (String(block.type) === "speech") return false;
      return true;
    })
  )
    return true;

  return Boolean(
    turn.iterations?.some((iteration) =>
      Boolean(
        iteration.assistant_prose?.trim() ||
          iteration.thinking?.trim() ||
          iteration.forms?.length ||
          iteration.error,
      ),
    ),
  );
}

/**
 * Whether a persisted transcript row is a safe replacement for a live bubble.
 *
 * Gateway turn ids and engine transcript ids are often different, so an exact id
 * match is sufficient but not required. For an id-less match, the authored
 * request is the stable identity shared by both representations. Merely seeing
 * any new settled row is not enough: after a session switch that row can be the
 * previous turn, and retiring the live bubble against it makes an answer that was
 * already visible disappear until the real row is persisted.
 */
export function settledTranscriptCoversLiveTurn(
  turns: readonly TranscriptTurn[] | null,
  before: ReadonlySet<string>,
  live: {
    id?: string;
    request?: string;
    startedAt?: number;
    /** Painted output may hand over only to a row that can paint it too. */
    requireOutput?: boolean;
    /** Painted answer prose may not hand over to a partially hydrated reasoning shell. */
    requireProse?: boolean;
  },
): boolean {
  if (!turns?.length) return false;
  const expectedId = live.id ?? "";
  const expectedRequest = live.request?.trim() ?? "";

  return turns.some((turn) => {
    if (!isSettledRow(turn)) return false;
    if (live.requireOutput && !rowCarriesOutput(turn)) return false;
    if (live.requireProse && !rowCarriesProse(turn)) return false;
    const id = rowId(turn);
    if (expectedId && id === expectedId) return true;
    if (before.has(id)) return false;

    // Different id namespaces need a semantic identity check. A missing request
    // cannot prove coverage and must leave the already-painted bubble in place.
    if (!expectedRequest || rowRequest(turn).trim() !== expectedRequest) return false;

    const created = turn.created_at;
    if (
      live.startedAt &&
      typeof created === "number" &&
      Number.isFinite(created)
    ) {
      // Allow clock skew between the gateway/engine and the device.
      return created >= live.startedAt - 60_000;
    }
    return true;
  });
}
