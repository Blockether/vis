/**
 * Prepare the multiplexed session-SSE batch after Live View has claimed its frames.
 * Queue, lifecycle and model-control frames pass through unchanged; only adjacent
 * token deltas are coalesced before the screen applies the batch.
 */
import { isLiveViewEvent } from "./live-view";
import type { SseEvent } from "./types";

export function eventString(event: SseEvent, key: string): string {
  const value = event[key];
  return typeof value === "string" ? value : "";
}

/** Read a typed content stream's string block id. */
export function eventBlockKey(event: SseEvent): string {
  return typeof event.block_id === "string" ? event.block_id : "";
}

/** Read a form frame's numeric position as the transcript form key. */
export function eventFormKey(event: SseEvent): string {
  const value = event.form_index;
  return typeof value === "number" && Number.isFinite(value) ? String(value) : "";
}

export function eventIterationPosition(event: SseEvent): number {
  const value = event.iteration;
  const parsed = typeof value === "number" ? value : Number(value);
  return Number.isFinite(parsed) ? parsed : 0;
}

function coalesceContentDeltas(events: SseEvent[]): SseEvent[] {
  const merged: SseEvent[] = [];
  for (const event of events) {
    const previous = merged.at(-1);
    const sameDelta =
      previous?.type === "content.block.delta" &&
      event.type === "content.block.delta" &&
      eventString(previous, "field") === eventString(event, "field") &&
      eventBlockKey(previous) === eventBlockKey(event) &&
      eventIterationPosition(previous) === eventIterationPosition(event);

    if (!previous || !sameDelta) {
      merged.push(event);
      continue;
    }

    merged[merged.length - 1] = event;
  }
  return merged;
}

export function sessionEventBatch(events: SseEvent[]): SseEvent[] {
  return coalesceContentDeltas(events.filter((event) => !isLiveViewEvent(event)));
}
