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

/** Normalize the wire's numeric form position and string stream id to one key. */
export function eventBlockKey(event: SseEvent): string {
  const value = event.block_id;
  if (typeof value === "string") return value;
  return typeof value === "number" && Number.isFinite(value)
    ? String(value)
    : "";
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

    const currentCumulative = eventString(event, "cumulative");
    const previousCumulative = eventString(previous, "cumulative");
    if (currentCumulative) {
      merged[merged.length - 1] = event;
    } else if (previousCumulative) {
      merged[merged.length - 1] = {
        ...previous,
        ...event,
        cumulative: previousCumulative + eventString(event, "text"),
        text: "",
      };
    } else {
      merged[merged.length - 1] = {
        ...previous,
        ...event,
        cumulative: "",
        text: eventString(previous, "text") + eventString(event, "text"),
      };
    }
  }
  return merged;
}

export function sessionEventBatch(events: SseEvent[]): SseEvent[] {
  return coalesceContentDeltas(events.filter((event) => !isLiveViewEvent(event)));
}
