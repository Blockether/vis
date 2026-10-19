/**
 * Prepare the multiplexed session-SSE batch after Live View has claimed its frames.
 * Queue, lifecycle and model-control frames pass through unchanged; only adjacent
 * token deltas are coalesced before the screen applies the batch.
 */
import { activityProjectionFromWire } from './activity';
import { isLiveViewEvent } from './live-view';
import type { SseEvent } from './types';

export function eventString(event: SseEvent, key: string): string {
  const value = event[key];
  return typeof value === 'string' ? value : '';
}

/** Read a typed content stream's string block id. */
export function eventBlockKey(event: SseEvent): string {
  return typeof event.block_id === 'string' ? event.block_id : '';
}

/** Read a form frame's numeric position as the transcript form key. */
export function eventFormKey(event: SseEvent): string {
  const value = event.form_index;
  return typeof value === 'number' && Number.isFinite(value) ? String(value) : '';
}

export function eventIterationPosition(event: SseEvent): number {
  const value = event.iteration;
  const parsed = typeof value === 'number' ? value : Number(value);
  return Number.isFinite(parsed) ? parsed : 0;
}

function eventOffset(event: SseEvent): number {
  const value = event.offset;
  return typeof value === 'number' && Number.isFinite(value) && value >= 0 ? value : 0;
}

/**
 * One delta frame with the text of `previous` and then `next` on the same block, or
 * `null` when `next` does not start inside the text span that `previous` covers.
 */
export function mergeBlockDeltas(previous: SseEvent, next: SseEvent): SseEvent | null {
  const start = eventOffset(previous);
  const text = eventString(previous, 'text');
  const offset = eventOffset(next);
  if (offset < start || offset > start + text.length) return null;
  return { ...next, offset: start, text: text.slice(0, offset - start) + eventString(next, 'text') };
}

function coalesceContentDeltas(events: SseEvent[]): SseEvent[] {
  const merged: SseEvent[] = [];
  for (const event of events) {
    const previous = merged.at(-1);
    const sameDelta =
      previous?.type === 'content.block.delta' &&
      event.type === 'content.block.delta' &&
      eventString(previous, 'field') === eventString(event, 'field') &&
      eventBlockKey(previous) === eventBlockKey(event) &&
      eventIterationPosition(previous) === eventIterationPosition(event);
    const joined = previous && sameDelta ? mergeBlockDeltas(previous, event) : null;

    if (!joined) {
      merged.push(event);
      continue;
    }

    merged[merged.length - 1] = joined;
  }
  return merged;
}

export function sessionEventBatch(events: SseEvent[]): SseEvent[] {
  return coalesceContentDeltas(events.filter((event) => !isLiveViewEvent(event)));
}

/**
 * The stream whose frames one buffered frame can hold, or `null`. Deltas of one block
 * join into one frame from the first offset, and a `block.activity` frame carries its
 * form's whole Activity, so a newer frame on the same stream replaces the older one.
 */
export function supersedingStreamKey(event: SseEvent): string | null {
  if (event.type === 'content.block.delta') {
    const stream = [eventIterationPosition(event), eventBlockKey(event), eventString(event, 'field')];
    return `delta\u0000${stream.join('\u0000')}`;
  }
  if (event.type === 'block.activity') {
    return `activity\u0000${eventIterationPosition(event)}\u0000${eventFormKey(event)}`;
  }
  return null;
}

/**
 * An Activity frame the running-turn reducer would not apply over `current`: it is
 * malformed, or an older revision of the same history that arrived late.
 */
function staleActivity(event: SseEvent, current: SseEvent): boolean {
  const next = activityProjectionFromWire(event.activity);
  if (!next) return true;
  const held = activityProjectionFromWire(current.activity)?.history;
  return Boolean(held && next.history?.id === held.id && next.history.revision < held.revision);
}

/**
 * Append `event` to a turn's replay buffer, in place, joining it with the frame of its
 * stream. The rest keeps its stream order, so a replay rebuilds what the live frames
 * built; a late or malformed Activity frame never replaces the one the reducer kept.
 */
export function bufferStreamEvent(buffered: SseEvent[], event: SseEvent): void {
  const stream = supersedingStreamKey(event);
  let next = event;
  if (stream !== null) {
    for (let index = buffered.length - 1; index >= 0; index -= 1) {
      const entry = buffered[index];
      if (entry.type !== event.type || supersedingStreamKey(entry) !== stream) continue;
      if (event.type === 'block.activity') {
        if (staleActivity(event, entry)) return;
      } else {
        const joined = mergeBlockDeltas(entry, event);
        if (!joined) break;
        next = joined;
      }
      buffered.splice(index, 1);
      break;
    }
  }
  buffered.push(next);
}
