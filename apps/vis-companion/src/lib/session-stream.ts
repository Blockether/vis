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

/**
 * The stream a frame replaces wholesale, or `null`. A canonical cumulative delta
 * carries its block's whole text and a `block.activity` frame its form's whole
 * Activity, so a newer frame on the same stream leaves the older one nothing to add.
 */
export function supersedingStreamKey(event: SseEvent): string | null {
  if (event.type === 'content.block.delta' && typeof event.cumulative === 'string') {
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
 * Append `event` to a turn's replay buffer, in place, dropping the frame it supersedes.
 * The rest keeps its stream order, so a replay rebuilds what the live frames built; a
 * late or malformed Activity frame never replaces the one the reducer kept.
 */
export function bufferStreamEvent(buffered: SseEvent[], event: SseEvent): void {
  const stream = supersedingStreamKey(event);
  if (stream !== null) {
    for (let index = buffered.length - 1; index >= 0; index -= 1) {
      const entry = buffered[index];
      if (entry.type !== event.type || supersedingStreamKey(entry) !== stream) continue;
      if (event.type === 'block.activity' && staleActivity(event, entry)) return;
      buffered.splice(index, 1);
      break;
    }
  }
  buffered.push(event);
}
