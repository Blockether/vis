/** The shared lifecycle envelope for every operator-facing View. */

import type { SseEvent } from './types';

export const VIEW_KINDS = ['input', 'live'] as const;
export const VIEW_OPEN_EVENT = 'view.open';
export const VIEW_PATCH_EVENT = 'view.patch';
export const VIEW_CLOSE_EVENT = 'view.close';
export const VIEW_EVENT_TYPES = [VIEW_OPEN_EVENT, VIEW_PATCH_EVENT, VIEW_CLOSE_EVENT] as const;

export type ViewKind = (typeof VIEW_KINDS)[number];

/** Read the CLOSED capability kind without accepting an arbitrary string. */
export function viewKind(event: SseEvent): ViewKind | null {
  return event.kind === 'input' || event.kind === 'live' ? event.kind : null;
}

/** True for one canonical lifecycle frame, irrespective of capability kind. */
export function isViewEvent(event: SseEvent): boolean {
  return (VIEW_EVENT_TYPES as readonly string[]).includes(event.type) && viewKind(event) !== null;
}
