/** The shared lifecycle envelope for every operator-facing View. */

import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import type { SseEvent } from './types';

export const VIEW_KINDS = ['input', 'live'] as const;
export const VIEW_OPEN_EVENT = 'view.open';
export const VIEW_PATCH_EVENT = 'view.patch';
export const VIEW_CLOSE_EVENT = 'view.close';
export const VIEW_EVENT_TYPES = [VIEW_OPEN_EVENT, VIEW_PATCH_EVENT, VIEW_CLOSE_EVENT] as const;

export type ViewKind = (typeof VIEW_KINDS)[number];

export type ViewGroupDirection = 'column' | 'row' | 'grid' | 'wrap';
/** How a group lays out its children (`group_direction` in view.json). CLOSED. */
export const VIEW_GROUP_DIRECTIONS = viewSchema.$defs.group_direction
  .enum as readonly ViewGroupDirection[];

type SchemaBranch = { properties: Record<string, { const?: string; maximum?: number }> };
/** The largest `max_columns` of a grid group (view.json). */
export const VIEW_MAX_COLUMNS =
  (viewSchema.$defs.component.oneOf as unknown as SchemaBranch[]).find(
    (branch) => branch.properties.type?.const === 'group',
  )?.properties.max_columns?.maximum ?? 1;

type ShortcutKeySchema = { oneOf: { enum?: string[] }[] };
/** The named keys that an app can bind (`shortcut_key` in view.json). CLOSED. */
export const VIEW_SHORTCUT_NAMED_KEYS: readonly string[] =
  (viewSchema.$defs.shortcut_key as unknown as ShortcutKeySchema).oneOf.find((branch) => branch.enum)
    ?.enum ?? [];
/** The most shortcuts that one app binds (view.json). */
export const VIEW_SHORTCUTS_MAX = viewSchema.$defs.live_view.properties.shortcuts.maxItems;
/** The longest legend label of a shortcut (view.json). */
export const VIEW_SHORTCUT_LABEL_MAX = viewSchema.$defs.shortcut.properties.label.maxLength;

/** Read a group direction; an unknown or absent one stacks as a column. */
export function viewGroupDirection(raw: unknown): ViewGroupDirection {
  return (VIEW_GROUP_DIRECTIONS as readonly unknown[]).includes(raw)
    ? (raw as ViewGroupDirection)
    : 'column';
}

/** Read a grid's column cap: a whole number from 1 to 12, else no cap. */
export function viewMaxColumns(raw: unknown): number | undefined {
  return typeof raw === 'number' && Number.isInteger(raw) && raw >= 1 && raw <= VIEW_MAX_COLUMNS
    ? raw
    : undefined;
}

export const VIEW_ACTIONS = [
  'activate',
  'cancel',
  'change',
  'draft',
  'interrupt',
  'select',
  'shortcut',
  'submit',
] as const;
export type ViewActionName = (typeof VIEW_ACTIONS)[number];

/** A diff line that a thread belongs to: the file, the old or new side and the line. */
export interface ViewAnchor {
  path: string;
  side: 'old' | 'new';
  line: number;
}

export type ViewAction =
  | {
      action: 'submit';
      values: Record<string, unknown>;
      node_id?: string;
      /** One key for each Send. A retry with the same key answers the same receipt. */
      idempotency_key?: string;
      /** The state version the client edited. */
      base_seq?: number;
      /** The diff `new_revision` the client reviewed. */
      revision?: string;
    }
  | { action: 'change'; node_id: string; value: unknown; base_seq?: number }
  | {
      action: 'draft';
      node_id: string;
      thread_id?: string;
      anchor?: ViewAnchor;
      /** Blank text removes the draft; a blank draft on a new anchor creates nothing. */
      body: string;
      base_seq?: number;
      revision?: string;
    }
  | { action: 'cancel' }
  | { action: 'activate'; node_id: string }
  | { action: 'select'; node_id: string; item_ids: string[] }
  /** A key that the app binds to an action of its owner. */
  | { action: 'shortcut'; key: string }
  | { action: 'interrupt'; note?: string };

export type ViewActionOutcome = {
  action: ViewActionName;
  view_id: string;
  is_accepted: boolean;
  errors?: Record<string, string>;
  reason?: string;
  node_id?: string;
  item_ids?: string[];
  /** The new state version: the Save acknowledgement of an app. */
  seq?: number;
  /** The thread that a `draft` saved into. Absent when nothing was created. */
  thread_id?: string;
  /** The receipt of a `submit` in an app. */
  submission?: unknown;
  /** The current diff revision of a `stale_revision` refusal. */
  revision?: string;
  /** The key of a `shortcut` action. */
  key?: string;
  /** The owner action that an accepted `shortcut` key ran. */
  shortcut?: string;
};

/** Read the CLOSED capability kind without accepting an arbitrary string. */
export function viewKind(event: SseEvent): ViewKind | null {
  return event.kind === 'input' || event.kind === 'live' ? event.kind : null;
}

/** True for one canonical lifecycle frame, irrespective of capability kind. */
export function isViewEvent(event: SseEvent): boolean {
  return (VIEW_EVENT_TYPES as readonly string[]).includes(event.type) && viewKind(event) !== null;
}
