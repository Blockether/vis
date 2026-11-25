/** The shared lifecycle envelope for every operator-facing View. */

import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import type { BoardAction } from './board';
import type { SseEvent } from './types';

export const VIEW_KINDS = ['input', 'live'] as const;
export const VIEW_OPEN_EVENT = 'view.open';
export const VIEW_PATCH_EVENT = 'view.patch';
export const VIEW_CLOSE_EVENT = 'view.close';
export const VIEW_EVENT_TYPES = [VIEW_OPEN_EVENT, VIEW_PATCH_EVENT, VIEW_CLOSE_EVENT] as const;

export type ViewKind = (typeof VIEW_KINDS)[number];

export type ViewFlexDirection = 'row' | 'column';
export type ViewGap = 'none' | 'xs' | 'sm' | 'md' | 'lg';
export type ViewFlexAlign = 'start' | 'center' | 'end' | 'stretch' | 'baseline';
export type ViewFlexJustify = 'start' | 'center' | 'end' | 'between';
export type ViewSize = 'xs' | 'sm' | 'md' | 'lg';

export interface ViewFlexLayout {
  direction: ViewFlexDirection;
  is_wrapping?: boolean;
  gap?: ViewGap;
  align?: ViewFlexAlign;
  justify?: ViewFlexJustify;
}

export interface ViewGridLayout {
  columns?: number;
  min_width?: ViewSize;
  gap?: ViewGap;
}

export const VIEW_CONTAINERS = ['flex', 'grid', 'section', 'collapsible', 'dialog'] as const;
export type ViewContainerType = (typeof VIEW_CONTAINERS)[number];
export function isViewContainerType(type: string): type is ViewContainerType {
  return (VIEW_CONTAINERS as readonly string[]).includes(type);
}

function readEnum<T extends string>(schema: { enum: string[]; default: string }, raw: unknown): T {
  return (schema.enum as readonly unknown[]).includes(raw) ? raw as T : schema.default as T;
}

const GRID_COLUMNS = viewSchema.$defs.component.oneOf.find(
  (branch) => branch.properties.type.const === 'grid',
)!.properties.columns as { minimum: number; maximum: number };

export function viewFlexLayout(raw: Record<string, unknown>): ViewFlexLayout {
  return {
    direction: readEnum(viewSchema.$defs.flex_direction, raw.direction),
    ...(raw.is_wrapping === true ? { is_wrapping: true } : {}),
    gap: readEnum(viewSchema.$defs.gap, raw.gap),
    align: readEnum(viewSchema.$defs.flex_align, raw.align),
    justify: readEnum(viewSchema.$defs.flex_justify, raw.justify),
  };
}

export function viewGridLayout(raw: Record<string, unknown>): ViewGridLayout {
  const columns = raw.columns;
  return {
    ...(typeof columns === 'number' && Number.isInteger(columns) &&
      columns >= GRID_COLUMNS.minimum && columns <= GRID_COLUMNS.maximum ? { columns } : {}),
    min_width: readEnum(viewSchema.$defs.size, raw.min_width),
    gap: readEnum(viewSchema.$defs.gap, raw.gap),
  };
}

export type ViewButtonVariant = 'primary' | 'secondary' | 'destructive';
type ButtonSchemaBranch = {
  properties: { type?: { const?: string }; variant?: { enum: string[]; default: string } };
};
const BUTTON_VARIANT = (viewSchema.$defs.component.oneOf as unknown as ButtonSchemaBranch[]).find(
  (branch) => branch.properties.type?.const === 'button',
)?.properties.variant;
/** The rank of a button (`variant` of the button component in view.json). CLOSED. */
export const VIEW_BUTTON_VARIANTS = (BUTTON_VARIANT?.enum ?? []) as readonly ViewButtonVariant[];

/** Read a button variant; an absent or unknown one is the schema default. */
export function viewButtonVariant(raw: unknown): ViewButtonVariant {
  return (VIEW_BUTTON_VARIANTS as readonly unknown[]).includes(raw)
    ? (raw as ViewButtonVariant)
    : ((BUTTON_VARIANT?.default ?? 'secondary') as ViewButtonVariant);
}

export type AppIconName = keyof typeof viewSchema.$defs.app_icon['x-vis-terminal'];
export const APP_ICONS = viewSchema.$defs.app_icon.enum as readonly AppIconName[];
export function appIconName(raw: unknown): AppIconName {
  return (APP_ICONS as readonly unknown[]).includes(raw)
    ? (raw as AppIconName)
    : (viewSchema.$defs.app_icon.default as AppIconName);
}

/** The command icon vocabulary comes from view.json. */
export const VIEW_COMMAND_ICONS: readonly string[] = viewSchema.$defs.command_icon.enum;
export type ViewCommandIcon = (typeof VIEW_COMMAND_ICONS)[number];

/** Read a command icon; an absent or unknown one is no icon. */
export function viewCommandIcon(raw: unknown): { icon?: ViewCommandIcon } {
  return typeof raw === 'string' && VIEW_COMMAND_ICONS.includes(raw) ? { icon: raw } : {};
}

type ShortcutKeySchema = { oneOf: { enum?: string[] }[] };
/** The named keys that an app can bind (`shortcut_key` in view.json). CLOSED. */
export const VIEW_SHORTCUT_NAMED_KEYS: readonly string[] =
  (viewSchema.$defs.shortcut_key as unknown as ShortcutKeySchema).oneOf.find((branch) => branch.enum)
    ?.enum ?? [];
/** The most shortcuts that one app binds (view.json). */
export const VIEW_SHORTCUTS_MAX = viewSchema.$defs.live_view.properties.shortcuts.maxItems;
/** The longest legend label of a shortcut (view.json). */
export const VIEW_SHORTCUT_LABEL_MAX = viewSchema.$defs.shortcut.properties.label.maxLength;
/** The longest note that an app or an HTML page gives the model (view.json). */
export const VIEW_AGENT_NOTE_MAX = viewSchema.$defs.live_view.properties.agent_note.maxLength;

export const VIEW_ACTIONS = [
  'activate',
  'cancel',
  'change',
  'connect',
  'draft',
  'edit',
  'interrupt',
  'move',
  'select',
  'shortcut',
  'submit',
] as const;
export type ViewActionName = (typeof VIEW_ACTIONS)[number];

/** The diff lines that a thread belongs to: the file, the old or new side and the line. */
export interface ViewAnchor {
  path: string;
  side: 'old' | 'new';
  /** The side of the first line of a range, when it is not `side`. */
  start_side?: 'old' | 'new';
  /** The first line of a range. `line` is then the last line. */
  start_line?: number;
  line: number;
}

export type ViewAction =
  | BoardAction
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
      /** The canvas item that a new thread of a canvas belongs to. */
      item_id?: string;
      /**
       * Null removes the draft. Blank text removes the draft of a thread without
       * messages and creates no new thread; on a thread with messages it adds them
       * to the next message of the session without a note.
       */
      body: string | null;
      base_seq?: number;
      revision?: string;
    }
  | { action: 'cancel' }
  /** A person opens an app whose owner is away: the host brings the owner back. */
  | { action: 'connect' }
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
