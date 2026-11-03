/**
 * Where a live app shows. The engine sends the EFFECTIVE `surfaces` of each app
 * (view.json `app_surfaces`): the opener's declaration after the person's settings.
 * These pure functions decide the surface, so both layouts and the tests share them.
 */

import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import { nestedLiveNodes, type LiveNode, type LiveView } from './live-view';

const SURFACES_SCHEMA = viewSchema.$defs.app_surfaces.properties;

export type AppSessionSurface = 'sidebar' | 'chat' | 'off';

/** Where the session apps button opens an app (`app_surfaces.session`). CLOSED. */
export const APP_SESSION_SURFACES = SURFACES_SCHEMA.session.enum as readonly AppSessionSurface[];

export type AppSurfaces = Readonly<{ inline: boolean; session: AppSessionSurface }>;

/** The surfaces of an app that sends none (the schema defaults). */
export const APP_SURFACE_DEFAULTS: AppSurfaces = {
  inline: SURFACES_SCHEMA.inline.default,
  session: SURFACES_SCHEMA.session.default as AppSessionSurface,
};

/** Read `surfaces` from the wire. An absent or unknown field takes its default. */
export function appSurfacesFromWire(raw: unknown): AppSurfaces | undefined {
  if (!raw || typeof raw !== 'object' || Array.isArray(raw)) return undefined;
  const given = raw as { inline?: unknown; session?: unknown };
  return {
    inline: typeof given.inline === 'boolean' ? given.inline : APP_SURFACE_DEFAULTS.inline,
    session: (APP_SESSION_SURFACES as readonly unknown[]).includes(given.session)
      ? (given.session as AppSessionSurface)
      : APP_SURFACE_DEFAULTS.session,
  };
}

/** The surfaces of `view`, with the defaults for an app that sends none. */
export function appSurfaces(view: Pick<LiveView, 'surfaces'> | undefined): AppSurfaces {
  return view?.surfaces ?? APP_SURFACE_DEFAULTS;
}

/** The apps that the session apps button lists: each app whose session surface is not `off`. */
export function sessionApps<T extends Pick<LiveView, 'placement' | 'surfaces'>>(views: readonly T[]): T[] {
  return views.filter(
    (view) => view.placement === 'attachment' && appSurfaces(view).session !== 'off',
  );
}

/**
 * Where a picked session app stands: `sidebar` beside the chat, `sheet` over the
 * whole session pane (a phone has no room beside the chat), `chat` over the
 * transcript. Null when the person turned the session surface off.
 */
export type SessionAppSlot = 'sidebar' | 'sheet' | 'chat';

export function sessionAppSlot(
  view: Pick<LiveView, 'surfaces'> | undefined,
  docksPanel: boolean,
): SessionAppSlot | null {
  const surface = appSurfaces(view).session;
  if (surface === 'off') return null;
  if (surface === 'chat') return 'chat';
  return docksPanel ? 'sidebar' : 'sheet';
}

/**
 * The picked session app and its slot, read again from the current list: a saved
 * setting can move the app or turn it off, and an archived app leaves the list.
 */
export function pickedSessionApp<T extends Pick<LiveView, 'id' | 'placement' | 'surfaces'>>(
  apps: readonly T[],
  pickedId: string | null,
  docksPanel: boolean,
): { view: T; slot: SessionAppSlot } | null {
  if (pickedId === null) return null;
  const view = apps.find((one) => one.id === pickedId);
  const slot = view ? sessionAppSlot(view, docksPanel) : null;
  return view && slot ? { view, slot } : null;
}

/** How long a press waits for the app that it reveals to open. */
export const APP_REVEAL_WAIT_MS = 10_000;

/** A press that asks to show the app with `key` of the owner `extension`. */
export type AppReveal = Readonly<{ extension: string | undefined; key: string }>;

/** The app key that a press of node `nodeId` reveals (view.json `reveals`), or null. */
export function pressReveal(view: { nodes: readonly LiveNode[] }, nodeId: string): string | null {
  const walk = (nodes: readonly LiveNode[]): string | null => {
    for (const node of nodes) {
      if (node.id === nodeId) return (node.type === 'card' || node.type === 'button') && node.reveals ? node.reveals : null;
      const found = walk(nestedLiveNodes(node));
      if (found !== null) return found;
    }
    return null;
  };
  return walk(view.nodes);
}

/**
 * The id of the open app that `reveal` names: the same owner and key. Null while
 * that app is not open, so the press waits for its open.
 */
export function revealedApp<T extends Pick<LiveView, 'id' | 'extension' | 'key' | 'is_closed'>>(
  apps: readonly T[],
  reveal: AppReveal,
): string | null {
  for (let at = apps.length - 1; at >= 0; at--) {
    const view = apps[at];
    if (view.key === reveal.key && view.extension === reveal.extension && view.is_closed !== true) return view.id;
  }
  return null;
}

/** How an app of a chat turn shows: its row, its frame in the turn, or full screen. */
export type AppFrame = 'row' | 'inline' | 'full';

/**
 * The frame that the turn shows. An app without the inline surface never shows its
 * frame in the turn: its row opens it full screen.
 */
export function shownAppFrame(frame: AppFrame, inline: boolean): AppFrame {
  return frame === 'inline' && !inline ? 'full' : frame;
}

/**
 * The next frame after one step. Open shows the app in the turn, or full screen
 * when the inline surface is off. Escape leaves full screen first, then closes.
 */
export function appFrameStep(
  frame: AppFrame,
  step: 'open' | 'full' | 'escape' | 'close',
  inline: boolean,
): AppFrame {
  if (step === 'open') return inline ? 'inline' : 'full';
  if (step === 'full') return 'full';
  if (step === 'close') return 'row';
  if (frame === 'full' && inline) return 'inline';
  return 'row';
}

/** A setting that changes the surfaces of apps, so the apps must be read again. */
export function isAppSurfaceSetting(id: string): boolean {
  return (
    id === 'apps_inline' ||
    id === 'apps_session' ||
    id === 'apps_global' ||
    /^app_[a-z0-9_]+_(?:inline|session)$/.test(id)
  );
}
