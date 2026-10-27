/**
 * The client state of one app attachment: its view, folded from the session
 * stream, and the actions that its controls send.
 */

import { applyLivePatch, liveViewFromWire, upsertSubmission, type LiveNode, type LiveView } from './live-view';
import type { SseEvent } from './types';
import { VIEW_CLOSE_EVENT, VIEW_OPEN_EVENT, VIEW_PATCH_EVENT, type ViewAction, type ViewAnchor } from './view';
import type { LiveSubmission } from './live-view';

/** What one session event does to an app: fold it in, or read the app again. */
export interface AppFold {
  view: LiveView;
  /** The view missed patches or closed: read it again from the gateway. */
  isReread: boolean;
}

/** Fold one session event into the app `view`. Events of other views change nothing. */
export function foldAppEvent(view: LiveView, event: SseEvent): AppFold {
  const viewId = typeof event.view_id === 'string' ? event.view_id : undefined;
  if (event.type === VIEW_CLOSE_EVENT) {
    if (viewId !== view.id) return { view, isReread: false };
    return { view: { ...view, is_closed: true }, isReread: true };
  }
  if (event.type === VIEW_OPEN_EVENT) {
    const opened = liveViewFromWire(event.view);
    if (!opened || opened.id !== view.id || opened.seq < view.seq) return { view, isReread: false };
    return { view: opened, isReread: false };
  }
  if (event.type !== VIEW_PATCH_EVENT || viewId !== view.id) return { view, isReread: false };
  const patched = applyLivePatch(view, event);
  if (patched.is_stale) return { view, isReread: true };
  return { view: patched, isReread: false };
}

/** Keep the newer of two pictures of one app: a slow read never undoes a patch. */
export function newerAppView(held: LiveView | null, read: LiveView): LiveView {
  if (!held || held.id !== read.id) return read;
  if (read.seq > held.seq) return read;
  if (read.seq === held.seq && (read.is_closed || read.is_disconnected !== held.is_disconnected)) {
    // A read without the flag is the owner back again: the held flag must not stay.
    const merged = { ...held, ...read };
    if (read.is_disconnected !== true) delete merged.is_disconnected;
    return merged;
  }
  return held;
}

/** Add one Send receipt to the app. */
export function withSubmission(view: LiveView, submission: LiveSubmission): LiveView {
  return { ...view, submissions: upsertSubmission(view.submissions ?? [], submission) };
}

function findNode(nodes: readonly LiveNode[], id: string): LiveNode | undefined {
  for (const node of nodes) {
    if (node.id === id) return node;
    if (node.type === 'group') {
      const found = findNode(node.children, id);
      if (found) return found;
    }
  }
  return undefined;
}

/** The diff revision the human reviews in node `nodeId`, when it is a diff. */
export function reviewedRevision(view: LiveView, nodeId: string): string | undefined {
  const node = findNode(view.nodes, nodeId);
  return node?.type === 'diff' ? node.new_revision : undefined;
}

/** The `draft` action of one thread or one new anchor, against the app's state version. */
export function draftAction(
  view: LiveView,
  nodeId: string,
  target: { threadId?: string; anchor?: ViewAnchor; body: string },
): ViewAction {
  const revision = reviewedRevision(view, nodeId);
  return {
    action: 'draft',
    node_id: nodeId,
    ...(target.threadId ? { thread_id: target.threadId } : {}),
    ...(target.anchor && !target.threadId ? { anchor: target.anchor } : {}),
    body: target.body,
    base_seq: view.seq,
    ...(revision ? { revision } : {}),
  };
}

/** The `submit` action of one Send. */
export function sendAction(
  view: LiveView,
  nodeId: string | undefined,
  idempotencyKey: string,
  values: Record<string, unknown> = {},
): ViewAction {
  const revision = nodeId ? reviewedRevision(view, nodeId) : undefined;
  return {
    action: 'submit',
    values,
    ...(nodeId ? { node_id: nodeId } : {}),
    idempotency_key: idempotencyKey,
    base_seq: view.seq,
    ...(revision ? { revision } : {}),
  };
}

/** The state word of an app card. */
export function appStateWord(view: Pick<LiveView, 'is_closed' | 'is_archived' | 'is_disconnected'>): string {
  if (view.is_closed) return 'Closed';
  if (view.is_archived) return 'Archived';
  if (view.is_disconnected) return 'Disconnected';
  return 'Live';
}

/** The name of an app before its view arrives: the filename without `.app.json`. */
export function appFallbackTitle(filename?: string): string {
  return (filename ?? '').replace(/\.app\.json$/i, '') || 'app';
}
