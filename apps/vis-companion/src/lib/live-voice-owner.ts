import { useSyncExternalStore } from 'react';
import type { LiveVoiceSession, LiveVoiceView } from './live-voice';

/**
 * The one hands-free voice of this device. A session screen starts it, but leaving
 * that screen does not end it: only End, a lock-screen control or a failure ends it.
 */
export type LiveVoiceOwner = {
  base: string;
  sid: string;
  /** The session title, so a surface outside the session can name it. */
  title: string;
  session: LiveVoiceSession;
  view: LiveVoiceView;
};

let owner: LiveVoiceOwner | null = null;
const listeners = new Set<() => void>();

function publish(next: LiveVoiceOwner): void {
  owner = next;
  for (const listener of listeners) listener();
}
function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}
export function currentLiveVoice(): LiveVoiceOwner | null {
  return owner;
}
export function isLiveVoiceActive(value: LiveVoiceOwner | null): value is LiveVoiceOwner {
  return value !== null && (value.view.phase === 'starting' || value.view.phase === 'live');
}
export function isLiveVoiceOf(value: LiveVoiceOwner | null, base: string, sid: string): boolean {
  return value !== null && value.base === base && value.sid === sid;
}
/** A device runs one voice. False when another voice is still active. */
export function claimLiveVoice(next: LiveVoiceOwner): boolean {
  if (isLiveVoiceActive(owner) && owner.session !== next.session) return false;
  publish(next);
  return true;
}
export function updateLiveVoice(
  session: LiveVoiceSession,
  patch: Partial<Pick<LiveVoiceOwner, 'title' | 'view'>>,
): void {
  if (owner?.session === session) publish({ ...owner, ...patch });
}
export function useLiveVoice(): LiveVoiceOwner | null {
  return useSyncExternalStore(subscribe, currentLiveVoice, currentLiveVoice);
}
