import type { GatewayClient } from './gateway';
import type { SseEvent } from './types';

type SessionListener = (event: SseEvent) => void;
type FleetListener = (event: SseEvent) => void;
type ConnectionListener = (connected: boolean) => void;

const MAX_BUFFERED_EVENTS = 2_048;

/**
 * One long-lived, multiplexed gateway subscription for every visited session.
 * Session views may mount/unmount without stopping their stream; a bounded
 * current-turn buffer lets a revisited chat immediately catch up.
 */
export class SessionSubscriptionHub {
  private readonly client: GatewayClient;
  private readonly watched = new Set<string>();
  private readonly cursors = new Map<string, number>();
  private readonly sessionListeners = new Map<string, Set<SessionListener>>();
  private readonly fleetListeners = new Set<FleetListener>();
  private readonly connectionListeners = new Set<ConnectionListener>();
  private readonly buffers = new Map<string, SseEvent[]>();
  private stopStream: (() => void) | null = null;
  private connected = false;
  private disposed = false;

  constructor(client: GatewayClient) {
    this.client = client;
  }

  watchSessions(sessionIds: Iterable<string>): void {
    let changed = false;
    for (const sid of sessionIds) {
      if (!sid || this.watched.has(sid)) continue;
      this.watched.add(sid);
      // -1 is the gateway's efficient live-only sentinel. subscription.ready
      // replaces it with the effective cursor before normal events arrive.
      this.cursors.set(sid, -1);
      changed = true;
    }
    if (changed) this.restart();
  }

  isWatching(sid: string): boolean {
    return this.watched.has(sid);
  }

  watchedSessionIds(): ReadonlySet<string> {
    return this.watched;
  }

  subscribeSession(
    sid: string,
    listener: SessionListener,
    { replay = true }: { replay?: boolean } = {},
  ): () => void {
    let listeners = this.sessionListeners.get(sid);
    if (!listeners) {
      listeners = new Set();
      this.sessionListeners.set(sid, listeners);
    }
    listeners.add(listener);
    this.watchSessions([sid]);

    if (replay) {
      const buffered = this.buffers.get(sid) ?? [];
      for (const event of buffered) listener(event);
    }

    return () => {
      const current = this.sessionListeners.get(sid);
      current?.delete(listener);
      if (current?.size === 0) this.sessionListeners.delete(sid);
      // Deliberately keep sid watched: visited sessions remain subscribed.
    };
  }

  subscribeFleet(listener: FleetListener): () => void {
    this.fleetListeners.add(listener);
    return () => this.fleetListeners.delete(listener);
  }

  subscribeConnection(listener: ConnectionListener): () => void {
    this.connectionListeners.add(listener);
    listener(this.connected);
    return () => this.connectionListeners.delete(listener);
  }

  dispose(): void {
    this.disposed = true;
    this.stopStream?.();
    this.stopStream = null;
    this.setConnected(false);
    this.sessionListeners.clear();
    this.fleetListeners.clear();
    this.connectionListeners.clear();
    this.buffers.clear();
  }

  private restart(): void {
    if (this.disposed) return;
    this.stopStream?.();
    this.stopStream = null;
    this.setConnected(false);
    if (this.cursors.size === 0) return;
    this.stopStream = this.client.streamSessionEvents(
      this.cursors,
      (event) => this.ingest(event),
      {
        onOpen: () => this.setConnected(true),
        onError: () => this.setConnected(false),
      },
    );
  }

  private ingest(event: SseEvent): void {
    const sid = event.session_id ?? event.sid;
    if (!sid) return;
    if (event.type === 'subscription.ready') return;

    const previous = event.type === 'turn.started' ? [] : (this.buffers.get(sid) ?? []);
    const buffered = [...previous, event];
    if (buffered.length > MAX_BUFFERED_EVENTS) {
      buffered.splice(0, buffered.length - MAX_BUFFERED_EVENTS);
    }
    this.buffers.set(sid, buffered);

    for (const listener of this.sessionListeners.get(sid) ?? []) listener(event);
    for (const listener of this.fleetListeners) listener(event);
  }

  private setConnected(next: boolean): void {
    if (this.connected === next) return;
    this.connected = next;
    for (const listener of this.connectionListeners) listener(next);
  }
}
