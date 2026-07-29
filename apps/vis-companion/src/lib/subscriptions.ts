import type { GatewayClient } from './gateway';
import { onWake } from './wake';
import type { SseEvent } from './types';

type SessionListener = (event: SseEvent) => void;
type FleetListener = (event: SseEvent) => void;
type ConnectionListener = (connected: boolean) => void;

const MAX_BUFFERED_EVENTS = 2_048;

// The frames that END a turn. After one of these the turn's story is told by the
// transcript, never by the event buffer.
const TURN_TERMINAL_EVENTS = new Set(['turn.completed', 'turn.failed', 'turn.cancelled']);

// iOS/Android fire visibilitychange, online and pageshow together on wake, and
// each handler calls resync(). Without a floor that is 3 stream teardowns and 3
// reconnects against the gateway in one tick; one is enough.
const RESYNC_MIN_INTERVAL_MS = 1_000;

// The stream's supervisor tick. `streamSessionEvents` retries on its own, but it
// can still END (the caller aborted, or it gave up); nothing else in the app
// notices, and while you stay inside one session NO wake event ever fires to
// call resync(). Without this the chat you are typing in silently stops
// updating until you leave and re-open it.
const SUPERVISOR_INTERVAL_MS = 10_000;

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
  // Sessions whose last seen lifecycle frame ENDED a turn. Absence means
  // "unknown" (e.g. the hub attached mid-turn), which is treated as in-flight.
  private readonly ended = new Set<string>();
  private stopStream: (() => void) | null = null;
  private connected = false;
  private disposed = false;
  private lastResyncAt = 0;
  private readonly stopWake: () => void;
  private supervisor: ReturnType<typeof setInterval> | null = null;

  constructor(client: GatewayClient) {
    this.client = client;
    // The stream is the app's only push channel, and a backgrounded webview
    // parks its fetch body reader without ever erroring. Reconnect on every
    // wake — no screen has to remember to ask, so a frozen socket can never
    // outlive the resume and force an app restart.
    this.stopWake = onWake(() => this.resync());
    // Second safety net, for the case wake events cannot cover: the app stays
    // in the foreground on one session and the stream dies anyway.
    this.supervisor = setInterval(() => this.ensureStream(), SUPERVISOR_INTERVAL_MS);
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
    else this.ensureStream();
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

  /**
   * Force the multiplexed SSE stream to reconnect NOW, resuming each watched
   * session at its last-seen cursor. Safety net for iOS/Android backgrounding:
   * a fetch-body reader can silently park on wake with no error firing, so a
   * visibility/online/pageshow handler calls this to guarantee catch-up.
   */
  resync(): void {
    if (this.disposed || this.cursors.size === 0) return;
    const now = Date.now();
    if (now - this.lastResyncAt < RESYNC_MIN_INTERVAL_MS) return;
    this.lastResyncAt = now;
    this.restart();
  }

  dispose(): void {
    this.disposed = true;
    this.stopWake();
    if (this.supervisor) clearInterval(this.supervisor);
    this.supervisor = null;
    this.stopStream?.();
    this.stopStream = null;
    this.setConnected(false);
    this.sessionListeners.clear();
    this.fleetListeners.clear();
    this.connectionListeners.clear();
    this.buffers.clear();
    this.ended.clear();
  }

  /**
   * Start the stream when it is NOT running — it ended, or never started. The
   * hub owns liveness: `stopStream` is nulled the moment the retry loop exits,
   * so a dead stream is always detectable instead of looking connected.
   */
  private ensureStream(): void {
    if (this.disposed || this.stopStream || this.cursors.size === 0) return;
    this.restart();
  }

  private restart(): void {
    if (this.disposed) return;
    this.stopStream?.();
    this.stopStream = null;
    this.setConnected(false);
    if (this.cursors.size === 0) return;
    const stop = this.client.streamSessionEvents(
      this.cursors,
      (event) => this.ingest(event),
      {
        onOpen: () => this.setConnected(true),
        onError: () => this.setConnected(false),
        // Only clear the handle when it is still OURS: a later restart() has
        // already installed its own stream and must not be torn down by the
        // old one's exit.
        onClosed: () => {
          if (this.stopStream !== stop) return;
          this.stopStream = null;
          this.setConnected(false);
        },
      },
    );
    this.stopStream = stop;
  }

  private ingest(event: SseEvent): void {
    const sid = event.session_id ?? event.sid;
    if (!sid) return;
    if (event.type === 'subscription.ready') return;

    // The buffer replays the turn that is STILL STREAMING to a screen that was
    // reopened mid-flight. A FINISHED turn must never be replayed: its
    // `turn.started` flips the composer to "running" and its progress frames
    // paint an activity line, so reopening an idle session showed work that had
    // long completed — corrected only by an async refetch, and not at all when
    // the terminal frame itself was missed. So: a turn's frames are buffered
    // between `turn.started` and its terminal frame, and the terminal frame
    // drops the buffer outright.
    if (TURN_TERMINAL_EVENTS.has(event.type)) {
      this.ended.add(sid);
      this.buffers.delete(sid);
    } else if (event.type === 'turn.started') {
      this.ended.delete(sid);
      this.buffers.set(sid, [event]);
    } else if (!this.ended.has(sid)) {
      const buffered = [...(this.buffers.get(sid) ?? []), event];
      if (buffered.length > MAX_BUFFERED_EVENTS) {
        buffered.splice(0, buffered.length - MAX_BUFFERED_EVENTS);
      }
      this.buffers.set(sid, buffered);
    }

    for (const listener of this.sessionListeners.get(sid) ?? []) listener(event);
    for (const listener of this.fleetListeners) listener(event);
  }

  private setConnected(next: boolean): void {
    if (this.connected === next) return;
    this.connected = next;
    for (const listener of this.connectionListeners) listener(next);
  }
}
