import type { GatewayClient } from './gateway';
import { onAway, onWake } from './wake';
import type { SseEvent } from './types';

type SessionListener = (event: SseEvent) => void;
type FleetListener = (event: SseEvent) => void;
type FleetStateListener = (streaming: boolean) => void;
type ConnectionListener = (connected: boolean) => void;

const MAX_BUFFERED_EVENTS = 2_048;

// The frames that END a turn. After one of these the turn's story is told by the
// transcript, never by the event buffer.
const TURN_TERMINAL_EVENTS = new Set(['turn.completed', 'turn.failed', 'turn.cancelled']);

// iOS/Android fire visibilitychange, online and pageshow together on wake, and
// each handler calls resync(). Without a floor that is 3 stream teardowns and 3
// reconnects against the gateway in one tick; one is enough.
const RESYNC_MIN_INTERVAL_MS = 1_000;

// A DELIBERATE reconnect (wake, watchdog) replaces a working-enough socket with
// a fresh one and is normally open again in well under a second. Announcing
// `connected = false` for that gap makes the whole app flinch — the header flips
// to "Reconnecting", the "Vis is running: …" ticker is replaced, composer
// affordances change — a visible reflow for something that was never an outage.
// So a graceful restart keeps the last known state and only downgrades if the
// replacement stream has still not opened after this long, i.e. when it really
// IS an outage. Involuntary drops (onError/onClosed) still report immediately.
const RECONNECT_GRACE_MS = 4_000;

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
  private readonly fleetStateListeners = new Set<FleetStateListener>();
  private stopFleetStream: (() => void) | null = null;
  private fleetStreaming = false;
  private readonly connectionListeners = new Set<ConnectionListener>();
  private readonly buffers = new Map<string, SseEvent[]>();
  // Sessions whose last seen lifecycle frame ENDED a turn. Absence means
  // "unknown" (e.g. the hub attached mid-turn), which is treated as in-flight.
  private readonly ended = new Set<string>();
  private stopStream: (() => void) | null = null;
  private connected = false;
  private disposed = false;
  private suspended = false;
  private lastResyncAt = 0;
  private graceTimer: ReturnType<typeof setTimeout> | null = null;
  private readonly stopWake: () => void;
  private readonly stopAway: () => void;
  private supervisor: ReturnType<typeof setInterval> | null = null;

  constructor(client: GatewayClient) {
    this.client = client;
    // The stream is the app's only push channel. Retire it while the webview is
    // still live, BEFORE the OS parks its fetch reader; otherwise a gateway that
    // dies while the app is away leaves WebKit sockets that can block every later
    // request. Wake then opens fresh streams and catches up from their cursors.
    this.stopAway = onAway(() => this.suspend());
    this.stopWake = onWake(() => this.resync());
    // Second safety net, for the case wake events cannot cover: the app stays
    // in the foreground on one session and the stream dies anyway.
    this.supervisor = setInterval(() => {
      this.ensureStream();
      this.ensureFleetStream();
    }, SUPERVISOR_INTERVAL_MS);
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

  /**
   * Has the turn this session was streaming already reached a terminal frame?
   *
   * A screen seeding its running-turn bubble from a cache needs to know that the answer
   * it remembers has since finished — the settled row is in the transcript, and
   * repainting the bubble would render it twice.
   */
  hasEndedTurn(sid: string): boolean {
    return this.ended.has(sid);
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

  /**
   * EVERY session's lifecycle on this machine, not only the visited ones.
   *
   * A list used to learn about a run it had never opened by re-reading its whole
   * window on a timer; the gateway's `?scope=fleet` stream carries those
   * transitions as small frames instead (`GatewayClient.streamFleetStatus`). The
   * stream runs only while somebody is listening — behind an open transcript the
   * list is off the glass, and a machine must not stream to nobody.
   *
   * Frames from the multiplexed SESSION stream still arrive here too: that is the
   * older, narrower channel, and it reaches only what this device has visited.
   */
  subscribeFleet(listener: FleetListener): () => void {
    this.fleetListeners.add(listener);
    this.ensureFleetStream();
    return () => {
      this.fleetListeners.delete(listener);
      if (this.fleetListeners.size === 0) this.stopFleet();
    };
  }

  /**
   * Is the fleet stream delivering right now? A list keeps a poll as its safety
   * net, and this is what tells it whether that net is the only thing holding it
   * up: a live stream earns the slow cadence, a dead one does not.
   */
  subscribeFleetState(listener: FleetStateListener): () => void {
    this.fleetStateListeners.add(listener);
    listener(this.fleetStreaming);
    return () => this.fleetStateListeners.delete(listener);
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
    if (this.disposed) return;
    this.suspended = false;
    const now = Date.now();
    if (now - this.lastResyncAt < RESYNC_MIN_INTERVAL_MS) return;
    this.lastResyncAt = now;
    // The fleet stream parks on a backgrounded webview exactly as the session one
    // does, and it has no cursor to catch up with: replace it and let the list read
    // its window once.
    this.restartFleet();
    if (this.cursors.size === 0) return;
    // Graceful: this is a precaution, not an observed failure — do not paint one.
    this.restart({ graceful: true });
  }

  /** Retire transports before the native webview itself is suspended. */
  private suspend(): void {
    if (this.disposed) return;
    this.suspended = true;
    this.lastResyncAt = 0;
    this.clearGrace();
    const stop = this.stopStream;
    this.stopStream = null;
    stop?.();
    this.setConnected(false);
    this.stopFleet();
  }

  dispose(): void {
    this.disposed = true;
    this.stopAway();
    this.stopWake();
    if (this.supervisor) clearInterval(this.supervisor);
    this.supervisor = null;
    this.stopStream?.();
    this.stopStream = null;
    this.setConnected(false);
    this.sessionListeners.clear();
    this.stopFleet();
    this.fleetListeners.clear();
    this.fleetStateListeners.clear();
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
    if (
      this.disposed ||
      this.suspended ||
      this.stopStream ||
      this.cursors.size === 0
    )
      return;
    this.restart();
  }

  private restart({ graceful = false }: { graceful?: boolean } = {}): void {
    if (this.disposed || this.suspended) return;
    this.stopStream?.();
    this.stopStream = null;
    this.clearGrace();
    if (graceful && this.connected) {
      this.graceTimer = setTimeout(() => {
        this.graceTimer = null;
        this.setConnected(false);
      }, RECONNECT_GRACE_MS);
    } else {
      this.setConnected(false);
    }
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

  /**
   * Start the fleet stream when it is NOT running and somebody is listening. The
   * hub owns its liveness the same way it owns the session stream's: the handle is
   * nulled the moment the retry loop exits, so a dead stream is detectable instead
   * of looking connected.
   */
  private ensureFleetStream(): void {
    if (
      this.disposed ||
      this.suspended ||
      this.stopFleetStream ||
      this.fleetListeners.size === 0
    )
      return;
    const stop = this.client.streamFleetStatus(
      (event) => {
        for (const listener of [...this.fleetListeners]) listener(event);
      },
      {
        onOpen: () => this.setFleetStreaming(true),
        onError: () => this.setFleetStreaming(false),
        onClosed: () => {
          // Only clear the handle when it is still OURS (see `restart`).
          if (this.stopFleetStream !== stop) return;
          this.stopFleetStream = null;
          this.setFleetStreaming(false);
        },
      },
    );
    this.stopFleetStream = stop;
  }

  private stopFleet(): void {
    this.stopFleetStream?.();
    this.stopFleetStream = null;
    this.setFleetStreaming(false);
  }

  private restartFleet(): void {
    if (this.fleetListeners.size === 0) return;
    this.stopFleet();
    this.ensureFleetStream();
  }

  private setFleetStreaming(streaming: boolean): void {
    if (this.fleetStreaming === streaming) return;
    this.fleetStreaming = streaming;
    for (const listener of [...this.fleetStateListeners]) listener(streaming);
  }

  private ingest(event: SseEvent): void {
    const sid = event.session_id ?? event.sid;
    if (!sid) return;
    // `subscription.ready` is the server's verdict about THIS subscribe, not a
    // transcript frame: it names the turn the daemon is running for the session
    // right now, before any replay. It is therefore handed to the session's
    // listeners directly and never buffered, never counted as lifecycle — a
    // screen uses it to decide whether the turn it paints is still the daemon's,
    // which is the one thing a reconnect cannot infer from its own cursor.
    if (event.type === 'subscription.ready') {
      for (const listener of this.sessionListeners.get(sid) ?? []) listener(event);
      return;
    }

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
        // Trim from the front, but NEVER evict the head `turn.started`: that is
        // the frame which RESETS a replaying screen's running-turn bubble. Drop it and a
        // reconnect replays this turn's deltas onto content the screen already
        // rendered, i.e. the same answer twice.
        const head = buffered[0];
        const isHeadStart = head?.type === 'turn.started';
        buffered.splice(
          isHeadStart ? 1 : 0,
          buffered.length - MAX_BUFFERED_EVENTS,
        );
      }
      this.buffers.set(sid, buffered);
    }

    for (const listener of this.sessionListeners.get(sid) ?? []) listener(event);
    for (const listener of this.fleetListeners) listener(event);
  }

  private clearGrace(): void {
    if (this.graceTimer) clearTimeout(this.graceTimer);
    this.graceTimer = null;
  }

  private setConnected(next: boolean): void {
    // Any definitive verdict settles the pending downgrade, in both directions.
    this.clearGrace();
    if (this.connected === next) return;
    this.connected = next;
    for (const listener of this.connectionListeners) listener(next);
  }
}
