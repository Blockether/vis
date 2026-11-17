import type { LiveVoiceAudio } from './live-voice-audio';
import { VoiceEchoGuard, VoiceEndpoint } from './live-voice-dsp';
import {
  LIVE_VOICE_LIMITS as L,
  decodeVoiceFrame,
  encodeVoiceFrame,
  encodeLiveVoiceControl,
  parseLiveVoiceEvent,
} from './live-voice-protocol';
import type {
  LiveVoiceCapabilities,
  LiveVoiceSnapshot,
  LiveVoiceStart,
  LiveVoiceState,
  VoiceAdmissionReceipt,
} from './live-voice-protocol';

export interface LiveVoiceGateway {
  liveVoiceCapabilities(signal?: AbortSignal): Promise<LiveVoiceCapabilities>;
  startLiveVoice(sid: string, clientId: string, muted: boolean): Promise<LiveVoiceStart>;
  liveVoiceSnapshot(sid: string, vid: string): Promise<LiveVoiceSnapshot>;
  endLiveVoice(sid: string, vid: string): Promise<unknown>;
  liveVoiceMediaUrl(path: string): string;
}
export type LiveVoiceView = {
  phase: 'starting' | 'live' | 'closed' | 'error';
  muted: boolean;
  outputMuted: boolean;
  transcript: string;
  final: boolean;
  state: LiveVoiceState | null;
  turnId: string | null;
  message: string | null;
  reconciled: boolean;
  receipts: VoiceAdmissionReceipt[];
};
type Segment = {
  epoch: number;
  id: number;
  sequence: number;
  received: number;
  decoded: number;
  scheduled: number;
  played: number;
  finished?: number;
};
type VoiceSocket = Pick<
  WebSocket,
  | 'readyState'
  | 'bufferedAmount'
  | 'binaryType'
  | 'send'
  | 'close'
  | 'onopen'
  | 'onmessage'
  | 'onerror'
  | 'onclose'
>;
export type VoiceSocketFactory = (url: string, protocols: string[]) => VoiceSocket;

/** Media never admits a provisional transcript or cancels an ordinary agent task. */
export class LiveVoiceSession {
  private readonly gateway: LiveVoiceGateway;
  private readonly sid: string;
  private readonly audio: LiveVoiceAudio;
  private readonly socketFactory: VoiceSocketFactory;
  private readonly changed: (view: LiveVoiceView) => void;
  private view: LiveVoiceView;
  private socket: VoiceSocket | null = null;
  private opened: LiveVoiceStart | null = null;
  private stopped = false;
  private stopping = false;
  private minimumCaptureGeneration = 0;
  private highestSegmentId = -1;
  private started = false;
  private ready = false;
  private captureGeneration: number | null = null;
  private endpoint = new VoiceEndpoint();
  private echo = new VoiceEchoGuard(this.endpoint.threshold);
  private utteranceId = 0;
  private uncertainAdmissions = new Set<number>();
  private sequence = 0;
  private sampleOffset = 0;
  private suppressed = false;
  private interruptEpoch: number | null = null;
  private segments = new Map<number, Segment>();
  private transcriptRevision = new Map<number, number>();
  private ownedTurns = new Set<string>();
  private heartbeat: ReturnType<typeof setInterval> | null = null;
  private connectTimer: ReturnType<typeof setTimeout> | null = null;
  private lastServerMessage = 0;

  constructor(options: {
    gateway: LiveVoiceGateway;
    sid: string;
    audio: LiveVoiceAudio;
    muted?: boolean;
    changed: (view: LiveVoiceView) => void;
    socketFactory?: VoiceSocketFactory;
  }) {
    this.gateway = options.gateway;
    this.sid = options.sid;
    this.audio = options.audio;
    this.changed = options.changed;
    this.socketFactory =
      options.socketFactory ?? ((url, protocols) => new WebSocket(url, protocols));
    this.view = {
      phase: 'starting',
      muted: options.muted ?? false,
      outputMuted: false,
      transcript: '',
      final: false,
      state: null,
      turnId: null,
      message: null,
      reconciled: false,
      receipts: [],
    };
  }
  private update(patch: Partial<LiveVoiceView>): void {
    this.view = { ...this.view, ...patch };
    this.changed(this.view);
  }
  async start(): Promise<void> {
    if (this.started) return;
    this.started = true;
    this.update({ phase: 'starting' });
    try {
      await this.audio.prepare((message) => {
        void this.stop(message, true);
      });
      if (this.stopped) return;
      const capabilities = await this.gateway.liveVoiceCapabilities();
      if (this.stopped) return;
      if (!capabilities.enabled || !capabilities.available)
        throw new Error(capabilities.reason ?? 'Local live speech is unavailable.');
      const opened = await this.gateway.startLiveVoice(
        this.sid,
        crypto.randomUUID(),
        this.view.muted,
      );
      this.opened = opened;
      if (this.stopped) {
        await this.gateway.endLiveVoice(this.sid, opened.voice_session_id);
        return;
      }
      if (opened.state.voice_session_id !== opened.voice_session_id)
        throw new Error('Voice session identity does not match.');
      const socket = this.socketFactory(this.gateway.liveVoiceMediaUrl(opened.media_path), [
        'vis-voice.v1',
        `vis-ticket.${opened.ticket}`,
      ]);
      this.socket = socket;
      socket.binaryType = 'arraybuffer';
      socket.onopen = () => this.connected();
      socket.onmessage = (event) => {
        if (this.stopped || this.socket !== socket) return;
        try {
          this.receive(event.data);
        } catch (error) {
          void this.stop(error instanceof Error ? error.message : 'Invalid voice media.', true);
        }
      };
      socket.onerror = () => {
        void this.stop('Voice connection failed. Start again to reconnect.', true);
      };
      socket.onclose = () => {
        void this.stop('Voice disconnected. Start again to reconnect.', true);
      };
      this.connectTimer = setTimeout(() => {
        void this.stop('Voice connection timed out.', true);
      }, L.ticket_ms);
      if (socket.readyState === 1) this.connected();
    } catch (error) {
      await this.stop(error instanceof Error ? error.message : 'Local live speech failed.', true);
    }
  }
  private connected(): void {
    if (this.stopped || this.heartbeat) return;
    if (this.connectTimer) clearTimeout(this.connectTimer);
    this.connectTimer = null;
    this.lastServerMessage = Date.now();
    this.heartbeat = setInterval(() => {
      if (Date.now() - this.lastServerMessage >= L.lease_ms) {
        void this.stop('Voice lease expired. Start again to reconnect.', true);
        return;
      }
      try {
        this.send({ type: 'heartbeat' });
      } catch {
        void this.stop('Voice connection failed.', true);
      }
    }, L.heartbeat_ms);
  }
  private send(value: unknown): void {
    const socket = this.socket;
    if (!socket || socket.readyState !== 1 || this.stopped)
      throw new Error('Voice is disconnected.');
    const data = value instanceof ArrayBuffer ? value : encodeLiveVoiceControl(value);
    const bytes =
      typeof data === 'string' ? new TextEncoder().encode(data).length : data.byteLength;
    if (
      socket.bufferedAmount + bytes >
      ((L.header_bytes + L.frame_samples * 2) * L.input_buffer_ms) / L.frame_ms
    )
      throw new Error('Voice input queue is full.');
    socket.send(data);
  }
  private receive(data: unknown): void {
    this.lastServerMessage = Date.now();
    if (data instanceof ArrayBuffer) {
      this.output(data);
      return;
    }
    if (typeof data !== 'string') throw new Error('Invalid voice message.');
    const event = parseLiveVoiceEvent(data);
    switch (event.type) {
      case 'session.ready':
        if (this.ready) throw new Error('Duplicate voice handshake.');
        this.ready = true;
        if (this.connectTimer) clearTimeout(this.connectTimer);
        this.connectTimer = null;
        this.update({ phase: 'live' });
        this.applyState(event.state);
        if (this.view.outputMuted) this.send({ type: 'output.mute', muted: true });
        break;
      case 'session.state':
        this.applyState(event.state);
        break;
      case 'task.cancellation':
        this.update({ message: event.message });
        break;
      case 'output.interrupted':
        if (!this.view.state || event.output_epoch < this.view.state.output_epoch) return;
        if (!this.suppressed && event.output_epoch === this.view.state.output_epoch) return;
        if (this.interruptEpoch !== null && event.output_epoch <= this.interruptEpoch) return;
        this.audio.stopOutput();
        this.echo.stopOutput();
        this.segments.clear();
        this.suppressed = false;
        this.interruptEpoch = null;
        this.update({
          state: {
            ...this.view.state,
            output_epoch: event.output_epoch,
            output: 'interrupted',
          },
        });
        break;
      case 'transcript': {
        const revision = this.transcriptRevision.get(event.utterance_id) ?? -1;
        if (event.utterance_id > this.utteranceId || event.revision <= revision) return;
        this.transcriptRevision.set(event.utterance_id, event.revision);
        // Only confirmed final controls bypass task admission on the gateway.
        const control = event.text
          .trim()
          .replace(/[.!?]+$/, '')
          .toLowerCase();
        if (
          event.final &&
          ['stop speaking', 'end voice mode', 'cancel that task'].includes(control)
        ) {
          this.uncertainAdmissions.delete(event.utterance_id);
        }
        if (this.transcriptRevision.size > 32)
          this.transcriptRevision.delete(this.transcriptRevision.keys().next().value!);
        if (event.utterance_id === this.utteranceId)
          this.update({ transcript: event.text, final: event.final });
        break;
      }
      case 'utterance.rejected':
        this.uncertainAdmissions.delete(event.utterance_id);
        this.update({
          message: 'Speech was not admitted. Input is paused. Unmute before trying again.',
        });
        break;
      case 'utterance.admitted':
        this.uncertainAdmissions.delete(event.utterance_id);
        this.ownedTurns.add(event.turn_id);
        if (this.ownedTurns.size > 128)
          this.ownedTurns.delete(this.ownedTurns.values().next().value!);
        this.update({ turnId: event.turn_id });
        break;
      case 'speech.committed':
        if (
          this.suppressed ||
          this.view.outputMuted ||
          event.output_epoch !== this.view.state?.output_epoch
        )
          return;
        if (!this.ownedTurns.has(event.turn_id))
          throw new Error('Voice output belongs to another turn.');
        if (event.segment_id <= this.highestSegmentId) return;
        this.highestSegmentId = event.segment_id;
        if (this.segments.size >= L.max_pending_segments)
          throw new Error('Too many pending speech segments.');
        this.segments.set(event.segment_id, {
          epoch: event.output_epoch,
          id: event.segment_id,
          sequence: 0,
          received: 0,
          decoded: 0,
          scheduled: 0,
          played: 0,
        });
        break;
      case 'output.finished': {
        if (event.output_epoch !== this.view.state?.output_epoch || this.suppressed) return;
        const segment = this.segments.get(event.segment_id);
        if (!segment || segment.received !== event.sample_count)
          throw new Error('Incomplete voice output.');
        segment.finished = event.sample_count;
        this.retire(segment);
        break;
      }
      case 'error':
        if (event.fatal) void this.stop(event.message, true);
        else {
          if (event.code === 'input_overflow') {
            this.releaseCapture();
            this.captureGeneration = null;
            this.endpoint.reset();
            this.update({ muted: true });
          }
          this.update({ message: event.message });
        }
        break;
      case 'session.closed':
        void this.stop(event.reason);
        break;
    }
  }
  private applyState(state: LiveVoiceState): void {
    if (!this.opened || state.voice_session_id !== this.opened.voice_session_id)
      throw new Error('Voice session identity does not match.');
    const previous = this.view.state;
    if (
      previous &&
      (state.capture_generation < previous.capture_generation ||
        state.output_epoch < previous.output_epoch)
    )
      return;
    if (previous && state.output_epoch !== previous.output_epoch) {
      this.audio.stopOutput();
      this.echo.stopOutput();
      this.segments.clear();
    }
    this.update({ state });
    if (!this.ready) return;
    if (this.view.muted || state.capture !== 'live') {
      this.releaseCapture();
      this.captureGeneration = null;
      this.endpoint.reset();
      if (this.view.muted && state.capture === 'live')
        this.send({ type: 'capture.mute', muted: true });
      if (!this.view.muted && state.capture === 'muted')
        this.send({ type: 'capture.mute', muted: false });
      return;
    }
    if (
      state.capture_generation < this.minimumCaptureGeneration ||
      this.captureGeneration === state.capture_generation
    )
      return;
    if (this.audio.captureRestartsOutput) this.releaseCapture(true);
    this.endpoint.reset();
    this.echo.resetCapture();
    this.captureGeneration = state.capture_generation;
    const generation = state.capture_generation;
    void this.audio
      .capture((samples) => {
        if (this.stopped || this.view.muted || this.captureGeneration !== generation) return;
        try {
          this.input(samples, generation);
        } catch (error) {
          void this.stop(error instanceof Error ? error.message : 'Voice capture failed.', true);
        }
      })
      .catch((error: unknown) => {
        if (!this.stopped && this.captureGeneration === generation)
          void this.stop(error instanceof Error ? error.message : 'Microphone unavailable.', true);
      });
  }
  private input(samples: Int16Array, generation: number): void {
    for (const event of this.endpoint.push(this.echo.filter(samples))) {
      if (event.type === 'start') {
        this.interrupt('barge_in');
        if (this.stopped) return;
        this.utteranceId++;
        this.sequence = this.sampleOffset = 0;
        this.update({ transcript: '', final: false });
        this.send({
          type: 'utterance.start',
          capture_generation: generation,
          utterance_id: this.utteranceId,
        });
      } else if (event.type === 'end') {
        this.uncertainAdmissions.add(this.utteranceId);
        this.send({
          type: 'utterance.end',
          capture_generation: generation,
          utterance_id: this.utteranceId,
        });
      } else {
        this.send(
          encodeVoiceFrame({
            kind: 'input',
            generation,
            streamId: this.utteranceId,
            sequence: this.sequence++,
            sampleOffset: this.sampleOffset,
            samples: event.samples,
          }),
        );
        this.sampleOffset += event.samples.length;
      }
    }
  }
  private output(data: ArrayBuffer): void {
    const frame = decodeVoiceFrame(data);
    if (frame.kind !== 'output') throw new Error('Unexpected input media from the gateway.');
    if (
      this.suppressed ||
      this.view.outputMuted ||
      frame.generation !== this.view.state?.output_epoch
    )
      return;
    const segment = this.segments.get(frame.streamId);
    if (!segment) throw new Error('Voice output has no committed segment.');
    if (
      frame.sequence < segment.sequence &&
      frame.sampleOffset + frame.samples.length <= segment.received
    )
      return;
    if (frame.sequence !== segment.sequence || frame.sampleOffset !== segment.received)
      throw new Error('Voice output is out of order.');
    segment.sequence++;
    segment.received += frame.samples.length;
    segment.decoded = segment.received;
    const start = frame.sampleOffset;
    const rendered = this.echo.submitted(frame.samples, this.audio.outputReferenceStartMs);
    this.audio.play(frame.samples, (count) => {
      if (this.stopped || this.segments.get(segment.id) !== segment) return;
      rendered(count);
      segment.played = Math.max(segment.played, start + count);
      try {
        this.receipt(segment);
      } catch {
        void this.stop('Voice playback receipt failed.', true);
      }
      this.retire(segment);
    });
    segment.scheduled = segment.received;
    this.receipt(segment);
  }
  private receipt(segment: Segment): void {
    this.send({
      type: 'playback.receipt',
      output_epoch: segment.epoch,
      segment_id: segment.id,
      received: segment.received,
      decoded: segment.decoded,
      scheduled: segment.scheduled,
      played: segment.played,
    });
  }
  private retire(segment: Segment): void {
    if (segment.finished !== undefined && segment.played >= segment.finished)
      this.segments.delete(segment.id);
  }
  interrupt(reason: 'barge_in' | 'stop_speech' = 'stop_speech'): void {
    this.audio.stopOutput();
    this.echo.stopOutput();
    this.segments.clear();
    if (!this.ready || !this.view.state || this.stopped || this.suppressed) return;
    this.suppressed = true;
    this.interruptEpoch = this.view.state.output_epoch;
    try {
      this.send({
        type: 'output.interrupt',
        output_epoch: this.interruptEpoch,
        reason,
      });
    } catch {
      void this.stop('Voice interruption could not be sent.', true);
    }
  }
  private releaseCapture(restartsOutput = this.captureGeneration !== null): void {
    // Native mute releases the microphone by changing the audio session.
    // Invalidate old output before that change instead of inventing playback credit.
    if (this.audio.captureRestartsOutput && restartsOutput && this.segments.size)
      this.interrupt();
    this.audio.stopCapture();
  }
  setMuted(muted: boolean): void {
    if (muted === this.view.muted) return;
    this.update({ muted });
    this.minimumCaptureGeneration = (this.view.state?.capture_generation ?? -1) + 1;
    this.releaseCapture();
    this.captureGeneration = null;
    this.endpoint.reset();
    if (!this.ready || this.stopped) return;
    try {
      this.send({ type: 'capture.mute', muted });
    } catch {
      void this.stop('Voice mute failed.', true);
    }
    // Only a new server state can start capture after an explicit unmute.
  }
  setOutputMuted(muted: boolean): void {
    this.update({ outputMuted: muted });
    if (!this.ready || this.stopped) return;
    try {
      if (muted) this.interrupt();
      this.send({ type: 'output.mute', muted });
    } catch {
      void this.stop('Voice output mute failed.', true);
    }
  }
  cancelTask(turnId: string): void {
    if (!this.ownedTurns.has(turnId)) return;
    try {
      this.send({ type: 'task.cancel', turn_id: turnId });
    } catch {
      void this.stop('Task cancellation could not be sent.', true);
    }
  }
  async stop(message: string | null = null, failed = false): Promise<void> {
    if (this.stopped || this.stopping) return;
    this.stopping = true;
    // Flush conservative receipts before invalidating this connection.
    this.audio.stopOutput();
    if (this.socket?.readyState === 1) {
      try {
        this.send({ type: 'session.end' });
      } catch {
        /* HTTP cleanup follows. */
      }
    }
    this.stopped = true;
    this.audio.close();
    this.endpoint.reset();
    this.echo.reset();
    this.segments.clear();
    this.ownedTurns.clear();
    if (this.heartbeat) clearInterval(this.heartbeat);
    if (this.connectTimer) clearTimeout(this.connectTimer);
    this.heartbeat = this.connectTimer = null;
    if (this.socket) {
      this.socket.onopen = this.socket.onmessage = this.socket.onerror = this.socket.onclose = null;
      this.socket.close();
      this.socket = null;
    }
    this.update({ phase: failed ? 'error' : 'closed', message });
    if (this.opened) {
      const vid = this.opened.voice_session_id;
      try {
        // Reconcile admissions for display only. Never replay an uncertain utterance.
        const snapshot = await this.gateway.liveVoiceSnapshot(this.sid, vid);
        const receipts = snapshot.receipts.filter((receipt) => receipt.voice_session_id === vid);
        for (const receipt of receipts) this.uncertainAdmissions.delete(receipt.utterance_id);
        this.update({
          reconciled: this.uncertainAdmissions.size === 0,
          receipts,
        });
      } catch {
        this.update({ reconciled: false });
      }
      try {
        await this.gateway.endLiveVoice(this.sid, vid);
      } catch {
        /* The gateway lease also expires. */
      }
    }
  }
}
