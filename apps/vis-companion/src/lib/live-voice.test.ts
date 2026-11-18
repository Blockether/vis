import { afterEach, describe, expect, it, vi } from 'vitest';
import { LiveVoiceSession } from './live-voice';
import type { LiveVoiceGateway, LiveVoiceView } from './live-voice';
import type { LiveVoiceAudio, LiveVoiceControl } from './live-voice-audio';
import { encodeVoiceFrame, LIVE_VOICE_LIMITS as L } from './live-voice-protocol';
import type { LiveVoiceState } from './live-voice-protocol';

const vid = '00000000-0000-4000-8000-000000000001';
const tid = '00000000-0000-4000-8000-000000000002';
const state: LiveVoiceState = {
  voice_session_id: vid,
  capture_generation: 1,
  output_epoch: 1,
  capture: 'live',
  utterance: 'idle',
  agent: 'idle',
  output: 'idle',
};
const sessions: LiveVoiceSession[] = [];
afterEach(async () => {
  for (const session of sessions.splice(0)) await session.stop();
  vi.useRealTimers();
});
function playbackFrame(frame: number): Int16Array {
  return Int16Array.from({ length: L.frame_samples }, (_, index) => {
    const t = frame * L.frame_samples + index;
    return Math.round(6500 * Math.sin(t * 0.073 + 2 * Math.sin(t * 0.0031)) +
      3000 * Math.sin(t * 0.117 + Math.sin(t * 0.0013)));
  });
}
function harness(muted = false) {
  vi.useFakeTimers();
  let capture: ((samples: Int16Array) => void) | null = null;
  const progress: ((count: number) => void)[] = [];
  const audio: LiveVoiceAudio = {
    prepare: vi.fn(async () => undefined),
    capture: vi.fn(async (callback) => {
      capture = callback;
    }),
    stopCapture: vi.fn(() => {
      capture = null;
    }),
    play: vi.fn((_samples, callback) => {
      progress.push(callback);
    }),
    stopOutput: vi.fn(),
    close: vi.fn(),
  };
  const gateway: LiveVoiceGateway = {
    liveVoiceCapabilities: vi.fn(async () => ({
      enabled: true,
      available: true,
    })),
    startLiveVoice: vi.fn(async (_sid, clientId, isMuted) => ({
      voice_session_id: vid,
      client_id: clientId,
      media_path: '/v1/sessions/s/voice/live/v/media',
      ticket: 'one-use',
      state: { ...state, capture: isMuted ? 'muted' : 'live' },
      limits: {},
      capabilities: { enabled: true, available: true },
    })),
    liveVoiceSnapshot: vi.fn(async () => ({
      state,
      receipts: [
        {
          voice_session_id: vid,
          utterance_id: 1,
          idempotency_key: 'v:1',
          text: 'Hello',
          revision: 1,
          turn_id: tid,
          status: 'running' as const,
        },
      ],
    })),
    endLiveVoice: vi.fn(async () => undefined),
    liveVoiceMediaUrl: vi.fn((path) => `wss://gateway.example.com${path}`),
  };
  const sent: (string | ArrayBuffer)[] = [];
  const socket = {
    readyState: 1,
    bufferedAmount: 0,
    binaryType: 'arraybuffer',
    onopen: null,
    onmessage: null as ((event: { data: unknown }) => void) | null,
    onclose: null as (() => void) | null,
    onerror: null as (() => void) | null,
    send: vi.fn((data: string | ArrayBuffer) => sent.push(data)),
    close: vi.fn(),
  };
  const factory = vi.fn(() => socket as unknown as WebSocket);
  const views: LiveVoiceView[] = [];
  const session = new LiveVoiceSession({
    gateway,
    sid: 's',
    audio,
    muted,
    socketFactory: factory,
    changed: (view) => views.push(view),
  });
  sessions.push(session);
  const emit = (event: object) => socket.onmessage?.({ data: JSON.stringify(event) });
  const ready = () =>
    emit({
      type: 'session.ready',
      state: { ...state, capture: muted ? 'muted' : 'live' },
      capabilities: {},
      limits: {},
    });
  const frames = (count: number, value: number) => {
    for (let i = 0; i < count; i++) capture?.(new Int16Array(480).fill(value));
  };
  const controls = () =>
    sent
      .filter((item): item is string => typeof item === 'string')
      .map((text) => JSON.parse(text) as Record<string, unknown>);
  const output = (epoch = 1, sequence = 0, segment = 1, samples: Int16Array = new Int16Array(480)) =>
    socket.onmessage?.({
      data: encodeVoiceFrame({
        kind: 'output',
        generation: epoch,
        streamId: segment,
        sequence,
        sampleOffset: sequence * 480,
        samples,
      }),
    });
  const commit = (epoch = 1, segment = 1) => {
    emit({
      type: 'utterance.admitted',
      utterance_id: 1,
      turn_id: tid,
      idempotency_key: 'v:1',
      status: 'running',
    });
    emit({
      type: 'speech.committed',
      output_epoch: epoch,
      segment_id: segment,
      turn_id: tid,
      text: 'Hello',
      source: 'final',
    });
  };
  return {
    session,
    gateway,
    audio,
    socket,
    factory,
    views,
    emit,
    ready,
    frames,
    input: (samples: Int16Array) => capture?.(samples),
    controls,
    output,
    commit,
    progress,
    sent,
  };
}

describe('hands-free live session', () => {
  it('keeps native output playing through repeated muted states', async () => {
    const h = harness(true);
    Object.assign(h.audio, { captureRestartsOutput: true });
    await h.session.start();
    h.ready();
    h.commit();
    h.output();
    const stops = vi.mocked(h.audio.stopOutput).mock.calls.length;
    h.emit({
      type: 'session.state',
      state: { ...state, capture: 'muted', output: 'playing' },
    });
    expect(h.audio.stopOutput).toHaveBeenCalledTimes(stops);
    expect(h.controls().filter((event) => event.type === 'output.interrupt')).toEqual([]);
    h.progress[0](480);
    expect(h.controls().at(-1)).toMatchObject({ type: 'playback.receipt', played: 480 });
    expect(h.audio.capture).not.toHaveBeenCalled();
  });

  it('invalidates native output before mute releases the microphone', async () => {
    const h = harness();
    Object.assign(h.audio, { captureRestartsOutput: true });
    await h.session.start();
    h.ready();
    h.commit();
    h.output();
    h.session.setMuted(true);
    expect(h.controls().slice(-2)).toEqual([
      { type: 'output.interrupt', output_epoch: 1, reason: 'stop_speech' },
      { type: 'capture.mute', muted: true },
    ]);
    const outputStop = vi.mocked(h.audio.stopOutput).mock.invocationCallOrder.at(-1)!;
    const captureStop = vi.mocked(h.audio.stopCapture).mock.invocationCallOrder.at(-1)!;
    expect(outputStop).toBeLessThan(captureStop);
    const receipts = h.controls().filter((event) => event.type === 'playback.receipt');
    h.progress[0](480);
    expect(h.controls().filter((event) => event.type === 'playback.receipt')).toEqual(receipts);
  });

  it('invalidates native output when an acknowledged unmute restarts capture', async () => {
    const h = harness(true);
    Object.assign(h.audio, { captureRestartsOutput: true });
    await h.session.start();
    h.ready();
    h.commit();
    h.output();
    h.session.setMuted(false);
    expect(h.audio.capture).not.toHaveBeenCalled();
    expect(h.controls().filter((event) => event.type === 'output.interrupt')).toEqual([]);
    h.emit({ type: 'session.state', state: { ...state, capture_generation: 2 } });
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
    expect(h.controls().at(-1)).toEqual({
      type: 'output.interrupt', output_epoch: 1, reason: 'stop_speech',
    });
    const outputStop = vi.mocked(h.audio.stopOutput).mock.invocationCallOrder.at(-1)!;
    const captureStart = vi.mocked(h.audio.capture).mock.invocationCallOrder.at(-1)!;
    expect(outputStop).toBeLessThan(captureStart);
  });

  it('invalidates native output before overflow releases the microphone', async () => {
    const h = harness();
    Object.assign(h.audio, { captureRestartsOutput: true });
    await h.session.start();
    h.ready();
    h.commit();
    h.output();
    h.emit({
      type: 'error', code: 'input_overflow', message: 'Input paused.', fatal: false,
    });
    expect(h.views.at(-1)?.muted).toBe(true);
    expect(h.controls().at(-1)).toEqual({
      type: 'output.interrupt', output_epoch: 1, reason: 'stop_speech',
    });
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
    const outputStop = vi.mocked(h.audio.stopOutput).mock.invocationCallOrder.at(-1)!;
    const captureStop = vi.mocked(h.audio.stopCapture).mock.invocationCallOrder.at(-1)!;
    expect(outputStop).toBeLessThan(captureStop);
  });

  it('keeps input overflow muted until explicit unmute without stopping output', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.commit();
    h.output();
    const stops = vi.mocked(h.audio.stopOutput).mock.calls.length;
    h.emit({
      type: 'error',
      code: 'input_overflow',
      message: 'Input paused. Unmute to retry.',
      fatal: false,
    });
    h.emit({
      type: 'session.state',
      state: {
        ...state,
        capture: 'muted',
        capture_generation: 2,
        utterance: 'rejected',
      },
    });
    expect(h.views.at(-1)?.muted).toBe(true);
    expect(h.views.at(-1)?.phase).toBe('live');
    expect(h.controls().filter((e) => e.type === 'capture.mute')).toEqual([]);
    expect(h.audio.stopOutput).toHaveBeenCalledTimes(stops);
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
    h.session.setMuted(false);
    expect(h.controls().at(-1)).toEqual({ type: 'capture.mute', muted: false });
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
    h.emit({
      type: 'session.state',
      state: { ...state, capture_generation: 3 },
    });
    expect(h.audio.capture).toHaveBeenCalledTimes(2);
  });

  it('resolves only explicitly rejected utterances without replaying them', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    for (let id = 1; id <= 3; id++) {
      h.frames(6, 3000);
      h.frames(Math.ceil(L.endpoint_silence_ms / L.frame_ms), 0);
    }
    h.emit({
      type: 'utterance.rejected',
      utterance_id: 2,
      reason: 'input_overflow',
    });
    expect(h.views.at(-1)?.message).toContain('not admitted');
    expect(h.views.at(-1)?.phase).toBe('live');
    const sent = h.sent.length;
    await h.session.stop();
    expect(h.views.at(-1)?.reconciled).toBe(false);
    expect(h.sent).toHaveLength(sent + 1);
  });
  it('clears uncertainty after rejection while retaining an admitted task', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    for (let id = 1; id <= 2; id++) {
      h.frames(6, 3000);
      h.frames(Math.ceil(L.endpoint_silence_ms / L.frame_ms), 0);
    }
    h.emit({
      type: 'utterance.rejected',
      utterance_id: 2,
      reason: 'input_overflow',
    });
    await h.session.stop();
    expect(h.views.at(-1)?.reconciled).toBe(true);
    expect(h.views.at(-1)?.receipts).toHaveLength(1);
  });
  it('keeps a missing ended utterance uncertain when only an older receipt survives', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    for (let id = 1; id <= 2; id++) {
      h.frames(6, 3000);
      h.frames(Math.ceil(L.endpoint_silence_ms / L.frame_ms), 0);
    }
    const sent = h.sent.length;
    await h.session.stop();
    expect(h.views.at(-1)?.reconciled).toBe(false);
    expect(h.views.at(-1)?.receipts).toHaveLength(1);
    expect(h.sent).toHaveLength(sent + 1);
  });
  it.each(['stop speaking', 'end voice mode'])(
    'does not require admission for confirmed spoken control %s',
    async (text) => {
      const h = harness();
      await h.session.start();
      h.ready();
      h.frames(6, 3000);
      h.frames(Math.ceil(L.endpoint_silence_ms / L.frame_ms), 0);
      h.emit({
        type: 'transcript',
        utterance_id: 1,
        revision: 1,
        text,
        final: true,
      });
      vi.mocked(h.gateway.liveVoiceSnapshot).mockResolvedValue({
        state,
        receipts: [],
      });
      await h.session.stop();
      expect(h.views.at(-1)?.reconciled).toBe(true);
      expect(h.controls().some((event) => event.type === 'task.cancel')).toBe(false);
    },
  );
  it('does not treat partial control text or an uncorrelated outcome as reconciliation', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.frames(6, 3000);
    h.frames(Math.ceil(L.endpoint_silence_ms / L.frame_ms), 0);
    h.emit({
      type: 'transcript',
      utterance_id: 1,
      revision: 1,
      text: 'end voice mode',
      final: false,
    });
    h.emit({
      type: 'task.cancellation',
      status: 'requested',
      message: 'Cancellation was requested.',
    });
    vi.mocked(h.gateway.liveVoiceSnapshot).mockResolvedValue({
      state,
      receipts: [],
    });
    await h.session.stop();
    expect(h.views.at(-1)?.reconciled).toBe(false);
  });
  it('keeps a confirmed admission resolved when a recovered snapshot omits it', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.frames(6, 3000);
    h.frames(Math.ceil(L.endpoint_silence_ms / L.frame_ms), 0);
    h.emit({
      type: 'utterance.admitted',
      utterance_id: 1,
      turn_id: tid,
      idempotency_key: 'v:1',
      status: 'running',
    });
    vi.mocked(h.gateway.liveVoiceSnapshot).mockResolvedValue({
      state,
      receipts: [],
    });
    await h.session.stop();
    expect(h.views.at(-1)?.reconciled).toBe(true);
  });
  it('rejects a recovered receipt from a different voice session', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.frames(6, 3000);
    h.frames(Math.ceil(L.endpoint_silence_ms / L.frame_ms), 0);
    const snapshot = await h.gateway.liveVoiceSnapshot('s', vid);
    vi.mocked(h.gateway.liveVoiceSnapshot).mockResolvedValue({
      state,
      receipts: snapshot.receipts.map((receipt) => ({
        ...receipt,
        voice_session_id: 'other',
      })),
    });
    await h.session.stop();
    expect(h.views.at(-1)?.reconciled).toBe(false);
  });
  it('does not expect admission for capture that never ended', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.frames(6, 3000);
    vi.mocked(h.gateway.liveVoiceSnapshot).mockResolvedValue({
      state,
      receipts: [],
    });
    await h.session.stop();
    expect(h.views.at(-1)?.reconciled).toBe(true);
  });
  it('displays a cancellation outcome without acting or retrying', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    const before = h.sent.length;
    h.emit({
      type: 'task.cancellation',
      status: 'requested',
      message: 'Use task controls to choose a task.',
      turn_id: tid,
    });
    expect(h.views.at(-1)).toMatchObject({
      message: 'Use task controls to choose a task.',
      turnId: null,
      phase: 'live',
    });
    expect(h.sent).toHaveLength(before);
    expect(h.gateway.startLiveVoice).toHaveBeenCalledTimes(1);
  });
  it('keeps pending input independent of capture and output without answering forms', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    const before = h.sent.length;
    h.emit({
      type: 'session.state',
      state: { ...state, agent: 'waiting_for_input', output: 'playing' },
    });
    expect(h.views.at(-1)?.state).toMatchObject({
      capture: 'live',
      agent: 'waiting_for_input',
      output: 'playing',
    });
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
    expect(h.sent).toHaveLength(before);
    h.emit({
      type: 'task.cancellation',
      status: 'requested',
      message: 'Approval is mentioned in a message.',
    });
    expect(h.views.at(-1)?.state?.agent).toBe('waiting_for_input');
    expect(h.sent).toHaveLength(before);
  });
  it('captures ten automatic turns without reopening the microphone or submitting partial text', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    expect(h.factory).toHaveBeenCalledWith(expect.not.stringContaining('one-use'), [
      'vis-voice.v1',
      'vis-ticket.one-use',
    ]);
    for (let turn = 1; turn <= 10; turn++) {
      h.frames(6, 3000);
      h.emit({
        type: 'transcript',
        utterance_id: turn,
        revision: 1,
        text: 'provisional',
        final: false,
      });
      h.emit({
        type: 'output.interrupted',
        output_epoch: turn + 1,
        abort: 'acknowledged',
      });
      h.frames(Math.ceil(L.endpoint_silence_ms / L.frame_ms), 0);
      h.emit({
        type: 'session.state',
        state: { ...state, output_epoch: turn + 1, agent: 'running' },
      });
      h.commit(turn + 1, turn);
      h.output(turn + 1, 0, turn);
      h.output(turn + 1, 1, turn);
      h.progress.at(-2)!(480);
      h.progress.at(-1)!(480);
      h.emit({
        type: 'output.finished',
        output_epoch: turn + 1,
        segment_id: turn,
        sample_count: 960,
      });
    }
    expect(h.audio.play).toHaveBeenCalledTimes(20);
    expect(h.controls().filter((event) => event.type === 'utterance.start')).toHaveLength(10);
    expect(h.controls().filter((event) => event.type === 'utterance.end')).toHaveLength(10);
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
    expect(h.controls().some((event) => event.type === 'task.cancel')).toBe(false);
    expect(h.gateway.startLiveVoice).toHaveBeenCalledTimes(1);
  });
  it('does not admit delayed rendered playback as a new voice request', async () => {
    // Regression: speaker playback was captured as a new request on the phone.
    const h = harness();
    await h.session.start();
    h.ready();
    h.commit();
    const reference: Int16Array[] = [];
    for (let frame = 0; frame < 24; frame++) {
      const samples = playbackFrame(frame);
      reference.push(samples);
      h.output(1, frame, 1, samples);
      h.progress.at(-1)!(samples.length);
      await vi.advanceTimersByTimeAsync(L.frame_ms);
      h.input(frame < 2 ? new Int16Array(L.frame_samples) :
        reference[frame - 2].map((sample) => Math.round(sample * 0.45)));
    }
    expect(h.controls().filter((event) => event.type === 'utterance.start')).toEqual([]);
    expect(h.controls().filter((event) => event.type === 'output.interrupt')).toEqual([]);
    expect(h.controls().some((event) => event.type === 'task.cancel')).toBe(false);
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
  });
  it.each([1, 4, 8])('keeps input when playback confirmation trails capture by %s frames', async (batch) => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.commit();
    const callbacks: ((count: number) => void)[] = [];
    for (let frame = 0; frame < 24; frame++) {
      const samples = playbackFrame(frame);
      h.output(1, frame, 1, samples);
      callbacks.push(h.progress.at(-1)!);
      await vi.advanceTimersByTimeAsync(L.frame_ms);
      h.input(samples.map((sample) => Math.round(sample * 0.45)));
      if ((frame + 1) % batch === 0)
        for (const played of callbacks.splice(0)) played(L.frame_samples);
    }
    expect(h.controls().filter((event) => event.type === 'utterance.start')).toHaveLength(1);
    expect(h.controls().filter((event) => event.type === 'output.interrupt')).toHaveLength(1);
    expect(h.controls().some((event) => event.type === 'task.cancel')).toBe(false);
  });
  it('interrupts for quiet human speech over echo without cancelling the task', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.commit();
    const stops = vi.mocked(h.audio.stopOutput).mock.calls.length;
    for (let frame = 0; frame < 24; frame++) {
      const samples = playbackFrame(frame);
      h.output(1, frame, 1, samples);
      h.progress.at(-1)!(L.frame_samples);
      await vi.advanceTimersByTimeAsync(L.frame_ms);
      h.input(samples.map((sample, index) => {
        const t = frame * L.frame_samples + index;
        const human = frame < 12 ? 0 : 0.016 * 32768 * Math.SQRT2 *
          Math.sin(t * 0.047 + 0.7 * Math.sin(t * 0.001));
        return Math.round(0.45 * sample + human);
      }));
    }
    expect(h.controls().filter((event) => event.type === 'output.interrupt')).toEqual([
      { type: 'output.interrupt', output_epoch: 1, reason: 'barge_in' },
    ]);
    expect(h.controls().filter((event) => event.type === 'utterance.start')).toHaveLength(1);
    expect(h.controls().some((event) => event.type === 'task.cancel')).toBe(false);
    expect(h.audio.stopOutput).toHaveBeenCalledTimes(stops + 1);
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
  });
  it('keeps playback through short noise and interrupts only after continuous speech qualifies', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.commit();
    h.output();
    const sent = h.sent.length;
    const stops = vi.mocked(h.audio.stopOutput).mock.calls.length;
    h.frames(5, 3000);
    h.frames(1, 0);
    h.frames(5, 3000);
    expect(h.sent).toHaveLength(sent);
    expect(h.audio.stopOutput).toHaveBeenCalledTimes(stops);
    h.output(1, 1);
    expect(h.audio.play).toHaveBeenCalledTimes(2);
    const beforeStart = h.sent.length;
    h.frames(1, 3000);
    expect(h.audio.stopOutput).toHaveBeenCalledTimes(stops + 1);
    expect(JSON.parse(h.sent[beforeStart] as string)).toMatchObject({
      type: 'output.interrupt',
      output_epoch: 1,
      reason: 'barge_in',
    });
    expect(JSON.parse(h.sent[beforeStart + 1] as string)).toMatchObject({
      type: 'utterance.start',
    });
    expect(h.sent.slice(beforeStart + 2)).toHaveLength(10);
    expect(h.sent.slice(beforeStart + 2).every((frame) => frame instanceof ArrayBuffer)).toBe(true);
    expect(vi.mocked(h.audio.stopOutput).mock.invocationCallOrder.at(-1)).toBeLessThan(
      h.socket.send.mock.invocationCallOrder[beforeStart],
    );
    h.frames(1, 3000);
    expect(h.audio.stopOutput).toHaveBeenCalledTimes(stops + 1);
    expect(h.controls().some((control) => control.type === 'task.cancel')).toBe(false);
  });
  it('stops local output before the interruption roundtrip and rejects stale PCM', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.commit();
    h.output();
    expect(h.audio.play).toHaveBeenCalledTimes(1);
    h.frames(6, 3000);
    const controls = h.controls();
    expect(controls.findIndex((c) => c.type === 'output.interrupt')).toBeLessThan(
      controls.findIndex((c) => c.type === 'utterance.start'),
    );
    expect(h.audio.stopOutput).toHaveBeenCalled();
    h.output();
    expect(h.audio.play).toHaveBeenCalledTimes(1);
    h.emit({ type: 'output.interrupted', output_epoch: 2, abort: 'requested' });
    h.commit(2, 2);
    h.output(2, 0, 2);
    const stops = vi.mocked(h.audio.stopOutput).mock.calls.length;
    h.emit({
      type: 'output.interrupted',
      output_epoch: 2,
      abort: 'acknowledged',
    });
    expect(h.audio.stopOutput).toHaveBeenCalledTimes(stops);
    h.output(1);
    expect(h.audio.play).toHaveBeenCalledTimes(2);
  });
  it('reports only played samples and ignores duplicate frames', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.commit();
    h.output();
    h.output();
    expect(h.audio.play).toHaveBeenCalledTimes(1);
    expect(h.controls().filter((event) => event.type === 'playback.receipt')).toEqual([]);
    h.progress[0](240);
    expect(h.controls().at(-1)).toEqual({
      type: 'playback.receipt',
      output_epoch: 1,
      segment_id: 1,
      played: 240,
    });
    h.session.interrupt();
    h.progress[0](480);
    expect(
      h
        .controls()
        .filter((event) => event.type === 'playback.receipt')
        .at(-1),
    ).toMatchObject({ played: 240 });
  });
  it('fails closed on out-of-order or uncommitted output', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.output();
    expect(h.views.at(-1)?.phase).toBe('error');
    expect(h.audio.play).not.toHaveBeenCalled();
    const other = harness();
    await other.session.start();
    other.ready();
    other.commit();
    other.output(1, 2);
    expect(other.views.at(-1)?.phase).toBe('error');
  });
  it('bounds the unsent socket queue instead of silently replaying audio', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.socket.bufferedAmount = 200_000;
    h.frames(6, 3000);
    expect(h.views.at(-1)?.phase).toBe('error');
    expect(h.audio.close).toHaveBeenCalled();
    expect(h.sent.filter((value) => value instanceof ArrayBuffer)).toHaveLength(0);
  });
  it('does not open the microphone during a muted startup or a stale unmute state', async () => {
    const h = harness(true);
    await h.session.start();
    h.ready();
    expect(h.audio.capture).not.toHaveBeenCalled();
    h.session.setMuted(false);
    h.emit({ type: 'session.state', state });
    expect(h.audio.capture).not.toHaveBeenCalled();
    h.emit({
      type: 'session.state',
      state: { ...state, capture_generation: 2 },
    });
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
    h.session.setMuted(true);
    h.frames(6, 3000);
    expect(h.controls().some((event) => event.type === 'utterance.start')).toBe(false);
  });
  it('honors mute during startup and never replays after disconnect', async () => {
    const h = harness();
    await h.session.start();
    h.session.setMuted(true);
    h.ready();
    expect(h.audio.capture).not.toHaveBeenCalled();
    h.socket.onclose?.();
    await Promise.resolve();
    await Promise.resolve();
    expect(h.audio.close).toHaveBeenCalled();
    expect(h.gateway.liveVoiceSnapshot).toHaveBeenCalledWith('s', vid);
    expect(h.views.at(-1)).toMatchObject({
      muted: true,
      reconciled: true,
      receipts: [expect.objectContaining({ turn_id: tid })],
    });
    await vi.advanceTimersByTimeAsync(30_000);
    expect(h.gateway.startLiveVoice).toHaveBeenCalledTimes(1);
  });
  it('ending mode or stopping speech never cancels work; explicit cancel is ownership guarded', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.commit();
    h.session.interrupt();
    h.session.cancelTask('other');
    expect(h.controls().some((event) => event.type === 'task.cancel')).toBe(false);
    h.session.cancelTask(tid);
    expect(h.controls().filter((event) => event.type === 'task.cancel')).toEqual([
      { type: 'task.cancel', turn_id: tid },
    ]);
    await h.session.stop();
    expect(h.controls().filter((event) => event.type === 'task.cancel')).toHaveLength(1);
    expect(h.gateway.endLiveVoice).toHaveBeenCalledWith('s', vid);
  });
  it('obeys controls from outside the app and reports each state for display', async () => {
    const h = harness();
    let control!: (control: LiveVoiceControl) => void;
    const present = vi.fn();
    Object.assign(h.audio, {
      remote: (handler: (control: LiveVoiceControl) => void) => { control = handler; },
      present,
    });
    await h.session.start();
    expect(present).toHaveBeenLastCalledWith(
      { state: 'connecting', muted: false, speaking: false, task: null });
    h.ready();
    h.commit();
    h.emit({ type: 'session.state', state: { ...state, agent: 'running', output: 'playing' } });
    expect(present).toHaveBeenLastCalledWith(
      { state: 'speaking', muted: false, speaking: true, task: 'running' });
    control('stop_speech');
    expect(h.controls().at(-1)).toMatchObject({ type: 'output.interrupt', reason: 'stop_speech' });
    control('mute');
    expect(h.controls().at(-1)).toEqual({ type: 'capture.mute', muted: true });
    expect(h.audio.stopCapture).toHaveBeenCalled();
    expect(present).toHaveBeenLastCalledWith(expect.objectContaining({ state: 'muted', muted: true }));
    control('unmute');
    expect(h.controls().at(-1)).toEqual({ type: 'capture.mute', muted: false });
    h.emit({ type: 'session.state', state: { ...state, agent: 'waiting_for_input' } });
    expect(present).toHaveBeenLastCalledWith(
      { state: 'waiting', muted: false, speaking: false, task: null });
    const shown = present.mock.calls.length;
    control('end');
    await vi.runOnlyPendingTimersAsync();
    expect(h.views.at(-1)).toMatchObject({ phase: 'closed' });
    expect(h.gateway.endLiveVoice).toHaveBeenCalledWith('s', vid);
    expect(h.controls().some((event) => event.type === 'task.cancel')).toBe(false);
    expect(present).toHaveBeenCalledTimes(shown);
  });
  it('ends the open utterance at a mute, so that the speech before it counts', async () => {
    const h = harness();
    await h.session.start();
    h.ready();
    h.frames(6, 3000);
    const { utterance_id, capture_generation } = h
      .controls()
      .find((event) => event.type === 'utterance.start')!;
    h.session.setMuted(true);
    expect(h.controls().slice(-2)).toEqual([
      { type: 'utterance.end', capture_generation, utterance_id },
      { type: 'capture.mute', muted: true },
    ]);
    h.session.setMuted(false);
    expect(h.controls().filter((event) => event.type === 'utterance.end')).toHaveLength(1);
  });
  it('does not substitute device speech when local engines are unavailable', async () => {
    const h = harness();
    vi.mocked(h.gateway.liveVoiceCapabilities).mockResolvedValue({
      enabled: true,
      available: false,
      reason: 'Local engines missing.',
    });
    await h.session.start();
    expect(h.gateway.startLiveVoice).not.toHaveBeenCalled();
    expect(h.audio.capture).not.toHaveBeenCalled();
    expect(h.views.at(-1)).toMatchObject({
      phase: 'error',
      message: 'Local engines missing.',
    });
  });
  it('keeps loading alive with heartbeat acknowledgements and closes a silent lease', async () => {
    const h = harness();
    await h.session.start();
    for (let i = 0; i < 5; i++) {
      await vi.advanceTimersByTimeAsync(5000);
      h.emit({ type: 'session.state', state });
    }
    expect(h.audio.capture).not.toHaveBeenCalled();
    h.ready();
    expect(h.audio.capture).toHaveBeenCalledTimes(1);
    await vi.advanceTimersByTimeAsync(15000);
    expect(h.audio.close).toHaveBeenCalled();
    expect(h.views.at(-1)?.message).toMatch(/lease expired/);
  });
  it('cleans up an HTTP start that completes after the user ends mode', async () => {
    const h = harness();
    const result = await h.gateway.startLiveVoice('s', 'c', false);
    let resolve!: (value: typeof result) => void;
    vi.mocked(h.gateway.startLiveVoice).mockReturnValue(
      new Promise((done) => {
        resolve = done;
      }),
    );
    const pending = h.session.start();
    await Promise.resolve();
    await Promise.resolve();
    await h.session.stop();
    resolve(result);
    await pending;
    expect(h.factory).not.toHaveBeenCalled();
    expect(h.gateway.endLiveVoice).toHaveBeenCalledWith('s', vid);
  });
});
