// @vitest-environment jsdom
import { ReadableStream } from 'node:stream/web';
import { TextEncoder } from 'node:util';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const finish = vi.hoisted(() => vi.fn());
const start = vi.hoisted(() => vi.fn(() => ({ request_id: 'req-stream', finish })));
vi.mock('./diagnostics', () => ({ startGatewayRequestDiagnostic: start }));

import { GatewayClient } from './gateway';

type StreamKind = 'sessions' | 'fleet';
let stop: (() => void) | undefined;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(new Date('2026-10-03T08:00:00Z'));
  start.mockClear();
  finish.mockClear();
});

afterEach(async () => {
  stop?.();
  stop = undefined;
  await vi.advanceTimersByTimeAsync(0);
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

function subscribe(kind: StreamKind) {
  const client = new GatewayClient({ url: 'https://gateway.example.com', token: 'private-token' });
  const onEvent = vi.fn();
  const options = { reason: 'wake' as const };
  stop =
    kind === 'sessions'
      ? client.streamSessionEvents(new Map([['session-one', -1]]), onEvent, options)
      : client.streamFleetStatus(onEvent, options);
  return onEvent;
}

function responseStream() {
  let controller!: ReadableStreamDefaultController<Uint8Array>;
  vi.stubGlobal(
    'fetch',
    vi.fn(async () => ({
      ok: true,
      status: 200,
      body: new ReadableStream<Uint8Array>({
        start(value) {
          controller = value;
        },
      }),
    })),
  );
  return {
    write: (text: string) => controller.enqueue(new TextEncoder().encode(text)),
    close: () => controller.close(),
  };
}

describe.each<StreamKind>(['sessions', 'fleet'])('%s stream diagnostics', (kind) => {
  it('records byte and comment timing without logging stream content', async () => {
    const stream = responseStream();
    const onEvent = subscribe(kind);
    await vi.advanceTimersByTimeAsync(15_000);
    stream.write(': pi');
    await vi.advanceTimersByTimeAsync(1_000);
    stream.write('ng\n\n');
    await vi.advanceTimersByTimeAsync(9_000);
    stream.write('data: {"type":"private-event"}\n\n');
    await vi.advanceTimersByTimeAsync(5_000);
    stop?.();
    await vi.advanceTimersByTimeAsync(0);

    expect(start).toHaveBeenCalledTimes(1);
    expect(start).toHaveBeenCalledWith(expect.objectContaining({ reason: 'wake', stream: kind }));
    expect(onEvent).toHaveBeenCalledTimes(1);
    expect(finish).toHaveBeenCalledWith(
      'info',
      expect.objectContaining({
        outcome: 'cancelled',
        status: 200,
        stream_phase: 'read',
        last_byte_age_ms: 5_000,
        last_heartbeat_age_ms: 14_000,
      }),
    );
    const diagnostics = JSON.stringify([start.mock.calls, finish.mock.calls]);
    expect(diagnostics).not.toContain('private-event');
    expect(diagnostics).not.toContain('private-token');
  });

  it('distinguishes a stalled body from a connect timeout', async () => {
    const stream = responseStream();
    subscribe(kind);
    await vi.advanceTimersByTimeAsync(15_000);
    stream.write(': ping\n\n');
    await vi.advanceTimersByTimeAsync(45_000);
    expect(finish).toHaveBeenCalledWith(
      'error',
      expect.objectContaining({
        outcome: 'timeout',
        status: 200,
        stream_phase: 'read',
        last_byte_age_ms: 45_000,
        last_heartbeat_age_ms: 45_000,
      }),
    );
    await vi.advanceTimersByTimeAsync(400);
    expect(start).toHaveBeenLastCalledWith(
      expect.objectContaining({ reason: 'stream_stall', attempt: 2 }),
    );
  });

  it('records a connect timeout even when fetch ignores abort', async () => {
    vi.stubGlobal('fetch', vi.fn(() => new Promise<never>(() => {})));
    subscribe(kind);
    await vi.advanceTimersByTimeAsync(10_400);
    expect(finish).toHaveBeenCalledWith(
      'error',
      expect.objectContaining({
        outcome: 'timeout',
        status: 0,
        stream_phase: 'connect',
        last_byte_age_ms: null,
        last_heartbeat_age_ms: null,
      }),
    );
    expect(start).toHaveBeenLastCalledWith(
      expect.objectContaining({ reason: 'connect_timeout', attempt: 2 }),
    );
  });

  it('does not count the end of a response as another byte', async () => {
    const stream = responseStream();
    subscribe(kind);
    await vi.advanceTimersByTimeAsync(15_000);
    stream.write(': ping\n\n');
    await vi.advanceTimersByTimeAsync(5_000);
    stream.close();
    await vi.advanceTimersByTimeAsync(400);
    expect(finish).toHaveBeenCalledWith(
      'warn',
      expect.objectContaining({
        outcome: 'closed',
        last_byte_age_ms: 5_000,
        last_heartbeat_age_ms: 5_000,
      }),
    );
    expect(start).toHaveBeenLastCalledWith(expect.objectContaining({ reason: 'eof', attempt: 2 }));
  });

  it.each(['http_error', 'network_error'] as const)('names retries after %s', async (reason) => {
    vi.stubGlobal(
      'fetch',
      vi.fn(async () => {
        if (reason === 'network_error') throw new TypeError('network unavailable');
        return { ok: false, status: 503, body: null };
      }),
    );
    subscribe(kind);
    await vi.advanceTimersByTimeAsync(400);
    expect(start).toHaveBeenLastCalledWith(expect.objectContaining({ reason, attempt: 2 }));
  });
});
