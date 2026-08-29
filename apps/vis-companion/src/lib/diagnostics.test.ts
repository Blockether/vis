import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
const nativeCompressionStream = globalThis.CompressionStream;
const nativeDecompressionStream = globalThis.DecompressionStream;
const files = vi.hoisted(() => new Map<string, string>());
const appendFile = vi.hoisted(() => vi.fn(async ({ path, data }: { path: string; data: string }) => {
  files.set(path, `${files.get(path) ?? ''}${data}`);
}));
const deleteFile = vi.hoisted(() => vi.fn(async ({ path }: { path: string }) => {
  files.delete(path);
}));
const shareArtifact = vi.hoisted(() =>
  vi.fn(
    async (
      _blob: Blob,
      _name: string,
      _mediaType: string,
      _options: Record<string, unknown>,
    ) => 'Diagnostics shared.',
  ),
);
const lifecycle = vi.hoisted(() => ({
  away: null as (() => void) | null,
  wake: null as ((info: { awayMs: number }) => void) | null,
}));

vi.mock('./wake', () => ({
  onAway: (listener: () => void) => {
    lifecycle.away = listener;
    return () => {
      lifecycle.away = null;
    };
  },
  onWake: (listener: (info: { awayMs: number }) => void) => {
    lifecycle.wake = listener;
    return () => {
      lifecycle.wake = null;
    };
  },
}));

vi.mock('@capacitor/filesystem', () => ({
  Directory: { LibraryNoCloud: 'LIBRARY_NO_CLOUD' },
  Encoding: { UTF8: 'utf8' },
  Filesystem: {
    appendFile,
    deleteFile,
    mkdir: vi.fn(async () => undefined),
    readFile: vi.fn(async ({ path }: { path: string }) => ({ data: files.get(path) ?? '' })),
    readdir: vi.fn(async () => ({
      files: [...files.entries()].map(([name, data]) => ({
        name: name.replace(/^diagnostics\//u, ''),
        type: 'file',
        size: new TextEncoder().encode(data).byteLength,
        mtime: 1,
      })),
    })),
  },
}));
vi.mock('./artifact-share', () => ({ shareArtifact }));

async function diagnostics() {
  return import('./diagnostics');
}

describe('persistent app diagnostics', () => {
  beforeEach(() => {
    files.clear();
    appendFile.mockClear();
    deleteFile.mockClear();
    shareArtifact.mockClear();
    lifecycle.away = null;
    lifecycle.wake = null;
    vi.resetModules();
    vi.stubGlobal('CompressionStream', undefined);
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it('writes ordered structured records to app-private storage and redacts secrets', async () => {
    const { flushDiagnostics, recordDiagnostic } = await diagnostics();

    recordDiagnostic('info', 'app', 'paired', {
      token: 'gateway-secret',
      nested: { authorization: 'Bearer private', useful: 'kept' },
      note: 'failed at https://gateway.example.com/path?access_token=url-private&cursor=7 token=label-private',
    });
    recordDiagnostic('warn', 'network', 'offline');
    await flushDiagnostics();

    expect(appendFile).toHaveBeenCalledTimes(2);
    expect(appendFile.mock.calls[0]?.[0]).toMatchObject({
      directory: 'LIBRARY_NO_CLOUD',
      encoding: 'utf8',
      path: expect.stringMatching(/^diagnostics\/vis-app-.*\.jsonl$/u),
    });
    const records = appendFile.mock.calls.map(([call]) => JSON.parse(call.data));
    expect(records.map((record) => record.event)).toEqual(['paired', 'offline']);
    expect(records[0]).toMatchObject({
      schema: 1,
      level: 'info',
      scope: 'app',
      details: {
        token: '[REDACTED]',
        nested: { authorization: '[REDACTED]', useful: 'kept' },
      },
    });
    const serialized = JSON.stringify(records);
    expect(serialized).not.toContain('gateway-secret');
    expect(serialized).not.toContain('Bearer private');
    expect(serialized).not.toContain('url-private');
    expect(serialized).not.toContain('label-private');
  });

  it('correlates session requests across a background freeze and resume', async () => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date('2026-03-10T12:00:00.000Z'));
    const { flushDiagnostics, installDiagnostics, startGatewayRequestDiagnostic } =
      await diagnostics();
    installDiagnostics();

    const request = startGatewayRequestDiagnostic({
      gateway: 'https://gateway.example.com',
      method: 'GET',
      path: '/v1/sessions/session-42',
      session_id: 'session-42',
      transport: 'fetch',
    });
    vi.advanceTimersByTime(125);
    lifecycle.away?.();
    vi.advanceTimersByTime(5_000);
    lifecycle.wake?.({ awayMs: 5_000 });
    vi.advanceTimersByTime(75);
    request.finish('info', { outcome: 'success', status: 200 });
    await flushDiagnostics();

    const records = appendFile.mock.calls.map(([call]) => JSON.parse(call.data));
    const started = records.find((record) => record.event === 'request_started');
    const backgrounded = records.find((record) => record.event === 'backgrounded');
    const resumed = records.find((record) => record.event === 'resumed');
    const finished = records.find((record) => record.event === 'request_finished');

    expect(started.details).toMatchObject({
      request_id: expect.any(String),
      session_id: 'session-42',
      wake_id: 0,
      app_state: 'foreground',
    });
    expect(backgrounded.details).toMatchObject({
      inflight_request_count: 1,
      inflight_requests: [
        expect.objectContaining({
          request_id: started.details.request_id,
          session_id: 'session-42',
          age_ms: 125,
        }),
      ],
    });
    expect(resumed.details).toMatchObject({
      wake_id: 1,
      away_ms: 5_000,
      inflight_request_count: 1,
      inflight_requests: [
        expect.objectContaining({ request_id: started.details.request_id, session_id: 'session-42' }),
      ],
    });
    expect(finished.details).toMatchObject({
      request_id: started.details.request_id,
      session_id: 'session-42',
      outcome: 'success',
      status: 200,
      duration_ms: 5_200,
      foreground_duration_ms: 200,
      background_duration_ms: 5_000,
      wake_count: 1,
      started_wake_id: 0,
      finished_wake_id: 1,
      crossed_background: true,
    });
  });

  it('prunes expired runs before it writes this launch', async () => {
    files.set('diagnostics/vis-app-expired.jsonl', '{"schema":1,"event":"expired"}\n');
    const { flushDiagnostics, installDiagnostics } = await diagnostics();

    installDiagnostics();
    await flushDiagnostics();

    expect(deleteFile).toHaveBeenCalledWith({
      path: 'diagnostics/vis-app-expired.jsonl',
      directory: 'LIBRARY_NO_CLOUD',
    });
    expect([...files.values()].join('')).toContain('"event":"started"');
  });
  it('exports every retained run as one portable diagnostics file', async () => {
    files.set('diagnostics/vis-app-older.jsonl', '{"schema":1,"event":"older"}\n');
    const { exportDiagnostics, recordDiagnostic } = await diagnostics();
    recordDiagnostic('error', 'react', 'render_failed', { message: 'screen stopped' });

    await expect(exportDiagnostics()).resolves.toBe('Diagnostics shared.');

    expect(shareArtifact).toHaveBeenCalledOnce();
    const [blob, name, mediaType, options] = shareArtifact.mock.calls[0]!;
    expect(name).toMatch(/^vis-diagnostics-.*\.jsonl$/u);
    expect(mediaType).toBe('application/x-ndjson');
    expect(options).toMatchObject({ noun: 'Diagnostics', dialogTitle: 'Export Vis diagnostics' });
    const text = await (blob as Blob).text();
    const records = text.trim().split('\n').map((line) => JSON.parse(line));
    expect(records[0]).toMatchObject({ kind: 'vis-app-diagnostics', schema: 1 });
    expect(records.some((record) => record.event === 'older')).toBe(true);
    expect(records.some((record) => record.event === 'render_failed')).toBe(true);
  });

  it('uses gzip when the webview provides the standard compression stream', async () => {
    if (!nativeCompressionStream || !nativeDecompressionStream) return;
    vi.stubGlobal('CompressionStream', nativeCompressionStream);
    const { exportDiagnostics, recordDiagnostic } = await diagnostics();
    recordDiagnostic('info', 'app', 'compress_me');

    await exportDiagnostics();

    const [blob, name, mediaType] = shareArtifact.mock.calls[0]!;
    expect(name).toMatch(/^vis-diagnostics-.*\.jsonl\.gz$/u);
    expect(mediaType).toBe('application/gzip');
    const decompressed = (blob as Blob)
      .stream()
      .pipeThrough(new nativeDecompressionStream('gzip'));
    const text = await new Response(decompressed).text();
    expect(text).toContain('"event":"compress_me"');
  });
});
