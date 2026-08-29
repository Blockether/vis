import { App as CapacitorApp } from '@capacitor/app';
import { Capacitor } from '@capacitor/core';
import {
  Directory,
  Encoding,
  Filesystem,
  type FileInfo,
} from '@capacitor/filesystem';

import { shareArtifact } from './artifact-share';
import { APP_BUILD_COMMIT, APP_BUILD_NUMBER } from './build-info';
import {
  APP_MIN_GATEWAY_PROTOCOL,
  APP_PROTOCOL,
  APP_VERSION,
} from './compat';
import { onAway, onWake } from './wake';

export type DiagnosticLevel = 'debug' | 'info' | 'warn' | 'error';

export type GatewayRequestDiagnosticStart = {
  gateway: string;
  method: string;
  path: string;
  transport: 'fetch' | 'sse' | 'xhr';
  session_id?: string;
  session_ids?: readonly string[];
  stream?: 'fleet' | 'session' | 'sessions' | 'voice_job';
  attempt?: number;
};

export type GatewayRequestDiagnosticFinish = {
  outcome:
    | 'success'
    | 'not_modified'
    | 'cancelled'
    | 'timeout'
    | 'http_error'
    | 'network_error'
    | 'closed';
  status: number;
  error?: string;
};

export type GatewayRequestDiagnostic = {
  request_id: string;
  finish: (level: DiagnosticLevel, result: GatewayRequestDiagnosticFinish) => void;
};

type TrackedGatewayRequest = {
  request_id: string;
  started_at_ms: number;
  started_wake_id: number;
  backgrounded_at_ms: number | null;
  background_duration_ms: number;
  wake_count: number;
  crossed_background: boolean;
  start: GatewayRequestDiagnosticStart;
};

const SCHEMA = 1;
const LOG_DIRECTORY = 'diagnostics';
const MAX_FILE_BYTES = 1024 * 1024;
const MAX_RETAINED_BYTES = 8 * 1024 * 1024;
const MAX_RETAINED_FILES = 24;
const MAX_CURRENT_SEGMENTS = Math.floor(MAX_RETAINED_BYTES / MAX_FILE_BYTES);
const MAX_RECORD_BYTES = 64 * 1024;
const MAX_RECORD_PREVIEW_CHARS = 12_000;
const RETAIN_FOR_MS = 7 * 24 * 60 * 60 * 1000;
const LOG_STORAGE = Directory.LibraryNoCloud;
const SECRET_KEY = /authorization|cookie|credential|password|passcode|secret|token|api[-_]?key/iu;
const encoder = new TextEncoder();
const runStamp = new Date().toISOString().replace(/[-:.TZ]/gu, '');
const runNonce =
  globalThis.crypto?.randomUUID?.().slice(0, 8) ?? Math.random().toString(16).slice(2, 10);
const runPrefix = `vis-app-${runStamp}-${runNonce}`;

let segment = 0;
let segmentBytes = 0;
let sequence = 0;
let installed = false;
let directoryReady: Promise<void> | null = null;
let writeQueue: Promise<void> = Promise.resolve();
let storageFailure: Error | null = null;
let requestSequence = 0;
let wakeId = 0;
let appState: 'foreground' | 'background' = 'foreground';
const inflightGatewayRequests = new Map<string, TrackedGatewayRequest>();

function errorOf(cause: unknown): Error {
  return cause instanceof Error ? cause : new Error(String(cause));
}

function redactText(value: string): string {
  return value
    .replace(/\bBearer\s+[^\s,;]+/giu, 'Bearer [REDACTED]')
    .replace(
      /\b((?:access[-_]?token|token|secret|password|passcode|api[-_]?key)\s*[:=]\s*)[^\s,;]+/giu,
      '$1[REDACTED]',
    )
    .replace(/([?&][^#\s=]{1,80}=)[^&#\s]*/gu, '$1[REDACTED]')
    .slice(0, 8_000);
}

function safeValue(
  value: unknown,
  key = '',
  depth = 0,
  seen: WeakSet<object> = new WeakSet<object>(),
): unknown {
  if (SECRET_KEY.test(key)) return '[REDACTED]';
  if (value === null || value === undefined || typeof value === 'boolean') return value ?? null;
  if (typeof value === 'string') return redactText(value);
  if (typeof value === 'number') return Number.isFinite(value) ? value : String(value);
  if (typeof value === 'bigint') return String(value);
  if (typeof value === 'function' || typeof value === 'symbol') return String(value);
  if (depth >= 5) return '[TRUNCATED]';
  if (value instanceof Error) {
    return {
      name: value.name,
      message: redactText(value.message),
      stack: value.stack ? redactText(value.stack) : undefined,
    };
  }
  if (typeof value !== 'object') return String(value);
  if (seen.has(value)) return '[CIRCULAR]';
  seen.add(value);
  if (Array.isArray(value)) {
    return value.slice(0, 40).map((item) => safeValue(item, '', depth + 1, seen));
  }
  const result: Record<string, unknown> = {};
  for (const [entryKey, entryValue] of Object.entries(value).slice(0, 60)) {
    result[entryKey] = safeValue(entryValue, entryKey, depth + 1, seen);
  }
  return result;
}

function currentLogPath(index = segment): string {
  return `${LOG_DIRECTORY}/${runPrefix}${index === 0 ? '' : `-${index}`}.jsonl`;
}

async function ensureDirectory(): Promise<void> {
  directoryReady ??= (async () => {
    try {
      await Filesystem.mkdir({
        path: LOG_DIRECTORY,
        directory: LOG_STORAGE,
        recursive: true,
      });
    } catch (cause) {
      try {
        await Filesystem.readdir({ path: LOG_DIRECTORY, directory: LOG_STORAGE });
      } catch {
        throw cause;
      }
    }
  })();
  return directoryReady;
}

function isLogFile(file: FileInfo): boolean {
  return file.type === 'file' && /^vis-app-.*\.jsonl$/u.test(file.name);
}

async function retainedFiles(): Promise<FileInfo[]> {
  await ensureDirectory();
  const listing = await Filesystem.readdir({
    path: LOG_DIRECTORY,
    directory: LOG_STORAGE,
  });
  return listing.files.filter(isLogFile);
}

async function pruneRetainedLogs(reserveBytes = 0): Promise<void> {
  const files = (await retainedFiles()).sort(
    (left, right) =>
      (right.mtime ?? right.ctime ?? 0) - (left.mtime ?? left.ctime ?? 0) ||
      right.name.localeCompare(left.name),
  );
  let retainedBytes = 0;
  let retainedCount = 0;
  const now = Date.now();
  for (const file of files) {
    const isCurrentRun = file.name.startsWith(runPrefix);
    const modified = file.mtime ?? file.ctime;
    const expired = modified !== undefined && now - modified > RETAIN_FOR_MS;
    const fits =
      retainedCount < MAX_RETAINED_FILES &&
      retainedBytes + file.size <= Math.max(0, MAX_RETAINED_BYTES - reserveBytes);
    if (!isCurrentRun && (expired || !fits)) {
      await Filesystem.deleteFile({
        path: `${LOG_DIRECTORY}/${file.name}`,
        directory: LOG_STORAGE,
      });
      continue;
    }
    retainedCount += 1;
    retainedBytes += file.size;
  }
}

function enqueue(work: () => Promise<void>): void {
  writeQueue = writeQueue
    .then(work)
    .catch((cause) => {
      storageFailure = errorOf(cause);
    });
}

async function appendRecord(line: string): Promise<void> {
  await ensureDirectory();
  const bytes = encoder.encode(line).byteLength;
  let rotated = false;
  if (segmentBytes > 0 && segmentBytes + bytes > MAX_FILE_BYTES) {
    segment += 1;
    segmentBytes = 0;
    rotated = true;
  }
  await Filesystem.appendFile({
    path: currentLogPath(),
    directory: LOG_STORAGE,
    encoding: Encoding.UTF8,
    data: line,
  });
  segmentBytes += bytes;
  if (rotated) {
    if (segment >= MAX_CURRENT_SEGMENTS) {
      await Filesystem.deleteFile({
        path: currentLogPath(segment - MAX_CURRENT_SEGMENTS),
        directory: LOG_STORAGE,
      });
    }
    await pruneRetainedLogs();
  }
}

/** Queue one bounded, redacted JSONL record without delaying the app action that produced it. */
export function recordDiagnostic(
  level: DiagnosticLevel,
  scope: string,
  event: string,
  details?: unknown,
): void {
  const record = {
    schema: SCHEMA,
    ts: new Date().toISOString(),
    seq: ++sequence,
    level,
    scope,
    event,
    ...(details === undefined ? {} : { details: safeValue(details) }),
  };
  let json = JSON.stringify(record);
  if (encoder.encode(json).byteLength > MAX_RECORD_BYTES) {
    json = JSON.stringify({
      ...record,
      details: { is_truncated: true, preview: json.slice(0, MAX_RECORD_PREVIEW_CHARS) },
    });
  }
  enqueue(() => appendRecord(`${json}\n`));
}

function trackedRequestDetails(request: TrackedGatewayRequest): Record<string, unknown> {
  return {
    request_id: request.request_id,
    ...request.start,
  };
}

function inflightRequestDetails(now: number): Array<Record<string, unknown>> {
  return [...inflightGatewayRequests.values()].map((request) => ({
    ...trackedRequestDetails(request),
    age_ms: Math.max(0, now - request.started_at_ms),
  }));
}

function recordBackgrounded(): void {
  if (appState === 'background') return;
  const now = Date.now();
  appState = 'background';
  for (const request of inflightGatewayRequests.values()) {
    request.backgrounded_at_ms = now;
    request.crossed_background = true;
  }
  recordDiagnostic('info', 'app', 'backgrounded', {
    wake_id: wakeId,
    inflight_request_count: inflightGatewayRequests.size,
    inflight_requests: inflightRequestDetails(now),
  });
  // Start draining while the native bridge is still live; the OS may freeze it immediately after.
  void flushDiagnostics();
}

function recordWake(awayMs: number): void {
  const now = Date.now();
  const resumed = appState === 'background';
  if (resumed) wakeId += 1;
  for (const request of inflightGatewayRequests.values()) {
    if (request.backgrounded_at_ms === null) continue;
    request.background_duration_ms += Math.max(0, now - request.backgrounded_at_ms);
    request.backgrounded_at_ms = null;
    request.wake_count += 1;
  }
  appState = 'foreground';
  recordDiagnostic('info', 'app', resumed ? 'resumed' : 'wake', {
    wake_id: wakeId,
    away_ms: Math.max(0, awayMs),
    inflight_request_count: inflightGatewayRequests.size,
    inflight_requests: inflightRequestDetails(now),
  });
}

/** Start one payload-free gateway span that remains visible even if a frozen request never ends. */
export function startGatewayRequestDiagnostic(
  start: GatewayRequestDiagnosticStart,
): GatewayRequestDiagnostic {
  const startedAt = Date.now();
  const request: TrackedGatewayRequest = {
    request_id: `req-${runNonce}-${(++requestSequence).toString(36)}`,
    started_at_ms: startedAt,
    started_wake_id: wakeId,
    backgrounded_at_ms: appState === 'background' ? startedAt : null,
    background_duration_ms: 0,
    wake_count: 0,
    crossed_background: appState === 'background',
    start,
  };
  inflightGatewayRequests.set(request.request_id, request);
  recordDiagnostic('info', 'gateway', 'request_started', {
    ...trackedRequestDetails(request),
    wake_id: wakeId,
    app_state: appState,
  });

  let finished = false;
  return {
    request_id: request.request_id,
    finish(level, result) {
      if (finished) return;
      finished = true;
      const finishedAt = Date.now();
      let backgroundDuration = request.background_duration_ms;
      if (request.backgrounded_at_ms !== null) {
        backgroundDuration += Math.max(0, finishedAt - request.backgrounded_at_ms);
      }
      const duration = Math.max(0, finishedAt - request.started_at_ms);
      inflightGatewayRequests.delete(request.request_id);
      recordDiagnostic(level, 'gateway', 'request_finished', {
        ...trackedRequestDetails(request),
        ...result,
        duration_ms: duration,
        foreground_duration_ms: Math.max(0, duration - backgroundDuration),
        background_duration_ms: backgroundDuration,
        wake_count: request.wake_count,
        started_wake_id: request.started_wake_id,
        finished_wake_id: wakeId,
        crossed_background: request.crossed_background,
      });
    },
  };
}

/** Wait until every record already accepted by the logger has reached durable storage. */
export async function flushDiagnostics(): Promise<void> {
  await writeQueue;
}

function exportManifest(): Record<string, unknown> {
  return {
    kind: 'vis-app-diagnostics',
    schema: SCHEMA,
    exported_at: new Date().toISOString(),
    app: {
      version: APP_VERSION,
      build: APP_BUILD_NUMBER,
      commit: APP_BUILD_COMMIT,
      platform: Capacitor.getPlatform(),
      is_native: Capacitor.isNativePlatform(),
    },
    compatibility: {
      client_protocol: APP_PROTOCOL,
      min_gateway_protocol: APP_MIN_GATEWAY_PROTOCOL,
    },
    retention: {
      days: 7,
      max_bytes: MAX_RETAINED_BYTES,
    },
    ...(storageFailure ? { storage_error: safeValue(storageFailure) } : {}),
  };
}

async function maybeGzip(blob: Blob): Promise<{ blob: Blob; suffix: string; mediaType: string }> {
  if (typeof globalThis.CompressionStream !== 'function') {
    return { blob, suffix: '.jsonl', mediaType: 'application/x-ndjson' };
  }
  try {
    const compressed = blob.stream().pipeThrough(new CompressionStream('gzip'));
    return {
      blob: new Blob([await new Response(compressed).arrayBuffer()], { type: 'application/gzip' }),
      suffix: '.jsonl.gz',
      mediaType: 'application/gzip',
    };
  } catch {
    return { blob, suffix: '.jsonl', mediaType: 'application/x-ndjson' };
  }
}

/** Flush, bundle and hand retained app logs to the platform share sheet or browser download. */
export async function exportDiagnostics(): Promise<string> {
  recordDiagnostic('info', 'diagnostics', 'export_requested');
  await flushDiagnostics();
  const files = (await retainedFiles()).sort(
    (left, right) =>
      (left.mtime ?? left.ctime ?? 0) - (right.mtime ?? right.ctime ?? 0) ||
      left.name.localeCompare(right.name),
  );
  const chunks = [`${JSON.stringify(exportManifest())}\n`];
  for (const file of files) {
    try {
      const result = await Filesystem.readFile({
        path: `${LOG_DIRECTORY}/${file.name}`,
        directory: LOG_STORAGE,
        encoding: Encoding.UTF8,
      });
      const text = typeof result.data === 'string' ? result.data : await result.data.text();
      chunks.push(text.endsWith('\n') ? text : `${text}\n`);
    } catch (cause) {
      chunks.push(
        `${JSON.stringify({
          schema: SCHEMA,
          level: 'error',
          scope: 'diagnostics',
          event: 'log_read_failed',
          details: { file: file.name, error: safeValue(errorOf(cause)) },
        })}\n`,
      );
    }
  }
  const plain = new Blob(chunks, { type: 'application/x-ndjson' });
  const exported = await maybeGzip(plain);
  const stamp = new Date().toISOString().replace(/[-:.TZ]/gu, '').slice(0, 14);
  return shareArtifact(
    exported.blob,
    `vis-diagnostics-${stamp}-${APP_BUILD_COMMIT}${exported.suffix}`,
    exported.mediaType,
    {
      title: `Vis diagnostics ${APP_VERSION} (${APP_BUILD_NUMBER})`,
      dialogTitle: 'Export Vis diagnostics',
      noun: 'Diagnostics',
    },
  );
}

/** Install crash, lifecycle and connectivity capture before React paints its first frame. */
export function installDiagnostics(): void {
  if (installed) return;
  installed = true;
  enqueue(() => pruneRetainedLogs(MAX_FILE_BYTES));
  recordDiagnostic('info', 'app', 'started', {
    version: APP_VERSION,
    build: APP_BUILD_NUMBER,
    commit: APP_BUILD_COMMIT,
    platform: Capacitor.getPlatform(),
    online: typeof navigator === 'undefined' ? null : navigator.onLine,
  });

  if (typeof window !== 'undefined') {
    window.addEventListener('error', (event) => {
      recordDiagnostic('error', 'window', 'uncaught_error', {
        message: event.message,
        source: event.filename,
        line: event.lineno,
        column: event.colno,
        error: event.error,
      });
    });
    window.addEventListener('unhandledrejection', (event) => {
      recordDiagnostic('error', 'window', 'unhandled_rejection', { reason: event.reason });
    });
    window.addEventListener('online', () => recordDiagnostic('info', 'network', 'online'));
    window.addEventListener('offline', () => recordDiagnostic('warn', 'network', 'offline'));
  }

  onAway(recordBackgrounded);
  onWake(({ awayMs }) => recordWake(awayMs));

  if (Capacitor.isNativePlatform()) {
    void CapacitorApp.getInfo()
      .then((info) => recordDiagnostic('info', 'app', 'native_build', info))
      .catch((cause) => recordDiagnostic('warn', 'app', 'native_build_unavailable', { cause }));
  }
}
