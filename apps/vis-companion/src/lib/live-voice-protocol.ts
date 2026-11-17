import schema from '../../../../packages/vis-contract/resources/vis-contract/schema/voice.json';

export const LIVE_VOICE_LIMITS = schema['x-vis-voice-limits'];
const FRAME = schema['x-vis-voice-frame'];
export type LiveVoiceState = {
  voice_session_id: string;
  capture_generation: number;
  output_epoch: number;
  capture: string;
  utterance: string;
  agent: string;
  output: string;
};
export type LiveVoiceCapabilities = {
  enabled: boolean;
  available: boolean;
  reason?: string;
  profile?: {
    language: string;
    privacy: string;
    concurrency: number;
    clients: string[];
    mobile_qualified: boolean;
    playback_precision: string;
  };
  limits?: Record<string, number>;
};
export type LiveVoiceStart = {
  voice_session_id: string;
  client_id: string;
  media_path: string;
  ticket: string;
  state: LiveVoiceState;
  limits: Record<string, number>;
  capabilities: LiveVoiceCapabilities;
};
export type VoiceAdmissionReceipt = {
  voice_session_id: string;
  utterance_id: number;
  idempotency_key: string;
  text: string;
  revision: number;
  turn_id?: string;
  status?: 'queued' | 'running';
  error?: string;
};
export type LiveVoiceSnapshot = { state: LiveVoiceState; receipts: VoiceAdmissionReceipt[] };
export type LiveVoiceEvent =
  | {
      type: 'session.ready';
      state: LiveVoiceState;
      capabilities: LiveVoiceCapabilities;
      limits: Record<string, number>;
    }
  | { type: 'session.state'; state: LiveVoiceState }
  | {
      type: 'task.cancellation';
      status: 'requested' | 'ambiguous' | 'none';
      message: string;
      turn_id?: string;
    }
  | { type: 'utterance.rejected'; utterance_id: number; reason: 'input_overflow' }
  | { type: 'transcript'; utterance_id: number; revision: number; text: string; final: boolean }
  | {
      type: 'utterance.admitted';
      utterance_id: number;
      turn_id: string;
      idempotency_key: string;
      status: 'queued' | 'running';
    }
  | {
      type: 'speech.committed';
      output_epoch: number;
      segment_id: number;
      turn_id: string;
      text: string;
      source: string;
    }
  | { type: 'output.interrupted'; output_epoch: number; abort: 'requested' | 'acknowledged' }
  | { type: 'output.finished'; output_epoch: number; segment_id: number; sample_count: number }
  | { type: 'session.closed'; reason: string }
  | { type: 'error'; code: string; message: string; fatal: boolean };

// Validate the portable subset used by this schema, not a second event catalog.
type Rule = {
  $ref?: string;
  oneOf?: Rule[];
  const?: unknown;
  enum?: unknown[];
  type?: string;
  properties?: Record<string, Rule>;
  required?: string[];
  additionalProperties?: boolean;
  minimum?: number;
  maximum?: number;
  maxLength?: number;
};
function valid(rule: Rule, value: unknown): boolean {
  if (rule.$ref)
    return valid((schema.$defs as Record<string, Rule>)[rule.$ref.split('/').at(-1)!], value);
  if (rule.oneOf) return rule.oneOf.filter((part) => valid(part, value)).length === 1;
  if ('const' in rule && value !== rule.const) return false;
  if (rule.enum && !rule.enum.includes(value)) return false;
  if (rule.type === 'integer')
    return (
      Number.isInteger(value) &&
      Number(value) >= (rule.minimum ?? -Infinity) &&
      Number(value) <= (rule.maximum ?? Infinity)
    );
  if (rule.type === 'string')
    return typeof value === 'string' && value.length <= (rule.maxLength ?? Infinity);
  if (rule.type === 'boolean') return typeof value === 'boolean';
  if (rule.type === 'object') {
    if (!value || typeof value !== 'object' || Array.isArray(value)) return false;
    const row = value as Record<string, unknown>;
    return (
      (rule.required ?? []).every((key) => key in row) &&
      Object.entries(row).every(([key, item]) =>
        rule.properties && Object.hasOwn(rule.properties, key)
          ? valid(rule.properties[key], item)
          : rule.additionalProperties !== false,
      )
    );
  }
  return true;
}
export function parseLiveVoiceEvent(text: string): LiveVoiceEvent {
  if (new TextEncoder().encode(text).length > LIVE_VOICE_LIMITS.max_control_bytes)
    throw new Error('Voice control exceeds its limit.');
  const value: unknown = JSON.parse(text);
  if (!valid(schema.$defs.server_event, value)) throw new Error('Invalid voice event.');
  return value as LiveVoiceEvent;
}
export function encodeLiveVoiceControl(value: unknown): string {
  if (!valid(schema.$defs.client_control, value)) throw new Error('Invalid voice control.');
  return JSON.stringify(value);
}
export type VoiceFrame = {
  kind: 'input' | 'output';
  generation: number;
  streamId: number;
  sequence: number;
  sampleOffset: number;
  samples: Int16Array;
};
export function encodeVoiceFrame(frame: VoiceFrame): ArrayBuffer {
  const count = frame.samples.length;
  if (count < 1 || count > LIVE_VOICE_LIMITS.frame_samples)
    throw new Error('Invalid voice frame size.');
  const buffer = new ArrayBuffer(LIVE_VOICE_LIMITS.header_bytes + count * 2);
  const view = new DataView(buffer);
  [...FRAME.magic].forEach((c, i) => view.setUint8(i, c.charCodeAt(0)));
  view.setUint8(4, FRAME.version);
  view.setUint8(5, FRAME.kinds[frame.kind]);
  [
    frame.generation,
    frame.streamId,
    frame.sequence,
    frame.sampleOffset,
    LIVE_VOICE_LIMITS.sample_rate,
    count,
  ].forEach((value, i) => {
    if (!Number.isInteger(value) || value < 0 || value > 0xffffffff)
      throw new Error('Invalid voice frame header.');
    view.setUint32(8 + i * 4, value, true);
  });
  frame.samples.forEach((value, i) =>
    view.setInt16(LIVE_VOICE_LIMITS.header_bytes + i * 2, value, true),
  );
  return buffer;
}
export function decodeVoiceFrame(buffer: ArrayBuffer): VoiceFrame {
  const view = new DataView(buffer);
  if (
    buffer.byteLength < LIVE_VOICE_LIMITS.header_bytes ||
    [...FRAME.magic].some((c, i) => view.getUint8(i) !== c.charCodeAt(0)) ||
    view.getUint8(4) !== FRAME.version ||
    view.getUint16(6) !== 0 ||
    !Object.values(FRAME.kinds).includes(view.getUint8(5))
  )
    throw new Error('Invalid voice frame header.');
  const count = view.getUint32(28, true);
  if (
    view.getUint32(24, true) !== LIVE_VOICE_LIMITS.sample_rate ||
    count < 1 ||
    count > LIVE_VOICE_LIMITS.frame_samples ||
    buffer.byteLength !== LIVE_VOICE_LIMITS.header_bytes + count * 2
  )
    throw new Error('Invalid voice frame size.');
  const samples = new Int16Array(count);
  for (let i = 0; i < count; i++)
    samples[i] = view.getInt16(LIVE_VOICE_LIMITS.header_bytes + i * 2, true);
  return {
    kind: view.getUint8(5) === FRAME.kinds.input ? 'input' : 'output',
    generation: view.getUint32(8, true),
    streamId: view.getUint32(12, true),
    sequence: view.getUint32(16, true),
    sampleOffset: view.getUint32(20, true),
    samples,
  };
}
