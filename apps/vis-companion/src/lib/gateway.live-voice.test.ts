// @vitest-environment jsdom
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient } from './gateway';
import { setSpeechGatewayVoice, setSpeechTtsEngine } from './storage';
import type { LiveVoiceCapabilities } from './live-voice-protocol';

afterEach(() => {
  vi.unstubAllGlobals();
});
it('uses authenticated HTTP lifecycle requests and sends only the one-use ticket to media', async () => {
  const fetching = vi.fn(
    async () =>
      new Response('{}', { status: 200, headers: { 'Content-Type': 'application/json' } }),
  );
  vi.stubGlobal('fetch', fetching);
  const client = new GatewayClient({
    url: 'https://gateway.example.com',
    token: 'test-only-token',
  });
  await client.liveVoiceCapabilities();
  await client.startLiveVoice('a/b', 'client-id', true);
  await client.liveVoiceSnapshot('a/b', 'voice-id');
  await client.endLiveVoice('a/b', 'voice-id');
  expect(fetching.mock.calls).toHaveLength(4);
  const calls = fetching.mock.calls as unknown as [string, RequestInit][];
  expect(calls[1][0]).toContain('/v1/sessions/a%2Fb/voice/live');
  expect(JSON.parse(String(calls[1][1].body))).toEqual({
    schema: 1,
    client_id: 'client-id',
    language: 'en',
    privacy: 'local_speech',
    muted: true,
  });
  for (const [, init] of calls)
    expect(new Headers(init.headers).get('Authorization')).toBe('Bearer test-only-token');
  expect(calls[3][1].method).toBe('DELETE');
  expect(client.liveVoiceMediaUrl('/v1/sessions/a/voice/live/v/media')).toBe(
    'wss://gateway.example.com/v1/sessions/a/voice/live/v/media',
  );
  expect(() => client.liveVoiceMediaUrl('https://other.example.com/v1/sessions/a/media')).toThrow();
  expect(() => client.liveVoiceMediaUrl('/v1/sessions/a/media?token=secret')).toThrow();
  const prefixed = new GatewayClient({ url: 'https://gateway.example.com/vis' });
  expect(prefixed.liveVoiceMediaUrl('/v1/sessions/a/voice/live/v/media')).toBe(
    'wss://gateway.example.com/vis/v1/sessions/a/voice/live/v/media',
  );
});

it.each([
  ['pocket-tts-local', 'my-recording'],
  ['piper-local', undefined],
])('sends the saved voice only for Pocket: %s', async (engine, expected) => {
  await setSpeechTtsEngine(engine!);
  await setSpeechGatewayVoice('my-recording');
  const fetcher = vi.fn(async (_url: string, init: RequestInit) => {
    expect(JSON.parse(String(init.body)).pocket_voice_id).toBe(expected);
    return new Response('{}', { headers: { 'Content-Type': 'application/json' } });
  });
  vi.stubGlobal('fetch', fetcher);
  try {
    await new GatewayClient({ url: 'https://gateway.example.com' }).startLiveVoice('s', 'c', false);
    expect(fetcher).toHaveBeenCalledOnce();
  } finally {
    await setSpeechGatewayVoice(null);
    await setSpeechTtsEngine(null);
  }
});

// Match gateway.voice/capabilities after canonical snake_case transport encoding.
it('preserves the structured live voice profile', async () => {
  const capabilities: LiveVoiceCapabilities = {
    enabled: false,
    available: false,
    profile: {
      language: 'en',
      privacy: 'local_speech',
      concurrency: 1,
      clients: ['companion_desktop', 'tui_headset'],
      mobile_qualified: false,
      playback_precision: 'rendered_minus_latency',
    },
  };
  vi.stubGlobal(
    'fetch',
    vi.fn(
      async () =>
        new Response(JSON.stringify(capabilities), {
          status: 200,
          headers: { 'Content-Type': 'application/json' },
        }),
    ),
  );
  const client = new GatewayClient({ url: 'https://gateway.example.com' });
  expect((await client.liveVoiceCapabilities()).profile).toEqual(capabilities.profile);
});
