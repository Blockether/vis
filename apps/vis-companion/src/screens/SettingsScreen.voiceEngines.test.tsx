// @vitest-environment jsdom
import { fireEvent, screen, waitFor, within } from '@testing-library/react';
import { renderOpenBands } from '../test-settings';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { useState } from 'react';

import { SpeechEnginesPanel } from './settings/SpeechSettings';
import { GatewayError } from '../lib/gateway';
import type { GatewayClient } from '../lib/gateway';
import { getSpeechPrefs } from '../lib/storage';
import type { SpeechPrefs, VoiceModelState } from '../lib/types';

class DevicePreviewUtterance {
  onend: (() => void) | null = null;
  onerror: ((event: { error: string }) => void) | null = null;
  rate = 1;
  voice: SpeechSynthesisVoice | null = null;

  readonly text: string;

  constructor(text: string) {
    this.text = text;
  }
}

const initialPrefs: SpeechPrefs = {
  asrEngine: null,
  ttsEngine: null,
  deviceVoice: null,
  gatewayVoice: null,
  rate: 1,
};

const ready = (engine: string): VoiceModelState => ({ status: 'ready', engine });
const absent = (message: string, reasons?: string[]) =>
  new GatewayError(501, message, {
    error: { type: 'engine-unavailable', message, ...(reasons ? { reasons } : {}) },
  });

function machine(
  listen: VoiceModelState | Error = ready('parakeet-local'),
  speak: VoiceModelState | Error = ready('pocket-tts-local'),
  options: { hasAsr?: boolean; hasTts?: boolean } = {},
) {
  const asked: string[] = [];
  const reply =
    (which: 'asr' | 'tts', answer: VoiceModelState | Error) =>
    ({ start = false, engine }: { start?: boolean; engine?: string | null } = {}) => {
      asked.push(`${which}:${engine ?? 'default'}:${start ? 'start' : 'read'}`);
      return answer instanceof Error ? Promise.reject(answer) : Promise.resolve(answer);
    };
  const asrEngines =
    options.hasAsr === false
      ? []
      : [
          { id: 'parakeet-local', label: 'Parakeet (local)' },
          { id: 'whisper-local', label: 'Whisper (local)' },
        ];
  const ttsEngines =
    options.hasTts === false
      ? []
      : [
          { id: 'piper-local', label: 'Piper (local)', voices: [] },
          { id: 'pocket-tts-local', label: 'Pocket TTS (local)', voices: [] },
        ];
  return {
    asked,
    client: {
      cachedCapabilities: vi.fn(() => null),
      capabilities: vi.fn().mockResolvedValue({
        features: {
          voice: {
            engines: asrEngines,
            selected: asrEngines[0]?.id ?? null,
            model: listen instanceof Error ? null : listen,
          },
          speech: {
            engines: ttsEngines,
            selected: ttsEngines[0]?.id ?? null,
            model: speak instanceof Error ? null : speak,
          },
        },
      }),
      voiceModel: vi.fn(reply('asr', listen)),
      speechModel: vi.fn(reply('tts', speak)),
      speechVoices: vi.fn(({ engine }: { engine?: string | null } = {}) =>
        Promise.resolve({
          engine: { id: engine ?? 'piper-local', label: 'Piper (local)' },
          voices: [
            {
              id: 'amy',
              label: 'Amy',
              language: 'en-US',
              model: { status: 'ready', engine: engine ?? 'piper-local' },
            },
          ],
        }),
      ),
    } as unknown as GatewayClient,
  };
}

function Harness({
  client,
  beforePaint,
}: {
  client: GatewayClient;
  beforePaint?: (prefs: SpeechPrefs) => Promise<void> | undefined;
}) {
  const [prefs, setPrefs] = useState(initialPrefs);
  async function onChange(write: () => Promise<void>) {
    await write();
    const next = await getSpeechPrefs();
    await beforePaint?.(next);
    setPrefs(next);
    return next;
  }
  return <SpeechEnginesPanel client={client} prefs={prefs} onChange={onChange} />;
}

beforeEach(() => {
  localStorage.clear();
  Object.defineProperty(window, 'speechSynthesis', {
    configurable: true,
    value: {
      getVoices: () => [
        { voiceURI: 'device-default', name: 'Daniel', lang: 'en-GB', default: true },
        {
          voiceURI: 'com.apple.voice.premium.en-US.Samantha',
          name: 'Samantha',
          lang: 'en-US',
          default: false,
        },
        {
          voiceURI: 'com.apple.voice.premium.en-US.Ava',
          name: 'Ava',
          lang: 'en-US',
          default: false,
        },
        {
          voiceURI: 'com.apple.voice.enhanced.en-US.Alex',
          name: 'Alex',
          lang: 'en-US',
          default: false,
        },
        {
          voiceURI: 'com.apple.voice.natural.en-US.Tom',
          name: 'Tom',
          lang: 'en-US',
          default: false,
        },
        {
          voiceURI: 'com.apple.voice.enhanced.en-US.Zoe',
          name: 'Zoe',
          lang: 'en-US',
          default: false,
        },
        {
          voiceURI: 'com.apple.voice.compact.en-US.Fred',
          name: 'Fred Compact',
          lang: 'en-US',
          default: false,
        },
      ],
      speak: () => undefined,
      cancel: () => undefined,
    },
  });
});

afterEach(() => {
  document.body.innerHTML = '';
  vi.restoreAllMocks();
});

function choice(name: RegExp) {
  return screen
    .getAllByRole('button', { name })
    .find((button) => button.hasAttribute('aria-pressed'));
}
describe('the speech-engines band', () => {
  it('stores the specific ASR engine chosen from the expanded list', async () => {
    const { client, asked } = machine();
    renderOpenBands(<Harness client={client} />);

    fireEvent.click(await screen.findByRole('button', { name: /ASR/ }));
    const whisper = await screen.findByRole('button', { name: /Whisper \(gateway\)/ });
    fireEvent.click(whisper);

    await waitFor(async () => expect((await getSpeechPrefs()).asrEngine).toBe('whisper-local'));
    // Preference persistence can settle before React paints the selected state.
    await waitFor(() => expect(whisper.getAttribute('aria-pressed')).toBe('true'));
    await waitFor(() => expect(asked).toContain('asr:whisper-local:read'));
  });

  it("separates gateway engines from the selected device's premium voices", async () => {
    const { client } = machine();
    let releasePaint!: () => void;
    const pendingPaint = new Promise<void>((resolve) => {
      releasePaint = resolve;
    });
    renderOpenBands(
      <Harness
        client={client}
        beforePaint={(prefs) => (prefs.ttsEngine === 'pocket-tts-local' ? pendingPaint : undefined)}
      />,
    );

    fireEvent.click(await screen.findByRole('button', { name: /TTS/ }));
    const engines = await screen.findByRole('group', { name: 'TTS engines' });
    const device = choice(/This device/)!;
    const pocket = choice(/Pocket TTS \(gateway\)/)!;
    expect(device.getAttribute('aria-pressed')).toBe('true');
    expect(choice(/Piper \(gateway\)/)).toBeVisible();
    const deviceEngine = device.closest('[data-speech-engine="device"]') as HTMLElement;
    expect(screen.queryByRole('button', { name: /Piper \(local\)/ })).toBeNull();

    fireEvent.click(within(engines).getByRole('button', { name: 'Settings for This device' }));
    const deviceVoices = within(deviceEngine).getByRole('group', { name: 'Voices' });
    expect(within(deviceVoices).getByRole('button', { name: /^Samantha/ })).toBeVisible();
    expect(within(deviceVoices).getByRole('button', { name: /^Ava/ })).toBeVisible();
    expect(within(deviceVoices).getByRole('button', { name: /^Tom/ })).toBeVisible();
    expect(within(deviceVoices).queryByRole('button', { name: /Piper/ })).toBeNull();
    expect(screen.queryByRole('button', { name: /Alex/ })).toBeNull();
    expect(screen.queryByRole('button', { name: /Zoe/ })).toBeNull();
    expect(screen.queryByRole('button', { name: /Daniel/ })).toBeNull();
    expect(screen.queryByRole('button', { name: /Fred Compact/ })).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: /^Samantha/ }));
    await waitFor(async () =>
      expect((await getSpeechPrefs()).deviceVoice).toBe('com.apple.voice.premium.en-US.Samantha'),
    );
    await waitFor(() =>
      expect(screen.getByRole('button', { name: /TTS.*This device · Samantha/ })).toBeVisible(),
    );
    fireEvent.click(pocket);

    await waitFor(async () => expect((await getSpeechPrefs()).ttsEngine).toBe('pocket-tts-local'));
    // CI run 35070917117: persisted preferences do not imply a painted selection.
    expect(pocket.getAttribute('aria-pressed')).toBe('false');
    releasePaint();
    await waitFor(() => expect(pocket.getAttribute('aria-pressed')).toBe('true'));
    expect(screen.getByRole('button', { name: /^Samantha/ })).toBeVisible();
  });

  // Regression, user report: iOS listed its system voices but gave them no sample action,
  // unlike every gateway voice beside them.
  it('plays and stops an exact system voice on this device', async () => {
    const { client } = machine();
    const spoken: DevicePreviewUtterance[] = [];
    const speak = vi.fn((utterance: DevicePreviewUtterance) => spoken.push(utterance));
    const cancel = vi.fn();
    const synthesis = window.speechSynthesis;
    Object.defineProperty(window, 'speechSynthesis', {
      configurable: true,
      value: { getVoices: () => synthesis.getVoices(), speak, cancel },
    });
    vi.stubGlobal('SpeechSynthesisUtterance', DevicePreviewUtterance);
    renderOpenBands(<Harness client={client} />);

    fireEvent.click(await screen.findByRole('button', { name: /TTS/ }));
    const engines = await screen.findByRole('group', { name: 'TTS engines' });
    fireEvent.click(within(engines).getByRole('button', { name: 'Settings for This device' }));

    fireEvent.click(await screen.findByRole('button', { name: 'Play a sample of Samantha' }));
    expect(await screen.findByRole('button', { name: 'Stop the sample of Samantha' })).toBeVisible();
    expect(spoken).toHaveLength(1);
    expect(spoken[0]?.text.length).toBeGreaterThan(0);
    expect(spoken[0]?.voice?.voiceURI).toBe('com.apple.voice.premium.en-US.Samantha');
    expect(screen.getByRole('button', { name: 'Play a sample of Ava' })).toBeVisible();

    fireEvent.click(screen.getByRole('button', { name: 'Play a sample of Ava' }));
    expect(await screen.findByRole('button', { name: 'Play a sample of Samantha' })).toBeVisible();
    const stopAva = screen.getByRole('button', { name: 'Stop the sample of Ava' });
    fireEvent.click(stopAva);

    expect(await screen.findByRole('button', { name: 'Play a sample of Ava' })).toBeVisible();
    expect(cancel).toHaveBeenCalled();
  });
  // Regression, user report: an engine whose model was not downloaded showed its Voices
  // group with a "not ready" error. Voices belong to a downloaded model, so only the
  // download action shows until then.
  it('shows no voices for a TTS engine until its model is downloaded', async () => {
    const { client } = machine(ready('parakeet-local'), {
      status: 'absent',
      engine: 'pocket-tts-local',
    });
    renderOpenBands(<Harness client={client} />);

    fireEvent.click(await screen.findByRole('button', { name: /TTS/ }));
    fireEvent.click(
      await screen.findByRole('button', { name: 'Settings for Pocket TTS (gateway)' }),
    );
    const engine = choice(/Pocket TTS \(gateway\)/)!.closest(
      '[data-speech-engine="pocket-tts-local"]',
    ) as HTMLElement;
    expect(
      await within(engine).findByRole('button', { name: 'Download Pocket TTS model' }),
    ).toBeVisible();
    expect(within(engine).queryByRole('group', { name: 'Voices' })).toBeNull();
    expect(client.speechVoices).not.toHaveBeenCalledWith(
      expect.objectContaining({ engine: 'pocket-tts-local' }),
    );
  });
  it('reads and downloads an unselected engine from its own row', async () => {
    const { client, asked } = machine();
    vi.mocked(client.voiceModel).mockImplementation(
      ({ start = false, engine }: { start?: boolean; engine?: string | null } = {}) => {
        asked.push(`asr:${engine ?? 'default'}:${start ? 'start' : 'read'}`);
        if (engine === 'whisper-local') {
          return Promise.resolve(
            start ? { status: 'downloading', engine, progress: 0 } : { status: 'absent', engine },
          );
        }
        return Promise.resolve(ready(engine ?? 'parakeet-local'));
      },
    );
    renderOpenBands(<Harness client={client} />);

    fireEvent.click(await screen.findByRole('button', { name: /ASR/ }));
    await waitFor(() => expect(asked).toContain('asr:whisper-local:read'));
    expect(choice(/Parakeet \(gateway\)/)?.getAttribute('aria-pressed')).toBe('true');
    fireEvent.click(await screen.findByRole('button', { name: 'Download Whisper model' }));

    await waitFor(() => expect(asked).toContain('asr:whisper-local:start'));
    expect(choice(/Parakeet \(gateway\)/)?.getAttribute('aria-pressed')).toBe('true');
  });

  it('reports a failed selected engine and retries only that engine', async () => {
    const { client, asked } = machine({
      status: 'failed',
      engine: 'parakeet-local',
      error: 'the archive did not match its checksum',
    });
    renderOpenBands(<Harness client={client} />);

    fireEvent.click(await screen.findByRole('button', { name: /ASR/ }));
    expect(
      (await screen.findAllByText('the archive did not match its checksum')).length,
    ).toBeGreaterThan(0);
    fireEvent.click(screen.getByRole('button', { name: 'Retry Parakeet model' }));

    await waitFor(() => expect(asked).toContain('asr:parakeet-local:start'));
    expect(asked.filter((one) => one.startsWith('tts:') && one.endsWith(':start'))).toHaveLength(0);
  });

  it("keeps both disclosures useful when the machine's voice runtime is unavailable", async () => {
    const { client } = machine(
      absent('no speech transcription engine is available', [
        'the built-in speech runtime could not load its native library',
      ]),
      absent('no speech synthesis engine is registered'),
      { hasAsr: false, hasTts: false },
    );
    renderOpenBands(<Harness client={client} />);

    fireEvent.click(await screen.findByRole('button', { name: /ASR/ }));
    expect(await screen.findByText(/could not load its native library/)).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: /TTS/ }));
    await waitFor(() => expect(choice(/This device/)).toBeVisible());
  });
});
