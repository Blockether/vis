import { Buffer } from 'node:buffer';
import { describe, expect, it } from 'vitest';
import { VoiceEchoGuard, VoiceEndpoint } from './live-voice-dsp';
import { LIVE_VOICE_LIMITS as L } from './live-voice-protocol';

function voiceSample(index: number): number {
  const envelope = 0.65 + 0.25 * Math.sin(index * 0.00037);
  return envelope * (8000 * Math.sin(index * 0.073 + 2 * Math.sin(index * 0.0031)) +
    3500 * Math.sin(index * 0.117 + Math.sin(index * 0.0013)));
}

function voiceFrame(frame: number): Int16Array {
  return Int16Array.from({ length: L.frame_samples }, (_, i) =>
    Math.round(voiceSample(frame * L.frame_samples + i)));
}

function spokenFrames(variable = 'VIS_TEST_LIVE_ECHO_PCM'): Int16Array[] {
  const bytes = Buffer.from(process.env[variable]!, 'base64');
  const speech = Int16Array.from({ length: bytes.length / 2 }, (_, i) => bytes.readInt16LE(i * 2));
  expect(speech.length).toBeGreaterThan((L.sample_rate * L.min_speech_ms * 2) / 1000);
  return Array.from({ length: Math.ceil(speech.length / L.frame_samples) }, (_, i) => {
    const samples = new Int16Array(L.frame_samples);
    samples.set(speech.subarray(i * L.frame_samples, (i + 1) * L.frame_samples));
    return samples;
  });
}

function frameSample(frames: Int16Array[], index: number): number {
  return index < 0 || index >= frames.length * L.frame_samples ? 0 :
    frames[Math.floor(index / L.frame_samples)][index % L.frame_samples];
}

function echoHarness() {
  let clock = 0;
  const guard = new VoiceEchoGuard(0.015, () => clock);
  const endpoint = new VoiceEndpoint();
  let starts = 0;
  return {
    guard,
    played: (samples: Int16Array) => guard.submitted(samples)(samples.length),
    input: (samples: Int16Array) => {
      clock += L.frame_ms;
      const filtered = guard.filter(samples);
      starts += endpoint.push(filtered).filter((event) => event.type === 'start').length;
      return filtered;
    },
    advance: (ms: number) => { clock += ms; },
    starts: () => starts,
  };
}

describe('local residual echo guard', () => {
  // Set VIS_TEST_LIVE_ECHO_PCM to base64 PCM from the installed Pocket model, at 24 kHz, S16LE.
  it.skipIf(!process.env.VIS_TEST_LIVE_ECHO_PCM).each([0, 2, 10])(
    'rejects local spoken output delayed by %s frames', (delay) => {
      const frames = spokenFrames();
      const h = echoHarness();
      for (let frame = 0; frame < frames.length + delay; frame++) {
        if (frame < frames.length) h.played(frames[frame]);
        h.input(frame < delay ? new Int16Array(L.frame_samples) :
          frames[frame - delay].map((sample) => Math.round(sample * 0.45)));
      }
      expect(h.starts()).toBe(0);
    },
  );
  it.skipIf(!process.env.VIS_TEST_LIVE_ECHO_PCM).each(['high-pass', 'reflection', 'clipping'])(
    'rejects local spoken output with %s distortion', (mode) => {
      const frames = spokenFrames();
      const source = (index: number) => frameSample(frames, index);
      const h = echoHarness();
      let previous = 0;
      let highPass = 0;
      const admissions: number[] = [];
      for (let frame = 0; frame < frames.length + 2; frame++) {
        if (frame < frames.length) h.played(frames[frame]);
        const starts = h.starts();
        h.input(Int16Array.from({ length: L.frame_samples }, (_, i) => {
          const t = (frame - 2) * L.frame_samples + i;
          const sample = 0.45 * source(t);
          highPass = 0.98 * (highPass + sample - previous);
          previous = sample;
          if (mode === 'high-pass') return Math.round(highPass);
          if (mode === 'reflection') return Math.round(0.4 * source(t) + 0.1 * source(t - 237));
          return Math.round(Math.max(-1800, Math.min(1800, sample)));
        }));
        if (h.starts() > starts) admissions.push(frame);
      }
      expect(admissions).toEqual([]);
    },
  );
  // Use a different spoken sample for VIS_TEST_LIVE_ECHO_NEAR_PCM.
  it.skipIf(!process.env.VIS_TEST_LIVE_ECHO_PCM || !process.env.VIS_TEST_LIVE_ECHO_NEAR_PCM).each(
    [0.016, 0.025, 0.06].flatMap((rms) => ['direct', 'high-pass', 'clipping', 'reflection'].map((mode) => ({ rms, mode }))),
  )('preserves overlapping speech with local spoken output: $mode at $rms RMS', ({ rms, mode }) => {
    const frames = spokenFrames();
    const near = spokenFrames('VIS_TEST_LIVE_ECHO_NEAR_PCM');
    const power = near.reduce((sum, frame) => sum + frame.reduce((total, sample) =>
      total + (sample / 32768) ** 2, 0), 0) / (near.length * L.frame_samples);
    const gain = rms / Math.sqrt(power);
    const h = echoHarness();
    let previous = 0;
    let highPass = 0;
    for (let frame = 0; frame < Math.max(frames.length + 2, near.length + 20); frame++) {
      if (frame < frames.length) h.played(frames[frame]);
      const human = near[frame - 20]?.map((sample) => Math.round(sample * gain));
      const microphone = Int16Array.from({ length: L.frame_samples }, (_, i) => {
        const t = (frame - 2) * L.frame_samples + i;
        const sample = 0.45 * frameSample(frames, t);
        highPass = 0.98 * (highPass + sample - previous);
        previous = sample;
        const echo = mode === 'high-pass' ? highPass : mode === 'clipping' ?
          Math.max(-1800, Math.min(1800, sample)) : mode === 'reflection' ?
            0.4 * frameSample(frames, t) + 0.1 * frameSample(frames, t - 237) : sample;
        return Math.round(Math.max(-32768, Math.min(32767, echo + (human?.[i] ?? 0))));
      });
      const filtered = h.input(microphone);
      if (frame === 19) expect(h.starts()).toBe(0);
      const humanPower = human?.reduce((sum, sample) => sum + (sample / 32768) ** 2, 0) ?? 0;
      if (humanPower / L.frame_samples >= 0.015 ** 2)
        expect(filtered === microphone).toBe(true);
    }
    expect(h.starts()).toBe(1);
  });
  it.each([0, 13, 227, 1917, 8317])('rejects an attenuated playback copy delayed by %s samples', (delay) => {
    // Regression: the phone admitted speaker playback through its microphone.
    const h = echoHarness();
    for (let frame = 0; frame < 60; frame++) {
      h.played(voiceFrame(frame));
      h.input(Int16Array.from({ length: L.frame_samples }, (_, i) => {
        const index = frame * L.frame_samples + i - delay;
        return index < 0 ? 0 : Math.round(voiceSample(index) * 0.45);
      }));
    }
    expect(h.starts()).toBe(0);
  });

  it.each([
    ['inverted polarity', (t: number) => -0.45 * voiceSample(t)],
    ['filtered output', (t: number) => 0.45 * (0.7 * voiceSample(t) + 0.3 * voiceSample(t - 1))],
    ['room reflection', (t: number) => 0.4 * voiceSample(t) + 0.035 * voiceSample(t - 237)],
    // Keep the drift within a physical path delay, rather than reading future output.
    ['sample clock drift', (t: number) => t < 24 ? 0 : 0.45 * voiceSample((t - 24) * 1.0002)],
    ['changing gain', (t: number) => (0.35 + 0.04 * Math.sin(t * 0.00017)) * voiceSample(t)],
    ['soft clipping', (t: number) => Math.max(-2800, Math.min(2800, 0.45 * voiceSample(t)))],
  ] as const)('rejects known playback with %s', (_name, sample) => {
    const h = echoHarness();
    const unfiltered: number[] = [];
    for (let frame = 0; frame < 60; frame++) {
      h.played(voiceFrame(frame));
      const input = Int16Array.from({ length: L.frame_samples }, (_, i) =>
        Math.round(sample(frame * L.frame_samples + i)));
      if (h.input(input) === input) unfiltered.push(frame);
    }
    expect(h.starts(), `Unfiltered frames: ${unfiltered.join(', ')}`).toBe(0);
  });

  it.each([0.016, 0.025, 0.06])('keeps overlapping speech with independent RMS %s', (rms) => {
    const h = echoHarness();
    for (let frame = 0; frame < 24; frame++) {
      h.played(voiceFrame(frame));
      const microphone = Int16Array.from({ length: L.frame_samples }, (_, i) => {
        const t = frame * L.frame_samples + i;
        const human = frame < 12 ? 0 : rms * 32768 * Math.SQRT2 *
          Math.sin(t * 0.047 + 0.7 * Math.sin(t * 0.001));
        return Math.round(0.45 * voiceSample(t) + human);
      });
      const filtered = h.input(microphone);
      if (frame >= 12) expect(filtered).toEqual(microphone);
    }
    expect(h.starts()).toBe(1);
  });

  it('does not switch to an old playback match during new input', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 28; frame++) {
      h.played(voiceFrame(frame));
      const source = frame < 12 ? frame : frame - 8;
      const input = voiceFrame(source).map((sample) => Math.round(sample * 0.45));
      const filtered = h.input(input);
      if (frame >= 16) expect(filtered === input).toBe(true);
    }
    expect(h.starts()).toBe(1);
  });

  it('does not treat elapsed queued audio as rendered playback', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 12; frame++) {
      h.guard.submitted(voiceFrame(frame));
      const microphone = voiceFrame(frame).map((sample) => Math.round(sample * 0.45));
      expect(h.input(microphone) === microphone).toBe(true);
    }
    expect(h.starts()).toBe(1);
  });

  it('preserves quiet near-end speech that partly correlates with playback', () => {
    const h = echoHarness();
    let humanPower = 0;
    for (let frame = 0; frame < 32; frame++) {
      const output = voiceFrame(frame);
      h.guard.submitted(output)(L.frame_samples);
      const rms = Math.sqrt(output.reduce((sum, sample) => sum + (sample / 32768) ** 2, 0) / output.length);
      const microphone = Int16Array.from(output, (sample, i) => {
        const t = frame * L.frame_samples + i;
        const human = frame < 12 ? 0 : (0.014 * sample / rms) +
          0.008 * 32768 * Math.SQRT2 * Math.sin(t * 0.047 + 0.7 * Math.sin(t * 0.001));
        humanPower += (human / 32768) ** 2;
        return Math.round(0.45 * sample + human);
      });
      const filtered = h.input(microphone);
      if (frame >= 16) expect(filtered === microphone).toBe(true);
    }
    expect(Math.sqrt(humanPower / (20 * L.frame_samples))).toBeGreaterThan(0.015);
    expect(h.starts()).toBe(1);
  });

  it('acquires a path after capture restarts with played history', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 60; frame++) {
      h.played(voiceFrame(frame));
      h.input(new Int16Array(L.frame_samples));
    }
    h.guard.resetCapture();
    const unfiltered: number[] = [];
    for (let frame = 60; frame < 90; frame++) {
      h.played(voiceFrame(frame));
      const input = Int16Array.from({ length: L.frame_samples }, (_, i) =>
        Math.round(0.45 * voiceSample((frame * L.frame_samples + i - 24) * 1.0002)));
      if (h.input(input) === input) unfiltered.push(frame);
    }
    expect(h.starts(), `Unfiltered frames: ${unfiltered.join(', ')}`).toBe(0);
  });

  it('acquires a new path after a playback gap', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 12; frame++) {
      const output = voiceFrame(frame);
      h.played(output);
      h.input(output.map((sample) => Math.round(sample * 0.45)));
    }
    for (let frame = 0; frame < 10; frame++) h.input(new Int16Array(L.frame_samples));
    for (let frame = 100; frame < 112; frame++) {
      const output = voiceFrame(frame);
      h.played(output);
      h.input(output.map((sample) => Math.round(sample * 0.45)));
    }
    expect(h.starts()).toBe(0);
  });

  it('matches output with a full second queued ahead of capture', () => {
    const h = echoHarness();
    for (let batch = 0; batch < 3; batch++) {
      const queued: ((count: number) => void)[] = [];
      for (let frame = batch * 50; frame < (batch + 1) * 50; frame++)
        queued.push(h.guard.submitted(voiceFrame(frame)));
      for (let frame = batch * 50; frame < (batch + 1) * 50; frame++) {
        queued[frame - batch * 50](L.frame_samples);
        h.input(voiceFrame(frame).map((sample) => Math.round(sample * 0.45)));
      }
    }
    expect(h.starts()).toBe(0);
  });

  // Regression: native capture startup can delay playback beyond the echo tail.
  it.each([1500, 3000])('keeps queued references through a %s ms startup without confirming them', (delay) => {
    const h = echoHarness();
    const frames = Array.from({ length: 30 }, (_, frame) => voiceFrame(frame));
    const receipts = frames.map((samples) => h.guard.submitted(samples));
    h.advance(delay);
    const human = voiceFrame(0);
    expect(h.guard.filter(human)).toBe(human);
    for (let frame = 0; frame < frames.length; frame++) {
      receipts[frame](L.frame_samples);
      h.input(frames[frame].map((sample) => Math.round(sample * 0.45)));
    }
    expect(h.starts()).toBe(0);
  });

  it('passes speech unchanged without output and after reference expiry', () => {
    const h = echoHarness();
    const input = voiceFrame(0);
    expect(h.input(input)).toBe(input);
    for (let frame = 0; frame < 6; frame++) h.played(voiceFrame(frame));
    h.advance(3000);
    for (let frame = 0; frame < 6; frame++) {
      const microphone = voiceFrame(frame);
      expect(h.input(microphone)).toBe(microphone);
    }
    expect(h.starts()).toBe(1);
  });

  it('drops cancelled output but retains a played echo tail', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 8; frame++)
      h.guard.submitted(voiceFrame(frame))(L.frame_samples);
    const stalePlayed = h.guard.submitted(voiceFrame(80));
    h.guard.stopOutput();
    stalePlayed(L.frame_samples);
    for (let frame = 0; frame < 8; frame++)
      h.input(voiceFrame(frame).map((sample) => Math.round(sample * 0.45)));
    expect(h.starts()).toBe(0);
    h.guard.resetCapture();
    for (let frame = 80; frame < 88; frame++) {
      const microphone = voiceFrame(frame);
      expect(h.input(microphone)).toBe(microphone);
    }
    expect(h.starts()).toBe(1);
  });

  it('never joins reference samples across output gaps', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 2; frame++) h.guard.submitted(voiceFrame(frame))(L.frame_samples);
    h.advance(200);
    for (let frame = 2; frame < 4; frame++) h.played(voiceFrame(frame));
    for (let frame = 0; frame < 4; frame++) {
      const microphone = voiceFrame(frame);
      expect(h.input(microphone)).toBe(microphone);
    }
  });

  it('does not match output before a known playback start', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 8; frame++) h.guard.submitted(voiceFrame(frame), 300);
    for (let frame = 0; frame < 8; frame++) {
      const microphone = voiceFrame(frame);
      expect(h.input(microphone) === microphone).toBe(true);
    }
    expect(h.starts()).toBe(1);
  });

  it('bounds references when output contains many tiny frames', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 8; frame++) h.guard.submitted(voiceFrame(frame))(L.frame_samples);
    for (let frame = 0; frame < 1000; frame++) h.guard.submitted(new Int16Array([1]));
    for (let frame = 0; frame < 8; frame++) {
      const microphone = voiceFrame(frame);
      expect(h.input(microphone) === microphone).toBe(true);
    }
    expect(h.starts()).toBe(1);
  });

  it.each([217, 247])('keeps ambiguous input with changing pitch near %s Hz', (pitch) => {
    const h = echoHarness();
    for (let frame = 0; frame < 16; frame++) {
      const output = Int16Array.from({ length: L.frame_samples }, (_, i) => {
        const t = frame * L.frame_samples + i;
        const phase = t * 2 * Math.PI * pitch / L.sample_rate + 0.2 * Math.sin(t * 0.0013);
        return Math.round(6000 * Math.sin(phase) + 2000 * Math.sin(phase * 2));
      });
      h.played(output);
      const microphone = output.map((sample) => Math.round(sample * 0.45));
      expect(h.input(microphone) === microphone).toBe(true);
    }
    expect(h.starts()).toBe(1);
  });

  it('does not match human input against future queued audio', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 50; frame++) h.guard.submitted(voiceFrame(frame));
    for (let frame = 30; frame < 38; frame++) {
      const microphone = voiceFrame(frame);
      expect(h.input(microphone) === microphone).toBe(true);
    }
    expect(h.starts()).toBe(1);
  });

  it('drops an unconfirmed tail on stop', () => {
    const h = echoHarness();
    for (let frame = 0; frame < 8; frame++) h.guard.submitted(voiceFrame(frame));
    h.advance(160);
    h.guard.stopOutput();
    for (let frame = 0; frame < 8; frame++) {
      const microphone = voiceFrame(frame).map((sample) => Math.round(sample * 0.45));
      expect(h.input(microphone) === microphone).toBe(true);
    }
    expect(h.starts()).toBe(1);
  });

  it('does not create a match across a short output gap', () => {
    const h = echoHarness();
    h.played(voiceFrame(0));
    h.advance(25);
    for (let frame = 1; frame < 4; frame++) h.played(voiceFrame(frame));
    for (let frame = 0; frame < 4; frame++) {
      const microphone = voiceFrame(frame);
      expect(h.input(microphone) === microphone).toBe(true);
    }
  });

  it.each([193, 217, 247, 311, 391, 523])('passes ambiguous periodic human input at %s Hz', (pitch) => {
    const h = echoHarness();
    for (let frame = 0; frame < 16; frame++) {
      const output = Int16Array.from({ length: L.frame_samples }, (_, i) => {
        const phase = (frame * L.frame_samples + i) * 2 * Math.PI * pitch / L.sample_rate;
        return Math.round(6000 * Math.sin(phase) + 2000 * Math.sin(phase * 2));
      });
      h.played(output);
      const input = output.map((sample) => Math.round(sample * 0.45));
      expect(h.input(input) === input).toBe(true);
    }
    expect(h.starts()).toBe(1);
  });
});
