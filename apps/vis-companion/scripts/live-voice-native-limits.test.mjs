import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

const root = join(dirname(fileURLToPath(import.meta.url)), '..');
const schema = join(root, '../../packages/vis-contract/resources/vis-contract/schema/voice.json');
const limits = JSON.parse(readFileSync(schema, 'utf8'))['x-vis-voice-limits'];
const expected = {
  rate: limits.sample_rate,
  frame: limits.frame_samples,
  inputFrames: limits.input_buffer_ms / limits.frame_ms,
  outputSamples: (limits.sample_rate * limits.output_credit_ms) / 1000,
};

function numbers(file, patterns) {
  const text = readFileSync(join(root, 'native', file), 'utf8');
  return Object.fromEntries(
    Object.entries(patterns).map(([name, pattern]) => [name, Number(text.match(pattern)?.[1])]),
  );
}

// The native code cannot read the schema, so its bounds must not drift from it.
describe('native live voice bounds', () => {
  it('match the voice contract on Android', () => {
    expect(
      numbers('android/LiveVoiceState.java', {
        rate: /\bRATE = (\d+);/,
        frame: /\bFRAME = (\d+);/,
        inputFrames: /\bINPUT_FRAMES = (\d+);/,
        outputSamples: /\bOUTPUT_SAMPLES = (\d+);/,
      }),
    ).toEqual(expected);
  });

  it('match the voice contract on iOS', () => {
    expect({
      ...numbers('live-voice/Sources/LiveVoice/LiveVoiceState.swift', {
        frame: /frameSamples == (\d+)/,
        inputFrames: /inputBufferFrames == (\d+)/,
        outputSamples: /outputBufferSamples == (\d+)/,
      }),
      ...numbers('ios/LiveVoicePlugin.swift', { rate: /"sampleRate"\) == (\d+)/ }),
    }).toEqual(expected);
  });
});
