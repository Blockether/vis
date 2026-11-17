import { LIVE_VOICE_LIMITS as L } from './live-voice-protocol';

/** Keep the interpolation position across render blocks, including non-integer ratios. */
export class VoiceResampler {
  private previous = 0;
  private index = 0;
  private next = 0;
  private pending: number[] = [];
  readonly inputRate: number;
  constructor(inputRate: number) {
    if (!Number.isFinite(inputRate) || inputRate < L.sample_rate)
      throw new Error('Unsupported microphone sample rate.');
    this.inputRate = inputRate;
  }
  push(input: Float32Array): Int16Array[] {
    const frames: Int16Array[] = [];
    for (const sample of input) {
      while (this.next <= this.index) {
        const fraction = this.next - (this.index - 1);
        const value =
          this.index === 0 ? sample : this.previous + fraction * (sample - this.previous);
        this.pending.push(Math.round(Math.max(-1, Math.min(1, value)) * 32767));
        if (this.pending.length === L.frame_samples) {
          frames.push(Int16Array.from(this.pending));
          this.pending = [];
        }
        this.next += this.inputRate / L.sample_rate;
      }
      this.previous = sample;
      this.index++;
    }
    return frames;
  }
}

export type VadEvent = { type: 'start' } | { type: 'end' } | { type: 'frame'; samples: Int16Array };
/** A deterministic pilot gate. Acoustic qualification is separate from these thresholds. */
export class VoiceEndpoint {
  private preroll: Int16Array[] = [];
  private active = false;
  private loudMs = 0;
  private quietMs = 0;
  private durationMs = 0;
  readonly threshold: number;
  constructor(threshold = 0.015) {
    this.threshold = threshold;
  }
  reset(): void {
    this.preroll = [];
    this.active = false;
    this.loudMs = this.quietMs = this.durationMs = 0;
  }
  push(samples: Int16Array): VadEvent[] {
    const ms = (samples.length * 1000) / L.sample_rate;
    const energy = Math.sqrt(
      samples.reduce((sum, v) => sum + (v / 32768) ** 2, 0) / samples.length,
    );
    const loud = energy >= this.threshold;
    if (!this.active) {
      this.preroll.push(samples);
      while (this.preroll.length > L.preroll_ms / L.frame_ms) this.preroll.shift();
      this.loudMs = loud ? this.loudMs + ms : 0;
      if (this.loudMs < L.min_speech_ms) return [];
      this.active = true;
      this.quietMs = 0;
      this.durationMs = this.preroll.length * L.frame_ms;
      const events: VadEvent[] = [
        { type: 'start' },
        ...this.preroll.map((frame) => ({ type: 'frame' as const, samples: frame })),
      ];
      this.preroll = [];
      return events;
    }
    this.durationMs += ms;
    this.quietMs = loud ? 0 : this.quietMs + ms;
    const events: VadEvent[] = [{ type: 'frame', samples }];
    if (this.quietMs >= L.endpoint_silence_ms || this.durationMs >= L.max_utterance_ms) {
      events.push({ type: 'end' });
      this.reset();
    }
    return events;
  }
}

const ECHO_DOWNSAMPLE = 8;
const ECHO_WINDOW = (L.sample_rate * 80) / 1000;
const ECHO_TAIL_MS = 1200;
const ECHO_HISTORY = (L.sample_rate * (L.output_credit_ms + ECHO_TAIL_MS)) / 1000;
const ECHO_FRAMES = Math.ceil(ECHO_HISTORY / L.frame_samples) + 1;
type EchoReference = {
  samples: Int16Array;
  start: number;
  end: number;
  played: number;
  playedAt: number;
  run: number;
};

/** Compare input with confirmed output. This guard does not replace AEC or grant playback credit. */
export class VoiceEchoGuard {
  private output: EchoReference[] = [];
  private microphone: number[] = [];
  private capturedSamples = 0;
  private matchedReference: { frame: EchoReference; offset: number; captured: number } | null = null;
  private nextStart = 0;
  private run = 0;
  private readonly threshold: number;
  private readonly now: () => number;

  constructor(threshold: number, now = () => performance.now()) {
    this.threshold = threshold;
    this.now = now;
  }

  submitted(samples: Int16Array, startAt = this.now()): (played: number) => void {
    const now = this.now();
    const start = Math.max(now, startAt, this.nextStart);
    const end = start + (samples.length * 1000) / L.sample_rate;
    const frame = { samples, start, end, played: 0, playedAt: -Infinity, run: this.run };
    this.output.push(frame);
    this.nextStart = end;
    this.trim(now);
    return (played) => {
      if (
        frame.run !== this.run || !this.output.includes(frame) ||
        !Number.isSafeInteger(played) || played < frame.played || played > samples.length
      ) return;
      frame.played = played;
      frame.playedAt = this.now();
    };
  }

  private trim(now: number): void {
    // Keep queued PCM through startup. The sample and frame limits still apply.
    this.output = this.output.filter((frame) =>
      !frame.played || now - Math.max(frame.end, frame.playedAt) <= ECHO_TAIL_MS);
    let count = this.output.reduce((sum, frame) => sum + frame.samples.length, 0);
    while (this.output.length && (count > ECHO_HISTORY || this.output.length > ECHO_FRAMES))
      count -= this.output.shift()!.samples.length;
  }

  stopOutput(): void {
    // Keep the confirmed tail. Discard output that has not played.
    const now = this.now();
    this.output = this.output.flatMap((frame) => {
      const count = frame.played;
      return count ? [{
        ...frame,
        samples: frame.samples.subarray(0, count),
        end: frame.start + (count * 1000) / L.sample_rate,
      }] : [];
    });
    this.nextStart = now;
    this.matchedReference = null;
    this.run++;
  }

  resetCapture(): void {
    this.microphone = [];
    this.capturedSamples = 0;
    this.matchedReference = null;
  }

  reset(): void {
    this.output = [];
    this.nextStart = 0;
    this.run++;
    this.resetCapture();
  }

  filter(samples: Int16Array): Int16Array {
    const now = this.now();
    this.trim(now);
    if (!this.output.length) {
      this.resetCapture();
      return samples;
    }
    this.capturedSamples += samples.length;
    for (const sample of samples) this.microphone.push(sample / 32768);
    if (this.microphone.length > ECHO_WINDOW)
      this.microphone.splice(0, this.microphone.length - ECHO_WINDOW);
    if (this.microphone.length < ECHO_WINDOW) return samples;
    const power = samples.reduce((sum, sample) => sum + (sample / 32768) ** 2, 0) / samples.length;
    if (power < this.threshold ** 2) return samples;
    const referencePcm: number[] = [];
    const spans: { start: number; end: number }[] = [];
    const ranges: { frame: EchoReference; start: number; end: number }[] = [];
    let previous: EchoReference | null = null;
    let previousEnd = -Infinity;
    for (const frame of this.output) {
      const count = frame.played;
      if (!count) continue;
      if (!previous || frame.run !== previous.run ||
        Math.abs(frame.start - previousEnd) > 1000 / L.sample_rate)
        spans.push({ start: referencePcm.length, end: referencePcm.length });
      ranges.push({ frame, start: referencePcm.length, end: referencePcm.length + count });
      for (let i = 0; i < count; i++) referencePcm.push(frame.samples[i] / 32768);
      spans.at(-1)!.end = referencePcm.length;
      previous = frame;
      previousEnd = frame.start + (count * 1000) / L.sample_rate;
    }
    if (!spans.some((span) => span.end - span.start >= ECHO_WINDOW)) return samples;
    // Search at 3 kHz, then refine the best delay using the original samples.
    const downsample = (values: number[]) => {
      const result: number[] = [];
      for (let start = 0; start + ECHO_DOWNSAMPLE <= values.length; start += ECHO_DOWNSAMPLE) {
        let sum = 0;
        for (let i = 0; i < ECHO_DOWNSAMPLE; i++) sum += values[start + i];
        result.push(sum / ECHO_DOWNSAMPLE);
      }
      return result;
    };
    const microphone = downsample(this.microphone);
    const reference = downsample(referencePcm);
    const mean = microphone.reduce((sum, sample) => sum + sample, 0) / microphone.length;
    const centered = microphone.map((sample) => sample - mean);
    const energy = centered.reduce((sum, sample) => sum + sample * sample, 0);
    if (energy < 1e-8) return samples;
    // A periodic match cannot distinguish playback from a person with the same pitch.
    let period = 0;
    let periodicity = 0;
    for (let lag = 6; lag <= 60; lag++) {
      let dot = 0;
      let left = 0;
      let right = 0;
      for (let i = lag; i < centered.length; i++) {
        dot += centered[i] * centered[i - lag];
        left += centered[i] ** 2;
        right += centered[i - lag] ** 2;
      }
      const similarity = left * right > 1e-12 ? (dot * dot) / (left * right) : 0;
      if (similarity > 0.995) return samples;
      if (similarity > periodicity) {
        periodicity = similarity;
        period = lag;
      }
    }
    if (periodicity > 0.9) {
      for (let step = -20; step <= 20; step++) {
        const lag = period + step / 20;
        let dot = 0;
        let left = 0;
        let right = 0;
        for (let i = Math.ceil(lag); i < centered.length; i++) {
          const index = i - lag;
          const lower = Math.floor(index);
          const fraction = index - lower;
          const sample = centered[lower] * (1 - fraction) + centered[lower + 1] * fraction;
          dot += centered[i] * sample;
          left += centered[i] ** 2;
          right += sample ** 2;
        }
        if (dot * dot > 0.995 * left * right) return samples;
      }
    }
    const previousMatch = this.matchedReference;
    const hintRange = previousMatch && ranges.find((range) => range.frame === previousMatch.frame);
    const hintSpan = hintRange && spans.find((span) => hintRange.start >= span.start && hintRange.end <= span.end);
    const predicted = hintRange && previousMatch ? hintRange.start + previousMatch.offset +
      this.capturedSamples - previousMatch.captured : -1;
    const hint = hintSpan && predicted >= hintSpan.start && predicted + ECHO_WINDOW <= hintSpan.end ?
      { span: hintSpan, position: predicted } : null;
    // Keep a matched path aligned with capture. Pass unknown changes.
    if (hintSpan && !hint && hintSpan === spans.at(-1)) return samples;
    const firstSpan = hintSpan && !hint ? spans.indexOf(hintSpan) + 1 : 0;
    let best = -1;
    let bestSpan: { start: number; end: number } | null = null;
    let spanIndex = hint ? spans.indexOf(hint.span) : firstSpan;
    let score = 0;
    const searchFirst = hint ? Math.ceil(Math.max(hint.span.start, hint.position - 16) / ECHO_DOWNSAMPLE) :
      Math.ceil(spans[firstSpan].start / ECHO_DOWNSAMPLE);
    const searchLast = hint ?
      Math.floor(Math.min(hint.span.end - ECHO_WINDOW, hint.position + 16) / ECHO_DOWNSAMPLE) :
      reference.length - centered.length;
    for (let start = searchFirst; start <= searchLast; start++) {
      const position = start * ECHO_DOWNSAMPLE;
      while (spanIndex < spans.length && position + ECHO_WINDOW > spans[spanIndex].end)
        spanIndex++;
      if (spanIndex >= spans.length) break;
      if (position < spans[spanIndex].start) continue;
      let dot = 0;
      let sum = 0;
      let squares = 0;
      for (let i = 0; i < centered.length; i++) {
        const sample = reference[start + i];
        dot += centered[i] * sample;
        sum += sample;
        squares += sample * sample;
      }
      const variance = squares - (sum * sum) / centered.length;
      const match = variance > 1e-8 ? (dot * dot) / (energy * variance) : 0;
      if (match > score) {
        score = match;
        best = start * ECHO_DOWNSAMPLE;
        bestSpan = spans[spanIndex];
      }
    }
    if (score < 0.75 || !bestSpan) return samples;

    const fullMean = this.microphone.reduce((sum, sample) => sum + sample, 0) / ECHO_WINDOW;
    const fullCentered = this.microphone.map((sample) => sample - fullMean);
    const fullEnergy = fullCentered.reduce((sum, sample) => sum + sample ** 2, 0);
    const span = bestSpan;
    const first = Math.max(span.start, best - 24);
    const last = Math.min(span.end - ECHO_WINDOW, best + 24);
    const quietPower = (this.threshold * 0.2) ** 2;
    let matchedPosition = best;
    const suppress = (): Int16Array => {
      const range = ranges.find((range) => matchedPosition >= range.start && matchedPosition < range.end);
      if (range) this.matchedReference = {
        frame: range.frame, offset: matchedPosition - range.start, captured: this.capturedSamples,
      };
      return new Int16Array(samples.length);
    };
    const linearMatch = (pcm: number[]): boolean => {
      let matched = -1;
      let gain = 0;
      let referenceMean = 0;
      let bestScore = 0;
      for (let start = first; start <= last; start++) {
        let dot = 0;
        let sum = 0;
        let squares = 0;
        for (let i = 0; i < ECHO_WINDOW; i++) {
          const sample = pcm[start + i];
          dot += (this.microphone[i] - fullMean) * sample;
          sum += sample;
          squares += sample * sample;
        }
        const variance = squares - (sum * sum) / ECHO_WINDOW;
        const match = variance > 1e-8 ? (dot * dot) / (fullEnergy * variance) : 0;
        if (match > bestScore) {
          bestScore = match;
          matched = start;
          gain = dot / variance;
          referenceMean = sum / ECHO_WINDOW;
        }
      }
      if (bestScore < 0.9 || matched < 0) return false;
      let residual = 0;
      for (let i = 0; i < samples.length; i++) {
        const echo = gain * (pcm[matched + ECHO_WINDOW - samples.length + i] - referenceMean);
        residual += (samples[i] / 32768 - fullMean - echo) ** 2;
      }
      if (residual / samples.length < quietPower) {
        matchedPosition = matched;
        return true;
      }
      if (bestScore < 0.99) return false;
      // Use only confirmed samples for fractional alignment.
      for (const fraction of [-0.5, -0.25, 0.25, 0.5]) {
        const shift = fraction < 0 ? -1 : 0;
        const start = matched + shift;
        const blend = fraction - shift;
        if (start < span.start || start + ECHO_WINDOW >= span.end) continue;
        let dot = 0;
        let sum = 0;
        let squares = 0;
        for (let i = 0; i < ECHO_WINDOW; i++) {
          const sample = (1 - blend) * pcm[start + i] + blend * pcm[start + i + 1];
          dot += fullCentered[i] * sample;
          sum += sample;
          squares += sample * sample;
        }
        const variance = squares - sum * sum / ECHO_WINDOW;
        if (variance < 1e-8 || dot * dot / (fullEnergy * variance) < 0.99) continue;
        const gain = dot / variance;
        const mean = sum / ECHO_WINDOW;
        let error = 0;
        for (let i = ECHO_WINDOW - samples.length; i < ECHO_WINDOW; i++) {
          const sample = (1 - blend) * pcm[start + i] + blend * pcm[start + i + 1];
          error += (fullCentered[i] - gain * (sample - mean)) ** 2;
        }
        if (error / samples.length < quietPower) {
          matchedPosition = start + blend;
          return true;
        }
      }
      return false;
    };
    if (linearMatch(referencePcm)) return suppress();

    // Model a common speech low cut without filtering the microphone input.
    const highPass = new Array<number>(referencePcm.length);
    const alpha = Math.exp((-2 * Math.PI * 80) / L.sample_rate);
    for (const range of spans) {
      let previous = 0;
      let filtered = 0;
      for (let i = range.start; i < range.end; i++) {
        filtered = alpha * (filtered + referencePcm[i] - previous);
        previous = referencePcm[i];
        highPass[i] = filtered;
      }
    }
    if (linearMatch(highPass)) return suppress();

    const clippedMatch = (): boolean => {
      // Fit clipping only when repeated samples establish a captured limit.
      const lower = Math.min(...this.microphone);
      const upper = Math.max(...this.microphone);
      const epsilon = 1 / 32768;
      const plateau = this.microphone.filter((sample) =>
        sample <= lower + epsilon || sample >= upper - epsilon).length;
      if (upper - lower <= 4 * epsilon || plateau < ECHO_WINDOW / 20) return false;
      for (let start = first; start <= last; start++) {
        let count = 0;
        let x = 0;
        let y = 0;
        let xx = 0;
        let xy = 0;
        for (let i = 0; i < ECHO_WINDOW; i++) {
          const input = this.microphone[i];
          if (input <= lower + epsilon || input >= upper - epsilon) continue;
          const reference = referencePcm[start + i];
          count++;
          x += reference;
          y += input;
          xx += reference * reference;
          xy += reference * input;
        }
        if (count < ECHO_WINDOW / 4) continue;
        const variance = xx - x * x / count;
        if (variance < 1e-8) continue;
        const gain = (xy - x * y / count) / variance;
        const offset = (y - gain * x) / count;
        let error = 0;
        let tail = 0;
        for (let i = 0; i < ECHO_WINDOW; i++) {
          const echo = Math.max(lower, Math.min(upper, gain * referencePcm[start + i] + offset));
          const residual = (this.microphone[i] - echo) ** 2;
          error += residual;
          if (i >= ECHO_WINDOW - samples.length) tail += residual;
        }
        if (error < 0.1 * fullEnergy && tail / samples.length < quietPower) {
          matchedPosition = start;
          return true;
        }
      }
      return false;
    };
    if (clippedMatch()) return suppress();

    // A weak reflected path needs stronger evidence than a single playback copy.
    const reflectedFit = (pcm: number[], input: number[], inputEnergy: number, start: number, lag: number) => {
      let x = 0;
      let z = 0;
      let xx = 0;
      let zz = 0;
      let xz = 0;
      let xy = 0;
      let zy = 0;
      for (let i = 0; i < input.length; i++) {
        const direct = pcm[start + i];
        const reflected = pcm[start + i - lag];
        x += direct;
        z += reflected;
        xx += direct * direct;
        zz += reflected * reflected;
        xz += direct * reflected;
        xy += direct * input[i];
        zy += reflected * input[i];
      }
      const a = xx - x * x / input.length;
      const b = xz - x * z / input.length;
      const c = zz - z * z / input.length;
      const determinant = a * c - b * b;
      if (a < 1e-8 || c < 1e-8 || determinant <= 0.05 * a * c) return null;
      const gain = (xy * c - zy * b) / determinant;
      const reflection = (zy * a - xy * b) / determinant;
      if (Math.abs(reflection) > 0.5 * Math.abs(gain)) return null;
      return { gain, reflection, mean: x / input.length, reflectedMean: z / input.length,
        score: (gain * xy + reflection * zy) / inputEnergy };
    };
    let reflectedStart = -1;
    let reflectedLag = 0;
    let reflectedScore = 0.9;
    const firstCoarse = Math.max(Math.ceil(span.start / ECHO_DOWNSAMPLE), Math.floor(first / ECHO_DOWNSAMPLE));
    const lastCoarse = Math.floor(last / ECHO_DOWNSAMPLE);
    const coarseLimit = Math.floor(0.04 * L.sample_rate / ECHO_DOWNSAMPLE);
    for (let start = firstCoarse; start <= lastCoarse; start++) {
      const available = start - Math.ceil(span.start / ECHO_DOWNSAMPLE);
      for (let lag = 1; lag <= Math.min(coarseLimit, available); lag++) {
        const fit = reflectedFit(reference, centered, energy, start, lag);
        if (fit && fit.score > reflectedScore) {
          reflectedScore = fit.score;
          reflectedStart = start * ECHO_DOWNSAMPLE;
          reflectedLag = lag * ECHO_DOWNSAMPLE;
        }
      }
    }
    if (reflectedStart >= 0) {
      for (
        let start = Math.max(span.start, reflectedStart - ECHO_DOWNSAMPLE);
        start <= Math.min(span.end - ECHO_WINDOW, reflectedStart + ECHO_DOWNSAMPLE);
        start++
      ) {
        for (
          let lag = Math.max(1, reflectedLag - ECHO_DOWNSAMPLE);
          lag <= Math.min(coarseLimit * ECHO_DOWNSAMPLE, reflectedLag + ECHO_DOWNSAMPLE, start - span.start);
          lag++
        ) {
          const fit = reflectedFit(referencePcm, fullCentered, fullEnergy, start, lag);
          if (!fit || fit.score < 0.99) continue;
          let residual = 0;
          for (let i = ECHO_WINDOW - samples.length; i < ECHO_WINDOW; i++) {
            const echo = fit.gain * (referencePcm[start + i] - fit.mean) +
              fit.reflection * (referencePcm[start + i - lag] - fit.reflectedMean);
            residual += (fullCentered[i] - echo) ** 2;
          }
          if (residual / samples.length < quietPower) {
            matchedPosition = start;
            return suppress();
          }
        }
      }
    }

    return samples;
  }
}
