import { VoiceResampler } from './live-voice-dsp';
import { LIVE_VOICE_LIMITS as L } from './live-voice-protocol';

declare const sampleRate: number;
declare class AudioWorkletProcessor {
  readonly port: MessagePort;
}
declare function registerProcessor(name: string, processor: typeof AudioWorkletProcessor): void;

class LiveVoiceCapture extends AudioWorkletProcessor {
  private resampler = new VoiceResampler(sampleRate);
  private enabled = true;
  private outstanding = 0;
  constructor() {
    super();
    this.port.onmessage = (event: MessageEvent<{ type: string }>) => {
      if (event.data.type === 'stop') this.enabled = false;
      if (event.data.type === 'ack') this.outstanding = Math.max(0, this.outstanding - 1);
    };
  }
  process(inputs: Float32Array[][]): boolean {
    if (!this.enabled) return false;
    const mono = inputs[0]?.[0];
    if (mono)
      for (const samples of this.resampler.push(mono)) {
        if (this.outstanding >= L.input_buffer_ms / L.frame_ms) {
          this.port.postMessage({ error: 'Voice capture queue is full.' });
          this.enabled = false;
          return false;
        }
        this.outstanding++;
        this.port.postMessage(samples, [samples.buffer]);
      }
    // The connected output stays silent. Capture never feeds the speakers.
    return true;
  }
}
registerProcessor('vis-live-voice-capture', LiveVoiceCapture);
