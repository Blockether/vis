/* Voice-capture AudioWorklet processor (loaded by ui.js via
   audioWorklet.addModule). Capture runs on the AUDIO RENDERING THREAD:
   main-thread jank — SSE swaps, markdown highlighting, layout — can no
   longer drop microphone buffers the way the deprecated main-thread
   ScriptProcessorNode did (observed live: a 2-minute dictation arriving
   as ~30s of audio). Batches 4096 samples per message (~85ms at 48kHz,
   the same cadence the old ScriptProcessor delivered, so the waveform
   code is unchanged) and TRANSFERS the buffer — zero copies. */
class VisCaptureProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.buf = new Float32Array(4096);
    this.n = 0;
  }
  process(inputs) {
    var ch = inputs[0] && inputs[0][0];
    if (ch) {
      var i = 0;
      while (i < ch.length) {
        var take = Math.min(ch.length - i, this.buf.length - this.n);
        this.buf.set(ch.subarray(i, i + take), this.n);
        this.n += take;
        i += take;
        if (this.n === this.buf.length) {
          this.port.postMessage(this.buf, [this.buf.buffer]);
          this.buf = new Float32Array(4096);
          this.n = 0;
        }
      }
    }
    return true;
  }
}
registerProcessor("vis-capture", VisCaptureProcessor);
