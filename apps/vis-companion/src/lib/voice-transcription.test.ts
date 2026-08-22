// A dictation used to be ONE opaque POST: the app said "Transcribing on your
// machine…" from the moment the microphone stopped until the words appeared,
// and the socket that held the request was also the only thing holding the
// result. These pin the replacement — bytes reported while they upload, then the
// gateway PUSHING the job's own phase and percentage until the transcript
// arrives in the closing frame. Nothing here may poll.
import { afterEach, beforeEach, expect, it, describe } from "vitest";
import type { GatewayConn, VoiceJob, VoiceProgress } from "./types";
import { voiceProgressLabel } from "./voice";

// Same reason as `gateway-providers.test.ts`: `gateway.ts` arms timers and reads
// storage at import time, so the browser shims exist BEFORE the dynamic import.
const memory = new Map<string, string>();
Object.defineProperty(globalThis, "localStorage", {
  configurable: true,
  value: {
    getItem: (key: string) => memory.get(key) ?? null,
    setItem: (key: string, value: string) => void memory.set(key, String(value)),
    removeItem: (key: string) => void memory.delete(key),
    clear: () => memory.clear(),
    key: () => null,
    length: 0,
  },
});
(globalThis as { window?: unknown }).window ??= globalThis;

const { GatewayClient, GatewayError, VOICE_JOB_EVENT } = await import(
  "./gateway",
);

const CONN = {
  id: "gw",
  name: "workstation",
  url: "http://gateway.example.com:7777",
  token: "secret",
} as unknown as GatewayConn;

/** What the fake upload answers, and what it recorded. */
const upload = {
  status: 202,
  body: {} as unknown,
  headers: {} as Record<string, string>,
  sentBytes: 0,
  url: "",
};

class FakeXhr {
  upload: { onprogress: ((event: ProgressEvent) => void) | null } = {
    onprogress: null,
  };
  status = 0;
  responseText = "";
  timeout = 0;
  onload: (() => void) | null = null;
  onerror: (() => void) | null = null;
  ontimeout: (() => void) | null = null;
  onabort: (() => void) | null = null;
  method = "";
  url = "";

  open(method: string, url: string): void {
    this.method = method;
    this.url = url;
    upload.url = url;
  }

  setRequestHeader(key: string, value: string): void {
    upload.headers[key.toLowerCase()] = value;
  }

  abort(): void {
    this.onabort?.();
  }

  send(body: Blob): void {
    upload.sentBytes = body.size;
    // The browser streams the body, THEN delivers the response.
    queueMicrotask(() => {
      this.upload.onprogress?.({
        lengthComputable: true,
        loaded: body.size / 2,
        total: body.size,
      } as ProgressEvent);
      this.status = upload.status;
      this.responseText = JSON.stringify(upload.body);
      this.onload?.();
    });
  }
}

interface Call {
  url: string;
  method: string;
}

let calls: Call[];
let realFetch: typeof globalThis.fetch;

/** One `voice.job` frame, exactly as the gateway writes it. */
function frame(job: Partial<VoiceJob>): string {
  return `event: voice.job\ndata: ${JSON.stringify(job)}\n\n`;
}

/** A body that hands the reader one chunk at a time, in order. */
function sseBody(chunks: string[]): ReadableStream<Uint8Array> {
  const encoder = new TextEncoder();
  let next = 0;
  return new ReadableStream<Uint8Array>({
    pull(controller) {
      if (next >= chunks.length) {
        controller.close();
        return;
      }
      controller.enqueue(encoder.encode(chunks[next++]));
    },
  });
}

/** The job's event stream for `…/events`, plain JSON for everything else. */
function gatewayStreams(...chunks: string[]): void {
  globalThis.fetch = (async (input: RequestInfo | URL, init?: RequestInit) => {
    const url = String(input);
    calls.push({ url, method: init?.method ?? "GET" });
    if (url.endsWith("/events")) {
      return {
        ok: true,
        status: 200,
        body: sseBody(chunks),
      } as unknown as Response;
    }
    return new Response(JSON.stringify({ is_forgotten: true }), {
      status: 200,
      headers: { "Content-Type": "application/json" },
    });
  }) as typeof globalThis.fetch;
}

beforeEach(() => {
  calls = [];
  realFetch = globalThis.fetch;
  upload.status = 202;
  upload.body = {};
  upload.headers = {};
  upload.sentBytes = 0;
  upload.url = "";
  (globalThis as { XMLHttpRequest?: unknown }).XMLHttpRequest = FakeXhr;
});

afterEach(() => {
  globalThis.fetch = realFetch;
  delete (globalThis as { XMLHttpRequest?: unknown }).XMLHttpRequest;
});

const wav = () => new Blob([new Uint8Array(4096)], { type: "audio/wav" });

describe("transcribeVoice", () => {
  it("reports the upload, then every pushed frame, and returns the transcript", async () => {
    upload.body = {
      id: "vj_1",
      engine: "parakeet-local",
      phase: "queued",
      progress: 0,
      is_done: false,
    };
    const done = frame({
      id: "vj_1",
      engine: "parakeet-local",
      phase: "done",
      progress: 100,
      is_done: true,
      text: "add this to the prompt",
    });
    gatewayStreams(
      ": ping\n\n",
      frame({
        id: "vj_1",
        engine: "parakeet-local",
        phase: "preparing",
        progress: 30,
        is_done: false,
      }),
      frame({
        id: "vj_1",
        engine: "parakeet-local",
        phase: "transcribing",
        progress: 50,
        is_done: false,
      }),
      // A frame split across two reads is still ONE frame.
      done.slice(0, 20),
      done.slice(20),
    );

    const seen: VoiceProgress[] = [];
    const client = new GatewayClient(CONN);
    const transcript = await client.transcribeVoice("s1", wav(), {
      onProgress: (progress) => seen.push(progress),
      engine: "parakeet-local",
    });

    expect(transcript.text).toBe("add this to the prompt");
    // The audio really was uploaded, with the auth and content type of a POST.
    expect(upload.sentBytes).toBe(4096);
    expect(upload.headers["content-type"]).toBe("audio/wav");
    expect(upload.headers["authorization"]).toBe("Bearer secret");
    expect(upload.url).toBe(
      "http://gateway.example.com:7777/v1/sessions/s1/voice?engine=parakeet-local",
    );
    // SENDING is distinct from TRANSCRIBING, and each carries its own number.
    expect(seen.map((p) => `${p.phase}:${p.progress}`)).toEqual([
      "uploading:0",
      "uploading:50",
      "uploading:100",
      "queued:0",
      "preparing:30",
      "transcribing:50",
      "done:100",
    ]);
    expect(seen.at(-1)?.engine).toBe("parakeet-local");
    // ONE GET, and it is the job's own stream: the client never asks again.
    expect(calls.filter((call) => call.method === "GET")).toHaveLength(1);
    expect(calls[0]?.url).toBe(
      "http://gateway.example.com:7777/v1/sessions/s1/voice/jobs/vj_1/events",
    );
    expect(calls.at(-1)?.method).toBe("DELETE");
  });

  it("ignores every frame that is not named `voice.job`", async () => {
    // This client used to take ANY JSON with an `id` off the socket, so a session
    // event, an intermediary's notice, or a future frame kind sharing the
    // connection would have been painted as transcription progress.
    expect(VOICE_JOB_EVENT).toBe("voice.job");
    upload.body = { id: "vj_4", phase: "queued", progress: 0, is_done: false };
    gatewayStreams(
      // A session-log frame, cursor and all: same framing, different resource.
      'id: 12\nevent: turn.delta\ndata: {"id":"vj_4","phase":"transcribing","progress":99}\n\n',
      // A frame that names nothing is just as unidentified.
      'data: {"id":"vj_4","phase":"transcribing","progress":98}\n\n',
      frame({ id: "vj_4", phase: "transcribing", progress: 10, is_done: false }),
      frame({
        id: "vj_4",
        phase: "done",
        progress: 100,
        is_done: true,
        text: "only the named frames counted",
      }),
    );

    const seen: VoiceProgress[] = [];
    const client = new GatewayClient(CONN);
    const transcript = await client.transcribeVoice("s1", wav(), {
      onProgress: (progress) => seen.push(progress),
    });

    expect(transcript.text).toBe("only the named frames counted");
    expect(seen.map((p) => `${p.phase}:${p.progress}`)).toEqual([
      "uploading:0",
      "uploading:50",
      "uploading:100",
      "queued:0",
      "transcribing:10",
      "done:100",
    ]);
  });

  it("fails with the engine's own reason, not a dead spinner", async () => {
    upload.body = { id: "vj_2", phase: "queued", progress: 0, is_done: false };
    gatewayStreams(
      frame({
        id: "vj_2",
        phase: "failed",
        progress: 40,
        is_done: true,
        error: "Voice recording too short - try again",
      }),
    );

    const client = new GatewayClient(CONN);
    await expect(client.transcribeVoice("s1", wav())).rejects.toThrowError(
      "Voice recording too short - try again",
    );
  });

  it("says so when the stream ends before the transcript, instead of hanging", async () => {
    upload.body = { id: "vj_3", phase: "queued", progress: 0, is_done: false };
    gatewayStreams(
      frame({ id: "vj_3", phase: "transcribing", progress: 10, is_done: false }),
    );

    const client = new GatewayClient(CONN);
    await expect(client.transcribeVoice("s1", wav())).rejects.toThrowError(
      /ended before the transcript/,
    );
  });

  it("refuses the recording when the engine is not ready yet", async () => {
    // 425 Too Early: the gateway never blocks a request thread on a download.
    upload.status = 425;
    upload.body = { status: "downloading", progress: 12 };
    gatewayStreams();

    const client = new GatewayClient(CONN);
    const failure = await client
      .transcribeVoice("s1", wav())
      .catch((cause: unknown) => cause);
    expect(failure).toBeInstanceOf(GatewayError);
    expect((failure as InstanceType<typeof GatewayError>).status).toBe(425);
    // Nothing was streamed: there is no job to watch.
    expect(calls).toEqual([]);
  });
});

describe("voiceProgressLabel", () => {
  it("names the phase and shows the number when there is one", () => {
    expect(voiceProgressLabel(null)).toBe("Sending recording…");
    expect(voiceProgressLabel({ phase: "uploading", progress: 42 })).toBe(
      "Sending recording · 42%",
    );
    expect(voiceProgressLabel({ phase: "queued", progress: 0 })).toBe(
      "Recording received · waiting for the engine",
    );
    expect(voiceProgressLabel({ phase: "preparing", progress: 0 })).toBe(
      "Preparing voice engine…",
    );
    expect(voiceProgressLabel({ phase: "preparing", progress: 66 })).toBe(
      "Preparing voice engine · 66%",
    );
    expect(voiceProgressLabel({ phase: "transcribing", progress: 0 })).toBe(
      "Transcribing · 0%",
    );
    expect(voiceProgressLabel({ phase: "transcribing", progress: 99.6 })).toBe(
      "Transcribing · 100%",
    );
    expect(voiceProgressLabel({ phase: "failed", progress: 0 })).toBe(
      "Transcription failed",
    );
  });
});
