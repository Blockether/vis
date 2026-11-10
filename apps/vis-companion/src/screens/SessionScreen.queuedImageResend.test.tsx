// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor } from "@testing-library/react";
import { afterEach, expect, it, vi } from "vitest";
import { GatewayClient } from "../lib/gateway";
import { clearDraftMessage, draftMessageKey } from "../lib/draft-messages";
import { renderSessionScreen, subscriptionHub } from "./session-screen-harness";

// The first bytes of a real PNG: signature, then the IHDR chunk header.
const PNG = Uint8Array.from([
  0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d, 0x49, 0x48,
  0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x08, 0x06, 0x00, 0x00,
  0x00, 0x1f, 0x15, 0xc4, 0x89, 0xff, 0xfe, 0x80,
]);

const runningTurn = () => ({
  turn: { id: "turn-1", request: "Check the logs", answer: "", iterations: [], startedAt: Date.now(), status: "running" },
  seq: 1,
});

afterEach(() => {
  vi.unstubAllGlobals();
  clearDraftMessage(draftMessageKey("http://gateway.example.com", "s1"));
});

async function bodyBytes(body: unknown): Promise<number[]> {
  const buffer = await new Promise<ArrayBuffer>((resolve, reject) => {
    const reader = new FileReader();
    reader.onerror = () => reject(reader.error);
    reader.onload = () => resolve(reader.result as ArrayBuffer);
    reader.readAsArrayBuffer(body as Blob);
  });
  return Array.from(new Uint8Array(buffer));
}

/** A gateway that keeps every uploaded body and queues every turn. */
function recordingGateway() {
  const uploads: unknown[] = [];
  const posts: { request: string; attachments: unknown[] }[] = [];
  let turns = 0;
  vi.stubGlobal(
    "fetch",
    vi.fn(async (url: string, init: RequestInit) => {
      if (url.includes("/attachments?")) {
        uploads.push(init.body);
        return new Response(JSON.stringify({ upload_id: `u${uploads.length}`, size: 1 }), { status: 201 });
      }
      if (!(init?.method === "POST" && url.endsWith("/turns"))) return new Response("{}", { status: 200 });
      turns += 1;
      posts.push(JSON.parse(String(init.body)));
      return new Response(JSON.stringify({ turn_id: `queued-${turns}`, status: "queued" }), { status: 200 });
    }),
  );
  const real = new GatewayClient({ url: "http://gateway.example.com" });
  return {
    uploads,
    posts,
    submitTurn: real.submitTurn.bind(real),
    // The client outlives the screen, so the bytes it sent stay with it.
    rememberSentAttachments: real.rememberSentAttachments.bind(real),
    cachedSentAttachments: real.cachedSentAttachments.bind(real),
    fetchTurnAttachments: real.fetchTurnAttachments.bind(real),
  };
}

// Reported: a queued message with an image, pulled back into the composer by Esc and sent
// again, arrived with an image the model could not read. After the screen was opened again,
// the row came back as the literal text `[IMAGE #1]` with no picture behind it.
it("re-sends the original image bytes after Esc when the screen was opened again", async () => {
  const events = subscriptionHub();
  const gateway = recordingGateway();
  const cancelTurn = vi.fn(() => Promise.resolve(null));
  const client = { cachedRunningTurn: runningTurn, cancelTurn, ...gateway };
  const first = renderSessionScreen({ client, subscriptions: events });
  const editor = () => screen.getByLabelText("Message Vis") as HTMLTextAreaElement;
  fireEvent.paste(editor(), {
    clipboardData: { files: [new File([PNG], "shot.png", { type: "image/png" })], items: [], getData: () => "" },
  });
  await waitFor(() => expect(editor().value).toBe("[IMAGE #1]"));
  fireEvent.click(screen.getByRole("button", { name: "Queue message" }));
  await waitFor(() => expect(gateway.uploads).toHaveLength(1));
  await act(async () => {});
  first.unmount();

  const row = {
    turnId: "queued-1",
    request: "[IMAGE #1]",
    preview: "[IMAGE #1]",
    attachments: [{ filename: "shot.png", mediaType: "image/png", sizeLabel: "36 B" }],
  };
  renderSessionScreen({
    client: {
      ...client,
      cachedQueuedTurns: () => [row],
      queuedTurns: () => Promise.resolve({ turns: [row], paused: null }),
    },
    subscriptions: events,
  });
  expect(await screen.findByRole("region", { name: "Queued messages" })).toBeInTheDocument();
  fireEvent.keyDown(document.body, { key: "Escape" });
  await waitFor(() => expect(cancelTurn).toHaveBeenCalled());
  await waitFor(() => expect(editor().value).toBe("[IMAGE #1]"));
  expect(await screen.findByRole("button", { name: "Remove shot.png" })).toBeInTheDocument();

  fireEvent.click(screen.getByRole("button", { name: /^(Send|Queue) message$/ }));
  await waitFor(() => expect(gateway.posts).toHaveLength(2));
  expect(gateway.posts[1]).toMatchObject({ request: "[IMAGE #1]", attachments: [{ filename: "shot.png" }] });
  expect(await bodyBytes(gateway.uploads[1])).toEqual(Array.from(PNG));
});

// The same report from the other side: another device queued the image and the stop came
// from any channel. This client never held the bytes; only the gateway frame carries them.
it("re-sends the image bytes that a cancelled frame carries for another device's row", async () => {
  const events = subscriptionHub();
  const gateway = recordingGateway();
  const row = {
    turnId: "queued-other",
    request: "[IMAGE #1] what is this",
    preview: "[IMAGE #1] what is this",
    attachments: [{ filename: "shot.png", mediaType: "image/png", sizeLabel: "36 B" }],
  };
  renderSessionScreen({
    client: {
      cachedRunningTurn: runningTurn,
      ...gateway,
      cachedQueuedTurns: () => [row],
      queuedTurns: () => Promise.resolve({ turns: [row], paused: null }),
    },
    subscriptions: events,
  });
  const editor = () => screen.getByLabelText("Message Vis") as HTMLTextAreaElement;
  expect(await screen.findByRole("region", { name: "Queued messages" })).toBeInTheDocument();
  await act(async () => {
    events.emit({
      type: "turn.queued.deleted",
      seq: 100,
      turn_id: "queued-other",
      reason: "cancelled",
      request: "[IMAGE #1] what is this",
      attachments: [
        {
          filename: "shot.png",
          media_type: "image/png",
          base64: btoa(String.fromCharCode(...PNG)),
          reference: "[IMAGE #1]",
        },
      ],
    });
  });
  await waitFor(() => expect(editor().value).toBe("[IMAGE #1] what is this"));
  expect(await screen.findByRole("button", { name: "Remove shot.png" })).toBeInTheDocument();

  fireEvent.click(screen.getByRole("button", { name: /^(Send|Queue) message$/ }));
  await waitFor(() => expect(gateway.posts).toHaveLength(1));
  expect(gateway.posts[0]).toMatchObject({ attachments: [{ filename: "shot.png" }] });
  expect(await bodyBytes(gateway.uploads[0])).toEqual(Array.from(PNG));
});
