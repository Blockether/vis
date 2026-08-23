// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from "vitest";
import { waitFor } from "@testing-library/react";

const pendingVoice = vi.hoisted(() => ({
  clear: vi.fn<() => Promise<void>>(),
  read: vi.fn<() => Promise<Blob | null>>(),
  save: vi.fn<() => Promise<void>>(),
}));

vi.mock("../lib/pending-voice", () => ({
  clearPendingVoice: pendingVoice.clear,
  readPendingVoice: pendingVoice.read,
  savePendingVoice: pendingVoice.save,
}));

import { renderSessionScreen } from "./session-screen-harness";

describe("pending voice", () => {
  beforeEach(() => {
    pendingVoice.clear.mockReset().mockResolvedValue(undefined);
    pendingVoice.read.mockReset();
    pendingVoice.save.mockReset().mockResolvedValue(undefined);
  });

  // Regression, user report: a terminal “recording too short” failure stayed in
  // the durable outbox, so opening or waking this one session retried it forever.
  it("discards audio after the transcription engine rejects it", async () => {
    const wav = new Blob([new Uint8Array(44)], { type: "audio/wav" });
    pendingVoice.read.mockResolvedValue(wav);
    const transcribeVoice = vi
      .fn()
      .mockRejectedValue(new Error("Voice recording too short - try again"));

    renderSessionScreen({ client: { transcribeVoice } });

    await waitFor(() => expect(transcribeVoice).toHaveBeenCalledTimes(1));
    await waitFor(() => expect(pendingVoice.clear).toHaveBeenCalledTimes(1));
    expect(pendingVoice.save).not.toHaveBeenCalled();
  });
});
