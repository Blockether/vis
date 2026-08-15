// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { VoiceEnginesPanel } from "./SettingsScreen";
import { GatewayError } from "../lib/gateway";
import type { GatewayClient } from "../lib/gateway";
import type { VoiceModelState } from "../lib/types";

/**
 * A machine that answers only the two questions this band asks, and remembers HOW it
 * was asked — a read or a "start the download" — because the difference is the point.
 */
function machine(listen: VoiceModelState | Error, speak: VoiceModelState | Error) {
  const asked: string[] = [];
  const reply =
    (which: "listen" | "speak", answer: VoiceModelState | Error) =>
    (start = false) => {
      asked.push(`${which}:${start ? "start" : "read"}`);
      return answer instanceof Error
        ? Promise.reject(answer)
        : Promise.resolve(answer);
    };
  return {
    asked,
    client: {
      voiceModel: vi.fn(reply("listen", listen)),
      speechModel: vi.fn(reply("speak", speak)),
    } as unknown as GatewayClient,
  };
}

const absent = (message: string, reasons?: string[]) =>
  new GatewayError(501, message, reasons ? { error: message, reasons } : { error: message });

afterEach(() => {
  document.body.innerHTML = "";
  vi.restoreAllMocks();
});

// Regression, user report: with the model already installed voice still failed, and the
// only cure anyone found was restarting Vis — because no screen ever said which half was
// broken or why.
describe("the speech-engines band", () => {
  it("says how far a download has got and which engine is doing it", async () => {
    const { client } = machine(
      { status: "downloading", progress: 42.4, engine: "parakeet-local" },
      { status: "ready", engine: "pocket-tts-local" },
    );
    const view = render(<VoiceEnginesPanel client={client} />);

    await waitFor(() =>
      expect(screen.getByText(/downloading 42%/)).toBeTruthy(),
    );
    expect(screen.getByText(/downloading 42%/).textContent).toContain(
      "parakeet-local",
    );
    expect(screen.getByText(/^ready · pocket-tts-local$/)).toBeTruthy();
    expect(screen.getByText("Listening")).toBeTruthy();
    expect(screen.getByText("Speaking")).toBeTruthy();
    view.unmount();
  });

  it("reports a download that died and retries it on the one button offered", async () => {
    const { client, asked } = machine(
      {
        status: "failed",
        engine: "parakeet-local",
        error: "the archive did not match its checksum",
      },
      { status: "ready", engine: "pocket-tts-local" },
    );
    const view = render(<VoiceEnginesPanel client={client} />);

    await waitFor(() =>
      expect(
        screen.getByText("the archive did not match its checksum"),
      ).toBeTruthy(),
    );
    fireEvent.click(screen.getByText("Try again"));

    await waitFor(() => expect(asked).toContain("listen:start"));
    // The half that works is never restarted to fix the half that does not.
    expect(asked.filter((one) => one === "speak:start")).toHaveLength(0);
    view.unmount();
  });

  it("names what failed to load when a direction has no engine at all", async () => {
    const { client } = machine(
      absent("no voice transcription engine is registered", [
        "com.blockether.vis.ext.foundation-voice: UnsatisfiedLinkError: libsherpa-onnx-jni",
      ]),
      { status: "ready", engine: "pocket-tts-local" },
    );
    const view = render(<VoiceEnginesPanel client={client} />);

    await waitFor(() =>
      expect(
        screen.getByText(/UnsatisfiedLinkError: libsherpa-onnx-jni/),
      ).toBeTruthy(),
    );
    expect(screen.getByText("not installed")).toBeTruthy();
    view.unmount();
  });

  it("stays out of the way on a machine that carries no voice extension", async () => {
    const { client } = machine(
      absent("no voice transcription engine is registered"),
      absent("no speech synthesis engine is registered"),
    );
    const { container } = render(<VoiceEnginesPanel client={client} />);

    await waitFor(() => expect(container.innerHTML).toBe(""));
  });
});
