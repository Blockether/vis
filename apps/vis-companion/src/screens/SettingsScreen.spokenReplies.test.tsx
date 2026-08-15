// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { SpokenRepliesPanel } from "./SettingsScreen";
import { speechOutput } from "../lib/speech";

// Every voice a device claims, so the picker can be proven to render the list the
// device has rather than a list the app hoped for.
const deviceVoices = [
  { voiceURI: "com.apple.voice.compact.en-GB.Daniel", name: "Daniel", lang: "en-GB", default: true },
  { voiceURI: "com.apple.voice.compact.en-US.Samantha", name: "Samantha", lang: "en-US", default: false },
];

beforeEach(() => {
  localStorage.clear();
  // The router caches its settings for the app's life; each test starts from the
  // shipped answer rather than from whatever the previous one chose.
  speechOutput.apply({
    route: "device",
    deviceVoice: null,
    gatewayVoice: null,
    rate: 1,
  });
  Object.defineProperty(window, "speechSynthesis", {
    configurable: true,
    value: {
      getVoices: () => deviceVoices,
      speak: () => undefined,
      cancel: () => undefined,
    },
  });
});

afterEach(() => {
  document.body.innerHTML = "";
});

const cell = (title: string) =>
  screen.getAllByRole("button").find((button) => button.textContent?.startsWith(title));

describe("the spoken-replies band", () => {
  it("offers the three places a reply can be spoken, and starts on this device", async () => {
    render(<SpokenRepliesPanel />);

    await waitFor(() => expect(cell("This device")).toBeTruthy());
    expect(cell("Off")).toBeTruthy();
    expect(cell("The machine")).toBeTruthy();
    expect(cell("This device")?.getAttribute("aria-pressed")).toBe("true");
  });

  it("silences the router itself when the reader picks Off, not just the cell", async () => {
    render(<SpokenRepliesPanel />);
    await waitFor(() => expect(cell("Off")).toBeTruthy());

    fireEvent.click(cell("Off")!);

    await waitFor(async () =>
      expect((await speechOutput.settings()).route).toBe("off"),
    );
    // Speed and voice belong to the device that speaks; nothing speaks now.
    expect(cell("System default")).toBeFalsy();
  });

  it("lists the voices this device really has, and marks the one it prefers", async () => {
    render(<SpokenRepliesPanel />);

    await waitFor(() => expect(cell("Samantha")).toBeTruthy());
    expect(cell("Daniel")?.textContent).toContain("device default");
    expect(cell("System default")?.getAttribute("aria-pressed")).toBe("true");

    fireEvent.click(cell("Samantha")!);

    await waitFor(async () =>
      expect((await speechOutput.settings()).deviceVoice).toBe(
        "com.apple.voice.compact.en-US.Samantha",
      ),
    );
  });
});
