// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import { screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";

import { renderSessionScreen } from "./session-screen-harness";

const toggle = (id: string, label: string, value: string, choices: string[]) => ({
  id,
  label,
  type: "enum",
  value,
  choices,
});

describe("composer response controls", () => {
  it("orders reasoning, verbosity, then fast mode and cycles verbosity", async () => {
    const user = userEvent.setup();
    const reasoning = toggle("reasoning_level", "Reasoning effort", "balanced", [
      "low",
      "balanced",
      "deep",
    ]);
    const verbosity = toggle("verbosity", "Verbosity", "low", ["low", "medium", "high"]);
    const fast = { id: "codex_fast_mode", label: "Fast mode", type: "boolean", enabled: false };
    const setSetting = vi.fn((id: string) =>
      Promise.resolve(id === "verbosity" ? { ...verbosity, value: "medium" } : fast),
    );

    renderSessionScreen({
      client: {
        cachedDefaultModel: () => ({ provider: "openai-codex", model: "gpt-5.6" }),
        defaultModel: () => Promise.resolve({ provider: "openai-codex", model: "gpt-5.6" }),
        cachedSetting: (id: string) =>
          id === "reasoning_level" ? reasoning : id === "verbosity" ? verbosity : fast,
        setting: (id: string) =>
          Promise.resolve(id === "reasoning_level" ? reasoning : id === "verbosity" ? verbosity : fast),
        setSetting,
      },
    });

    const fastButton = await screen.findByRole("button", { name: /fast mode — off/i });
    const verbosityButton = await screen.findByRole("button", {
      name: /verbosity — low, tap for the next level/i,
    });
    const reasoningButton = screen.getByRole("button", { name: /reasoning effort — balanced/i });
    expect(reasoningButton).toHaveTextContent("◇ balanced");
    expect(verbosityButton).toHaveTextContent("≡ low");
    expect(fastButton).toHaveTextContent("» standard");
    expect(reasoningButton.compareDocumentPosition(verbosityButton) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(verbosityButton.compareDocumentPosition(fastButton) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    await user.click(verbosityButton);

    expect(setSetting).toHaveBeenCalledWith("verbosity", "cycle");
    await waitFor(() =>
      expect(screen.getByRole("button", { name: /verbosity — medium/i })).toBeInTheDocument(),
    );
  });
});
