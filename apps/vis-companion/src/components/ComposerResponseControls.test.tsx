// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { ComposerResponseControls } from "./ComposerResponseControls";

afterEach(cleanup);

describe("composer response controls", () => {
  it("owns the complete response-option vocabulary", () => {
    const choose = vi.fn();
    const cycleReasoning = vi.fn();
    const cycleVerbosity = vi.fn();
    const toggleFast = vi.fn();
    render(
      <ComposerResponseControls
        controls={{
          model: {
            value: "claude-opus-5",
            title: "anthropic/claude-opus-5",
            choose,
          },
          reasoning: {
            label: "Reasoning",
            value: "high",
            busy: false,
            cycle: cycleReasoning,
          },
          verbosity: {
            label: "Verbosity",
            value: "medium",
            busy: false,
            cycle: cycleVerbosity,
          },
          fast: { enabled: true, busy: false, toggle: toggleFast },
        }}
      />,
    );

    fireEvent.click(
      screen.getByRole("button", { name: "Change provider and model" }),
    );
    fireEvent.click(
      screen.getByRole("button", {
        name: "Reasoning — high, tap for the next level",
      }),
    );
    fireEvent.click(
      screen.getByRole("button", {
        name: "Verbosity — medium, tap for the next level",
      }),
    );
    fireEvent.click(screen.getByRole("button", { name: "Fast mode — on" }));

    expect(choose).toHaveBeenCalledOnce();
    expect(cycleReasoning).toHaveBeenCalledOnce();
    expect(cycleVerbosity).toHaveBeenCalledOnce();
    expect(toggleFast).toHaveBeenCalledOnce();
  });

  it("omits response knobs the provider does not expose", () => {
    render(
      <ComposerResponseControls
        controls={{
          model: {
            value: "model",
            title: "Change provider and model",
            choose: vi.fn(),
          },
        }}
      />,
    );

    expect(screen.getAllByRole("button")).toHaveLength(1);
    expect(screen.queryByRole("button", { name: /Reasoning/ })).toBeNull();
  });
});
