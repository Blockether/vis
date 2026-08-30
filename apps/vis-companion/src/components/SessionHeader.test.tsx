// @vitest-environment jsdom

import { fireEvent, render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import { SessionHeader } from "./SessionHeader";

const model = {
  title: "Investigate stalled stream",
  sessionId: "123e4567-e89b-12d3-a456-426614174000",
  connected: true,
  artifacts: { count: 3, isOpen: false },
} as const;

describe("SessionHeader", () => {
  it("exposes one session identity and its two navigation commands", () => {
    const back = vi.fn();
    const toggleArtifacts = vi.fn();
    render(
      <SessionHeader model={model} commands={{ back, toggleArtifacts }} />,
    );

    expect(
      screen.getByRole("heading", { name: model.title }),
    ).toBeInTheDocument();
    expect(screen.getByText("Connected")).toBeInTheDocument();
    expect(
      screen.getByRole("button", { name: "Copy session id" }),
    ).toHaveTextContent("123e4567");

    fireEvent.click(screen.getByRole("button", { name: "Back to sessions" }));
    fireEvent.click(
      screen.getByRole("button", { name: "3 artifacts produced by the model" }),
    );
    expect(back).toHaveBeenCalledOnce();
    expect(toggleArtifacts).toHaveBeenCalledOnce();
  });

  it("renders the reconnecting state and omits an empty artifact door", () => {
    render(
      <SessionHeader
        model={{
          ...model,
          connected: false,
          artifacts: { count: 0, isOpen: false },
        }}
        commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
      />,
    );

    expect(screen.getByText("Reconnecting")).toBeInTheDocument();
    expect(screen.queryByText(/artifacts produced/)).not.toBeInTheDocument();
  });
});
