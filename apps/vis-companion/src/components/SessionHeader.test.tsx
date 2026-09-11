// @vitest-environment jsdom

import { act, fireEvent, render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import { STORY_GOAL } from "../dev/story-data";
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
  it("shows only explicitly supplied goals and opens the full objective", () => {
    const { rerender } = render(<SessionHeader model={model} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />);
    expect(screen.queryByRole("button", { name: /^Goal:/ })).not.toBeInTheDocument();
    const goal = { ...STORY_GOAL, objective: "Verify a very long objective ".repeat(50), status: "blocked" as const, reason: "Awaiting test credentials." };
    rerender(<SessionHeader model={{ ...model, goal }} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />);
    fireEvent.click(screen.getByRole("button", { name: /^Goal: Blocked/ }));
    expect(screen.getByRole("dialog", { name: "Session goal" })).toBeInTheDocument();
    expect(screen.getByText(goal.reason)).toBeInTheDocument();
    expect(screen.getByText("Iterations: 12 / 30")).toBeInTheDocument();
    expect(screen.getByText("Time in goal: 32s")).toBeInTheDocument();
    expect(screen.queryByText(/tokens used/)).not.toBeInTheDocument();
    expect(screen.queryByText(/100,000 budget/)).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole("button", { name: "Close session goal" }));
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });
  it("distinguishes unlimited iterations from a reached iteration limit", () => {
    const { rerender } = render(<SessionHeader model={{ ...model, goal: { ...STORY_GOAL, iteration_budget: null } }} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />);
    fireEvent.click(screen.getByRole("button", { name: /^Goal:/ }));
    expect(screen.getByText("Iterations: 12 / unlimited")).toBeInTheDocument();
    rerender(<SessionHeader model={{ ...model, goal: { ...STORY_GOAL, status: "budget_limited", iterations_used: 30 } }} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />);
    expect(screen.getByText("Iterations: 30 / 30")).toBeInTheDocument();
    expect(screen.getByText("Iteration limit reached")).toBeInTheDocument();
    fireEvent.click(screen.getByRole("button", { name: "Close session goal" }));
  });
  it("ticks active wall time, freezes inactive goals and cleans up the clock", () => {
    vi.useFakeTimers();
    vi.setSystemTime(STORY_GOAL.updated_at + 3_693_000);
    try {
      const { rerender, unmount } = render(<SessionHeader model={{ ...model, goal: STORY_GOAL }} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />);
      fireEvent.click(screen.getByRole("button", { name: /^Goal:/ }));
      expect(screen.getByText("Time in goal: 1h 2m 5s")).toBeInTheDocument();
      act(() => vi.advanceTimersByTime(2_000));
      expect(screen.getByText("Time in goal: 1h 2m 7s")).toBeInTheDocument();
      for (const status of ["paused", "blocked", "budget_limited", "complete", "cancelled"] as const) {
        rerender(<SessionHeader model={{ ...model, goal: { ...STORY_GOAL, status, time_used_ms: 3_727_000 } }} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />);
        act(() => vi.advanceTimersByTime(5_000));
        expect(screen.getByText("Time in goal: 1h 2m 7s")).toBeInTheDocument();
      }
      rerender(<SessionHeader model={{ ...model, goal: { ...STORY_GOAL, time_used_ms: 3_727_000, updated_at: Date.now() } }} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />);
      act(() => vi.advanceTimersByTime(1_000));
      expect(screen.getByText("Time in goal: 1h 2m 8s")).toBeInTheDocument();
      fireEvent.click(screen.getByRole("button", { name: "Close session goal" }));
      act(() => vi.advanceTimersByTime(10_000));
      fireEvent.click(screen.getByRole("button", { name: /^Goal:/ }));
      expect(screen.getByText("Time in goal: 1h 2m 18s")).toBeInTheDocument();
      rerender(<SessionHeader model={{ ...model, goal: { ...STORY_GOAL, id: "replacement", time_used_ms: 0, updated_at: Date.now() + 1_000 } }} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />);
      expect(screen.getByText("Time in goal: 0s")).toBeInTheDocument();
      unmount();
      expect(vi.getTimerCount()).toBe(0);
    } finally {
      vi.useRealTimers();
    }
  });
});
