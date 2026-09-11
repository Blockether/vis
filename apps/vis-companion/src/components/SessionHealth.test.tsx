// @vitest-environment jsdom
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { describe, expect, it } from "vitest";

import {
  STORY_SESSION_HEALTH,
  STORY_HEALTH_USAGE,
  STORY_SESSION_ROW,
} from "../dev/story-data";
import { SessionStatsPanel } from "./SessionList";

function paint(health = STORY_SESSION_HEALTH) {
  return render(
    <SessionStatsPanel
      session={STORY_SESSION_ROW}
      usage={STORY_HEALTH_USAGE}
      phase="ready"
      health={health}
    />,
  );
}

describe("session health in metrics", () => {
  it("separates the last measured context, the operating budget and lifetime input", () => {
    paint();
    expect(
      screen.getByRole("meter", { name: "Context budget" }),
    ).toHaveAttribute("value", "138020");
    expect(screen.getByRole("meter")).toHaveAttribute("max", "200000");
    expect(screen.getByText("69%")).toBeInTheDocument();
    expect(screen.getByText("Within budget")).toBeInTheDocument();
    expect(screen.getByText("Total input")).toBeInTheDocument();
    expect(screen.getByText("2.1M")).toBeInTheDocument();
    expect(screen.getByText("Last measured call · #23")).toBeInTheDocument();
    expect(screen.queryByText(/not live/i)).not.toBeInTheDocument();
  });

  it("opens the estimated prompt parts and distinguishes access from loaded guidance", async () => {
    paint();
    await userEvent.click(
      screen.getByRole("button", { name: /Context breakdown/ }),
    );
    expect(screen.getByText("Main AGENTS.md")).toBeInTheDocument();
    expect(screen.getByText("≈3.1k")).toBeInTheDocument();
    expect(screen.getByText("~/vis/AGENTS.md")).toBeInTheDocument();
    await userEvent.click(
      screen.getByRole("button", { name: /Linked filesystems/ }),
    );
    expect(screen.getByText("~/library")).toBeInTheDocument();
    expect(
      screen.getByText("AGENTS.md · ≈1.2k tokens on disk"),
    ).toBeInTheDocument();
    expect(screen.getByText("No AGENTS.md or CLAUDE.md")).toBeInTheDocument();
    expect(screen.getByText(/Could not read guidance/)).toBeInTheDocument();
    expect(screen.getByRole("meter")).toHaveAttribute("value", "138020");
    expect(
      screen.getByText(/Disk estimates do not add to context usage/),
    ).toBeInTheDocument();
    await userEvent.click(
      screen.getByRole("button", { name: /Context breakdown/ }),
    );
    expect(screen.queryByText("Main AGENTS.md")).not.toBeInTheDocument();
  });

  // #186: a large local estimate must not look like measured context pressure.
  it("compares the logical estimate with the same request, not cumulative input", async () => {
    paint({
      ...STORY_SESSION_HEALTH,
      lastRequestTokens: 162177,
      breakdown: [
        { label: "Conversation and tool results", tokens: 216546 },
        { label: "Tool declarations", tokens: 404 },
      ],
    });
    await userEvent.click(
      screen.getByRole("button", { name: /Context breakdown/ }),
    );
    expect(screen.getByText("216,950 tokens")).toBeInTheDocument();
    expect(screen.getByText("162,177 tokens")).toBeInTheDocument();
    expect(screen.getByText("+54,773 tokens (+33.8%)")).toBeInTheDocument();
    expect(screen.getByText(/before provider adaptation/)).toBeInTheDocument();
    expect(screen.getByText(/including cached input/)).toBeInTheDocument();
    expect(screen.getByText("81%")).toBeInTheDocument();
    expect(screen.getByText("Fold reminder")).toBeInTheDocument();
    expect(screen.queryByText("Over budget")).not.toBeInTheDocument();
    expect(screen.getByRole("meter")).toHaveAttribute("value", "162177");
    expect(
      screen.queryByText(/four characters per token/),
    ).not.toBeInTheDocument();
  });

  it("uses the persisted prepared projection without changing measured utilization", async () => {
    render(
      <SessionStatsPanel
        session={STORY_SESSION_ROW}
        usage={{
          ...STORY_HEALTH_USAGE,
          health: {
            last_request_tokens: 100,
            budget_tokens: 200,
            call: 4,
            counted_projection: "prepared-request",
            breakdown: [
              { label: "Conversation and tool results", tokens: 120 },
            ],
          },
        }}
        phase="ready"
      />,
    );
    await userEvent.click(
      screen.getByRole("button", { name: /Context breakdown/ }),
    );
    expect(
      screen.getByText("Prepared request · not measured usage"),
    ).toBeInTheDocument();
    expect(
      screen.getByText(/full prepared request after provider adaptation/),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/before provider adaptation/),
    ).not.toBeInTheDocument();
    expect(screen.getByText("120 tokens")).toBeInTheDocument();
    expect(screen.getByText("+20 tokens (+20.0%)")).toBeInTheDocument();
    expect(screen.getByRole("meter")).toHaveAttribute("value", "100");
    expect(screen.getByText("50%")).toBeInTheDocument();
  });

  it.each([
    [100, 90, "−10 tokens (−10.0%)"],
    [100, 100, "0 tokens (0.0%)"],
    [0, 100, "+100 tokens"],
  ])(
    "compares %i measured tokens with an estimate of %i",
    async (input, estimate, difference) => {
      paint({
        ...STORY_SESSION_HEALTH,
        lastRequestTokens: input,
        breakdown: [{ label: "System instructions", tokens: estimate }],
      });
      await userEvent.click(
        screen.getByRole("button", { name: /Context breakdown/ }),
      );
      expect(screen.getByText(difference)).toBeInTheDocument();
    },
  );

  it("does not present an unavailable empty breakdown as a zero estimate", () => {
    paint({ ...STORY_SESSION_HEALTH, breakdown: [] });
    expect(
      screen.getByText("Prompt breakdown unavailable"),
    ).toBeInTheDocument();
    expect(
      screen.queryByRole("button", { name: /Context breakdown/ }),
    ).not.toBeInTheDocument();
  });

  it.each([
    [150000, "Fold reminder"],
    [207000, "Over budget"],
    [272000, "Input limit reached"],
  ])("shows the state at %i tokens", (lastRequestTokens, state) => {
    paint({ ...STORY_SESSION_HEALTH, lastRequestTokens });
    expect(screen.getByText(state)).toBeInTheDocument();
  });

  it("does not replace missing health telemetry with total input or zero", () => {
    render(
      <SessionStatsPanel
        session={STORY_SESSION_ROW}
        usage={STORY_HEALTH_USAGE}
        phase="ready"
      />,
    );
    expect(screen.queryByRole("meter")).not.toBeInTheDocument();
    expect(
      screen.getByText("Context measurement unavailable"),
    ).toBeInTheDocument();
    expect(screen.getByText("2.1M")).toBeInTheDocument();
  });

  it("keeps a stale measurement visibly stale and leaves absent detail counts unknown", () => {
    paint({
      ...STORY_SESSION_HEALTH,
      stale: true,
      breakdown: undefined,
      roots: undefined,
    });
    expect(screen.getByText(/Earlier measurement/)).toBeInTheDocument();
    expect(
      screen.getByText("Prompt breakdown unavailable"),
    ).toBeInTheDocument();
    expect(
      screen.getByText("Linked filesystem details unavailable"),
    ).toBeInTheDocument();
    expect(
      screen.queryByRole("button", { name: /Context breakdown/ }),
    ).not.toBeInTheDocument();
  });
  it("shows historical input without inventing a budget or a zero-percent meter", () => {
    paint({
      ...STORY_SESSION_HEALTH,
      budgetTokens: undefined,
      reminderTokens: undefined,
    });
    expect(screen.getByText("138k")).toBeInTheDocument();
    expect(screen.getByText("Budget not reported")).toBeInTheDocument();
    expect(screen.queryByRole("meter")).not.toBeInTheDocument();
  });

  it("does not turn an unrecorded instruction read into a negative claim", async () => {
    paint({ ...STORY_SESSION_HEALTH, roots: [{ path: "/linked" }] });
    await userEvent.click(
      screen.getByRole("button", { name: /Linked filesystems/ }),
    );
    expect(
      screen.getByText("Guidance estimate unavailable"),
    ).toBeInTheDocument();
    expect(
      screen.queryByText("Instructions not loaded"),
    ).not.toBeInTheDocument();
  });
});
