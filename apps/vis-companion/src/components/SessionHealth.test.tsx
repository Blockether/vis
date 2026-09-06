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
    expect(screen.getByText(/Last measured call · #23/)).toBeInTheDocument();
    expect(screen.getByText(/272k/)).toBeInTheDocument();
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
    expect(screen.getByText("~/spel")).toBeInTheDocument();
    expect(screen.getAllByText("Instructions not loaded")).toHaveLength(2);
    expect(
      screen.getByText(/Access does not load repository contents/),
    ).toBeInTheDocument();
    await userEvent.click(
      screen.getByRole("button", { name: /Context breakdown/ }),
    );
    expect(screen.queryByText("Main AGENTS.md")).not.toBeInTheDocument();
  });

  it.each([
    [150000, "Fold reminder"],
    [207000, "Over budget"],
    [272000, "Input limit reached"],
  ])(
    "shows the state at %i tokens without promising automatic folding",
    (lastRequestTokens, state) => {
      paint({ ...STORY_SESSION_HEALTH, lastRequestTokens });
      expect(screen.getByText(state)).toBeInTheDocument();
      expect(
        screen.getByText(/Reminder, not an automatic fold/),
      ).toBeInTheDocument();
    },
  );

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
      screen.getByText("Instruction read not recorded"),
    ).toBeInTheDocument();
    expect(
      screen.queryByText("Instructions not loaded"),
    ).not.toBeInTheDocument();
  });
});
