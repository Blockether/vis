// @vitest-environment jsdom
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { describe, expect, it } from "vitest";

import {
  STORY_SESSION_HEALTH,
  STORY_HEALTH_USAGE,
  STORY_SESSION_ROW,
} from "../dev/story-data";
import healthParity from "../../../../packages/vis-contract/resources/vis-contract/fixtures/session-health.json";
import type { SessionHealthData } from "../lib/types";
import { SessionStatsPanel } from "./SessionList";

function paint(health: SessionHealthData | undefined = STORY_SESSION_HEALTH) {
  return render(
    <SessionStatsPanel
      session={STORY_SESSION_ROW}
      usage={{ ...STORY_HEALTH_USAGE, health }}
      phase="ready"
    />,
  );
}

describe("session health in metrics", () => {
  // #186: the gateway verifies these exact responses from persisted request facts.
  it.each(healthParity.cases)(
    "renders backend metrics: $name",
    async ({ health, expected }) => {
      render(
        <SessionStatsPanel
          session={STORY_SESSION_ROW}
          usage={{
            ...healthParity.usage,
            health: (health ?? undefined) as SessionHealthData | undefined,
          }}
          phase="ready"
        />,
      );
      expect(screen.getByText(expected.pressure)).toBeInTheDocument();
      expect(screen.getByText("2.1M")).toBeInTheDocument();
      if (expected.percent !== null) {
        expect(screen.getByText(`${expected.percent}%`)).toBeInTheDocument();
        expect(screen.getByRole("meter")).toHaveAttribute(
          "value",
          String(health!.budget_used_ratio),
        );
        expect(screen.getByRole("meter")).toHaveAttribute("max", "1");
        expect(screen.getByRole("meter")).toHaveAttribute(
          "aria-valuetext",
          `${health!.last_request_tokens.toLocaleString("en-US")} of ${health!.budget_tokens!.toLocaleString("en-US")} tokens; ${expected.percent}% of working budget`,
        );
      } else {
        expect(screen.queryByRole("meter")).not.toBeInTheDocument();
      }
      if (expected.projection) {
        await userEvent.click(
          screen.getByRole("button", { name: /Context breakdown/ }),
        );
        expect(
          screen.getByText(`${expected.projection} · not measured usage`),
        ).toBeInTheDocument();
        for (const [label, value] of [
          ["Local estimate", expected.estimate],
          ["Provider-reported input", expected.reported],
          ["Estimate − reported", expected.difference],
        ] as const) {
          expect(screen.getByText(label).nextElementSibling).toHaveTextContent(
            value!,
          );
        }
        expect(
          screen.getByText(new RegExp(expected.scope!)),
        ).toBeInTheDocument();
        expect(screen.getByText(/including cached input/)).toBeInTheDocument();
      } else {
        expect(
          screen.queryByRole("button", { name: /Context breakdown/ }),
        ).not.toBeInTheDocument();
      }
      if (health?.stale)
        expect(screen.getByText(/Earlier measurement/)).toBeInTheDocument();
    },
  );

  it("renders supplied metrics rather than reconstructing them in the client", async () => {
    // #186: deliberately different rows/raw thresholds detect any client recalculation.
    paint({
      ...STORY_SESSION_HEALTH,
      last_request_tokens: 300000,
      breakdown: [{ label: "Partial row", tokens: 1 }],
      estimated_input_tokens: 120,
      estimate_difference_tokens: 20,
      estimate_difference_percent: 20,
      budget_state: "within-budget",
      budget_used_percent: 17,
      budget_used_ratio: 0.17,
      budget_remaining_tokens: 123,
      root_count: 7,
      estimated_root_count: 6,
    });
    expect(screen.getByText("Within budget")).toBeInTheDocument();
    expect(screen.getByText("17%")).toBeInTheDocument();
    expect(screen.getByRole("meter")).toHaveAttribute("value", "0.17");
    expect(screen.getByText("123 budget left")).toBeInTheDocument();
    expect(
      screen.getByText("7 available · 6 with guidance estimates"),
    ).toBeInTheDocument();
    await userEvent.click(
      screen.getByRole("button", { name: /Context breakdown/ }),
    );
    expect(screen.getByText("120 tokens")).toBeInTheDocument();
    expect(screen.getByText("+20 tokens (+20.0%)")).toBeInTheDocument();
  });

  // #186: sample counts do not override the already classified server metric.
  it.each([
    [false, 3, "91%"],
    [true, 0, "≈91%"],
  ] as const)(
    "renders the supplied cache estimate flag %s",
    (estimated, samples, expected) => {
      render(
        <SessionStatsPanel
          session={STORY_SESSION_ROW}
          phase="ready"
          usage={{
            ...STORY_HEALTH_USAGE,
            reusable_prefix_coverage_percent: 91,
            reusable_prefix_estimated: estimated,
            prompt_cache_estimated_sample_count: samples,
          }}
        />,
      );
      expect(screen.getByText(expected)).toBeInTheDocument();
    },
  );
  it("does not reconstruct missing derived metrics from raw fields", () => {
    paint({
      last_request_tokens: 100,
      budget_tokens: 200,
      call: 1,
      breakdown: [{ label: "Partial row", tokens: 123 }],
    } as SessionHealthData);
    expect(screen.queryByRole("meter")).not.toBeInTheDocument();
    expect(
      screen.queryByRole("button", { name: /Context breakdown/ }),
    ).not.toBeInTheDocument();
    expect(screen.getByText("Budget not reported")).toBeInTheDocument();
  });

  it("opens recorded prompt parts and distinguishes filesystem access from loaded guidance", async () => {
    paint();
    expect(screen.getByText("69%")).toBeInTheDocument();
    expect(screen.getByText("Last measured call · #23")).toBeInTheDocument();
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
    expect(screen.getByRole("meter")).toHaveAttribute("value", "0.6901");
    expect(
      screen.getByText(/Disk estimates do not add to context usage/),
    ).toBeInTheDocument();
    await userEvent.click(
      screen.getByRole("button", { name: /Context breakdown/ }),
    );
    expect(screen.queryByText("Main AGENTS.md")).not.toBeInTheDocument();
  });

  it("does not turn an unrecorded guidance read into a negative claim", async () => {
    paint({
      ...STORY_SESSION_HEALTH,
      roots: [{ path: "/linked" }],
      root_count: 1,
      estimated_root_count: 0,
    });
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
