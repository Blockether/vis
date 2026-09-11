import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, userEvent } from "storybook/test";

import {
  STORY_SESSION_ROW,
  STORY_SESSION_USAGE,
  STORY_SESSION_HEALTH,
  STORY_HEALTH_USAGE,
} from "../dev/story-data";
import { SessionStatsPanel } from "./SessionList";
import healthParity from "../../../../packages/vis-contract/resources/vis-contract/fixtures/session-health.json";
import type { SessionHealthData } from "../lib/types";

// The same golden /usage health responses are verified by the backend and both clients.
const preparedHealth = healthParity.cases[0].health as SessionHealthData;
const logicalHealth = healthParity.cases[1].health as SessionHealthData;

const meta = {
  title: "Session/Navigator stats",
  component: SessionStatsPanel,
  parameters: { layout: "fullscreen" },
  decorators: [
    (Story) => (
      <div className="h-dvh overflow-y-auto">
        <Story />
      </div>
    ),
  ],
  args: {
    session: STORY_SESSION_ROW,
    usage: STORY_SESSION_USAGE,
    phase: "ready",
  },
} satisfies Meta<typeof SessionStatsPanel>;

export default meta;
type Story = StoryObj<typeof meta>;

/** A mixed exact/estimated sample says so in both the number and its explanation. */
export const EstimatedReuse: Story = {
  play: async ({ canvas }) => {
    await expect(canvas.getByText("≈98%")).toBeVisible();
    await expect(
      canvas.getByText(/Estimated share of reusable prior input/),
    ).toBeVisible();
  },
};

export const SessionHealth: Story = {
  args: { usage: { ...STORY_HEALTH_USAGE, health: STORY_SESSION_HEALTH } },
};

export const HealthInteractions: Story = {
  ...SessionHealth,
  play: async ({ canvas }) => {
    await expect(
      canvas.getByRole("meter", { name: "Context budget" }),
    ).toHaveAttribute("value", "0.6901");
    await expect(canvas.getByText("69%")).toBeVisible();
    const parts = canvas.getByRole("button", { name: /Context breakdown/ });
    await userEvent.click(parts);
    await expect(canvas.getByText("~/vis/AGENTS.md")).toBeVisible();
    const roots = canvas.getByRole("button", { name: /Linked filesystems/ });
    await userEvent.click(roots);
    await expect(canvas.getByText("~/library")).toBeVisible();
    await expect(
      canvas.getByText("AGENTS.md · ≈1.2k tokens on disk"),
    ).toBeVisible();
    await expect(canvas.getByText("No AGENTS.md or CLAUDE.md")).toBeVisible();
    await expect(canvas.getByText(/Could not read guidance/)).toBeVisible();
    // Expanded details must leave the totals reachable inside the fixed viewport.
    const input = canvas.getByText("Total input");
    input.scrollIntoView({ block: "center" });
    await expect(input.getBoundingClientRect().bottom).toBeLessThanOrEqual(
      window.innerHeight,
    );
    await userEvent.click(parts);
    await userEvent.click(roots);
  },
};

/** #186: the local estimate can exceed budget while measured context does not. */
export const EstimateDrift: Story = {
  args: { usage: { ...STORY_HEALTH_USAGE, health: logicalHealth } },
  play: async ({ canvas }) => {
    await userEvent.click(
      canvas.getByRole("button", { name: /Context breakdown/ }),
    );
    await expect(canvas.getByText("+54,773 tokens (+33.8%)")).toBeVisible();
    await expect(canvas.getByText("81%")).toBeVisible();
    await expect(canvas.getByRole("meter")).toHaveAttribute(
      "value",
      "0.810885",
    );
    await expect(canvas.queryByText("Over budget")).not.toBeInTheDocument();
  },
};

/** Synthetic prepared-request /usage fixture; no client-side derivation. */
export const PreparedRequest: Story = {
  args: { usage: { ...STORY_HEALTH_USAGE, health: preparedHealth } },
  play: async ({ canvas }) => {
    await userEvent.click(
      canvas.getByRole("button", { name: /Context breakdown/ }),
    );
    await expect(
      canvas.getByText("Prepared request · not measured usage"),
    ).toBeVisible();
    await expect(canvas.getByText("165,953 tokens")).toBeVisible();
    await expect(canvas.getByText("+3,776 tokens (+2.3%)")).toBeVisible();
    await expect(canvas.getByText("81%")).toBeVisible();
    await expect(canvas.getByRole("meter")).toHaveAttribute(
      "value",
      "0.810885",
    );
  },
};

export const FoldReminder: Story = {
  args: { usage: { ...STORY_HEALTH_USAGE, health: preparedHealth } },
};
export const OverBudget: Story = {
  args: {
    usage: {
      ...STORY_HEALTH_USAGE,
      health: healthParity.cases[10].health as SessionHealthData,
    },
  },
};
export const InputLimit: Story = {
  args: {
    usage: {
      ...STORY_HEALTH_USAGE,
      health: healthParity.cases[11].health as SessionHealthData,
    },
  },
};
export const EarlierMeasurement: Story = {
  args: {
    usage: {
      ...STORY_HEALTH_USAGE,
      health: { ...STORY_SESSION_HEALTH, stale: true },
    },
  },
};
export const PartialMeasurement: Story = {
  args: {
    usage: {
      ...STORY_HEALTH_USAGE,
      health: {
        last_request_tokens: 138020,
        call: 23,
        budget_tokens: 200000,
        budget_state: "within-budget",
        budget_used_percent: 69,
        budget_used_ratio: 0.6901,
        budget_remaining_tokens: 61980,
      },
    },
  },
};
export const HistoricalMeasurement: Story = {
  args: {
    usage: {
      ...STORY_HEALTH_USAGE,
      health: {
        last_request_tokens: 138020,
        call: 23,
        budget_state: "budget-unreported",
      },
    },
  },
};
export const UnrecordedReads: Story = {
  args: {
    usage: {
      ...STORY_HEALTH_USAGE,
      health: {
        ...STORY_SESSION_HEALTH,
        roots: [{ path: "~/library" }, { path: "~/svar" }],
        root_count: 2,
        estimated_root_count: 0,
      },
    },
  },
};
export const NoMeasurement: Story = { args: { usage: STORY_HEALTH_USAGE } };
export const Loading: Story = { args: { usage: null, phase: "loading" } };
export const Unavailable: Story = { args: { usage: null, phase: "error" } };
export const NoCalls: Story = { args: { usage: null } };
