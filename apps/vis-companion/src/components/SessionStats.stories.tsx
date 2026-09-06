import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, userEvent } from "storybook/test";

import {
  STORY_SESSION_ROW,
  STORY_SESSION_USAGE,
  STORY_SESSION_HEALTH,
  STORY_HEALTH_USAGE,
} from "../dev/story-data";
import { SessionStatsPanel } from "./SessionList";

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
  args: { usage: STORY_HEALTH_USAGE, health: STORY_SESSION_HEALTH },
};

export const HealthInteractions: Story = {
  ...SessionHealth,
  play: async ({ canvas }) => {
    await expect(
      canvas.getByRole("meter", { name: "Context budget" }),
    ).toHaveAttribute("value", "138020");
    await expect(canvas.getByText("69%")).toBeVisible();
    const parts = canvas.getByRole("button", { name: /Context breakdown/ });
    await userEvent.click(parts);
    await expect(canvas.getByText("~/vis/AGENTS.md")).toBeVisible();
    const roots = canvas.getByRole("button", { name: /Linked filesystems/ });
    await userEvent.click(roots);
    await expect(canvas.getByText("~/spel")).toBeVisible();
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

export const FoldReminder: Story = {
  args: {
    usage: STORY_HEALTH_USAGE,
    health: {
      ...STORY_SESSION_HEALTH,
      lastRequestTokens: 166_000,
      breakdown: undefined,
    },
  },
};
export const OverBudget: Story = {
  args: {
    usage: STORY_HEALTH_USAGE,
    health: {
      ...STORY_SESSION_HEALTH,
      lastRequestTokens: 207_000,
      breakdown: undefined,
    },
  },
};
export const InputLimit: Story = {
  args: {
    usage: STORY_HEALTH_USAGE,
    health: {
      ...STORY_SESSION_HEALTH,
      lastRequestTokens: 272_000,
      breakdown: undefined,
    },
  },
};
export const EarlierMeasurement: Story = {
  args: {
    usage: STORY_HEALTH_USAGE,
    health: { ...STORY_SESSION_HEALTH, stale: true },
  },
};
export const PartialMeasurement: Story = {
  args: {
    usage: STORY_HEALTH_USAGE,
    health: {
      ...STORY_SESSION_HEALTH,
      breakdown: undefined,
      roots: undefined,
      modelInputLimit: undefined,
    },
  },
};
export const HistoricalMeasurement: Story = {
  args: {
    usage: STORY_HEALTH_USAGE,
    health: {
      ...STORY_SESSION_HEALTH,
      budgetTokens: undefined,
      reminderTokens: undefined,
      modelInputLimit: undefined,
      breakdown: undefined,
      roots: undefined,
    },
  },
};
export const UnrecordedReads: Story = {
  args: {
    usage: STORY_HEALTH_USAGE,
    health: {
      ...STORY_SESSION_HEALTH,
      roots: [{ path: "~/spel" }, { path: "~/svar" }],
    },
  },
};
export const NoMeasurement: Story = { args: { usage: STORY_HEALTH_USAGE } };
export const Loading: Story = { args: { usage: null, phase: "loading" } };
export const Unavailable: Story = { args: { usage: null, phase: "error" } };
export const NoCalls: Story = { args: { usage: null } };
