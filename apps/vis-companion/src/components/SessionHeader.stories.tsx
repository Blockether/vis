import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, userEvent, within } from "storybook/test";

import { STORY_GOAL, STORY_SESSION } from "../dev/story-data";
import { SessionHeader } from "./SessionHeader";

const meta = {
  title: "Session/Header",
  component: SessionHeader,
  parameters: { layout: "fullscreen" },
  args: {
    model: {
      title: STORY_SESSION.title,
      sessionId: STORY_SESSION.id,
      connected: true,
      artifacts: { count: 4, isOpen: false },
    },
    commands: { back: () => {}, toggleArtifacts: () => {} },
  },
} satisfies Meta<typeof SessionHeader>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Connected: Story = {};

export const Reconnecting: Story = {
  args: {
    model: {
      title: STORY_SESSION.title,
      sessionId: STORY_SESSION.id,
      connected: false,
      artifacts: { count: 4, isOpen: false },
    },
  },
};

export const ActiveGoal: Story = {
  args: { model: { ...meta.args.model, goal: STORY_GOAL } },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const button = canvas.getByRole("button", { name: /^Goal: Active/ });
    const label = button.querySelector("span")!;
    expect(label.getBoundingClientRect().right).toBeLessThanOrEqual(button.getBoundingClientRect().right);
    await userEvent.click(button);
    const page = within(document.body);
    expect(page.getByRole("dialog", { name: "Session goal" })).toBeVisible();
    expect(page.getByText("Iterations: 12 / 30")).toBeVisible();
    expect(page.getByText(/^Time in goal:/)).toBeVisible();
    expect(page.queryByText(/tokens used/)).toBeNull();
    await userEvent.click(page.getByRole("button", { name: "Close session goal" }));
  },
};
export const CompletedGoal: Story = { args: { model: { ...meta.args.model, goal: { ...STORY_GOAL, status: "complete", reason: "SDK tests and header interaction checks passed." } } } };
export const BlockedGoal: Story = { args: { model: { ...meta.args.model, goal: { ...STORY_GOAL, status: "blocked", reason: "The requested test device is unavailable." } } } };
export const BudgetLimitedGoal: Story = { args: { model: { ...meta.args.model, goal: { ...STORY_GOAL, status: "budget_limited", iterations_used: 30 } } } };
export const LongGoal: Story = { args: { model: { ...meta.args.model, goal: { ...STORY_GOAL, objective: "Preserve the complete objective, including the SDK boundary, session isolation, cancellation, reconnects, accessibility, phone layouts and verification against current state." } } } };
