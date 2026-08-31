import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, userEvent } from "storybook/test";

import { STORY_PROVIDER_ERRORS } from "../dev/story-data";
import { ErrorBlockCard } from "./ChatContent";

const failure = (kind: string) => {
  const block = STORY_PROVIDER_ERRORS.find((one) => one.kind === kind);
  if (!block) throw new Error(`Missing provider failure story: ${kind}`);
  return block;
};

const meta = {
  title: "Components/Provider failure",
  component: ErrorBlockCard,
  parameters: { layout: "fullscreen" },
  decorators: [
    (Story) => (
      <main className="mx-auto w-full max-w-3xl px-3.5 py-6 sm:px-6">
        <Story />
      </main>
    ),
  ],
  args: { block: failure("rate-limit") },
} satisfies Meta<typeof ErrorBlockCard>;

export default meta;
type Story = StoryObj<typeof meta>;

/** The provider answered with an HTTP rejection and a correlation id. */
export const HttpResponse: Story = {
  play: async ({ canvas }) => {
    await expect(canvas.getByText("HTTP", { selector: "dt" })).toBeVisible();
    await expect(canvas.getByText("429", { selector: "dd" })).toBeVisible();
    await expect(canvas.getByText("Request", { selector: "dt" })).toBeVisible();
  },
};

/** Routing failed before a request left Vis: there is deliberately no HTTP row. */
export const NotSent: Story = {
  args: { block: failure("unroutable") },
  play: async ({ canvas }) => {
    await expect(canvas.getByText("Route", { selector: "dt" })).toBeVisible();
    await expect(canvas.queryByText("HTTP", { selector: "dt" })).toBeNull();
  },
};

/** A connection failed before any response: also no HTTP row, for a different reason. */
export const NoResponse: Story = {
  args: { block: failure("transport") },
  play: async ({ canvas }) => {
    await expect(canvas.queryByText("HTTP", { selector: "dt" })).toBeNull();
  },
};

/** HTTP succeeded, then the response stream failed: 200 is evidence, not success. */
export const SuccessfulStreamFailed: Story = {
  args: { block: failure("stream-interrupted") },
  play: async ({ canvas }) => {
    await expect(canvas.getByText("HTTP", { selector: "dt" })).toBeVisible();
    await expect(canvas.getByText("200", { selector: "dd" })).toBeVisible();
  },
};

/** Machine evidence stays available without competing with the human next step. */
export const Diagnostics: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(canvas.getByRole("button", { name: "Diagnostics" }));
    await expect(canvas.getByText("provider_rate-limit")).toBeVisible();
    await expect(canvas.getByText(/anthropic\/claude-opus-4/)).toBeVisible();
  },
};

/** The complete engine taxonomy, compared on one deterministic sheet. */
export const AllKinds: Story = {
  render: () => (
    <div className="grid gap-5">
      {STORY_PROVIDER_ERRORS.map((block) => (
        <ErrorBlockCard key={block.id} block={block} />
      ))}
    </div>
  ),
  play: async ({ canvas }) => {
    await expect(canvas.getAllByRole("alert")).toHaveLength(STORY_PROVIDER_ERRORS.length);
  },
};
