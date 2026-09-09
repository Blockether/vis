import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, userEvent, within } from "storybook/test";
import {
  ACTIVITY_ALL_GROUPS,
  ACTIVITY_CHRONOLOGY,
  ACTIVITY_INTERLEAVED,
  ACTIVITY_REPEATED_ARGUMENTS,
  ACTIVITY_LONG_RUNNING,
  ACTIVITY_LISTING,
  ACTIVITY_LISTING_BATCH,
  ACTIVITY_RICH,
  ACTIVITY_FAILED,
  ACTIVITY_RUNNING,
  ACTIVITY_SETTLED,
  ACTIVITY_TREE_CHANGES,
} from "../dev/story-data";
import { ActivityPanel } from "./ActivityPanel";

/**
 * WHAT THE MODEL IS DOING, WHILE IT IS DOING IT.
 *
 * The axis has one job the transcript cannot do: report a bounded run — how many
 * calls, which one is moving, what it produced — in one reading, with nothing
 * inside it behind a second chevron. It is what the invocation's own band opens
 * onto, and the reason a reader opens that band at all. The states below are the
 * sentences it can say, and they are drawn here rather than described, because
 * `running` has to read as one moving thread, `succeeded` has to go quiet
 * without disappearing, and `failed` has to be findable in a settled transcript
 * scrolled past — with the head of its output already on the page.
 *
 * The payloads are the ENGINE's own (`activityProjectionFromWire`), so a wire
 * change breaks this sheet before it reaches a screen.
 */
const meta = {
  title: "Components/Activity panel",
  component: ActivityPanel,
  parameters: { layout: "padded" },
} satisfies Meta<typeof ActivityPanel>;

export default meta;

type Story = StoryObj<typeof meta>;

export const RepeatedArguments: Story = {
  args: { activity: ACTIVITY_REPEATED_ARGUMENTS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Expand Activity" }),
    );
    await userEvent.click(canvas.getByRole("button", { name: /Search ×6/ }));
    const repeated = canvas.getByRole("button", { name: /same query ×3/ });
    await expect(repeated).toHaveAttribute("aria-expanded", "false");
    await expect(
      canvas.getByText(/Search directory unavailable/),
    ).toBeVisible();
    repeated.focus();
    await userEvent.keyboard("{Enter}");
    const first = within(
      canvasElement.querySelector<HTMLElement>(
        '[data-activity-row="search-1"]',
      )!,
    );
    await userEvent.click(first.getByRole("button"));
    await expect(canvas.getByText("First search: 2 matches")).toBeVisible();
    await userEvent.click(repeated);
    await expect(
      canvas.queryByText("First search: 2 matches"),
    ).not.toBeInTheDocument();
    await expect(
      canvasElement.querySelector('[data-activity-row="search-2"]'),
    ).toBeVisible();
  },
};

/** A turn in flight: one call answered, one still running. */
export const Running: Story = {
  args: { activity: ACTIVITY_RUNNING },
};

/** Pointer and keyboard can open retained evidence without waiting for settlement. */
export const LiveDisclosure: Story = {
  args: { activity: ACTIVITY_LONG_RUNNING },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Expand Activity" }),
    );
    await userEvent.click(canvas.getByRole("button", { name: /Search ×7/ }));
    const step = canvas.getByRole("button", { name: /Searched search-4/ });
    step.focus();
    await userEvent.keyboard("{Enter}");
    await expect(step).toHaveAttribute("aria-expanded", "true");
    await userEvent.click(canvas.getByRole("button", { name: /Search ×7/ }));
    await expect(canvas.queryByText("result-4")).not.toBeInTheDocument();
  },
};

/** Settled and read: three calls, a diff among them, nothing moving. */
export const Settled: Story = {
  args: { activity: ACTIVITY_SETTLED },
};

/** Failure counts remain visible; details wait for explicit expansion. */
export const Failed: Story = {
  args: { activity: ACTIVITY_FAILED },
};

/** No projection at all — what a turn has before its first tool call. */
export const Idle: Story = {
  args: {},
};

/** The thread doing its job: reads, a patch, a failed check, one step still moving. */
export const Chronology: Story = {
  args: { activity: ACTIVITY_CHRONOLOGY },
};

/** Repeated operations share one group even when ten read/search runs are interleaved. */
export const InterleavedOperations: Story = {
  args: { activity: ACTIVITY_INTERLEAVED },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Expand Activity" }),
    );
    const groups = canvas.getByRole("list", { name: "Operation groups" });
    await expect(groups.children).toHaveLength(4);
    await expect(
      canvas.getByRole("button", { name: /Search ×10/ }),
    ).toBeVisible();
    await expect(
      canvas.getByRole("button", { name: /Test ×2/ }),
    ).toHaveTextContent("1 running · 1 failed");
    await expect(canvas.getByText(/Assertion failed/)).toBeVisible();
    const reads = canvas.getByRole("button", { name: /Read ×10/ });
    reads.focus();
    await userEvent.keyboard("{Enter}");
    const members = canvas.getByRole("list", { name: "Read ×10 operations" });
    await expect(members.children).toHaveLength(10);
    await expect(
      [...members.children].map((row) => row.getAttribute("data-activity-row")),
    ).toEqual(Array.from({ length: 10 }, (_, index) => `cat-${index + 1}`));
    await userEvent.click(reads);
    await expect(reads).toHaveAttribute("aria-expanded", "false");
  },
};

/** All groups are available immediately; individual step details still fold independently. */
export const AllOperationGroups: Story = {
  args: { activity: ACTIVITY_ALL_GROUPS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Expand Activity" }),
    );
    const groups = canvas.getByRole("list", { name: "Operation groups" });
    await expect(groups.children).toHaveLength(7);
    await expect(
      canvas.queryByRole("button", {
        name: /(?:show|hide).*(?:more|fewer).*groups?/i,
      }),
    ).not.toBeInTheDocument();
    const lastStep = within(groups.children[6] as HTMLElement).getByRole(
      "button",
      {
        expanded: false,
      },
    );
    await expect(lastStep).toBeVisible();
    lastStep.focus();
    await userEvent.keyboard("{Enter}");
    await expect(lastStep).toHaveAttribute("aria-expanded", "true");
    await expect(canvas.getByText("Build completed")).toBeVisible();
    await userEvent.click(lastStep);
    await expect(lastStep).toHaveAttribute("aria-expanded", "false");
  },
};

/** What a block did to the tree with no tool call of its own: one row per kind. */
export const TreeChanges: Story = {
  args: { activity: ACTIVITY_TREE_CHANGES },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Expand Activity" }),
    );
    const step = canvas.getByRole("button", {
      name: /Changed 13 files and 2 directories/,
    });
    await userEvent.click(step);
    const children = canvasElement.querySelector("[data-activity-children]")!;
    await expect(children).toBeVisible();
    await expect(getComputedStyle(children).marginTop).toBe("0px");
    await userEvent.click(step);
    await expect(
      canvasElement.querySelector("[data-activity-children]"),
    ).toBeNull();
  },
};

export const SymbolContent: Story = { args: { activity: ACTIVITY_RICH } };

export const Listing: Story = {
  args: { activity: ACTIVITY_LISTING },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Expand Activity" }),
    );
    const step = canvas.getByRole("button", { name: /Listed apps/ });
    await expect(canvas.getByText("3 directories · 2 files")).toBeVisible();
    await expect(canvas.queryByRole("table")).not.toBeInTheDocument();
    step.focus();
    await userEvent.keyboard("{Enter}");
    await expect(canvas.getByRole("table")).toBeVisible();
    await userEvent.click(step);
    await expect(canvas.getByText("3 directories · 2 files")).toBeVisible();
    await expect(canvas.queryByRole("table")).not.toBeInTheDocument();
  },
};

export const ListingBatch: Story = {
  args: { activity: ACTIVITY_LISTING_BATCH },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Expand Activity" }),
    );
    await expect(canvas.getByText("0 directories · 2 files")).toBeVisible();
    const step = canvas.getByRole("button", { name: /Listed 2 directories/ });
    await userEvent.click(step);
    await expect(canvas.getAllByRole("table")).toHaveLength(2);
    // The first result follows its header closely; separate results keep one line.
    const sections = [
      ...canvasElement.querySelectorAll("[data-activity-section]"),
    ];
    const bodies = [
      ...canvasElement.querySelectorAll("[data-activity-content]"),
    ];
    await expect(
      sections[0].getBoundingClientRect().top -
        step.getBoundingClientRect().bottom,
    ).toBe(4);
    await expect(
      sections[1].getBoundingClientRect().top -
        sections[0].getBoundingClientRect().bottom,
    ).toBe(16);
    for (const body of bodies) {
      await expect(getComputedStyle(body).marginTop).toBe("4px");
      await expect(getComputedStyle(body).rowGap).toBe("4px");
    }
    const chronology = canvas.getByRole("list", {
      name: "Operation groups",
    });
    await expect(getComputedStyle(chronology).paddingBottom).toBe("4px");
    await expect(step.getBoundingClientRect().height).toBeGreaterThanOrEqual(
      28,
    );
    step.focus();
    await userEvent.keyboard("{Enter}");
    await expect(step).toHaveAttribute("aria-expanded", "false");
    await expect(canvas.queryByRole("table")).not.toBeInTheDocument();
  },
};
