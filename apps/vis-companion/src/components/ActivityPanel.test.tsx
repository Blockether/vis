// @vitest-environment jsdom
// Activity is a FIELD of the form that produced it, so every case here hands the
// axis the engine's own bounded snapshot — the fixture the host projects — and
// reads the document that landed. Nothing here opens, patches or closes a view:
// that is the Live View rail, and it is a different file for that reason.
import { cleanup, fireEvent, render, screen } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";
import {
  ActivityPanel,
  activityCostParts,
  activityReceiptText,
} from "./ActivityPanel";
import activityPanelSource from "./ActivityPanel.tsx?raw";
import activityFixture from "../../../../packages/vis-contract/resources/vis-contract/fixtures/activity.json";
import {
  ACTIVITY_LONG_RUNNING,
  ACTIVITY_TREE_CHANGES,
} from "../dev/story-data";
import * as storyData from "../dev/story-data";
import { WorkspaceRootsContext } from "../lib/workspace-roots";
import {
  activityProjectionFromWire,
  type ActivityProjection,
} from "../lib/activity";

afterEach(cleanup);

/**
 * The engine's own Activity fixture, parsed. Protocol 7 ships it as a bare
 * projection on the form that produced it, not as a classified view, so the
 * axis takes the snapshot itself.
 */
function activityProjection(): ActivityProjection {
  const projection = activityProjectionFromWire(activityFixture);
  if (!projection)
    throw new Error("the engine Activity fixture must be paintable");
  return projection;
}

function paintActivity(
  props: Partial<Parameters<typeof ActivityPanel>[0]> = {},
) {
  const { activity = activityProjection(), ...rest } = props;
  render(<ActivityPanel activity={activity} {...rest} />);
  const expand = screen.queryByRole("button", { name: "Expand Activity" });
  if (expand) fireEvent.click(expand);
  return document.body.innerHTML;
}

/**
 * Every step starts shut: what it did is one line, and what it left waits behind
 * its chevron. A case that reads UNDER a step presses it first — the step's own
 * headline is the toggle. Grouped changes appear with their group, so the press
 * repeats until no step is shut; the folds inside a step (a patch, a count) are
 * left as they stand.
 */
function openEverySettledStep() {
  for (;;) {
    const shut = document.querySelectorAll<HTMLElement>(
      '[data-activity-row] > div > :is(h4, p) > [data-disclosure-toggle][aria-expanded="false"]',
    );
    if (shut.length === 0) return;
    shut.forEach((toggle) => fireEvent.click(toggle));
  }
}

describe("joined Activity operation groups", () => {
  function reads() {
    const base = activityProjection();
    const rows = ["a", "b", "c"].map((id, sequence) => ({
      ...base.rows[0],
      id,
      sequence,
      operation: "cat",
      state: "succeeded" as const,
      summary: `${id}.clj`,
      result_summary: `${id} content`,
      presentation: undefined,
      children: undefined,
      evidence: [],
      resources: [{ type: "file" as const, id: "core.clj" }],
    }));
    return {
      ...base,
      state: "succeeded" as const,
      counts: { running: 0, succeeded: 3, failed: 0, cancelled: 0 },
      rows,
    };
  }

  it("groups adjacent reads, counts unique files and preserves disclosure through updates", () => {
    const activity = reads();
    const { rerender } = render(<ActivityPanel activity={activity} />);
    const initiallyShut = screen.getByRole("button", {
      name: "Expand Activity",
    });
    expect(initiallyShut.textContent).toContain("ACTIVITY");
    expect(initiallyShut.textContent).toContain("3 operations");
    expect(screen.queryByRole("button", { name: /Read ×3/ })).toBeNull();
    fireEvent.click(initiallyShut);
    const group = screen.getByRole("button", { name: /Read ×3/ });
    expect(group.textContent).toContain("1 file");
    expect(group.getAttribute("aria-expanded")).toBe("false");
    fireEvent.click(group);
    expect(document.querySelector('[data-activity-row="b"]')).toBeTruthy();
    rerender(
      <ActivityPanel
        activity={{
          ...activity,
          rows: [
            ...activity.rows,
            { ...activity.rows[0], id: "d", sequence: 3 },
          ],
        }}
      />,
    );
    expect(
      screen
        .getByRole("button", { name: /Read ×4/ })
        .getAttribute("aria-expanded"),
    ).toBe("true");
    const band = screen.getByRole("button", { name: /Collapse Activity/ });
    fireEvent.click(band);
    fireEvent.click(screen.getByRole("button", { name: /Expand Activity/ }));
    expect(
      screen
        .getByRole("button", { name: /Read ×4/ })
        .getAttribute("aria-expanded"),
    ).toBe("true");
  });

  it("never sorts separated runs together or hides failure behind the band", () => {
    const activity = reads();
    activity.rows[1] = { ...activity.rows[1], operation: "patch" };
    render(<ActivityPanel activity={activity} />);
    expect(screen.queryByRole("button", { name: /Read ×/ })).toBeNull();
    expect(
      [...document.querySelectorAll("[data-activity-row]")].map((row) =>
        row.getAttribute("data-activity-row"),
      ),
    ).toEqual(["a", "b", "c"]);
  });

  it("keeps failed and cancelled outcomes visible while a group is collapsed", () => {
    const activity = reads();
    render(
      <ActivityPanel
        activity={{
          ...activity,
          state: "failed",
          counts: { running: 0, succeeded: 1, failed: 1, cancelled: 1 },
          rows: activity.rows.map((row, index) => ({
            ...row,
            state:
              index === 1 ? "failed" : index === 2 ? "cancelled" : "succeeded",
            error_summary: index === 1 ? "Permission denied" : undefined,
          })),
        }}
      />,
    );
    expect(screen.queryByRole("button", { name: /Read ×3/ })).toBeNull();
    fireEvent.click(screen.getByRole("button", { name: "Expand Activity" }));
    expect(
      screen.getByRole("button", { name: /Read ×3/ }).textContent,
    ).toContain("1 failed");
    expect(screen.getByText(/Permission denied/)).toBeTruthy();
    expect(screen.getByText(/c.clj.*cancelled/)).toBeTruthy();
    fireEvent.click(screen.getByRole("button", { name: /Collapse Activity/ }));
    expect(
      screen.getByRole("button", { name: /Expand Activity/ }).textContent,
    ).toContain("1 failed");
  });
  it("bounds a collapsed receipt while keeping omitted operations explicit", () => {
    const activity = reads();
    const rows = Array.from({ length: 10 }, (_, index) => ({
      ...activity.rows[0],
      id: String(index),
      sequence: index,
      operation: index % 2 ? "patch" : "cat",
    }));
    render(
      <ActivityPanel
        activity={{
          ...activity,
          rows,
          omitted: { rows: 7, by_classification: { observation: 7 } },
        }}
      />,
    );
    const receipt = screen.getByRole("button", { name: "Expand Activity" });
    expect(receipt.textContent).toContain("17 operations");
    expect(receipt.textContent).toContain("7 omitted");
  });

  it("updates commands by snapshot identity, not by parsing identical command strings", () => {
    const activity = reads();
    const rows = activity.rows.slice(0, 2).map((row) => ({
      ...row,
      operation: "shell",
      summary: "npm test",
      state: "running" as const,
    }));
    const { rerender } = render(
      <ActivityPanel activity={{ ...activity, rows }} />,
    );
    fireEvent.click(screen.getByRole("button", { name: "Expand Activity" }));
    fireEvent.click(screen.getByRole("button", { name: /Shell ×2/ }));
    const next = {
      ...activity,
      rows: rows.map((row) => ({ ...row, state: "succeeded" as const })),
    };
    const original = JSON.stringify(next);
    rerender(<ActivityPanel activity={next} />);
    expect(
      screen
        .getByRole("button", { name: /Shell ×2/ })
        .getAttribute("aria-expanded"),
    ).toBe("true");
    expect(document.querySelectorAll("[data-activity-row]")).toHaveLength(2);
    expect(JSON.stringify(next)).toBe(original);
  });
});

describe("one form's Activity on the phone", () => {
  it("keeps root operations as siblings on one flat list", () => {
    paintActivity();
    const rows = [...document.querySelectorAll("[data-activity-row]")];
    expect(rows.length).toBeGreaterThan(1);
    expect(
      rows.every((row) => row.parentElement === rows[0].parentElement),
    ).toBe(true);
  });
  it("keeps the headline and one-line summary visible; the chevron opens only content", () => {
    const projection = activityProjection();
    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...projection.rows[0],
            operation: "ls",
            summary: "",
            resources: [],
            evidence: [],
            children: [],
            presentation: {
              headline: "Listed apps/vis-companion/src",
              summary: "3 directories · 2 files",
              content: [{ type: "text", text: "Listing details" }],
            },
          },
        ],
      },
    });
    const toggle = screen.getByRole("button", {
      name: /Listed apps\/vis-companion\/src/,
    });
    expect(toggle.getAttribute("aria-expanded")).toBe("false");
    expect(screen.getByText("3 directories · 2 files")).toBeTruthy();
    expect(screen.queryByText("Listing details")).toBeNull();
    fireEvent.click(toggle);
    expect(screen.getByText("Listing details")).toBeTruthy();
    expect(screen.getAllByText("3 directories · 2 files")).toHaveLength(1);
    fireEvent.click(toggle);
    expect(screen.getByText("3 directories · 2 files")).toBeTruthy();
    expect(screen.queryByText("Listing details")).toBeNull();
  });
  it("keeps batch headers and summaries visible, spaces sections, and replaces them without resetting disclosure", () => {
    const activity = storyData.ACTIVITY_LISTING_BATCH;
    const { rerender } = render(<ActivityPanel activity={activity} />);
    fireEvent.click(screen.getByRole("button", { name: "Expand Activity" }));
    const toggle = screen.getByRole("button", { name: /Listed 2 directories/ });
    expect(screen.getByText("3 directories · 2 files")).toBeTruthy();
    expect(screen.getByText("0 directories · 2 files")).toBeTruthy();
    expect(screen.queryByRole("table")).toBeNull();
    const sections = [...document.querySelectorAll("[data-activity-section]")];
    expect(sections[0].classList.contains("mt-1")).toBe(true);
    expect(
      sections[1].classList.contains("mt-[var(--text-ui--line-height)]"),
    ).toBe(true);
    fireEvent.click(toggle);
    expect(screen.getAllByRole("table")).toHaveLength(2);
    expect(
      screen.getAllByRole("group", { name: "Activity table" }),
    ).toHaveLength(2);
    const row = activity.rows[0];
    rerender(
      <ActivityPanel
        activity={{
          ...activity,
          rows: [
            {
              ...row,
              presentation: {
                ...row.presentation!,
                summary: "8 entries",
              },
            },
          ],
        }}
      />,
    );
    expect(toggle.getAttribute("aria-expanded")).toBe("true");
    expect(screen.getByText("8 entries")).toBeTruthy();
    expect(screen.getAllByRole("table")).toHaveLength(2);
    fireEvent.click(toggle);
    expect(screen.queryByRole("table")).toBeNull();
    expect(screen.getByText("0 directories · 2 files")).toBeTruthy();
  });
  it("does not put a chevron on a summary-only presentation or infer content headings", () => {
    const base = activityProjection();
    paintActivity({
      activity: {
        ...base,
        rows: [
          {
            ...base.rows[0],
            resources: [],
            evidence: [],
            presentation: {
              headline: "Listed src",
              summary: "2 files",
              content: [],
            },
          },
        ],
      },
    });
    expect(screen.getByText("2 files")).toBeTruthy();
    expect(screen.queryByRole("button", { name: /Listed src/ })).toBeNull();
  });
  it("keeps the engine failure visible when custom content is collapsed", () => {
    const base = activityProjection();
    paintActivity({
      activity: {
        ...base,
        rows: [
          {
            ...base.rows[0],
            state: "failed",
            resources: [],
            evidence: [],
            error_summary: "Permission denied",
            presentation: {
              headline: "List src",
              summary: "Preparing listing",
              content: [{ type: "text", text: "Listing details" }],
            },
          },
        ],
      },
    });
    fireEvent.click(screen.getByRole("button", { name: /List src/ }));
    expect(screen.getByText("Permission denied")).toBeTruthy();
    expect(screen.queryByText("Preparing listing")).toBeNull();
    expect(screen.queryByText("Listing details")).toBeNull();
  });
  it("draws the chronology without being asked, in engine sequence", () => {
    paintActivity();

    const chronology = screen.getByRole("list", {
      name: "Invocation chronology",
    });
    const chronologyText = chronology.textContent ?? "";
    expect(chronologyText.indexOf("Searched 18 matches")).toBeLessThan(
      chronologyText.indexOf("Running tests suite"),
    );
    // The band folds independently; it does not add another live region.
    expect(chronologyText).not.toContain("[{query: needle}]");
    expect(chronologyText).not.toContain("24 passed");
    expect(
      screen.getByRole("button", { name: "Collapse Activity" }),
    ).toBeTruthy();
    expect(screen.queryByRole("status")).toBeNull();
    expect(screen.queryByRole("button", { name: /interrupt/i })).toBeNull();
  });

  // Regression, issue td-5b6b08: settled Companion receipts said SUCCEEDED,
  // omitted the operation and elapsed time, and retained "activities run".
  it("matches the settled TUI receipt grammar and durations", () => {
    const projection = activityProjection();
    const settled = {
      ...projection,
      state: "succeeded" as const,
      counts: { running: 0, succeeded: 2, failed: 0, cancelled: 0 },
      rows: projection.rows.map(
        (row: ActivityProjection["rows"][number], index: number) => ({
          ...row,
          state: "succeeded" as const,
          ...(index === 0
            ? {
                operation: "shell",
                summary: "running: git status",
                duration_ms: 66,
              }
            : { duration_ms: 12_500 }),
        }),
      ),
    };

    paintActivity({ activity: settled });

    expect(activityReceiptText(settled, 12_600)).toBe(
      "SHELL · RUN_TESTS · 12.6s",
    );
    expect(
      screen.getByLabelText("Invocation chronology").textContent,
    ).toContain("Ran git status");
    expect(screen.getByText("66ms")).toBeTruthy();
    expect(screen.getByText("12.5s")).toBeTruthy();
  });

  it("adds no empty panel before the first operation", () => {
    paintActivity({
      activity: {
        ...activityProjection(),
        state: "idle",
        rows: [],
        counts: { running: 0, succeeded: 0, failed: 0, cancelled: 0 },
      },
    });
    expect(document.querySelector("[data-activity-axis]")).toBeNull();
  });
});

// The band is named by WHAT IT COST: what changed the repository, what was only
// read, what was checked. `0 mutations` is about the rows that are NOT there, so
// it always prints; the other two print only when they happened.
describe("what the iteration cost", () => {
  it("states the mutations, and stays quiet about a kind that did not happen", () => {
    const parts = activityCostParts(activityProjection());

    expect(parts.map((part) => part.text)).toEqual([
      "0 mutations",
      "1 observation",
      "1 check",
    ]);
    // Colour REPEATS the noun. Reading the words alone must lose nothing, so the
    // quiet count wears the margin's own ink and no tone at all.
    expect(parts.map((part) => part.tone)).toEqual([
      "text-accent-ink",
      "text-code-syntax-keyword",
      "",
    ]);
  });

  it("leaves the failures to the marks and the state word", () => {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    expect(
      activityCostParts({
        ...projection,
        counts: { running: 0, succeeded: 1, failed: 1, cancelled: 0 },
        rows: [
          { ...first, signal: "mutation" as const, state: "failed" as const },
          ...rest,
        ],
      }).map((part) => part.text),
    ).toEqual(["1 mutation", "1 check"]);
  });

  // The ENGINE's own bound is what drops rows, so the cost covers the whole run:
  // a chronology that shows four of ten calls must not report the cost of four,
  // and its tail can say `+6 more` but never what the six WERE.
  it("counts the rows the engine dropped, so a bounded axis cannot under-report", () => {
    const projection = activityProjection();

    expect(
      activityCostParts({
        ...projection,
        rows: [],
        omitted: {
          rows: 6,
          by_classification: { mutation: 6, observation: 2 },
        },
      }).map((part) => part.text),
    ).toEqual(["6 mutations", "2 observations"]);
  });
});

describe("the axis is built from the closed vocabulary", () => {
  it("borrows the app's controls and writes no styles of its own", () => {
    expect(activityPanelSource).toContain("<Disclosure");
    expect(activityPanelSource).toContain("<LoadMore");
    // No spinner: a mark that turns says only "still here", while one word says
    // whether the form is still working and, once it is not, how it ended.
    expect(activityPanelSource).not.toContain("<Spinner");
    expect(activityPanelSource).not.toContain("<button");
    expect(activityPanelSource).not.toContain("style={");
    expect(activityPanelSource).not.toContain('style="');
  });
});

// A flat chronology shares the turn spine; it never draws a second set of markers.
describe("a run reads as one thread", () => {
  it("keeps the one list and removes decorative per-operation marks", () => {
    paintActivity();
    expect(
      screen.getAllByRole("list", { name: "Invocation chronology" }),
    ).toHaveLength(1);
    for (const step of document.querySelectorAll("[data-activity-row]")) {
      expect(step.querySelector("span.absolute")).toBeNull();
    }
  });
  it("states running, failed and cancelled work in words rather than colour alone", () => {
    const base = activityProjection();
    render(
      <ActivityPanel
        activity={{
          ...base,
          rows: [
            {
              ...base.rows[0],
              id: "failed",
              state: "failed",
              error_summary: "Read refused",
            },
            { ...base.rows[1], id: "cancelled", state: "cancelled" },
          ],
        }}
      />,
    );
    expect(screen.getByText("Read refused")).toBeTruthy();
    expect(screen.getByText("Cancelled")).toBeTruthy();
  });

  it("names the work with a verb", () => {
    paintActivity();

    const chronology =
      screen.getByLabelText("Invocation chronology").textContent ?? "";
    expect(chronology).toContain("Searched 18 matches");
    expect(chronology).toContain("Running tests suite");
  });

  it("answers a patch with what it changed, and folds only the patch itself", () => {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...first,
            operation: "patch",
            summary: "2 files",
            resources: [{ type: "file", id: "src/components/ui.tsx" }],
            evidence: [
              {
                kind: "diff" as const,
                text: "src/components/ui.tsx",
                lines: [{ kind: "addition" as const, text: "added" }],
                additions: 7,
                deletions: 3,
                modifications: 0,
                is_truncated: false,
                is_redacted: false,
              },
            ],
          },
          ...rest,
        ],
      },
    });

    // Regression, T107 design review: the patch hung its paths inside a bordered
    // card, under the word "Patch" in bold — the row's own head printed a second
    // time, twenty pixels lower. The head is gone; only the diff still folds.
    openEverySettledStep();
    expect(screen.queryByText("Patch")).toBeNull();
    expect(screen.queryByText("Changed files")).toBeNull();
    expect(screen.queryByText("1 file")).toBeNull();
    expect(
      document.querySelector('[data-path="src/components/ui.tsx"]'),
    ).toBeTruthy();

    // Only the patch text folds.
    expect(screen.queryByText("added")).toBeNull();
    fireEvent.click(
      screen.getByRole("button", {
        name: "Expand the diff of src/components/ui.tsx",
      }),
    );
    expect(screen.getByText("added")).toBeTruthy();
  });

  it("gives a step with nothing to open no chevron and no toggle of its own", () => {
    paintActivity();

    // The band folds, but an operation with no details promises no disclosure.
    expect(screen.queryByRole("button", { name: /Searched/ })).toBeNull();
    expect(
      document.querySelectorAll("[data-activity-row] [data-disclosure-toggle]"),
    ).toHaveLength(0);
  });

  it("keeps what a step left behind its chevron until the step is pressed", () => {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...first,
            resources: [{ type: "file", id: "src/components/ui.tsx" }],
            result_summary:
              "src/components/ui.tsx:12: matched\nsrc/components/ui.tsx:40: matched",
          },
          ...rest,
        ],
      },
    });

    const step = screen.getByRole("button", { name: /Searched/ });
    expect(step.getAttribute("aria-expanded")).toBe("false");
    expect(
      document.querySelector('[data-path="src/components/ui.tsx"]'),
    ).toBeNull();
    expect(screen.queryByText(/ui\.tsx:40: matched/)).toBeNull();

    fireEvent.click(step);
    expect(step.getAttribute("aria-expanded")).toBe("true");
    expect(
      document.querySelector('[data-path="src/components/ui.tsx"]'),
    ).toBeTruthy();
    expect(screen.getByText(/ui\.tsx:40: matched/)).toBeTruthy();
  });

  it("lists the paths a step touched under its own line", () => {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...first,
            resources: [{ type: "file", id: "src/components/ui.tsx" }],
          },
          ...rest,
        ],
      },
    });

    openEverySettledStep();
    expect(
      document.querySelector('[data-path="src/components/ui.tsx"]'),
    ).toBeTruthy();
    // A path is a path everywhere on the axis: the type badge belonged to the
    // patch card, and that card is gone.
    expect(screen.queryByText("TSX")).toBeNull();
  });

  it("prints four paths and folds the rest behind one quiet count", () => {
    const projection = activityProjection();
    const [first] = projection.rows;
    const paths = [
      "src/components/ActivityPanel.tsx",
      "src/components/ChatContent.tsx",
      "src/components/ui.tsx",
      "src/index.css",
      "src/lib/activity.ts",
      "src/dev/story-data.ts",
    ];

    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...first,
            resources: paths.map((id) => ({ type: "file" as const, id })),
          },
        ],
      },
    });

    // Six paths under one step is the row with the least to say spending the most
    // height on saying it. Four print; the rest are a count, one press away.
    openEverySettledStep();
    expect(document.querySelectorAll("[data-path]")).toHaveLength(4);
    expect(
      document.querySelector('[data-path="src/lib/activity.ts"]'),
    ).toBeNull();

    fireEvent.click(screen.getByRole("button", { name: "Show 2 more paths" }));
    expect(document.querySelectorAll("[data-path]")).toHaveLength(6);
  });
});

// An error is the one thing on the axis nobody should have to go looking for,
// and also the one thing that can be forty lines long. It opens itself, and it
// opens CLAMPED — the whole of it lives in the raw result the invocation opens.
describe("a step that ended badly", () => {
  function paintFailure(text: string) {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    paintActivity({
      activity: {
        ...projection,
        state: "failed" as const,
        rows: [
          {
            ...first,
            state: "failed" as const,
            error_summary: "no match",
            evidence: [{ kind: "error" as const, text }],
          },
          ...rest,
        ],
      },
    });
  }

  it("says how it failed on its own line and opens itself", () => {
    paintFailure("patch refused: no anchor matched");

    // The machine's own text IS the reason. The row stamps no word on top of it:
    // the filled mark is the whole of the colour a failure gets.
    expect(
      document.querySelector('[data-activity-row="call-1"]')?.textContent,
    ).not.toContain("NO MATCH");
    expect(screen.getByText("patch refused: no anchor matched")).toBeTruthy();
  });

  // Regression, T131: a refusal was clamped to three lines with the rest behind a
  // rule, so the reader had to leave the axis to learn why the patch was refused.
  it("says the whole of what the machine said, however many lines", () => {
    paintFailure(["one", "two", "three", "four", "five"].join("\n"));

    expect(screen.getByText("one")).toBeTruthy();
    expect(screen.getByText("three")).toBeTruthy();
    expect(screen.getByText("five")).toBeTruthy();
    expect(screen.queryByText("2 more lines")).toBeNull();
  });
});

// One fact, one place. The mark, the verb and the pill each said "this failed",
// the error card repeated the row it hangs under, and a row counted the very list
// of paths printed below it — four spellings of two facts.
describe("the axis says a thing once", () => {
  function paintStep(row: Partial<ActivityProjection["rows"][number]>) {
    const projection = activityProjection();
    const [first] = projection.rows;
    paintActivity({
      activity: {
        ...projection,
        state: "failed" as const,
        rows: [{ ...first, ...row }],
      },
    });
  }

  const refusedPatch = {
    operation: "patch",
    summary: "src/components/ui.tsx",
    state: "failed" as const,
    error_summary: "no match",
    evidence: [
      { kind: "error" as const, text: "patch refused: no anchor matched" },
    ],
  };

  it("gives a failed step its own verb instead of the settled one", () => {
    paintStep(refusedPatch);

    const chronology =
      screen.getByLabelText("Invocation chronology").textContent ?? "";
    expect(chronology).toContain("Patch refused");
    expect(chronology).not.toContain("Patched");
  });

  it("names the operation and its object once, never again as a card head", () => {
    paintStep(refusedPatch);

    const chronology =
      screen.getByLabelText("Invocation chronology").textContent ?? "";
    expect(chronology).not.toContain("NO MATCH");
    expect(chronology.match(/src\/components\/ui\.tsx/g) ?? []).toHaveLength(1);
  });

  it("prints the engine's reason only when no text opens under the step", () => {
    paintStep({
      state: "failed" as const,
      error_summary: "the provider closed the stream before the first token",
      evidence: [],
    });

    const chronology =
      screen.getByLabelText("Invocation chronology").textContent ?? "";
    expect(chronology).not.toContain("FAILED");
    expect(chronology).toContain(
      "the provider closed the stream before the first token",
    );
  });

  it("lets the paths stand for a summary that does nothing but count them", () => {
    paintStep({
      operation: "cat",
      summary: "2 files",
      state: "succeeded" as const,
      resources: [
        { type: "file", id: "src/components/ui.tsx" },
        { type: "file", id: "src/index.css" },
      ],
      evidence: [],
    });

    openEverySettledStep();
    const chronology =
      screen.getByLabelText("Invocation chronology").textContent ?? "";
    expect(chronology).toContain("Read");
    expect(chronology).not.toContain("2 files");
    expect(document.querySelector('[data-path="src/index.css"]')).toBeTruthy();
  });

  it("keeps the file name whole and lets the directory be the part that gives way", () => {
    paintStep({
      operation: "cat",
      summary: "one file",
      state: "succeeded" as const,
      resources: [
        {
          type: "file",
          id: "src/com/blockether/vis/internal/channel/render.clj",
        },
      ],
      evidence: [],
    });

    openEverySettledStep();
    const path = document.querySelector(
      '[data-path="src/com/blockether/vis/internal/channel/render.clj"]',
    );
    const name = path?.lastElementChild;

    expect(name?.textContent).toBe("render.clj");
    expect(name?.className ?? "").not.toContain("truncate");
  });
});

// A chronology inside a live region is re-read from the top on every render, and a
// running step whose time column stands empty reads as a number that went missing.
describe("what the axis does while the work is still moving", () => {
  it("opens retained steps while live and keeps them open across replacements", () => {
    const activity = ACTIVITY_LONG_RUNNING;
    const rows = activity.rows;
    const { rerender } = render(<ActivityPanel activity={activity} />);
    fireEvent.click(screen.getByRole("button", { name: "Expand Activity" }));
    expect(document.querySelector('[data-activity-row="live-4"]')).toBeNull();
    expect(screen.getByText(/search-6 · running/)).toBeTruthy();
    fireEvent.click(screen.getByRole("button", { name: /Search ×7/ }));
    const step = screen.getByRole("button", { name: "Searched search-4" });
    fireEvent.click(step);
    expect(screen.getByText("result-4")).toBeTruthy();
    rerender(
      <ActivityPanel
        activity={{
          ...activity,
          rows: rows.map((row) =>
            row.id === "live-6" ? { ...row, state: "succeeded" } : row,
          ),
        }}
      />,
    );
    expect(screen.getByText("result-4")).toBeTruthy();
    fireEvent.click(screen.getByRole("button", { name: /Search ×7/ }));
    expect(document.querySelector('[data-activity-row="live-4"]')).toBeNull();
    expect(
      screen
        .getByRole("button", { name: /Search ×7/ })
        .getAttribute("aria-expanded"),
    ).toBe("false");
  });

  it("never hides failed or cancelled steps behind the retained-step fold", () => {
    const base = activityProjection();
    const rows = Array.from({ length: 6 }, (_, index) => ({
      ...base.rows[0],
      id: `step-${index}`,
      sequence: index,
      summary: `search-${index}`,
      operation: "grep",
      state: (index === 4
        ? "failed"
        : index === 5
          ? "cancelled"
          : "succeeded") as "failed" | "cancelled" | "succeeded",
    }));
    render(<ActivityPanel activity={{ ...base, rows }} />);
    expect(screen.getByText(/search-4 · failed/)).toBeTruthy();
    expect(screen.getByText(/search-5 · cancelled/)).toBeTruthy();
    expect(screen.queryByRole("button", { name: /more steps/i })).toBeNull();
  });

  it("says the clock is still counting instead of leaving the column empty", () => {
    const projection = activityProjection();
    const [first] = projection.rows;
    const running = { ...first, state: "running" as const };
    delete running.duration_ms;

    paintActivity({ activity: { ...projection, rows: [running] } });

    expect(
      screen.getByLabelText("Invocation chronology").textContent,
    ).toContain("…");
  });

  it("silences the live region it sits inside", () => {
    paintActivity();

    expect(
      document.querySelector("[data-activity-axis]")?.getAttribute("aria-live"),
    ).toBe("off");
  });

  it("discloses when a retained step has lost detail to the Activity limit", () => {
    const base = activityProjection();
    const row = {
      ...base.rows[0],
      state: "succeeded" as const,
      result_summary: undefined,
      evidence: [],
      resources: [],
      presentation: { headline: "Searched", summary: "2 matches", content: [] },
      is_truncated: true,
    };
    render(<ActivityPanel activity={{ ...base, rows: [row] }} />);
    expect(screen.queryByText("Details truncated")).toBeNull();
    fireEvent.click(
      document.querySelector(`[data-activity-row="${row.id}"] button`)!,
    );
    expect(screen.getByText("Details truncated")).toBeTruthy();
  });

  it("labels discarded steps as unavailable, not as a show-more control", () => {
    paintActivity({
      activity: {
        ...activityProjection(),
        omitted: { rows: 6, by_classification: { observation: 6 } },
      },
    });
    const tail = screen.getByText("6 steps omitted · Activity limit");
    expect(tail.closest("button")).toBeNull();
    expect(screen.queryByRole("button", { name: /6 more steps/i })).toBeNull();
  });
});

// Regression: a code block's own writes entered the chronology as unrelated top-level
// rows, and every row's words were read literally, so `probe_1.json` could not be marked
// as code and a group had no head naming the one cause that produced it.
describe("what a code block changed with its own hands", () => {
  it("hangs every change under one cause, indented, and stops at three levels", () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    const chronology = screen.getByLabelText("Invocation chronology");
    const heads = chronology.querySelectorAll(
      ':scope > [data-activity-depth="0"]',
    );
    const children = chronology.querySelectorAll('[data-activity-depth="1"]');

    expect(heads).toHaveLength(1);
    expect(heads[0].querySelector("h4")?.textContent).toContain(
      "Changed 13 files and 2 directories",
    );
    expect(children).toHaveLength(5);
    expect(
      Array.from(children, (child) => {
        const head = child.querySelector("p")!;
        const duration = head.querySelector("time")?.textContent ?? "";
        return (head.textContent ?? "").replace(duration, "");
      }),
    ).toEqual([
      "Created 2 directories",
      // A change that carries a diff prints it, and a count already answered by the
      // paths listed under it is not printed a second time.
      "Wrote +4 −2",
      "Copied",
      "Moved vis/PLAN.md → docs/PLAN.md",
      "Deleted 6 files",
    ]);
    // Three levels, hard stop: the cause, the change, the paths it touched.
    expect(chronology.querySelector('[data-activity-depth="2"]')).toBeNull();
  });

  it("says who did it in the head, and marks only what the engine marked", () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    const head = document.querySelector('[data-activity-depth="0"]');
    const moved = document.querySelectorAll('[data-activity-depth="1"]')[3];

    // The head's own sentence is markdown BECAUSE the engine declared it so: `patch`
    // and `shell` are the tools it is telling the reader were not involved.
    expect(head?.querySelector("code")?.textContent).toBe("patch");
    expect(head?.textContent).toContain("The code block changed these itself");
    expect(head?.textContent).not.toContain("`");
    // A marked name is code; the row keeps no backtick of its own.
    expect(moved?.querySelector("code")?.textContent).toBe("vis/PLAN.md");
  });

  it("leaves the paths to the change that touched them", () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    const head = document.querySelector('[data-activity-depth="0"]');
    const deleted = document.querySelectorAll('[data-activity-depth="1"]')[4];

    // The head carries every child's resource on the wire; painting them there AND
    // under each change is the same paths printed twice.
    expect(head?.querySelector(":scope > div [data-path]")).toBeNull();
    expect(deleted?.querySelectorAll("[data-path]").length).toBe(4);
    expect(deleted?.textContent).toContain("show 2 more files");
  });
});

// Regression, T120 design review: a step that changed several files hung ONE fold over
// every patch concatenated behind a `--- (path)` line, so the reader found a file by
// reading a header out of the diff and the payload's bound was spent on whichever file
// came first.
describe("a change opens under the file it changed", () => {
  it("gives every changed file its own fold, and opens only that one", () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    const write = document.querySelectorAll('[data-activity-depth="1"]')[1];
    // The change's own headline opened with the group; what is left to fold is
    // one patch per file.
    const folds = write.querySelectorAll(
      "[data-disclosure-toggle][aria-label]",
    );

    expect(
      Array.from(folds, (fold) => fold.getAttribute("aria-label")),
    ).toEqual([
      "Expand the diff of /Users/dev/vis/apps/vis-companion/src/dev/story-data.ts",
      "Expand the diff of /Users/dev/vis/apps/vis-companion/src/components/ActivityPanel.tsx",
      "Expand the diff of /Users/dev/vis/apps/vis-companion/src/lib/path.ts",
    ]);

    expect(screen.queryByLabelText("Unified diff")).toBeNull();
    fireEvent.click(folds[2]);

    const opened = screen.getAllByLabelText("Unified diff");
    expect(opened).toHaveLength(1);
    expect(opened[0].textContent).toContain("homeifyPath(root)");
    expect(opened[0].textContent).not.toContain("summary_format");
  });
});

// Regression, T120 design review: every row printed the machine's whole
// `/Users/…/vis/` prefix, which is the one part `truncate` never eats.
describe("a path reads short and stays addressable", () => {
  const pathOf = (id: string) => document.querySelector(`[data-path="${id}"]`);
  const written = "/Users/dev/vis/apps/vis-companion/src/lib/path.ts";

  it("shortens against the workspace root and keeps the absolute id", () => {
    render(
      <WorkspaceRootsContext.Provider value={["/Users/dev/vis"]}>
        <ActivityPanel activity={ACTIVITY_TREE_CHANGES} />
      </WorkspaceRootsContext.Provider>,
    );
    openEverySettledStep();

    expect(pathOf(written)?.textContent).toBe(
      "apps/vis-companion/src/lib/path.ts",
    );
  });

  it("falls back to the home form when no root owns the file", () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    expect(pathOf(written)?.textContent).toBe(
      "~/vis/apps/vis-companion/src/lib/path.ts",
    );
  });
});

// Regression, T121: a story fixture spelled a diff line's own `+`/`-` into its text
// while the renderer draws that sign in its own marker column, so the review picture
// showed `+ +` and `- -` on a surface the engine never feeds that way — it strips the
// sign in `internal/activity/event.clj` and leaves the column to say it.
describe("a diff line carries its sign only once", () => {
  const SIGNED = new Set(["addition", "deletion", "context"]);

  it("leaves the sign to the marker column in every story fixture", () => {
    const doubled: string[] = [];
    const seen = new Set<unknown>();
    const visit = (value: unknown) => {
      if (!value || typeof value !== "object" || seen.has(value)) return;
      seen.add(value);
      if (Array.isArray(value)) {
        value.forEach(visit);
        return;
      }
      const node = value as Record<string, unknown>;
      if (node.kind === "diff" && Array.isArray(node.lines)) {
        for (const line of node.lines as { kind: string; text: string }[]) {
          if (SIGNED.has(line.kind) && /^[-+]/.test(line.text)) {
            doubled.push(`${line.kind}: ${line.text}`);
          }
        }
      }
      Object.values(node).forEach(visit);
    };

    visit(storyData);
    expect(doubled).toEqual([]);
  });
});

describe("bounded activity outcomes", () => {
  it("opens the complete result on demand, not a quoted payload wall", () => {
    const activity = activityProjection();
    const full = Array.from(
      { length: 30 },
      (_, i) => `match ${i}: source detail`,
    ).join("\n");
    activity.rows = [{ ...activity.rows[0], result_summary: full }];
    paintActivity({ activity });
    openEverySettledStep();
    expect(screen.queryByText(/match 29/)).toBeNull();
    fireEvent.click(
      screen.getByRole("button", { name: "Expand result summary" }),
    );
    expect(screen.getByText(/match 29/).textContent).toContain(full);
    fireEvent.click(
      screen.getByRole("button", { name: "Collapse result summary" }),
    );
    expect(screen.queryByText(/match 29/)).toBeNull();
  });
});

it("renders symbol content and replaces progress without changing lifecycle", () => {
  const activity = activityProjection();
  activity.rows = [
    {
      ...activity.rows[0],
      state: "running",
      presentation: {
        headline: "Verification",
        summary: "1 of 2 checks",
        content: [
          { type: "markdown", text: "**Prepared** workspace" },
          {
            type: "table",
            columns: ["Suite", "Result"],
            rows: [["unit", "passed"]],
          },
          { type: "code", language: "python", text: "print(42)" },
          { type: "diff", text: "+added\n-removed" },
          { type: "progress", label: "Checking", value: 1, total: 2 },
          { type: "image", attachment_id: "screen", label: "Screenshot" },
        ],
      },
    },
  ];
  const { rerender } = render(<ActivityPanel activity={activity} />);
  fireEvent.click(screen.getByRole("button", { name: "Expand Activity" }));
  expect(screen.getByRole("heading", { name: /Verification/ })).toBeTruthy();
  expect(screen.getByText("Prepared").tagName).toBe("STRONG");
  expect(screen.getByRole("cell", { name: "passed" })).toBeTruthy();
  expect(
    screen.getByRole("progressbar", { name: "Checking" }).getAttribute("value"),
  ).toBe("1");
  expect(screen.getByText("Attachment unavailable")).toBeTruthy();
  const next = {
    ...activity,
    rows: [
      {
        ...activity.rows[0],
        presentation: {
          headline: "Verification",
          summary: "2 of 2 checks",
          content: [{ type: "text" as const, text: "Finished stage" }],
        },
      },
    ],
  };
  rerender(<ActivityPanel activity={next} />);
  expect(screen.queryByRole("progressbar")).toBeNull();
  expect(screen.getByText("Finished stage")).toBeTruthy();
});
