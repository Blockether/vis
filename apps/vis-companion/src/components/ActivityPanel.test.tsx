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
import activityFixture from "../lib/activity.fixture.json";
import { ACTIVITY_TREE_CHANGES } from "../dev/story-data";
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
  return document.body.innerHTML;
}

describe("one form's Activity on the phone", () => {
  it("draws the chronology without being asked, in engine sequence", () => {
    paintActivity();

    const chronology = screen.getByRole("list", {
      name: "Invocation chronology",
    });
    const chronologyText = chronology.textContent ?? "";
    expect(chronologyText.indexOf("Searched 18 matches")).toBeLessThan(
      chronologyText.indexOf("Running tests suite"),
    );
    // The program and the bytes it printed stay behind the invocation's own
    // disclosure. What the iteration DID is not something a reader opens, so
    // the axis has no expander of its own and no second live region either.
    expect(chronologyText).not.toContain("[{query: needle}]");
    expect(chronologyText).not.toContain("24 passed");
    expect(screen.queryByRole("button", { name: /Activity/ })).toBeNull();
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
      "DONE · SHELL and more · 12.6s",
    );
    expect(
      screen.getByLabelText("Invocation chronology").textContent,
    ).toContain("Ran git status");
    expect(screen.getByText("66ms")).toBeTruthy();
    expect(screen.getByText("12.5s")).toBeTruthy();
  });

  it("shows an explicit quiet empty state", () => {
    paintActivity({
      activity: { ...activityProjection(), state: "idle", rows: [] },
    });
    expect(screen.getByText("No operations yet")).toBeTruthy();
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
    // TWO chevrons and no more: a file's own patch, and the pathless one a `patch`
    // row folds under its bare name — the only things on the axis long enough to be
    // worth folding. A STEP still never opens, and neither does the thread. What the
    // axis HOLDS BACK wears no chevron at all: a cut is the one rule both surfaces
    // draw (`LoadMore`), and only TWO things on this axis are ever cut - the paths a
    // step touched, and the steps a long thread holds back. A patch and a failure's
    // own words are never cut: they are what the reader came here to read.
    expect((activityPanelSource.match(/<Disclosure/g) ?? []).length).toBe(2);
    expect((activityPanelSource.match(/<LoadMore/g) ?? []).length).toBe(2);
    // No spinner: a mark that turns says only "still here", while one word says
    // whether the form is still working and, once it is not, how it ended.
    expect(activityPanelSource).not.toContain("<Spinner");
    expect(activityPanelSource).not.toContain("<button");
    expect(activityPanelSource).not.toContain("style={");
    expect(activityPanelSource).not.toContain('style="');
  });
});

// A run is a chronology, so the axis draws one: the marks hang on a single line
// in engine sequence, the line stops where the work stopped, and a step owns no
// box of its own. These cases pin that line, the words on it, and the evidence
// that opens under one step without moving the others.
describe("a run reads as one thread", () => {
  it("hangs every step on one line and joins each mark to it", () => {
    paintActivity();

    // ONE line for the whole turn: the chronology draws none of its own. It
    // bleeds left onto the line the turn already runs down its column, and every
    // ring reaches that line with a tick — a dot floating beside a line is a
    // bullet in a list, a dot JOINED to it is a moment on a timeline. Nothing
    // brackets the group; that was a second border saying what the named row
    // above it already said.
    const branch = screen.getByRole("list", { name: "Invocation chronology" });
    expect(branch.className).toContain("-ml-6");
    expect(branch.className).not.toContain("before:");
    expect(branch.className).not.toContain("after:");

    const steps = [...document.querySelectorAll("[data-activity-row]")];
    expect(steps).toHaveLength(2);
    for (const step of steps) {
      // A step owns no box of its own, and its mark is a ring rather than a glyph.
      expect(step.className).not.toContain("border-t");
      expect(step.className).not.toContain("bg-result");
      const mark = step.querySelector("span.absolute")?.className ?? "";
      expect(mark).toContain("rounded-full");
      expect(mark).toContain("before:right-full");
      expect(step.querySelector("svg")).toBeNull();
    }
  });

  // Regression, T125: the mark said the state with its SHAPE - a grey ring once a
  // step had finished, a filled disc when it failed - so a reader had to learn a
  // legend to see which steps were done.
  it("says the state in the mark's colour: green done, yellow running, red failed", () => {
    const base = activityProjection();
    const [first, second] = base.rows;
    const marked: ActivityProjection = {
      ...base,
      rows: [
        { ...first, id: "done-1", state: "succeeded" as const },
        { ...second, id: "run-1", state: "running" as const },
        { ...first, id: "fail-1", state: "failed" as const },
      ],
    };

    paintActivity({ activity: marked });

    const markOf = (id: string) =>
      document.querySelector(`[data-activity-row="${id}"] span.absolute`)
        ?.className ?? "";

    expect(markOf("done-1")).toContain("border-ok");
    expect(markOf("run-1")).toContain("border-accent-ink");
    expect(markOf("fail-1")).toContain("border-err-ink");
    // ONE shape for all three, so nothing but the hue separates a finished step
    // from a failed one: every mark is the same ring with the same filled centre.
    for (const id of ["done-1", "run-1", "fail-1"]) {
      expect(markOf(id)).toContain("rounded-full");
      expect(
        document.querySelector(
          `[data-activity-row="${id}"] span.absolute > span`,
        ),
      ).toBeTruthy();
    }
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

  it("gives a step no chevron and no toggle of its own", () => {
    paintActivity();

    // The axis never closes and a step never opens: what the iteration DID is the
    // one thing a reader is not asked to go looking for. The program and the bytes
    // it printed stay behind the invocation's own disclosure, one level up.
    expect(screen.queryByRole("button", { name: /Searched/ })).toBeNull();
    expect(document.querySelectorAll("[data-disclosure-toggle]")).toHaveLength(
      0,
    );
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
        { type: "file", id: "src/com/blockether/vis/internal/render.clj" },
      ],
      evidence: [],
    });

    const path = document.querySelector(
      '[data-path="src/com/blockether/vis/internal/render.clj"]',
    );
    const name = path?.lastElementChild;

    expect(name?.textContent).toBe("render.clj");
    expect(name?.className ?? "").not.toContain("truncate");
  });
});

// A chronology inside a live region is re-read from the top on every render, and a
// running step whose time column stands empty reads as a number that went missing.
describe("what the axis does while the work is still moving", () => {
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

  // Regression, user report ("every show-more is the same rule with the words in
  // it"): the dropped tail ended on a node of its own and a bare `+6 more`.
  it("ends on the one rule when the engine dropped the tail", () => {
    paintActivity({
      activity: {
        ...activityProjection(),
        omitted: { rows: 6, by_classification: { observation: 6 } },
      },
    });

    const tail = screen.getByText("6 more steps");

    expect(tail.closest("li")?.querySelectorAll(".bg-dialog-edge").length).toBe(
      2,
    );
  });
});

// Regression: a code block's own writes entered the chronology as unrelated top-level
// rows, and every row's words were read literally, so `probe_1.json` could not be marked
// as code and a group had no head naming the one cause that produced it.
describe("what a code block changed with its own hands", () => {
  it("hangs every change under one cause, indented, and stops at three levels", () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);

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
      Array.from(children, (child) => child.querySelector("p")?.textContent),
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

    const write = document.querySelectorAll('[data-activity-depth="1"]')[1];
    const folds = write.querySelectorAll("[data-disclosure-toggle]");

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

    expect(pathOf(written)?.textContent).toBe(
      "apps/vis-companion/src/lib/path.ts",
    );
  });

  it("falls back to the home form when no root owns the file", () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);

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
