// @vitest-environment jsdom
// Activity is a FIELD of the form that produced it, so every case here hands the
// axis the engine's own bounded snapshot — the fixture the host projects — and
// reads the document that landed. Nothing here opens, patches or closes a view:
// that is the Live View rail, and it is a different file for that reason.
import { cleanup, fireEvent, render, screen } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";
import {
  ActivityPanel,
  activityCallText,
  activityCostParts,
  activityReceiptText,
} from "./ActivityPanel";
import activityPanelSource from "./ActivityPanel.tsx?raw";
import activityFixture from "../lib/activity.fixture.json";
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

// The band is named by WHAT IT CALLED, and its margin says what that cost: what
// changed the repository, what was only read, what was checked. `0 mutations` is
// about the rows that are NOT there, so it always prints; the other two print
// only when they happened.
describe("what the iteration called and what it cost", () => {
  it("names the calls as they were typed, in the engine's order", () => {
    expect(activityCallText(activityProjection())).toBe("grep · run_tests");
  });

  it("keeps three names and counts the rest, the dropped rows included", () => {
    const projection = activityProjection();
    const [first] = projection.rows;
    const rows = [1, 2, 3, 4, 5].map((sequence) => ({
      ...first,
      id: `call-${sequence}`,
      sequence,
      operation: `tool_${sequence}`,
    }));

    expect(
      activityCallText({
        ...projection,
        rows,
        omitted: { rows: 2, by_classification: {} },
      }),
    ).toBe("tool_1 · tool_2 · tool_3 +4");
  });

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
    // The patch is the one thing on the axis long enough to fold, so it owns the
    // one chevron: a step itself never opens.
    expect((activityPanelSource.match(/<Disclosure/g) ?? []).length).toBe(1);
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
                text: "+added",
                lines: [{ kind: "addition" as const, text: "added" }],
                additions: 7,
                deletions: 3,
                modifications: 0,
                omitted_lines: 0,
                is_truncated: false,
                is_redacted: false,
              },
            ],
          },
          ...rest,
        ],
      },
    });

    // The head names what OPENS. The row above already printed the count and the
    // totals, so the card says neither a second time.
    expect(screen.getByText("Patch")).toBeTruthy();
    expect(screen.queryByText("Changed files")).toBeNull();
    expect(screen.queryByText("1 file")).toBeNull();
    expect(screen.getByText("TSX")).toBeTruthy();
    expect(
      document.querySelector('[data-path="src/components/ui.tsx"]'),
    ).toBeTruthy();

    // Only the patch text folds.
    expect(screen.queryByText("added")).toBeNull();
    fireEvent.click(
      screen.getByRole("button", {
        name: "Expand the patch of Patched · 2 files",
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
    // A read is a path and nothing else: the type mark belongs to the change card,
    // where a file is one of several and its kind is what tells them apart.
    expect(screen.queryByText("TSX")).toBeNull();
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

  it("clamps the output to its head and says how much it kept back", () => {
    paintFailure(["one", "two", "three", "four", "five"].join("\n"));

    expect(screen.getByText("one")).toBeTruthy();
    expect(screen.getByText("three")).toBeTruthy();
    expect(screen.queryByText("four")).toBeNull();
    expect(screen.getByText("+2 more lines")).toBeTruthy();
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

  it("ends on a mark of its own when the engine dropped the tail", () => {
    paintActivity({
      activity: {
        ...activityProjection(),
        omitted: { rows: 6, by_classification: { observation: 6 } },
      },
    });

    const tail = screen.getByText("+6 more", { exact: false });

    expect(tail.querySelector('span[aria-hidden="true"]')).toBeTruthy();
  });
});
