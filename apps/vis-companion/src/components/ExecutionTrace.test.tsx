// @vitest-environment jsdom
import { act, cleanup, fireEvent, render } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";
import { IterationTrace } from "./ChatContent";
import {
  readPythonCodeShown,
  setPythonCodeShown,
} from "../lib/transcript-display";
import type { TranscriptForm, TranscriptIteration } from "../lib/types";
import type { ActivityProjection } from "../lib/activity";

const activity = (
  state: ActivityProjection["state"],
  label: string,
): ActivityProjection => ({
  state,
  counts: {
    running: +(state === "running"),
    succeeded: +(state === "succeeded"),
    failed: +(state === "failed"),
    cancelled: +(state === "cancelled"),
  },
  rows: [
    {
      id: "same-id",
      sequence: 1,
      operation: "grep",
      presenter: "observation",
      signal: "observation",
      state,
      summary: label,
      resources: [],
      evidence: [],
    },
  ],
  omitted: { rows: 0, by_classification: {} },
});
const iterations = (forms: TranscriptForm[]): TranscriptIteration[] => [
  { id: "one", position: 1, forms },
];
afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
  setPythonCodeShown(true);
});

describe("execution grouping", () => {
  it("owns source and result in one CODE disclosure before Activity", () => {
    const view = render(<IterationTrace whole iterations={iterations([
      { source: "print(42)\nprint(43)", stdout: "out-42\nout-43", duration_ms: 57,
        activity: activity("succeeded", "first stage") },
    ])} />);
    const band = view.container.querySelector("[data-execution-code]")!;
    // Like THINKING: a chevron leads the name, and a tally says what is folded.
    expect(view.getByRole("button", { name: "Expand code" }).textContent).toBe("CODE +2 more");
    expect(view.getByRole("button", { name: "Expand code" }).querySelector("svg")).not.toBeNull();
    expect(band.querySelector("[data-code-node]")).toBeNull();
    expect(view.queryByText("RESULT")).toBeNull();
    fireEvent.click(view.getByRole("button", { name: "Expand code" }));
    expect(band.textContent).toContain("print(42)");
    expect(view.getByRole("button", { name: "Collapse code" }).textContent).toBe("CODE");
    // The RESULT is its own fold and starts closed.
    expect(band.textContent).toContain("RESULT +2 more");
    expect(band.textContent).not.toContain("out-42");
    fireEvent.click(view.getByRole("button", { name: "Expand result" }));
    expect(band.textContent).toContain("out-42");
    expect(view.getByRole("button", { name: "Collapse result" }).querySelector("svg")).not.toBeNull();
    expect(band.querySelector("summary")).toBeNull();
    expect(band.querySelector("details")).toBeNull();
    expect(band.compareDocumentPosition(view.container.querySelector("[data-activity-row]")!) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(view.getByText("57ms")).toBeTruthy();
    fireEvent.click(view.getByRole("button", { name: "Collapse code" }));
    expect(view.queryByText(/RESULT/)).toBeNull();
    act(() => setPythonCodeShown(false));
    expect(view.getByRole("button", { name: "Expand result" })).toBeTruthy();
    expect(view.container.textContent).not.toContain("print(42)");
    expect(view.container.textContent).toContain("first stage");
  });
  it("preserves ordered stages and scopes repeated invocation ids without changing wire data", () => {
    const forms = [
      {
        source: "first()",
        duration_ms: 500,
        activity: activity("succeeded", "first stage"),
      },
      {
        source: "second()",
        duration_ms: 700,
        activity: activity("succeeded", "second stage"),
      },
    ];
    const original = JSON.stringify(forms);
    const view = render(
      <IterationTrace whole iterations={iterations(forms)} />,
    );
    expect(
      view.queryByRole("button", { name: "Expand execution trace" }),
    ).toBeNull();
    fireEvent.click(view.getByRole("button", { name: /Search ×2/ }));
    const rows = [...view.container.querySelectorAll("[data-activity-row]")];
    expect(rows).toHaveLength(2);
    expect(rows.map((row) => row.getAttribute("data-activity-row"))).toEqual([
      "0:same-id",
      "1:same-id",
    ]);
    expect(rows[0].textContent).toContain("first stage");
    expect(rows[1].textContent).toContain("second stage");
    expect(JSON.stringify(forms)).toBe(original);
  });

  it.each(["running", "failed", "cancelled"] as const)(
    "keeps the group %s and every member's evidence",
    (state) => {
      const view = render(
        <IterationTrace
          whole
          live
          iterations={iterations([
            {
              source: "first()",
              error: { message: "InterruptedException" },
              activity: activity("cancelled", "cancelled stage"),
            },
            { source: "second()", activity: activity(state, "next stage") },
          ])}
        />,
      );
      const trace = view.container.querySelector(
        '[aria-label="Execution trace"]',
      )!;
      expect(trace.getAttribute("role")).toBe(
        state === "running" ? "status" : null,
      );
      if (state === "failed") expect(trace.textContent).toContain("Failed");
      fireEvent.click(view.getByRole("button", { name: /Search ×2/ }));
      expect(
        view.container.querySelectorAll("[data-activity-row]"),
      ).toHaveLength(2);
    },
  );

  it.each([
    { comment: "A separate step" },
    { display_language: "bash" },
    { tag: "user-shell" },
  ])("does not group across a semantic boundary %j", (boundary) => {
    const view = render(
      <IterationTrace
        whole
        iterations={iterations([
          { source: "first()", duration_ms: 1 },
          { source: "second()", duration_ms: 1, ...boundary },
        ])}
      />,
    );
    expect(
      view.container.querySelectorAll("[data-execution-code]"),
    ).toHaveLength(2);
  });

  it("copies the complete combined source once, even while code and results are collapsed", async () => {
    const writeText = vi.fn().mockResolvedValue(undefined);
    vi.stubGlobal("navigator", { ...navigator, clipboard: { writeText } });
    try {
      const view = render(
        <IterationTrace
          whole
          iterations={iterations([
            { source: "first()\nfirst_detail()", duration_ms: 1 },
            { source: "second()", duration_ms: 1 },
          ])}
        />,
      );
      fireEvent.click(view.getByRole("button", { name: "Copy code" }));
      expect(writeText).toHaveBeenCalledWith(
        "first()\nfirst_detail()\n\nsecond()",
      );
    } finally {
      vi.unstubAllGlobals();
    }
  });
  it("does not add empty result cards below an activity-only group", () => {
    const view = render(
      <IterationTrace
        whole
        live
        iterations={iterations([
          {
            source: "first()",
            duration_ms: 80,
            activity: activity("succeeded", "first stage"),
          },
          { source: "second()", activity: activity("running", "next stage") },
        ])}
      />,
    );
    fireEvent.click(view.getByRole("button", { name: /Search ×2/ }));
    expect(view.container.querySelectorAll("[data-activity-row]")).toHaveLength(
      2,
    );
    expect(view.queryByText("RESULT")).toBeNull();
    expect(view.container.querySelector(".bg-result")).toBeNull();
  });
});

describe("device-local source visibility", () => {
  it("updates mounted traces, persists across remounts, and keeps Activity available", () => {
    const data = iterations([
      {
        source: "secret_source()",
        activity: activity("succeeded", "retained stage"),
      },
    ]);
    const view = render(<IterationTrace whole iterations={data} />);
    expect(view.queryByRole("button", { name: "Copy code" })).not.toBeNull();
    act(() => setPythonCodeShown(false));
    expect(readPythonCodeShown()).toBe(false);
    expect(localStorage.getItem("vis.show_python_code")).toBe("hidden");
    expect(view.container.textContent).not.toContain("secret_source()");
    expect(view.queryByRole("button", { name: "Copy code" })).toBeNull();
    expect(view.container.textContent).toContain("retained stage");
    view.unmount();
    const restored = render(<IterationTrace whole iterations={data} />);
    expect(restored.container.textContent).not.toContain("secret_source()");
    act(() => setPythonCodeShown(true));
    fireEvent.click(restored.getByRole("button", { name: "Expand code" }));
    expect(restored.container.textContent).toContain("secret_source()");
  });
});

it("anchors the first source line to the rail and opens the complete program after a gap", () => {
  const code = "paths = await ls(root)\nprint(paths)";
  const view = render(
    <IterationTrace
      whole
      iterations={iterations([
        { source: code, activity: activity("succeeded", "Files listed") },
      ])}
    />,
  );
  const band = view.container.querySelector("[data-execution-code]")!;
  expect(band.classList.contains("-ml-4")).toBe(true);
  expect(
    view.queryByRole("button", { name: "Expand execution trace" }),
  ).toBeNull();
  expect(view.container.textContent).toContain("Files listed");
  expect(band.textContent).not.toContain("print(paths)");
  fireEvent.click(view.getByRole("button", { name: "Expand code" }));
  expect(
    band.querySelector("[data-code-body]")?.classList.contains("py-3"),
  ).toBe(true);
  expect(band.querySelector("pre")?.textContent).toContain(
    "paths = await ls(root)",
  );
  expect(band.querySelector("pre")?.textContent).toContain("print(paths)");
});

it("folds even a one-line program under the CODE header", () => {
  const view = render(
    <IterationTrace whole iterations={iterations([{ source: "print(42)" }])} />,
  );
  fireEvent.click(view.getByRole("button", { name: "Expand code" }));
  expect(view.container.textContent).toContain("print(42)");
  expect(view.getByRole("button", { name: "Copy code" })).toBeTruthy();
});
