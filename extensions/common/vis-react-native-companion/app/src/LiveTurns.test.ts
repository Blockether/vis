import {
  reduceLiveEvent,
  toolColor,
  toolLabel,
  traceCards,
  LiveState,
} from "./LiveTurns";
import { c } from "./theme";
import { GatewayEvent, TraceIteration } from "./VisClient";

/* The heart of live streaming on iOS: the SSE→card reducer. These lock the
   contract the gateway wire depends on (turn_id keying, complete prose/thinking snapshots, silent suppression, block upsert, immutability). */

const ev = (e: Partial<GatewayEvent>): GatewayEvent => e as GatewayEvent;

describe("toolLabel", () => {
  it("uppercases a plain wire name", () => {
    expect(toolLabel("rg")).toBe("RG");
    expect(toolLabel("cat")).toBe("CAT");
  });
  it("applies the label overrides", () => {
    expect(toolLabel("python_execution")).toBe("RESULT");
    expect(toolLabel("repl_eval")).toBe("REPL");
    expect(toolLabel("shell_run")).toBe("SHELL RUN");
    expect(toolLabel("shell_bg")).toBe("SHELL BACKGROUND");
  });
  it("falls back to RUN with no tool name (the model's own op)", () => {
    expect(toolLabel(undefined)).toBe("RUN");
    expect(toolLabel("")).toBe("RUN");
  });
});

describe("toolColor", () => {
  it("maps a namespaced color role to its hex", () => {
    expect(toolColor("tool-color/search")).toBe("#7C3AED");
    expect(toolColor("tool-color/edit")).toBe("#B45309");
    expect(toolColor("tool-color/delete")).toBe("#DC2626");
  });
  it("accepts a bare role without the namespace", () => {
    expect(toolColor("read")).toBe("#2563EB");
  });
  it("falls back to dim for unknown / missing roles", () => {
    expect(toolColor("tool-color/nope")).toBe(c.dim);
    expect(toolColor(undefined)).toBe(c.dim);
  });
});

describe("reduceLiveEvent", () => {
  it("ignores an event with no turn_id (returns the same state ref)", () => {
    const s: LiveState = {};
    expect(reduceLiveEvent(s, ev({ type: "context.updated" }))).toBe(s);
  });

  it("turn.started seeds an empty turn", () => {
    const s = reduceLiveEvent({}, ev({ type: "turn.started", turn_id: "t1" }));
    expect(s.t1).toEqual({ cards: [], prose: "", thinking: "", done: false });
  });

  it("block.started opens a running card with label + color + code", () => {
    const s = reduceLiveEvent(
      {},
      ev({
        type: "block.started",
        turn_id: "t1",
        iteration: 0,
        block_id: 3,
        tool_name: "rg",
        tool_color_role: "tool-color/search",
        code: '(rg "foo")',
      }),
    );
    const card = s.t1!.cards[0]!;
    expect(card).toMatchObject({
      key: "0:3",
      toolName: "rg",
      label: "RG",
      colorRole: "tool-color/search",
      code: '(rg "foo")',
      running: true,
      hideCode: true /* successful native tool → code hidden */,
    });
  });

  it("keeps code visible for python_execution (the model's own program)", () => {
    const s = reduceLiveEvent(
      {},
      ev({
        type: "block.started",
        turn_id: "t1",
        tool_name: "python_execution",
        code: "print(1)",
      }),
    );
    expect(s.t1!.cards[0]!.hideCode).toBe(false);
  });

  it("block.output updates the SAME card (upsert by iteration:block key)", () => {
    let s = reduceLiveEvent(
      {},
      ev({
        type: "block.started",
        turn_id: "t1",
        iteration: 1,
        block_id: 2,
        tool_name: "rg",
      }),
    );
    s = reduceLiveEvent(
      s,
      ev({
        type: "block.output",
        turn_id: "t1",
        iteration: 1,
        block_id: 2,
        tool_name: "rg",
        result_summary: "3 hits",
        stdout: "a\nb\nc",
        duration_ms: 42,
      }),
    );
    expect(s.t1!.cards).toHaveLength(1);
    expect(s.t1!.cards[0]).toMatchObject({
      key: "1:2",
      running: false,
      summary: "3 hits",
      body: "a\nb\nc",
      durationMs: 42,
    });
  });

  it("prefers result_render over stdout for the card body", () => {
    const s = reduceLiveEvent(
      {},
      ev({
        type: "block.output",
        turn_id: "t1",
        block_id: 0,
        result_render: "RENDERED",
        stdout: "RAW",
      }),
    );
    expect(s.t1!.cards[0]!.body).toBe("RENDERED");
  });

  it("a silent result removes the card entirely", () => {
    let s = reduceLiveEvent(
      {},
      ev({ type: "block.started", turn_id: "t1", block_id: 0, tool_name: "x" }),
    );
    expect(s.t1!.cards).toHaveLength(1);
    s = reduceLiveEvent(
      s,
      ev({ type: "block.output", turn_id: "t1", block_id: 0, silent: true }),
    );
    expect(s.t1!.cards).toHaveLength(0);
  });

  it("an errored output keeps its code and carries the error", () => {
    const s = reduceLiveEvent(
      {},
      ev({
        type: "block.output",
        turn_id: "t1",
        block_id: 0,
        tool_name: "rg",
        error: "boom",
        code: "(rg)",
      }),
    );
    expect(s.t1!.cards[0]).toMatchObject({ error: "boom", hideCode: false });
  });

  it("iteration.completed sets complete prose and thinking without text deltas", () => {
    const s = reduceLiveEvent(
      {},
      ev({
        type: "iteration.completed",
        turn_id: "t1",
        assistant_prose: "Full prose.",
        thinking: "Full thinking.",
      }),
    );
    expect(s.t1!.prose).toBe("Full prose.");
    expect(s.t1!.thinking).toBe("Full thinking.");
  });

  it("iteration.completed / error replaces the thinking snapshot", () => {
    let s = reduceLiveEvent(
      {},
      ev({ type: "iteration.completed", turn_id: "t1", thinking: "x" }),
    );
    expect(s.t1!.thinking).toBe("x");
    s = reduceLiveEvent(
      s,
      ev({ type: "iteration.error", turn_id: "t1", thinking: "failed" }),
    );
    expect(s.t1!.thinking).toBe("failed");
  });

  it("turn.completed / failed marks the turn done", () => {
    let s = reduceLiveEvent(
      {},
      ev({ type: "block.started", turn_id: "t1", block_id: 0 }),
    );
    expect(s.t1!.done).toBe(false);
    s = reduceLiveEvent(s, ev({ type: "turn.completed", turn_id: "t1" }));
    expect(s.t1!.done).toBe(true);
  });

  it("is immutable and never touches unrelated turns", () => {
    const s0: LiveState = {
      other: { cards: [], prose: "keep", thinking: "", done: true },
    };
    const s1 = reduceLiveEvent(
      s0,
      ev({
        type: "iteration.completed",
        turn_id: "t1",
        assistant_prose: "new",
      }),
    );
    expect(s1).not.toBe(s0);
    expect(s1.other).toBe(s0.other); /* untouched turn shares the same ref */
    expect(s1.t1!.prose).toBe("new");
  });

  it("folds a full turn lifecycle into the expected final state", () => {
    const seq: Partial<GatewayEvent>[] = [
      { type: "turn.started", turn_id: "t1" },
      { type: "iteration.completed", turn_id: "t1", thinking: "let me search" },
      {
        type: "block.started",
        turn_id: "t1",
        iteration: 0,
        block_id: 0,
        tool_name: "rg",
        tool_color_role: "tool-color/search",
        code: "(rg)",
      },
      {
        type: "block.output",
        turn_id: "t1",
        iteration: 0,
        block_id: 0,
        tool_name: "rg",
        result_summary: "1 hit",
        stdout: "x",
        duration_ms: 10,
      },
      {
        type: "iteration.completed",
        turn_id: "t1",
        assistant_prose: "Found it.",
      },
      { type: "iteration.completed", turn_id: "t1" },
      { type: "turn.completed", turn_id: "t1" },
    ];
    const final = seq.reduce(
      (s, e) => reduceLiveEvent(s, ev(e)),
      {} as LiveState,
    );
    expect(final.t1).toMatchObject({
      prose: "Found it.",
      thinking: "",
      done: true,
    });
    expect(final.t1!.cards).toHaveLength(1);
    expect(final.t1!.cards[0]).toMatchObject({
      label: "RG",
      summary: "1 hit",
      running: false,
    });
  });

  it("turn.started mid-stream resets the accumulated cards/prose", () => {
    let s = reduceLiveEvent(
      {},
      ev({
        type: "iteration.completed",
        turn_id: "t1",
        assistant_prose: "stale",
      }),
    );
    s = reduceLiveEvent(s, ev({ type: "turn.started", turn_id: "t1" }));
    expect(s.t1).toEqual({ cards: [], prose: "", thinking: "", done: false });
  });
});

/* Rehydration: a settled turn's persisted trace must rebuild the SAME cards it
   streamed live, so scrolled-back history keeps its op-cards. */
describe("traceCards", () => {
  const iter = (o: Partial<TraceIteration>): TraceIteration =>
    o as TraceIteration;

  it("maps one form to a settled card (src→code, result_render→body)", () => {
    const cards = traceCards([
      iter({
        position: 0,
        forms: [
          {
            tool_name: "shell_run",
            tool_color_role: "tool-color/shell",
            src: 'shell_run("ls")',
            result_summary: "$ ls (success)",
            result_render: "a\nb\nc",
            duration_ms: 520,
          },
        ],
      }),
    ]);
    expect(cards).toHaveLength(1);
    expect(cards[0]).toMatchObject({
      key: "0:0",
      label: "SHELL RUN",
      colorRole: "tool-color/shell",
      code: 'shell_run("ls")',
      summary: "$ ls (success)",
      body: "a\nb\nc",
      durationMs: 520,
      running: false,
    });
    /* a successful native tool hides its synthesized code, same as live */
    expect(cards[0]!.hideCode).toBe(true);
    expect(cards[0]!.error).toBeUndefined();
  });

  it("keeps code for python_execution (the model's own program)", () => {
    const cards = traceCards([
      iter({
        position: 0,
        forms: [{ tool_name: "python_execution", src: "print(1)" }],
      }),
    ]);
    expect(cards[0]!.hideCode).toBe(false);
  });

  it("never styles a trace card as an error (persisted forms carry no status)", () => {
    const cards = traceCards([
      iter({
        position: 0,
        forms: [
          {
            tool_name: "cat",
            tag: "observation",
            src: "cat('nope')",
            result_summary: "no such file",
          },
        ],
      }),
    ]);
    expect(cards[0]!.error).toBeUndefined();
    expect(cards[0]!.summary).toBe("no such file");
  });

  it("flattens forms across iterations with unique keys", () => {
    const cards = traceCards([
      iter({ position: 0, forms: [{ tool_name: "rg" }, { tool_name: "cat" }] }),
      iter({ position: 1, forms: [{ tool_name: "shell_run" }] }),
    ]);
    expect(cards.map((k) => k.key)).toEqual(["0:0", "0:1", "1:0"]);
  });

  it("falls back to array index when position is absent", () => {
    const cards = traceCards([
      iter({ forms: [{ tool_name: "rg" }] }),
      iter({ forms: [{ tool_name: "cat" }] }),
    ]);
    expect(cards.map((k) => k.key)).toEqual(["0:0", "1:0"]);
  });

  it("is empty for a turn that ran no tools", () => {
    expect(
      traceCards([
        iter({ position: 0, thinking: "just thinking", forms: [] }),
        iter({}),
      ]),
    ).toEqual([]);
  });
});
