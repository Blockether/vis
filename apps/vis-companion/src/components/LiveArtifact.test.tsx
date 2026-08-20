// @vitest-environment jsdom
// A settled run is opened from a FILE, so every case here folds a real record —
// the three lines `human-input.live-sink` writes — and reads the document that
// landed. The point of the screen is that a finished run stays readable without
// the terminal that ran it, and that reading it costs the phone a bounded
// picture rather than the whole log.
import {
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  LiveArtifact,
  liveRecordFromEdges,
  liveRunName,
  LiveRunRow,
  liveVerdictLine,
  LIVE_RECORD_FOLD_LIMIT,
} from "./LiveArtifact";
import liveArtifactSource from "./LiveArtifact.tsx?raw";
import fixture from "../lib/live-view.fixture.json";
import { liveViewFromWire, type LiveRecord } from "../lib/live-view";
import type { GatewayClient } from "../lib/gateway";
import type { IterationAttachment } from "../lib/types";

afterEach(cleanup);

const openLine = JSON.stringify({ kind: "open", at: 1, view: fixture });

const sealed = {
  ...fixture,
  nodes: [{ id: "verdict", type: "status", text: "swept 3 hosts", tone: "ok" }],
};

function closeLine(result: Record<string, unknown>): string {
  return JSON.stringify({ kind: "close", at: 99, result });
}

/** A verdict with the engine's own view behind it — only the trailer keys vary. */
function ended(verdict: Partial<LiveRecord>): LiveRecord {
  const view = liveViewFromWire(fixture);
  if (!view) throw new Error("the engine fixture must be paintable");
  return { view, ...verdict };
}

function client(): GatewayClient {
  return {
    liveViewLog: vi.fn(async () => ({
      node_id: "x",
      from: 0,
      lines: [],
      total: 0,
    })),
  } as unknown as GatewayClient;
}

function serve(text: string) {
  vi.stubGlobal("fetch", async () => ({
    ok: true,
    blob: async () => new Blob([text]),
  }));
}

describe("how a finished run reads", () => {
  it("says how it ended in the words a person uses, and carries the note", () => {
    expect(liveVerdictLine(ended({ reason: "completed" }))).toBe("finished");
    expect(liveVerdictLine(ended({ reason: "timeout" }))).toBe("timed out");
    expect(liveVerdictLine(ended({ reason: "superseded" }))).toBe("superseded");
    expect(
      liveVerdictLine(ended({ reason: "interrupted", note: "flaky on rerun" })),
    ).toBe("stopped by hand — flaky on rerun");
  });

  it('shows a reason it has no word for verbatim rather than as "ended"', () => {
    // The engine owns `live-reasons`; a vocabulary that grew there must reach the
    // phone as itself instead of being flattened into a word that says nothing.
    expect(liveVerdictLine(ended({ reason: "evicted" }))).toBe("evicted");
    expect(liveVerdictLine(ended({}))).toBe("still recording");
  });
});

describe("a long record, read at its two ends", () => {
  it("paints the verdict picture without parsing a single patch line", () => {
    const head = `${openLine}\n{"kind":"patch","at":2,"patch":{"seq":1,"ops":[]}}`;
    const tail = `{"kind":"patch","at":8,"pa\n${closeLine({ reason: "completed", is_completed: true, view: sealed })}`;
    const record = liveRecordFromEdges(head, tail);
    expect(record?.view.nodes.map((node) => node.id)).toEqual(["verdict"]);
    expect(record?.reason).toBe("completed");
  });

  it("is null when neither end holds one complete line", () => {
    // A tail slice starts mid-line, so the FIRST line in it is only half a patch:
    // taking it would paint a picture out of a line the engine never finished.
    expect(liveRecordFromEdges(openLine, "half a lin")).not.toBeNull();
    expect(liveRecordFromEdges("half a lin", "half a lin")).toBeNull();
  });
});

describe("the artifact on screen", () => {
  it("paints the picture the run ended on, with nothing left running", async () => {
    serve(
      [
        openLine,
        closeLine({ reason: "completed", is_completed: true, view: sealed }),
      ].join("\n"),
    );
    render(
      <LiveArtifact
        client={client()}
        sid="s1"
        url="blob:record"
        chrome={({ subtitle, body }) => (
          <div>
            <p>{subtitle}</p>
            {body}
          </div>
        )}
      />,
    );
    await waitFor(() => expect(screen.getByText("swept 3 hosts")).toBeTruthy());
    expect(screen.getByText("finished")).toBeTruthy();
    // A record cannot change again: no live region, and nothing to interrupt.
    expect(document.querySelector('[role="status"]')).toBeNull();
    expect(document.querySelector("[aria-live]")).toBeNull();
    expect(screen.queryByRole("button", { name: /interrupt/i })).toBeNull();
  });

  it("says so when the record cannot be read, instead of an empty picture", async () => {
    serve("not a record at all");
    render(
      <LiveArtifact
        client={client()}
        sid="s1"
        url="blob:broken"
        chrome={({ body }) => <div>{body}</div>}
      />,
    );
    await waitFor(() =>
      expect(
        screen.getByText("This run's record could not be read."),
      ).toBeTruthy(),
    );
  });

  it("reads only the ends of a record past the fold limit", async () => {
    // The middle here is not JSON at all: a screen that folded the whole file
    // would stop at it and have no verdict to show. The verdict on screen IS the
    // proof that a 100 000-line run never came into this document.
    const middle = "x".repeat(LIVE_RECORD_FOLD_LIMIT);
    serve(
      [openLine, middle, closeLine({ reason: "failed", view: sealed })].join(
        "\n",
      ),
    );
    render(
      <LiveArtifact
        client={client()}
        sid="s1"
        url="blob:huge"
        chrome={({ subtitle, body }) => (
          <div>
            <p>{subtitle}</p>
            {body}
          </div>
        )}
      />,
    );
    await waitFor(() => expect(screen.getByText("failed")).toBeTruthy());
    expect(screen.getByText("swept 3 hosts")).toBeTruthy();
  });

  it("is built out of the app's own controls", () => {
    expect(liveArtifactSource).toContain("<LiveViewPanel");
    expect(liveArtifactSource).not.toContain("<button");
    expect(liveArtifactSource).not.toContain("style=");
  });
});

// Reported from the app, with a screenshot: a `gh` watch that had finished left
// "1 file · release.live.ndjson" in the step — the record was in the session and
// the transcript said nothing about the run. A settled run is an ARTIFACT, so
// it reads like one: a row where it happened, and it opens.
describe("the settled run in the transcript", () => {
  function record(): IterationAttachment {
    return {
      index: 2,
      iteration_id: "it-1",
      filename: "release.live.ndjson",
      media_type: "application/vnd.vis.live+ndjson",
      size: 4096,
    } as IterationAttachment;
  }

  function rowClient(url: string | null = "blob:record") {
    const attachmentUrl = vi.fn(async () => url);
    const retainAttachment = vi.fn(() => () => undefined);
    return {
      client: {
        attachmentUrl,
        retainAttachment,
        liveViewLog: vi.fn(async () => ({
          node_id: "x",
          from: 0,
          lines: [],
          total: 0,
        })),
      } as unknown as GatewayClient,
      attachmentUrl,
    };
  }

  it("is one row, and costs nothing until it is opened", () => {
    serve(
      [openLine, closeLine({ reason: "completed", view: sealed })].join("\n"),
    );
    const { client: c, attachmentUrl } = rowClient();
    render(<LiveRunRow client={c} sid="s1" attachment={record()} />);
    // Named after the RUN, not after the file the engine filed it under.
    expect(
      screen.getByRole("button", { name: "Open run release" }),
    ).toBeTruthy();
    expect(screen.getByText("RUN")).toBeTruthy();
    // A 40MB record must not be fetched to paint a line of text.
    expect(attachmentUrl).not.toHaveBeenCalled();
    expect(screen.queryByRole("dialog")).toBeNull();
  });

  it("opens the picture the run ended on, and nothing about it can be pressed", async () => {
    serve(
      [
        openLine,
        closeLine({ reason: "interrupted", note: "enough", view: sealed }),
      ].join("\n"),
    );
    const { client: c, attachmentUrl } = rowClient();
    render(<LiveRunRow client={c} sid="s1" attachment={record()} />);
    fireEvent.click(screen.getByRole("button", { name: "Open run release" }));
    await waitFor(() => expect(screen.getByText("swept 3 hosts")).toBeTruthy());
    expect(attachmentUrl).toHaveBeenCalledWith("s1", "it-1", 2);
    // The band REPORTS the verdict; the run is over, so it is what it took, not
    // an offer to stop it.
    expect(screen.getByText("stopped by hand — enough")).toBeTruthy();
    expect(screen.queryByRole("button", { name: /interrupt/i })).toBeNull();
    expect(document.querySelector('[role="status"]')).toBeNull();
    // The way out is the band's own, and it names the run.
    expect(screen.getByRole("button", { name: "Close release" })).toBeTruthy();
  });

  it("says so when the record cannot be fetched, in the same screen", async () => {
    const { client: c } = rowClient(null);
    render(<LiveRunRow client={c} sid="s1" attachment={record()} />);
    fireEvent.click(screen.getByRole("button", { name: "Open run release" }));
    await waitFor(() => expect(screen.getByText("Loading…")).toBeTruthy());
  });

  it("paints nothing for a run no iteration owns", () => {
    const { client: c } = rowClient();
    render(
      <LiveRunRow
        client={c}
        sid="s1"
        attachment={
          { ...record(), iteration_id: undefined } as IterationAttachment
        }
      />,
    );
    expect(screen.queryByText("RUN")).toBeNull();
  });

  it("names the run out of the file the close filed it under", () => {
    expect(liveRunName("release.live.ndjson")).toBe("release");
    expect(liveRunName("scan.LIVE.NDJSON")).toBe("scan");
    expect(liveRunName(undefined)).toBe("run");
  });
});
