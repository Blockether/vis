/**
 * A SETTLED live view, re-opened from its record.
 *
 * A run that showed its work leaves the screen when it ends — and leaves an
 * ARTIFACT behind: the NDJSON the engine appended as it ran. Opening that row
 * here paints the picture the run ended on, with its log still walkable a page at
 * a time from the gateway, so "what did that scan actually find?" is answered
 * from the gallery instead of from a terminal that has since scrolled away.
 *
 * WHAT IS NEVER PARSED is the middle. The record arrives as ONE artifact, through
 * the same cache every artifact uses — the byte endpoint serves no ranges — but a
 * long one must not become that many megabytes of `JSON.parse`: past
 * `LIVE_RECORD_FOLD_LIMIT` only the two ENDS are read, which are the whole picture
 * anyway (the declared view, and the verdict that sealed it carrying the final
 * materialized state), and the patches between them are never touched. The LOG is
 * never held at all: scrollback comes from the range route, which reads the same
 * file server-side, one window at a time.
 */

import { useEffect, useState, type ReactNode } from "react";
import type { GatewayClient } from "../lib/gateway";
import { liveRecordFromText, type LiveRecord } from "../lib/live-view";
import { LiveViewPanel } from "./LiveView";

/** Under this, the whole record is folded patch by patch — the honest replay. */
export const LIVE_RECORD_FOLD_LIMIT = 1_000_000;

/** How much of each END of a longer record is read: one line is far less than this. */
export const LIVE_RECORD_EDGE = 128_000;

/**
 * The engine's closed `live-reasons`, in the words a person uses for them. An
 * unknown reason is shown VERBATIM rather than swallowed: a vocabulary that grew
 * in the engine must not read as "ended" on the phone.
 */
const REASON_WORDS: Record<string, string> = {
  completed: "finished",
  interrupted: "stopped by hand",
  timeout: "timed out",
  undeliverable: "lost its surface",
  failed: "failed",
};

/** How the run ended, and the comment whoever stopped it left. */
export function liveVerdictLine(record: LiveRecord): string {
  const reason = record.reason ?? "";
  const word = reason
    ? (REASON_WORDS[reason] ?? reason)
    : record.is_completed
      ? "finished"
      : "still recording";
  return record.note ? `${word} — ${record.note}` : word;
}

/** The first COMPLETE line of a slice taken from the head of a record. */
function firstLine(head: string): string {
  const cut = head.indexOf("\n");
  return cut < 0 ? head : head.slice(0, cut);
}

/** The last COMPLETE line of a slice taken from the tail of a record. */
function lastLine(tail: string): string {
  const lines = tail.split("\n").filter((line) => line.trim() !== "");
  return lines.length > 1 ? lines[lines.length - 1] : "";
}

/**
 * The record folded from its two ENDS — what a long run is read as. Pure, so the
 * decision "the verdict is the picture" is testable without a byte of network.
 */
export function liveRecordFromEdges(
  head: string,
  tail: string,
): LiveRecord | null {
  return liveRecordFromText(`${firstLine(head)}\n${lastLine(tail)}`);
}

/** The record behind one artifact URL, read whole or at its edges by SIZE. */
export async function readLiveRecord(url: string): Promise<LiveRecord | null> {
  const response = await fetch(url);
  if (!response.ok) throw new Error(`live record ${response.status}`);
  const blob = await response.blob();
  if (blob.size <= LIVE_RECORD_FOLD_LIMIT) {
    return liveRecordFromText(await blob.text());
  }
  const [head, tail] = await Promise.all([
    blob.slice(0, LIVE_RECORD_EDGE).text(),
    blob.slice(Math.max(0, blob.size - LIVE_RECORD_EDGE)).text(),
  ]);
  return liveRecordFromEdges(head, tail);
}

/**
 * The opened artifact. `chrome` wraps it in the overlay the sheet already owns —
 * the same seam the note reader uses — because the band's subtitle is the verdict,
 * and the verdict is only known once the record has been read.
 */
export function LiveArtifact({
  client,
  sid,
  url,
  chrome,
}: {
  client: GatewayClient;
  sid: string;
  url: string;
  chrome: (parts: { subtitle: string; body: ReactNode }) => ReactNode;
}) {
  const [record, setRecord] = useState<LiveRecord | null>(null);
  const [failed, setFailed] = useState(false);

  useEffect(() => {
    let alive = true;
    setRecord(null);
    setFailed(false);
    readLiveRecord(url)
      .then((next) => {
        if (!alive) return;
        setRecord(next);
        setFailed(next === null);
      })
      .catch(() => {
        if (alive) setFailed(true);
      });
    return () => {
      alive = false;
    };
  }, [url]);

  const body = record ? (
    <div className="min-h-0 flex-1 overflow-y-auto p-3">
      <LiveViewPanel
        view={record.view}
        isSettled
        // The log is NOT in what was read: every page comes from the record on
        // the gateway, which is why a run that logged 100 000 lines opens here at
        // all. The view id is the record's own, so a page names the same file.
        load={(nodeId, from, limit) =>
          client.liveViewLog(sid, record.view.id, nodeId, from, limit)
        }
      />
    </div>
  ) : (
    <p className="p-4 font-mono text-meta text-dialog-hint">
      {failed ? "This run's record could not be read." : "Loading…"}
    </p>
  );

  return <>{chrome({ subtitle: record ? liveVerdictLine(record) : "", body })}</>;
}
