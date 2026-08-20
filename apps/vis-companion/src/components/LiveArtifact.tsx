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

import {
  memo,
  useCallback,
  useEffect,
  useState,
  type ReactNode,
} from "react";
import { createPortal } from "react-dom";
import { attachmentBytes } from "../lib/artifacts";
import type { GatewayClient } from "../lib/gateway";
import { liveRecordFromText, type LiveRecord } from "../lib/live-view";
import { useStickyOverlay } from "../lib/sticky-overlay";
import type { IterationAttachment } from "../lib/types";
import { LiveViewPanel } from "./LiveView";
import { ChevronIcon } from "./icons";
import { ListRow, overlayLayer, OverlayScreen } from "./ui";

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

/**
 * The run's own NAME, out of the file the close filed it under. The engine
 * names the record after the view's title (`release.live.ndjson`), so the row
 * says what ran rather than what the file is called.
 */
export function liveRunName(filename?: string): string {
  const name = filename ?? "";
  return name.replace(/\.live\.ndjson$/i, "") || "run";
}

/**
 * A SETTLED RUN, IN THE TRANSCRIPT WHERE IT HAPPENED — one row, and it opens.
 *
 * The pane a run paints while it works is a live surface with a stop in it, and
 * it is gone the moment the run ends. What is left is the RECORD, and it
 * belongs in the trace beside the block that produced it — not only in the
 * gallery, and not as an unnamed line in the recorded files ("1 file ·
 * release.live.ndjson") which is what a settled run used to read as here.
 *
 * IT IS A ROW, NOT A PANEL IN PLACE. A run that logged thousands of lines
 * embedded in the transcript would stand taller than the whole turn that made
 * it, so the transcript STATES what ran and reading it is one press away — the
 * same rule a document artifact obeys one row above.
 *
 * WHAT OPENS IS A PHOTOGRAPH. The panel is `isSettled`: the picture the run
 * ended on, its log still walkable a page at a time out of the record on the
 * gateway, no spinner, no live region, and NO `onInterrupt` — nothing about it
 * can be stopped or answered, because the run it reports on is over.
 *
 * Nothing is fetched until the row is opened: a step with a 40 MB record costs
 * a row of text until someone asks for it.
 */
export const LiveRunRow = memo(function LiveRunRow({
  client,
  sid,
  attachment,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
}) {
  const name = liveRunName(attachment.filename);
  const iterationId = attachment.iteration_id ?? "";
  const index = attachment.index ?? 0;
  // Keyed by the RECORD, not by this row: a turn settling re-mounts the row
  // under a different subtree, and an opened run must stay opened.
  const [opened, setOpened] = useStickyOverlay(`run:${iterationId}:${index}`);
  const [url, setUrl] = useState<string | null>(null);
  const [failed, setFailed] = useState(false);

  useEffect(() => {
    if (!opened || !iterationId || !sid) return;
    let alive = true;
    const release = client.retainAttachment(sid, iterationId, index);
    client
      .attachmentUrl(sid, iterationId, index)
      .then((next) => {
        if (alive) setUrl(next);
      })
      .catch(() => {
        if (alive) setFailed(true);
      });
    return () => {
      alive = false;
      release();
    };
  }, [client, sid, iterationId, index, opened]);

  const close = useCallback(() => setOpened(false), [setOpened]);
  const open = useCallback(() => setOpened(true), [setOpened]);
  const sizeLabel = attachmentBytes(attachment.size);

  if (!iterationId) return null;

  return (
    <>
      {/* The row IS the control, so the whole line is the target a finger gets:
          `ListRow` is the app's one pressable row and owns that box. */}
      <ListRow onClick={open} title={name} aria-label={`Open run ${name}`}>
        <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-accent-ink">
          RUN
        </span>
        <span className="min-w-0 flex-1 truncate font-mono text-chip text-muted">
          {name}
        </span>
        {sizeLabel ? (
          <span className="shrink-0 font-mono text-chip text-footer-muted">
            {sizeLabel}
          </span>
        ) : null}
        <ChevronIcon className="size-3 shrink-0 text-footer-muted opacity-70" />
      </ListRow>
      {/* The opened run is a SCREEN, not a part of the transcript — portalled
          out of the turn into the viewport-pinned shell, exactly as an opened
          document is, so the composer strip cannot paint over it. */}
      {opened &&
        createPortal(
          url ? (
            <LiveArtifact
              client={client}
              sid={sid}
              url={url}
              chrome={({ subtitle, body }) => (
                <OverlayScreen
                  title={name}
                  subtitle={subtitle}
                  onClose={close}
                >
                  {body}
                </OverlayScreen>
              )}
            />
          ) : (
            <OverlayScreen title={name} onClose={close}>
              <p className="p-4 font-mono text-meta text-dialog-hint">
                {failed ? "This run's record could not be read." : "Loading…"}
              </p>
            </OverlayScreen>
          ),
          overlayLayer().host,
        )}
    </>
  );
});
