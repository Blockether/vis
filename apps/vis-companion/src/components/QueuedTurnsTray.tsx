import { useState } from "react";

import type { GatewayClient } from "../lib/gateway";
import type { QueuedTurn, QueuePausedInfo } from "../lib/types";
import { Button, CloseButton, TextButton } from "./ui";

type QueuedTurnsTrayProps = {
  client: GatewayClient;
  sid: string;
  queued: readonly QueuedTurn[];
  paused: QueuePausedInfo | null;
  onError: (message: string) => void;
};

/** Gateway-owned queued turns, including edit, remove, and paused-queue recovery. */
export function QueuedTurnsTray({
  client,
  sid,
  queued,
  paused,
  onError,
}: QueuedTurnsTrayProps) {
  // The gateway is the one writer of the queue. A mutation marks the row busy,
  // but never hides or rewrites it before the daemon confirms its own truth.
  const [busyIds, setBusyIds] = useState<ReadonlySet<string>>(() => new Set());
  const [editing, setEditing] = useState<{
    turnId: string;
    text: string;
  } | null>(null);
  const [resuming, setResuming] = useState(false);

  const markBusy = (turnId: string, busy: boolean) => {
    setBusyIds((current) => {
      const next = new Set(current);
      if (busy) next.add(turnId);
      else next.delete(turnId);
      return next;
    });
  };

  const report = (cause: unknown) => onError((cause as Error).message);

  return (
    <>
      {paused && (
        <div className="mb-1.5 flex flex-wrap items-center gap-x-2 gap-y-1 rounded-field border border-warn-strong bg-warn-surface shadow-[3px_3px_0_var(--dialog-shadow)] px-2.5 py-1.5 font-mono text-meta text-warn">
          <span
            className="size-1.5 shrink-0 bg-warn-strong"
            aria-hidden="true"
          />
          <span className="font-bold text-warn">Queue paused</span>
          <span className="min-w-0 flex-1 truncate">
            {paused.held} held · {paused.reason.replace(/_/g, " ")}
          </span>
          <Button
            variant="secondary"
            density="compact"
            disabled={resuming}
            className="shrink-0"
            onClick={() => {
              setResuming(true);
              void client
                .resumeQueue(sid)
                .catch(report)
                .finally(() => setResuming(false));
            }}
          >
            {resuming ? "Continuing…" : "Continue queue"}
          </Button>
        </div>
      )}

      {/* This is paper arriving over the dock, not a full-width page band. It
          shares the composer's field radius and shadow so both read as one object. */}
      {queued.length > 0 && (
        <div className="mb-1.5 overflow-clip rounded-field border border-dialog-edge bg-panel shadow-[3px_3px_0_var(--dialog-shadow)]">
          <div className="flex items-center gap-1.5 border-b border-dialog-edge bg-dialog-title px-2.5 py-1 font-mono text-meta font-bold text-dialog-title-foreground">
            <span aria-hidden="true">┌</span>
            Queued · {queued.length}
          </div>
          <div
            role="region"
            aria-label="Queued messages"
            tabIndex={0}
            className="max-h-64 overflow-y-auto overscroll-contain focus-visible:outline-2 focus-visible:outline-accent focus-visible:outline-offset-[-2px]"
          >
            <div role="list">
              {queued.map((item, index) => {
            const isEditing = editing?.turnId === item.turnId;
            const isBusy = busyIds.has(item.turnId);
            return (
                <div
                  key={item.turnId}
                  role="listitem"
                  className={`flex items-center gap-2 border-t border-dialog-edge px-2.5 py-1 first:border-t-0 transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none${isBusy ? " opacity-50" : ""}`}
                >
                <span className="shrink-0 font-mono text-meta font-bold text-accent-ink">
                  #{index + 1}
                </span>
                {isEditing ? (
                  <input
                    // eslint-disable-next-line jsx-a11y/no-autofocus
                    autoFocus
                    value={editing.text}
                    onChange={(event) =>
                      setEditing({
                        turnId: item.turnId,
                        text: event.target.value,
                      })
                    }
                    onKeyDown={(event) => {
                      if (event.key === "Enter") {
                        event.preventDefault();
                        const text = editing.text.trim();
                        if (text && text !== item.request) {
                          markBusy(item.turnId, true);
                          void client
                            .updateQueuedTurn(sid, item.turnId, text)
                            .catch(report)
                            .finally(() => markBusy(item.turnId, false));
                        }
                        setEditing(null);
                      } else if (event.key === "Escape") {
                        event.preventDefault();
                        setEditing(null);
                      }
                    }}
                    onBlur={() => setEditing(null)}
                    className="min-w-0 flex-1 border border-accent bg-input px-1 py-0.5 font-mono text-ui text-dialog-foreground outline-none"
                    aria-label={`Edit queued message ${index + 1}`}
                  />
                ) : (
                  <TextButton
                    disabled={isBusy}
                    onClick={() =>
                      setEditing({ turnId: item.turnId, text: item.request })
                    }
                    className="flex flex-1 items-center gap-1"
                    title="Tap to edit"
                  >
                    {item.attachments.map((attachment) => (
                      <span
                        key={attachment.filename}
                        className="inline-flex shrink-0 items-center gap-1 border border-dialog-edge bg-input px-1 text-chip text-dialog-hint"
                        title={`${attachment.filename}${attachment.sizeLabel ? ` · ${attachment.sizeLabel}` : ""}`}
                      >
                        <span className="max-w-[7rem] truncate">
                          {attachment.filename}
                        </span>
                      </span>
                    ))}
                    <span className="min-w-0 flex-1 truncate">
                      {item.preview ||
                        (item.attachments.length ? "" : "(empty)")}
                    </span>
                  </TextButton>
                )}
                <CloseButton
                  label={`Remove queued message ${index + 1}`}
                  isStandalone
                  className="-me-2.5"
                  disabled={isBusy}
                  onClick={() => {
                    setEditing((current) =>
                      current?.turnId === item.turnId ? null : current,
                    );
                    markBusy(item.turnId, true);
                    void client
                      .deleteQueuedTurn(sid, item.turnId)
                      .catch(report)
                      .finally(() => markBusy(item.turnId, false));
                  }}
                />
              </div>
            );
              })}
            </div>
          </div>
        </div>
      )}
    </>
  );
}
