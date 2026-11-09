import { useRef, useState } from 'react';

import type { GatewayClient } from '../lib/gateway';
import { isCommandRequest } from '../lib/gateway';
import { useCommands } from '../lib/commands';
import type { QueuedTurn, QueuePausedInfo } from '../lib/types';
import { PencilIcon } from './icons';
import { Button, CloseButton, IconButton } from './ui';

type QueuedTurnsTrayProps = {
  client: GatewayClient;
  sid: string;
  queued: readonly QueuedTurn[];
  paused: QueuePausedInfo | null;
  /**
   * A turn is running, so a queued message has a next step to go into. Without one
   * the Send now buttons stay hidden: an unmarked row waits for the turn end as before.
   */
  running: boolean;
  /** Take the row out of the queue and put it back in the composer, images included. */
  onEdit: (item: QueuedTurn) => Promise<void>;
  onError: (message: string) => void;
};

/** Send now applies to plain messages only; a slash or bang command runs as its own turn. */
function canSendNow(item: QueuedTurn): boolean {
  return !isCommandRequest(item.request);
}

/**
 * Gateway-owned queued turns, including edit in the composer, remove, Send now (send into
 * the running turn at its next step) and paused-queue recovery.
 */
export function QueuedTurnsTray({
  client,
  sid,
  queued,
  paused,
  running,
  onEdit,
  onError,
}: QueuedTurnsTrayProps) {
  // The gateway is the one writer of the queue. A mutation marks the row busy,
  // but never hides or rewrites it before the daemon confirms its own truth.
  const [busyIds, setBusyIds] = useState<ReadonlySet<string>>(() => new Set());
  const [resuming, setResuming] = useState(false);
  const [sendingAll, setSendingAll] = useState(false);
  const markable = running ? queued.filter(canSendNow) : [];
  const unmarked = markable.filter((item) => item.deliver !== 'next_iteration');
  const list = useRef<HTMLDivElement>(null);
  const sendAllNow = () => {
    setSendingAll(true);
    void client
      .sendQueueNow(sid)
      .catch(report)
      .finally(() => setSendingAll(false));
  };
  // C-x q s and C-x q e run the same actions as the tray.
  useCommands({
    'queue-send-all-now': !sendingAll && unmarked.length > 0 && sendAllNow,
    'queue-edit': queued.length > 0 && (() => list.current?.focus()),
  });

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
        <div className="mb-1.5 flex flex-wrap items-center gap-x-2 gap-y-1 rounded-none border border-warn-strong bg-warn-surface px-2.5 py-1.5 font-mono text-meta text-warn">
          <span className="size-1.5 shrink-0 bg-warn-strong" aria-hidden="true" />
          <span className="font-bold text-warn">Queue paused</span>
          <span className="min-w-0 flex-1 truncate">
            {paused.held} held · {paused.reason.replace(/_/g, ' ')}
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
            {resuming ? 'Continuing…' : 'Continue queue'}
          </Button>
        </div>
      )}

      {/* The queue shares the composer's flat square frame above the dock. */}
      {queued.length > 0 && (
        <div className="mb-1.5 overflow-clip rounded-none border border-dialog-edge bg-panel">
          <div className="flex items-center gap-1.5 border-b border-dialog-edge bg-dialog-title px-2.5 py-1 font-mono text-meta font-bold text-dialog-title-foreground">
            QUEUED · {queued.length}
            {/* Primary brings its own ink pair; the page ink vanishes on the dark band. */}
            {/* User decision: one markable row has its own button, so this one starts at two. */}
            {markable.length > 1 && (
              <Button
                variant="primary"
                density="inline"
                aria-label="Send all queued messages now"
                title="Deliver every queued message into the running turn at its next step"
                disabled={sendingAll || unmarked.length === 0}
                className="ml-auto shrink-0"
                onClick={sendAllNow}
              >
                Send all now
              </Button>
            )}
          </div>
          <div
            ref={list}
            role="region"
            aria-label="Queued messages"
            tabIndex={0}
            className="max-h-64 overflow-y-auto overscroll-contain focus-visible:outline-2 focus-visible:outline-accent focus-visible:outline-offset-[-2px]"
          >
            <div role="list">
              {queued.map((item, index) => {
                const isBusy = busyIds.has(item.turnId);
                const isMarked = item.deliver === 'next_iteration';
                const sendNow = running && canSendNow(item);
                return (
                  <div
                    key={item.turnId}
                    role="listitem"
                    className={`flex items-center gap-2 border-t border-dialog-edge px-2.5 py-0.5 first:border-t-0 transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none${isBusy ? ' opacity-50' : ''}`}
                  >
                    <span className="shrink-0 font-mono text-ui font-bold text-accent-ink">
                      #{index + 1}
                    </span>
                    <span className="min-w-0 flex-1 truncate px-1 font-mono text-ui text-dialog-foreground">
                      {item.preview ||
                        item.attachments.map((attachment) => attachment.filename).join(', ') ||
                        '(empty)'}
                    </span>
                    {/* User decision: edit in the composer, where a picture can join the text. */}
                    <IconButton
                      label={`Edit queued message ${index + 1}`}
                      title="Edit in the composer"
                      variant="quiet"
                      disabled={isBusy}
                      onClick={() => {
                        markBusy(item.turnId, true);
                        void onEdit(item)
                          .catch(report)
                          .finally(() => markBusy(item.turnId, false));
                      }}
                    >
                      <PencilIcon className="size-3.5" />
                    </IconButton>
                    {isMarked && (
                      <span className="shrink-0 font-mono text-meta text-accent-ink">next iter</span>
                    )}
                    {/* A command row has no Send now button; × stays at the row end. */}
                    {sendNow && (
                      <Button
                        aria-label={
                          isMarked
                            ? `Unsend queued message ${index + 1}`
                            : `Send queued message ${index + 1} now`
                        }
                        title={isMarked ? 'Keep for the turn end' : 'Send at the next step'}
                        // A marked row shows the accent fill and the label of the undo.
                        variant={isMarked ? 'primary' : 'secondary'}
                        density="inline"
                        className="shrink-0"
                        disabled={isBusy}
                        onClick={() => {
                          const deliver = isMarked ? 'turn_end' : 'next_iteration';
                          markBusy(item.turnId, true);
                          void client
                            .markQueuedTurn(sid, item.turnId, deliver)
                            .catch(report)
                            .finally(() => markBusy(item.turnId, false));
                        }}
                      >
                        {isMarked ? 'Unsend' : 'Send it now'}
                      </Button>
                    )}
                    <CloseButton
                      label={`Remove queued message ${index + 1}`}
                      isStandalone
                      className="-mr-2"
                      disabled={isBusy}
                      onClick={() => {
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
