import { useCallback, useEffect, useState } from 'react';

import type { GatewayClient } from '../lib/gateway';
import type { FileWindow } from '../lib/types';
import { ArrowOutIcon } from './icons';
import { BandButton, Banner, DialogFrame, Modal, Spinner } from './ui';

/**
 * THE FILE A PRESS NAMED, READ WHERE THE SESSION IS READ.
 *
 * A step names the files it touched by their path on the machine that ran it.
 * Opening the editor there is the right answer for whoever sits at that machine —
 * and no answer at all for a reader holding a phone, who watches nothing happen.
 * So the press shows the file itself: the gateway reads a bounded window of text
 * and the sheet stands at the line the press named, with an editor icon for the
 * machine that actually holds the tree.
 *
 * What the gateway refuses — a path outside this session's workspace, a file that
 * is gone, a binary file — is shown in its own words rather than as an empty box.
 */
export function FilePreview({
  client,
  sid,
  path,
  line,
  onClose,
}: {
  client: GatewayClient;
  sid: string;
  path: string;
  /** Where in the file to stand, 1-based. A patch header points at its first hunk. */
  line?: number;
  onClose: () => void;
}) {
  const [file, setFile] = useState<FileWindow | null>(null);
  const [failure, setFailure] = useState<string | null>(null);
  const [isOpening, setIsOpening] = useState(false);
  const name = path.slice(path.lastIndexOf('/') + 1) || path;

  useEffect(() => {
    const abort = new AbortController();
    let isLive = true;
    setFile(null);
    setFailure(null);
    void client
      .readPath(sid, path, line, abort.signal)
      .then((answer) => {
        if (isLive) setFile(answer);
      })
      .catch((cause: unknown) => {
        if (isLive) setFailure((cause as Error).message);
      });
    return () => {
      isLive = false;
      abort.abort();
    };
  }, [client, sid, path, line]);

  // The window is built around the anchor, so the anchor is what the sheet opens on.
  const standAt = useCallback((element: HTMLDivElement | null) => {
    element?.scrollIntoView?.({ block: 'center' });
  }, []);

  const openThere = () => {
    setIsOpening(true);
    setFailure(null);
    void client
      .openPath(sid, path)
      .catch((cause: unknown) => setFailure((cause as Error).message))
      .finally(() => setIsOpening(false));
  };

  return (
    <Modal within="session" onDismiss={onClose}>
      <DialogFrame
        title={name}
        subtitle={path}
        isTitleHidden
        closeLabel={`Close ${name}`}
        onClose={onClose}
        actions={
          <BandButton label="Open in editor" disabled={isOpening} onClick={openThere}>
            <ArrowOutIcon />
          </BandButton>
        }
      >
        <div className="min-h-0 flex-1 overflow-auto py-1 font-mono text-chip">
          {failure ? (
            <div className="p-3">
              <Banner kind="err">{failure}</Banner>
            </div>
          ) : !file ? (
            <p className="flex items-center gap-2 p-3 text-dialog-hint">
              <Spinner /> Reading {name}…
            </p>
          ) : file.lines.length === 0 ? (
            <p className="p-3 text-dialog-hint">This file is empty.</p>
          ) : (
            <>
              {file.lines.map((text, at) => {
                const number = file.first_line + at;
                const isAnchor = line !== undefined && number === file.line;
                return (
                  <div
                    key={number}
                    ref={isAnchor ? standAt : undefined}
                    data-line={number}
                    data-anchor={isAnchor ? 'true' : undefined}
                    className={`flex gap-3 px-3 ${isAnchor ? 'bg-accent/15 text-foreground' : ''}`}
                  >
                    <span className="shrink-0 select-none tabular-nums text-right text-dialog-hint">
                      {number}
                    </span>
                    <span className="min-w-0 whitespace-pre-wrap [overflow-wrap:anywhere]">
                      {text}
                    </span>
                  </div>
                );
              })}
              {file.is_truncated ? (
                <p className="px-3 py-2 text-dialog-hint">
                  The rest of this file is past what a preview reads. Open it in the editor to
                  read on.
                </p>
              ) : null}
            </>
          )}
        </div>
      </DialogFrame>
    </Modal>
  );
}
