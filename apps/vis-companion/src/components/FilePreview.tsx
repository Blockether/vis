import { useCallback, useEffect, useState } from 'react';

import { GatewayError, type GatewayClient } from '../lib/gateway';
import type { FileWindow } from '../lib/types';
import { ArrowOutIcon } from './icons';
import { ImageViewer } from './ImageViewer';
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
 * A file that is not text (415) is asked for again as a picture, and a picture
 * opens in the image viewer (#432). Another binary file says that it has no
 * preview here. Other refusals — a path this session cannot read, a file that
 * is gone — are shown in the gateway's own words rather than as an empty box.
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
  const [picture, setPicture] = useState<string | null>(null);
  const [isWithoutPreview, setIsWithoutPreview] = useState(false);
  const [failure, setFailure] = useState<string | null>(null);
  const [isOpening, setIsOpening] = useState(false);
  const name = path.slice(path.lastIndexOf('/') + 1) || path;

  useEffect(() => {
    const abort = new AbortController();
    let isLive = true;
    let url: string | null = null;
    const isNotText = (cause: unknown) => cause instanceof GatewayError && cause.status === 415;
    const fail = (cause: unknown) => {
      if (isLive) setFailure((cause as Error).message);
    };
    setFile(null);
    setPicture(null);
    setIsWithoutPreview(false);
    setFailure(null);
    void client
      .readPath(sid, path, line, abort.signal)
      .then((answer) => {
        if (isLive) setFile(answer);
      })
      .catch((cause: unknown) => {
        if (!isNotText(cause)) return fail(cause);
        return client.readImagePath(sid, path, abort.signal).then(
          (blob) => {
            if (!isLive) return;
            url = URL.createObjectURL(blob);
            setPicture(url);
          },
          (refusal: unknown) => {
            if (!isNotText(refusal)) return fail(refusal);
            if (isLive) setIsWithoutPreview(true);
          },
        );
      });
    return () => {
      isLive = false;
      abort.abort();
      if (url) URL.revokeObjectURL(url);
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

  if (picture) return <ImageViewer src={picture} name={name} onClose={onClose} />;

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
          ) : isWithoutPreview ? (
            <p className="p-3 font-sans text-dialog-hint">
              Vis has no preview for this kind of file. Open it in the editor on the machine
              that ran the step.
            </p>
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
