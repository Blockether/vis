import { useCallback, useEffect, useRef, useState } from 'react';

import type { GatewayClient } from '../lib/gateway';
import { mediaStreamSrc } from '../lib/media-stream';
import type { FileMedia, TranscriptionSegment, WorkspaceFile } from '../lib/types';
import { ArrowOutIcon } from './icons';
import { ImageViewer } from './ImageViewer';
import { MediaRecording, mediaMeta, RecordingPlayer } from './Media';
import { BandButton, Banner, DialogFrame, Modal, Spinner } from './ui';

/**
 * The bytes of a base64 string. A picture can be several MiB, so this fills one
 * array in a plain loop, without a callback for each byte.
 */
function decodeBase64(data: string): Uint8Array<ArrayBuffer> {
  const text = atob(data);
  const bytes = new Uint8Array(text.length);
  for (let at = 0; at < text.length; at += 1) bytes[at] = text.charCodeAt(at);
  return bytes;
}

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
 * One read answers every file, and its `kind` says what came back: a picture
 * opens in the image viewer (#432), a recording or a video plays from a stream
 * of the gateway (#449), and another binary file says that it has no preview
 * here. Other refusals — a path this session cannot read, a file that
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
  const [file, setFile] = useState<WorkspaceFile | null>(null);
  const [picture, setPicture] = useState<string | null>(null);
  const [failure, setFailure] = useState<string | null>(null);
  const [isOpening, setIsOpening] = useState(false);
  const name = path.slice(path.lastIndexOf('/') + 1) || path;
  /** The media preview was read again once for a new ticket; a second error is final. */
  const isRenewed = useRef(false);

  useEffect(() => {
    const abort = new AbortController();
    let isLive = true;
    let url: string | null = null;
    setFile(null);
    setPicture(null);
    setFailure(null);
    isRenewed.current = false;
    void client
      .readPath(sid, path, line, abort.signal)
      .then((answer) => {
        if (!isLive) return;
        if (answer.kind === 'image') {
          // `<img src>` cannot carry the bearer header, so the bytes come in the answer.
          url = URL.createObjectURL(
            new Blob([decodeBase64(answer.data)], { type: answer.media_type }),
          );
          setPicture(url);
        }
        setFile(answer);
      })
      .catch((cause: unknown) => {
        if (isLive) setFailure((cause as Error).message);
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

  // The ticket in the stream address has an expiry. A player that seeks after it gets
  // an error, so the sheet reads the preview again once for a new address. Another
  // error is a real failure.
  const mediaFailed = (media: FileMedia) => {
    if (isRenewed.current || Date.now() < media.expires_at) {
      setFailure(`Vis cannot play ${name}.`);
      return;
    }
    isRenewed.current = true;
    void client
      .readPath(sid, path, line)
      .then(setFile)
      .catch((cause: unknown) => setFailure((cause as Error).message));
  };

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
          ) : file === null ? (
            <p className="flex items-center gap-2 p-3 text-dialog-hint">
              <Spinner /> Reading {name}…
            </p>
          ) : file.kind === 'media' ? (
            <div className="p-3 font-sans">
              <MediaPreview
                key={file.content_url}
                file={file}
                name={name}
                src={mediaStreamSrc(client.gatewayUrl(file.content_url))}
                onError={() => mediaFailed(file)}
              />
            </div>
          ) : file.kind !== 'text' ? (
            // A binary file, or a kind that a newer gateway sends and this Companion
            // does not know. The answer has come, so the sheet must not keep reading.
            <p className="p-3 font-sans text-dialog-hint">
              Vis has no preview for this kind of file. Open it in the editor on the machine
              that ran the step.
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

/**
 * A recording in the player of the app, or a video in the player of the platform. The
 * element loads `src` by itself, so it streams the file with range requests and can seek
 * without the whole file. A long recording never becomes one Blob in memory (#449). An
 * attachment that the session database does not keep plays here too (#450).
 */
export function MediaPreview({
  file,
  name,
  src,
  onError,
  transcription,
  transcriptionSegments,
  transcriptionStatus,
}: {
  file: FileMedia;
  name: string;
  src: string;
  onError: () => void;
  /** The words of a recording, when the gateway read them; see `MediaRecording`. */
  transcription?: string;
  transcriptionSegments?: TranscriptionSegment[];
  transcriptionStatus?: string;
}) {
  const meta = mediaMeta({ filename: name, media_type: file.media_type, size: file.size_bytes });
  if (file.media_type.startsWith('video/'))
    return (
      <figure className="min-w-0">
        <video
          src={src}
          controls
          playsInline
          preload="metadata"
          aria-label={name}
          className="max-h-[70vh] w-full bg-code"
          onError={onError}
        />
        <figcaption className="mt-1 font-mono text-chip uppercase tracking-wider text-footer-muted">
          {meta}
        </figcaption>
      </figure>
    );
  return (
    <MediaRecording
      name={name}
      meta={meta}
      transcription={transcription}
      transcriptionSegments={transcriptionSegments}
      transcriptionStatus={transcriptionStatus}
    >
      <RecordingPlayer src={src} onError={onError} />
    </MediaRecording>
  );
}
