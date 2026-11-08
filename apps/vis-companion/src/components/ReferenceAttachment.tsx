import { useEffect, useRef, useState } from 'react';

import type { GatewayClient } from '../lib/gateway';
import { artifactMedia, attachmentBytes, referenceShape } from '../lib/artifacts';
import { mediaStreamSrc } from '../lib/media-stream';
import { useOpenPath } from '../lib/open-path';
import type { FileMedia, IterationAttachment } from '../lib/types';
import { MediaPreview } from './FilePreview';
import { ArrowOutIcon } from './icons';
import { TextButton } from './ui';

/** What the gateway said about the file of a reference, read once for its stream address. */
type ReferenceRead =
  | { state: 'reading' }
  | { state: 'media'; file: FileMedia }
  | { state: 'card'; note?: string };

/**
 * A FILE THAT THE SESSION DATABASE DOES NOT KEEP (#450).
 *
 * `attach` keeps a reference, not the bytes, for a file that is larger than the
 * attachment limit. The bytes are only on the disk of the gateway machine, so this row
 * never downloads them whole: the attachment route would put a 150 MB episode into one
 * Blob and into the attachment cache of the device.
 *
 * A recording or a clip plays from the stream of the file preview (#449). The element
 * reads the file with Range requests and can seek, and the signed ticket in the stream
 * address is its authorization, because a media element cannot send the gateway token.
 * Any other file is a card with its name, size and path. A press opens the file preview,
 * which shows what the gateway can show of it and opens the editor on its machine.
 */
export function ReferenceAttachment({
  client,
  sid,
  attachment,
  onOpenPath,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
  /** What a press on the card does. The default is the file preview of the screen. */
  onOpenPath?: ((path: string) => void) | null;
}) {
  return referenceShape(attachment) === 'stream' ? (
    <ReferenceStream client={client} sid={sid} attachment={attachment} onOpenPath={onOpenPath} />
  ) : (
    <ReferenceCard attachment={attachment} onOpenPath={onOpenPath} />
  );
}

/** A recording or a clip of a reference, streamed from the file preview of the gateway. */
function ReferenceStream({
  client,
  sid,
  attachment,
  onOpenPath,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
  onOpenPath?: ((path: string) => void) | null;
}) {
  const path = attachment.path ?? '';
  const name = attachment.filename || path.slice(path.lastIndexOf('/') + 1) || 'attachment';
  const [read, setRead] = useState<ReferenceRead>({ state: 'reading' });
  // The stream address was read again once for a new ticket; a second error is final.
  const isRenewed = useRef(false);

  useEffect(() => {
    const abort = new AbortController();
    let isLive = true;
    setRead({ state: 'reading' });
    isRenewed.current = false;
    void client
      .readPath(sid, path, undefined, abort.signal)
      .then((answer) => {
        if (!isLive) return;
        // The gateway reads the first bytes of the file. When they are not a recording
        // or a clip that a media element plays, the card is the honest answer.
        setRead(answer.kind === 'media' ? { state: 'media', file: answer } : { state: 'card' });
      })
      .catch((cause: unknown) => {
        if (isLive) setRead({ state: 'card', note: (cause as Error).message });
      });
    return () => {
      isLive = false;
      abort.abort();
    };
  }, [client, sid, path]);

  // The ticket in the stream address has an expiry. A player that seeks after it gets an
  // error, so the row reads the preview again once for a new address, as the file
  // preview does. Another error is a real failure.
  const failed = (file: FileMedia) => {
    if (isRenewed.current || Date.now() < file.expires_at) {
      setRead({ state: 'card', note: `Vis cannot play ${name}.` });
      return;
    }
    isRenewed.current = true;
    void client
      .readPath(sid, path)
      .then((answer) =>
        setRead(answer.kind === 'media' ? { state: 'media', file: answer } : { state: 'card' }),
      )
      .catch((cause: unknown) => setRead({ state: 'card', note: (cause as Error).message }));
  };

  if (read.state === 'media') {
    const file = read.file;
    return (
      <div className="mt-2 min-w-0" data-attachment-reference>
        <MediaPreview
          key={file.content_url}
          file={file}
          name={name}
          src={mediaStreamSrc(client.gatewayUrl(file.content_url))}
          onError={() => failed(file)}
          transcription={attachment.transcription}
          transcriptionSegments={attachment.transcription_segments}
          transcriptionStatus={attachment.transcription_status}
        />
      </div>
    );
  }
  return (
    <ReferenceCard
      attachment={attachment}
      onOpenPath={onOpenPath}
      note={read.state === 'reading' ? 'Reading…' : read.note}
    />
  );
}

/**
 * The card of a reference: its name, what it is, its size and where it is. The path is
 * the one thing that tells two files of one name apart, and it says that the file is on
 * the gateway machine and not in the session.
 */
export function ReferenceCard({
  attachment,
  onOpenPath,
  note,
}: {
  attachment: IterationAttachment;
  onOpenPath?: ((path: string) => void) | null;
  /** A line under the card, such as why a recording does not play. */
  note?: string;
}) {
  const screenOpen = useOpenPath();
  const open = onOpenPath === undefined ? screenOpen : onOpenPath;
  const path = attachment.path ?? '';
  const name = attachment.filename || path.slice(path.lastIndexOf('/') + 1) || 'attachment';
  const meta = [artifactMedia(attachment), attachmentBytes(attachment.size), 'not in the session']
    .filter(Boolean)
    .join(' · ');
  return (
    <div
      className="mt-2 min-w-0 border border-code-edge bg-input px-3 py-2 font-mono text-chip text-footer-muted"
      data-attachment-reference
    >
      <div className="flex min-w-0 items-center gap-1.5">
        <ArrowOutIcon className="size-3 shrink-0 opacity-70" />
        {open && path ? (
          <TextButton isToken aria-label={`Open ${name}`} className="flex-1" onClick={() => open(path)}>
            {name}
          </TextButton>
        ) : (
          <span className="min-w-0 flex-1 truncate text-dialog-foreground">{name}</span>
        )}
      </div>
      <p className="mt-0.5 min-w-0 truncate">{meta}</p>
      <p className="min-w-0 truncate" title={path}>
        {path}
      </p>
      {note ? <p className="mt-0.5 min-w-0 truncate">{note}</p> : null}
    </div>
  );
}
