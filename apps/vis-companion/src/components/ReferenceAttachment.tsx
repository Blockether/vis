import { useEffect, useMemo, useRef, useState } from 'react';
import { createPortal } from 'react-dom';

import type { GatewayClient } from '../lib/gateway';
import { artifactMedia, attachmentBytes, referenceShape } from '../lib/artifacts';
import { mediaStreamSrc } from '../lib/media-stream';
import { useOpenPath } from '../lib/open-path';
import {
  handOffPlan,
  handOffReference,
  handOffVerb,
  ReferenceLink,
  referencePlan,
  referenceRange,
} from '../lib/reference-open';
import type { FileMedia, IterationAttachment } from '../lib/types';
import { OverlayScreen } from './DocArtifact';
import { MediaPreview } from './FilePreview';
import { PdfReader } from './PdfArtifact';
import { ArrowOutIcon } from './icons';
import { BandButton, overlayLayer, TextButton } from './ui';

/** What the gateway said about the file of a reference, read once for its stream address. */
type ReferenceRead =
  | { state: 'reading' }
  | { state: 'media'; file: FileMedia }
  | { state: 'card'; note?: string };

/**
 * A FILE THAT THE SESSION DATABASE DOES NOT KEEP (#450, #451).
 *
 * `attach` keeps a reference, not the bytes, for a file that is larger than the
 * attachment limit. The bytes are only on the disk of the gateway machine, so this row
 * never downloads them whole: the attachment route would put a 150 MB episode into one
 * Blob and into the attachment cache of the device.
 *
 * A recording or a clip plays from the stream of the file preview (#449). The element
 * reads the file with Range requests and can seek, and the signed ticket in the stream
 * address is its authorization, because a media element cannot send the gateway token.
 * Any other file is a card with its name, size and path. A press on the name opens the
 * file (`lib/reference-open`): a PDF in the reader of the app, another file in its app on
 * the desktop, in the browser of a phone, or as a download. A press on the path opens the
 * file preview, which opens the editor on the gateway machine.
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
  /** What a press on the path does. The default is the file preview of the screen. */
  onOpenPath?: ((path: string) => void) | null;
}) {
  return referenceShape(attachment) === 'stream' ? (
    <ReferenceStream client={client} sid={sid} attachment={attachment} onOpenPath={onOpenPath} />
  ) : (
    <ReferenceFile client={client} sid={sid} attachment={attachment} onOpenPath={onOpenPath} />
  );
}

/** The display name of a reference: its file name, else the last part of its path. */
function referenceName(attachment: IterationAttachment): string {
  const path = attachment.path ?? '';
  return attachment.filename || path.slice(path.lastIndexOf('/') + 1) || 'attachment';
}

/**
 * A document, a workbook, an archive or another file of a reference. Its press opens the
 * file from the signed address of the gateway and never makes one Blob of it.
 */
function ReferenceFile({
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
  const name = referenceName(attachment);
  const plan = referencePlan(attachment);
  const iterationId = attachment.iteration_id;
  const link = useMemo(
    () => (iterationId ? new ReferenceLink(client, sid, iterationId, attachment.index) : null),
    [client, sid, iterationId, attachment.index],
  );
  const [note, setNote] = useState('');
  const [isReading, setReading] = useState(false);

  // A phone opens another window only inside the press, so the address is asked for
  // before the press. A press after the ticket expired asks again and opens late, which
  // the system browser of Android accepts.
  useEffect(() => {
    if (plan === 'system') link?.url().catch(() => undefined);
  }, [link, plan]);

  const handOff = (to: 'open' | 'reveal' | 'download' | 'system') => {
    if (!link) return;
    setNote('');
    const report = (done: Promise<string>) =>
      done.then(setNote, (cause: unknown) =>
        setNote(`Could not open ${name}: ${(cause as Error).message}`),
      );
    const held = link.peek();
    // Called at once with a held address, so that a phone opens it inside the press.
    if (held) void report(handOffReference(to, held, name));
    else void report(link.url().then((url) => handOffReference(to, url, name)));
  };
  const open = () => {
    if (plan === 'read') {
      setNote('');
      setReading(true);
    } else if (plan !== 'stream') handOff(plan);
  };

  return (
    <>
      <ReferenceCard
        attachment={attachment}
        onOpen={link ? open : undefined}
        onOpenPath={onOpenPath}
        note={note}
      />
      {isReading && link
        ? createPortal(
            <ReferencePdf
              name={name}
              link={link}
              size={attachment.size ?? 0}
              keepVerb={handOffVerb(handOffPlan(attachment))}
              onKeep={() => handOff(handOffPlan(attachment))}
              onClose={() => setReading(false)}
            />,
            overlayLayer().host,
          )
        : null}
    </>
  );
}

/**
 * A referenced PDF in the reader of the app. The reader reads only the parts that a page
 * needs from the signed address, so a PDF of hundreds of megabytes opens at once and
 * never stays in memory whole.
 */
function ReferencePdf({
  name,
  link,
  size,
  keepVerb,
  onKeep,
  onClose,
}: {
  name: string;
  link: ReferenceLink;
  size: number;
  /** The verb that gives the whole file to the platform. */
  keepVerb: string;
  onKeep: () => void;
  onClose: () => void;
}) {
  const range = useMemo(() => referenceRange(link, size), [link, size]);
  return (
    <PdfReader
      name={name}
      range={range}
      chrome={({ actions, note, body }) => (
        <OverlayScreen
          title={name}
          subtitle={note || ['PDF', attachmentBytes(size), 'not in the session'].join(', ')}
          actions={
            <>
              {actions}
              <BandButton onClick={onKeep}>{keepVerb}</BandButton>
            </>
          }
          onClose={onClose}
        >
          {body}
        </OverlayScreen>
      )}
    />
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
 *
 * A press on the name opens the file (`onOpen`). Without `onOpen` it opens the file
 * preview, as the path does.
 */
export function ReferenceCard({
  attachment,
  onOpen,
  onOpenPath,
  note,
}: {
  attachment: IterationAttachment;
  /** What a press on the name does. */
  onOpen?: () => void;
  onOpenPath?: ((path: string) => void) | null;
  /** A line under the card, such as why a recording does not play. */
  note?: string;
}) {
  const screenOpen = useOpenPath();
  const openPath = onOpenPath === undefined ? screenOpen : onOpenPath;
  const path = attachment.path ?? '';
  const name = referenceName(attachment);
  const meta = [artifactMedia(attachment), attachmentBytes(attachment.size), 'not in the session']
    .filter(Boolean)
    .join(' · ');
  const openName = onOpen ?? (openPath && path ? () => openPath(path) : undefined);
  return (
    <div
      className="mt-2 min-w-0 border border-code-edge bg-input px-3 py-2 font-mono text-chip text-footer-muted"
      data-attachment-reference
    >
      <div className="flex min-w-0 items-center gap-1.5">
        <ArrowOutIcon className="size-3 shrink-0 opacity-70" />
        {openName ? (
          <TextButton isToken aria-label={`Open ${name}`} className="flex-1" onClick={openName}>
            {name}
          </TextButton>
        ) : (
          <span className="min-w-0 flex-1 truncate text-dialog-foreground">{name}</span>
        )}
      </div>
      <p className="mt-0.5 min-w-0 truncate">{meta}</p>
      {onOpen && openPath && path ? (
        <TextButton
          aria-label={`Preview ${path}`}
          title={path}
          className="block min-w-0 max-w-full truncate !px-0 text-left !text-chip !text-footer-muted"
          onClick={() => openPath(path)}
        >
          {path}
        </TextButton>
      ) : (
        <p className="min-w-0 truncate" title={path}>
          {path}
        </p>
      )}
      {note ? (
        <p className="mt-0.5 min-w-0 truncate" role="status">
          {note}
        </p>
      ) : null}
    </div>
  );
}
