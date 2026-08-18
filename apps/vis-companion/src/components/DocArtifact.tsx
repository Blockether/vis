import {
  memo,
  type ReactNode,
  useCallback,
  useEffect,
  useState,
} from "react";
import { createPortal } from "react-dom";

import type { GatewayClient } from "../lib/gateway";
import {
  attachmentBytes,
  docKindLabel,
  isMarkdownMedia,
  isPdfMedia,
  isTextMedia,
} from "../lib/artifacts";
import { parseAnnotated } from "../lib/markdown-annotations";
import type { IterationAttachment } from "../lib/types";
import {
  type DocumentChrome,
  MarkdownArtifact,
} from "./MarkdownArtifact";
import { PdfAnnotator } from "./PdfArtifact";
import { readArtifactText, TextFrame } from "./TextArtifact";
import { ChevronIcon } from "./icons";
import { BandButton, DialogHeader, ListRow, overlayLayer } from "./ui";
import { useStickyOverlay } from "../lib/sticky-overlay";

/**
 * What an OPENED artifact needs in order to be marked up: which session and
 * iteration own it, and a client to save the revision through. Given this, a
 * note opened from the transcript is annotatable and a PDF can be drawn on —
 * both saving under the same filename, which is the next version.
 */
export type AnnotateContext = {
  client: GatewayClient;
  sid: string;
  iterationId: string;
};

// A PDF or an HTML page is a DOCUMENT, not a picture and not data: nothing in it
// is worth spending a model's context on, so `attach` clamps it to
// `audience: "user"` and emits a ````vis-doc` fence with five header lines
// (summary / host path / mime / name / size) and NO payload. The TUI opens the
// host file in the system viewer; this component is the app's half.
//
// THE DOCUMENT APPEARS ONCE. The fence and the attachment rail describe the same
// artifact, so painting a card for the fence AND a tile for the attachment put
// two boxes for one file in the turn. The attachment tile is the one that can
// open and annotate the bytes, so it is the only one: `ChatContent` renders
// nothing for the fence itself.

/**
 * An attached page is UNTRUSTED markup, so it is never mounted in the app's own
 * document: it renders in an iframe, which is a separate document with its own
 * CSS scope — a `body { background: red }` in the artifact cannot reach a single
 * pixel of the app around it.
 *
 * The `sandbox` attribute is what makes that isolation a SECURITY boundary
 * rather than a styling one, and the danger is a COMBINATION, not a flag:
 * a blob: URL inherits the app's origin, so `allow-same-origin` would hand the
 * artifact the app's storage, the gateway's bearer token and the ability to
 * strip its own sandbox. It is never granted, to any media type.
 *
 * Everything else is. A page is a DESIGN, and a design that cannot run its own
 * script is a picture of one: no CDN framework, no tab, no modal, no live data.
 * With the origin withheld the frame runs opaque — `localStorage` throws,
 * `document.cookie` is empty, `parent` is unreadable — so the script can style
 * and animate its own document and reach the network, and nothing else. Two
 * capabilities stay off because they act on the app AROUND the frame rather
 * than inside it: `allow-top-navigation`, which would yank the user off the
 * companion, and `allow-popups`, which would open windows behind it. A PDF
 * needs `allow-scripts` for the same reason it always did — Chromium's built-in
 * viewer refuses to paint without it.
 */
export function docSandbox(mime: string | undefined): string {
  return isPdfMedia(mime)
    ? "allow-scripts"
    : "allow-scripts allow-forms allow-modals allow-pointer-lock allow-downloads";
}

/**
 * The artifact itself, quarantined. `url` is an object URL for the attachment's
 * bytes, and an opened document always fills its box.
 */
export const DocFrame = memo(function DocFrame({
  url,
  mime,
  name,
}: {
  url: string;
  mime: string;
  name: string;
}) {
  return (
    <iframe
      title={name}
      src={url}
      sandbox={docSandbox(mime)}
      referrerPolicy="no-referrer"
      loading="lazy"
      className="min-h-0 w-full flex-1 border-0 bg-input"
    />
  );
});

/** The artifact's own bytes: a note read by the app, anything else in a frame. */
function DocBody({
  name,
  mime,
  url,
  failed,
}: {
  name: string;
  mime: string;
  url: string | null;
  failed: boolean;
}) {
  if (failed)
    return (
      <p className="px-2 py-3 text-meta text-footer-muted">
        This document could not be loaded from the gateway.
      </p>
    );
  if (!url)
    return <p className="px-2 py-3 text-meta text-footer-muted">Loading…</p>;
  // Markdown and plain text are read by the APP: an iframe would paint
  // `# Heading` as `# Heading`, the source instead of the document.
  return isTextMedia(mime, name) ? (
    <TextFrame url={url} mime={mime} name={name} fill />
  ) : (
    <DocFrame url={url} mime={mime} name={name} />
  );
}

/**
 * THE DOCUMENTS OF ONE STEP, in one stack.
 *
 * A card per file made every artifact its own framed box, so four files were four
 * frames with four edges and four kind chips down the transcript — the turn read
 * as a settings screen rather than as "this step wrote four documents". The stack
 * is the frame; a document is a ROW inside it, and the rows share one rule.
 *
 * The header is the group's own report — `4 documents · 31.4KB` — and it only
 * exists when there IS a group: one document is a single row with no header at
 * all, so the common turn pays nothing for the rarer one.
 */
export const DocStack = memo(function DocStack({
  summary,
  children,
}: {
  /** What the whole group is. Omitted for a lone document. */
  summary?: string;
  children: ReactNode;
}) {
  return (
    <div className="mt-2 min-w-0 border border-code-edge bg-input">
      {summary ? (
        <p className="border-b border-code-edge bg-panel px-3 py-1 font-mono text-chip text-footer-muted">
          {summary}
        </p>
      ) : null}
      <div className="divide-y divide-code-edge">{children}</div>
    </div>
  );
});

/**
 * `4 documents · 31.4KB` — what a stack is, said once above it.
 *
 * It counts ARTIFACTS, never cuts: the rail hands it one head per name
 * (`collapseAttachmentVersions`), so a note saved a second time is one document
 * of its newest weight instead of "2 documents" of both weights added together.
 *
 * The weight is claimed only when every document reported one, exactly as
 * `mediaSummary` claims a gallery's: a partial total is a wrong number, not a
 * smaller one.
 */
export function docStackSummary(docs: { size?: number }[]): string {
  const things = `${docs.length} ${docs.length === 1 ? "document" : "documents"}`;
  const total = docs.every((doc) => typeof doc.size === "number")
    ? attachmentBytes(docs.reduce((sum, doc) => sum + (doc.size ?? 0), 0))
    : "";
  return total ? `${things} · ${total}` : things;
}

/**
 * An opened document owns the WHOLE viewport — full height and full width —
 * with nothing above it but its own caption row and the app's one X.
 */
export const DocOverlay = memo(function DocOverlay({
  name,
  mime,
  sizeLabel,
  url,
  failed,
  annotate,
  versions,
  shownAt = 0,
  onPick,
  onClose,
}: {
  name: string;
  mime: string;
  sizeLabel?: string;
  url: string | null;
  failed: boolean;
  /** Present when the human may mark this artifact up. */
  annotate?: AnnotateContext;
  /** Every cut of this name, newest first and the head included. */
  versions?: IterationAttachment[];
  /** Which cut is being read, an index into `versions`. */
  shownAt?: number;
  /** Read another cut; the owner fetches its bytes. */
  onPick?: (at: number) => void;
  onClose: () => void;
}) {
  useEffect(() => {
    function onKey(event: KeyboardEvent) {
      if (event.key === "Escape") onClose();
    }
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [onClose]);

  // THE HISTORY OF A NAME BELONGS TO THE BAND, NOT TO THE TRANSCRIPT.
  //
  // A revised document is ONE row in the step (`collapseAttachmentVersions`), so
  // the thread behind it needs a door — and it is a cell of the band that already
  // names the document, one hairline from the way out and beside Save. `v2 of 2`
  // opens the cuts and closes them again, so the transcript keeps its single row
  // and the chevron that has always opened it. A file written once has no thread
  // and no cell.
  const [isHistory, setHistory] = useState(false);
  const cuts = versions ?? [];
  const shown = cuts[shownAt];
  const shownLabel = shown ? attachmentBytes(shown.size) : sizeLabel;
  const versionCell =
    cuts.length > 1 ? (
      <BandButton
        onClick={() => setHistory((open) => !open)}
        aria-expanded={isHistory}
        aria-label={`Versions of ${name}`}
        title={`Versions of ${name}`}
      >
        v{shown?.version ?? 1} of {cuts.length}
      </BandButton>
    ) : null;

  // THE OPENED DOCUMENT IS THE VIEWPORT-PINNED LAYER, NOT THE GLASS.
  //
  // Regression, user report: with the comment composer open the keyboard covered
  // it. This screen was `fixed` on `document.body` at `100dvh`, and neither the
  // native iOS keyboard (the webview is never resized) nor a mobile browser's
  // `dvh` subtracts a keyboard — so the annotator's field sat under it. It is the
  // same layer `Modal` is, and it mounts where `Modal` mounts: inside the shell
  // the keyboard driver pins, `absolute` in it and `h-full` of it.
  const { position } = overlayLayer();

  const chrome: DocumentChrome = ({ actions, note, body }) => (
    <div
      className={`${position} inset-0 z-50 flex h-full min-h-0 min-w-0 flex-col overflow-hidden overscroll-contain bg-panel pt-[env(safe-area-inset-top)]`}
    >
      {/* The one header band, exactly as an artifact opened from the sheet or any
          other surface that opens over another wears it: the name is the title,
          what the file IS is the subtitle, and the way out is the header's own.
          A document that can be SAVED puts that verb in this same run of cells,
          and the band reports what became of it under its name. */}
      <DialogHeader
        title={name}
        subtitle={
          note || [docKindLabel(mime), shownLabel].filter(Boolean).join(" · ")
        }
        actions={
          <>
            {versionCell}
            {actions}
          </>
        }
        closeLabel={`Close ${name}`}
        onClose={onClose}
      />
      <div className="flex min-h-0 min-w-0 flex-1 flex-col">{body}</div>
    </div>
  );

  // The thread itself, in the document's own screen: newest first, each row the
  // cut it opens. Nothing is fetched until one is picked — the same rule the
  // gallery's `ArtifactVersions` follows.
  if (isHistory)
    return chrome({
      actions: null,
      note: `${cuts.length} versions`,
      body: (
        <ul
          aria-label={`Versions of ${name}`}
          className="flex min-h-0 flex-1 flex-col gap-2 overflow-y-auto p-3 sm:p-4"
        >
          {cuts.map((cut, at) => (
            <li key={`${cut.iteration_id ?? "iter"}-${cut.index}`}>
              <ListRow
                isFramed
                onClick={() => {
                  onPick?.(at);
                  setHistory(false);
                }}
                aria-label={`Read v${cut.version ?? 1} of ${name}`}
                className="gap-3"
              >
                <span className="font-mono text-meta font-bold text-white">
                  v{cut.version ?? 1}
                </span>
                <span className="min-w-0 flex-1 truncate font-mono text-chip text-footer-muted">
                  {attachmentBytes(cut.size)}
                </span>
                {at === shownAt || at === 0 ? (
                  <span className="font-mono text-chip text-accent-ink">
                    {at === shownAt ? "reading" : "latest"}
                  </span>
                ) : null}
              </ListRow>
            </li>
          ))}
        </ul>
      ),
    });

  // Opened, the artifact is not only READ: a note can be commented on and a PDF
  // drawn on, and either one saves as the next version of the same filename.
  if (annotate && url && !failed && isTextMedia(mime, name)) {
    return (
      <MarkdownArtifact
        client={annotate.client}
        sid={annotate.sid}
        iterationId={annotate.iterationId}
        name={name}
        mediaType={mime}
        url={url}
        plain={!isMarkdownMedia(mime, name)}
        chrome={chrome}
      />
    );
  }

  return chrome({
    actions: null,
    note: "",
    body:
      annotate && url && !failed && isPdfMedia(mime) ? (
        <PdfAnnotator
          client={annotate.client}
          sid={annotate.sid}
          iterationId={annotate.iterationId}
          name={name}
          mediaType={mime}
          url={url}
          frame={<DocFrame url={url} mime={mime} name={name} />}
        />
      ) : (
        <DocBody name={name} mime={mime} url={url} failed={failed} />
      ),
  });
});

/**
 * ONE document in the transcript: a row of a {@link DocStack}, and the row IS the
 * verb — pressing anywhere on it throws the artifact over the whole screen.
 *
 * The screen's only verb used to be a chip at the far trailing edge, so nine
 * tenths of the card was dead paper a finger could land on for nothing. A row
 * that opens needs no `Open` beside it; the `›` says where the press goes.
 *
 * The bytes are NOT painted here. A note embedded in place stood taller than the
 * turn that produced it — the reader scrolled a whole document to reach the next
 * line of the conversation — and a PDF beside it did the same in a 60vh frame.
 * The transcript states what was produced; reading it is one tap away.
 */
export const DocPreview = memo(function DocPreview({
  name,
  mime,
  sizeLabel,
  url,
  failed,
  annotate,
  versions,
  shownAt = 0,
  onPick,
  onNeeded,
}: {
  name: string;
  mime: string;
  sizeLabel?: string;
  /** The artifact's object URL, or null while it is still being fetched. */
  url: string | null;
  failed: boolean;
  /** Present when the opened artifact may be marked up. */
  annotate?: AnnotateContext;
  /** Every cut of this name, newest first — one entry for a file written once. */
  versions?: IterationAttachment[];
  /** Which cut the opened document is reading, an index into `versions`. */
  shownAt?: number;
  /** Read another cut; the owner fetches its bytes. */
  onPick?: (at: number) => void;
  /** Asked for the bytes — the parent starts the fetch. */
  onNeeded: () => void;
}) {
  // Keyed by the document, not by this row: a turn settling re-mounts the row
  // under a different subtree, and an opened document must stay opened.
  const [opened, setOpened] = useStickyOverlay(`doc:${name}`);

  // WHAT THE NEW VERSION IS, IN THE ROW THE DOCUMENT ALREADY HAD.
  //
  // Reported: "I commented on it and saved, and now there are two". A revision
  // does not deserve a row — it deserves a LINE. `v2 · 3 comments` under the name
  // says the thing the second row was accidentally saying, and costs no height:
  // the row's box is its own minimum either way.
  //
  // The comments are not metadata beside the file, they are IN it, under one
  // `## Comments` heading (`lib/markdown-annotations`) — so the count is read
  // from the bytes this row already holds, and only for a note that was actually
  // revised and is showing its newest cut.
  const [comments, setComments] = useState(0);
  const latest = versions?.[0];
  const revision = latest && (latest.version ?? 1) > 1 ? (latest.version ?? 1) : 0;

  useEffect(() => {
    onNeeded();
  }, [onNeeded]);

  useEffect(() => {
    if (!revision || shownAt !== 0 || !url || !isTextMedia(mime, name)) return;
    let alive = true;
    readArtifactText(url)
      .then((text) => {
        if (alive) setComments(parseAnnotated(text).comments.length);
      })
      .catch(() => undefined);
    return () => {
      alive = false;
    };
  }, [revision, shownAt, url, mime, name]);

  const close = useCallback(() => setOpened(false), [setOpened]);
  const open = useCallback(() => setOpened(true), [setOpened]);

  const revisionLine = revision
    ? [
        `v${revision}`,
        comments > 0
          ? `${comments} ${comments === 1 ? "comment" : "comments"}`
          : "",
      ]
        .filter(Boolean)
        .join(" · ")
    : "";

  return (
    <>
      {/* The row IS the control, so the whole line is the target a finger gets:
          `ListRow` is the app's one pressable row and owns that box. */}
      <ListRow onClick={open} title={name} aria-label={`Open ${name}`}>
        <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-warn">
          {docKindLabel(mime, name)}
        </span>
        <span className="flex min-w-0 flex-1 flex-col">
          <span className="truncate font-mono text-chip text-muted">{name}</span>
          {revisionLine ? (
            <span className="truncate font-mono text-chip text-footer-muted">
              {revisionLine}
            </span>
          ) : null}
        </span>
        {sizeLabel ? (
          <span className="shrink-0 font-mono text-chip text-footer-muted">
            {sizeLabel}
          </span>
        ) : null}
        <ChevronIcon className="size-3 shrink-0 text-footer-muted opacity-70" />
      </ListRow>
      {/* The opened document is a SCREEN, not a part of the transcript: it is
          portalled out of the turn, so the composer strip the session screen pins
          to the bottom cannot paint on top of it — into the viewport-pinned shell
          (`overlayLayer`), so a keyboard raised over it does not bury the
          annotator's own field. */}
      {opened &&
        createPortal(
          <DocOverlay
            name={name}
            mime={mime}
            sizeLabel={sizeLabel}
            url={url}
            failed={failed}
            annotate={annotate}
            versions={versions}
            shownAt={shownAt}
            onPick={onPick}
            onClose={close}
          />,
          overlayLayer().host,
        )}
    </>
  );
});
