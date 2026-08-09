import { memo, type ReactNode, useCallback, useEffect, useState } from "react";
import { createPortal } from "react-dom";

import type { GatewayClient } from "../lib/gateway";
import {
  attachmentBytes,
  docKindLabel,
  isMarkdownMedia,
  isPdfMedia,
  isTextMedia,
} from "../lib/artifacts";
import { MarkdownArtifact } from "./MarkdownArtifact";
import { PdfAnnotator } from "./PdfArtifact";
import { TextFrame } from "./TextArtifact";
import { ChevronIcon } from "./icons";
import { DialogHeader, ListRow } from "./ui";

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
  onClose,
}: {
  name: string;
  mime: string;
  sizeLabel?: string;
  url: string | null;
  failed: boolean;
  /** Present when the human may mark this artifact up. */
  annotate?: AnnotateContext;
  onClose: () => void;
}) {
  useEffect(() => {
    function onKey(event: KeyboardEvent) {
      if (event.key === "Escape") onClose();
    }
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [onClose]);

  return (
    <div className="fixed inset-0 z-50 flex h-[100dvh] min-h-0 min-w-0 flex-col overflow-hidden overscroll-contain bg-panel pt-[env(safe-area-inset-top)]">
      {/* The one header band, exactly as an artifact opened from the sheet or any
          other surface that opens over another wears it: the name is the title,
          what the file IS is the subtitle, and the way out is the header's own. */}
      <DialogHeader
        title={name}
        subtitle={[docKindLabel(mime), sizeLabel].filter(Boolean).join(" · ")}
        closeLabel={`Close ${name}`}
        onClose={onClose}
      />
      <div className="flex min-h-0 min-w-0 flex-1 flex-col">
        {/* Opened, the artifact is not only READ: a note can be commented on and a
            PDF drawn on, and either one saves as the next version of the same
            filename. */}
        {annotate && url && !failed && isTextMedia(mime, name) ? (
          <MarkdownArtifact
            client={annotate.client}
            sid={annotate.sid}
            iterationId={annotate.iterationId}
            name={name}
            mediaType={mime}
            url={url}
            plain={!isMarkdownMedia(mime, name)}
          />
        ) : annotate && url && !failed && isPdfMedia(mime) ? (
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
        )}
      </div>
    </div>
  );
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
  /** Asked for the bytes — the parent starts the fetch. */
  onNeeded: () => void;
}) {
  const [opened, setOpened] = useState(false);

  useEffect(() => {
    onNeeded();
  }, [onNeeded]);

  const close = useCallback(() => setOpened(false), []);
  const open = useCallback(() => setOpened(true), []);

  return (
    <>
      {/* The row IS the control, so the whole line is the target a finger gets:
          `ListRow` is the app's one pressable row and owns that box. */}
      <ListRow onClick={open} title={name} aria-label={`Open ${name}`}>
        <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-warn">
          {docKindLabel(mime, name)}
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
      {/* The opened document is a SCREEN, not a part of the transcript: it is
          portalled to the document body, so the composer strip the session screen
          pins to the bottom cannot paint on top of it. */}
      {opened &&
        createPortal(
          <DocOverlay
            name={name}
            mime={mime}
            sizeLabel={sizeLabel}
            url={url}
            failed={failed}
            annotate={annotate}
            onClose={close}
          />,
          document.body,
        )}
    </>
  );
});
