import { memo, useCallback, useEffect, useState } from "react";
import { createPortal } from "react-dom";

import type { GatewayClient } from "../lib/gateway";
import {
  docKindLabel,
  isMarkdownMedia,
  isPdfMedia,
  isTextMedia,
} from "../lib/artifacts";
import { MarkdownArtifact } from "./MarkdownArtifact";
import { PdfAnnotator } from "./PdfArtifact";
import { TextFrame } from "./TextArtifact";
import { Button, DialogHeader } from "./ui";

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
// is worth spending a model's context on, so `vis_attach` clamps it to
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
 * rather than a styling one. A blob: URL inherits the app's origin, so
 * `allow-same-origin` would hand the artifact the app's storage and the
 * gateway's bearer token: it is never granted. HTML gets the empty sandbox — no
 * scripts, no forms, no top-level navigation, opaque origin. A PDF is not
 * markup and cannot run anything of its own; `allow-scripts` there is for the
 * BROWSER's built-in viewer (Chromium refuses to paint one otherwise), and
 * without `allow-same-origin` it still runs in an opaque origin that can see
 * nothing of ours.
 */
export function docSandbox(mime: string | undefined): string {
  return isPdfMedia(mime) ? "allow-scripts" : "";
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

/** The caption row both the card and the opened document wear. */
function DocCaption({
  mime,
  name,
  sizeLabel,
  action,
}: {
  mime: string;
  name: string;
  sizeLabel?: string;
  action: React.ReactNode;
}) {
  return (
    <div className="flex shrink-0 flex-wrap items-center gap-2 border-b border-code-edge bg-panel px-2 py-1">
      <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-warn">
        {docKindLabel(mime)}
      </span>
      <span className="min-w-0 flex-1 truncate font-mono text-chip text-muted">
        {[name, sizeLabel].filter(Boolean).join(" · ")}
      </span>
      {action}
    </div>
  );
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
 * A document attachment in the transcript: ONE caption row and the single `Open`
 * chip that throws it over the whole screen.
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
    <div className="mt-2 min-w-0 border border-code-edge bg-input">
      <DocCaption
        mime={mime}
        name={name}
        sizeLabel={sizeLabel}
        action={
          <Button
            variant="ghost"
            density="compact"
            onClick={open}
            aria-label={`Open ${name}`}
          >
            Open
          </Button>
        }
      />
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
    </div>
  );
});
