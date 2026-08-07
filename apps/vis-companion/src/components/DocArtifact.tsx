import { memo, useCallback, useEffect, useState } from "react";
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
// host file in the system viewer; this component is the app's half — a card in
// the transcript and a SANDBOXED frame for the artifact's own bytes.

export type DocArtifact = {
  /** `[Document: report.pdf PDF, 1.2 MB]` — the caption row. */
  summary: string;
  /** The host path the file was written to (what the TUI opens). */
  path: string;
  mime: string;
  name: string;
  sizeLabel: string;
};

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

/** Parse a `vis-doc` fence body: five header lines, no payload. */
export function parseDocBlock(body: string): DocArtifact {
  const lines = body.replace(/\n+$/, "").split("\n");
  const at = (index: number) => (lines[index] ?? "").trim();
  return {
    summary: at(0),
    path: at(1),
    mime: at(2),
    name: at(3) || at(1).split("/").pop() || "document",
    sizeLabel: at(4),
  };
}

/**
 * The artifact itself, quarantined. `url` is an object URL for the attachment's
 * bytes; the frame is deliberately tall enough to read a page on a phone and
 * capped so a document never eats the whole transcript.
 */
export const DocFrame = memo(function DocFrame({
  url,
  mime,
  name,
  fill = false,
}: {
  url: string;
  mime: string;
  name: string;
  /** Opened rather than previewed: take the whole box. */
  fill?: boolean;
}) {
  return (
    <iframe
      title={name}
      src={url}
      sandbox={docSandbox(mime)}
      referrerPolicy="no-referrer"
      loading="lazy"
      className={`w-full border-0 bg-input ${
        fill ? "min-h-0 flex-1" : "h-[60vh] max-h-[34rem]"
      }`}
    />
  );
});

/**
 * The fence as it appears in a tool result. The transcript carries descriptors
 * only — the bytes live on the gateway — so the card states WHAT was produced
 * and where it landed on the host; the rail below the block is where the same
 * artifact is actually opened.
 */
export const DocCard = memo(function DocCard({
  body,
  compact,
  frameless = false,
}: {
  body: string;
  compact: boolean;
  /** Keep the spacing but drop the frame: an enclosing card already draws one. */
  frameless?: boolean;
}) {
  const artifact = parseDocBlock(body);
  const kind = docKindLabel(artifact.mime);
  return (
    <div
      className={`${compact ? "my-2" : "my-3"} flex w-full max-w-full min-w-0 flex-col overflow-hidden bg-input ${frameless ? "" : "border border-code-edge"}`}
    >
      <div className="flex flex-wrap items-center gap-2 border-b border-code-edge bg-panel px-2 py-1">
        <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-warn">
          {kind}
        </span>
        <span className="min-w-0 flex-1 truncate text-chip text-muted">
          {artifact.summary || artifact.name}
        </span>
      </div>
      <div className="grid min-w-0 gap-0.5 px-2 py-1.5">
        <span className="min-w-0 truncate font-mono text-meta text-code-foreground">
          {artifact.name}
        </span>
        <span className="min-w-0 truncate font-mono text-chip text-muted">
          {[artifact.mime, artifact.sizeLabel].filter(Boolean).join(" · ")}
        </span>
        {artifact.path && (
          <span className="min-w-0 truncate font-mono text-chip text-footer-muted">
            {artifact.path}
          </span>
        )}
      </div>
    </div>
  );
});

/** The artifact's own bytes: a note read by the app, anything else in a frame. */
function DocBody({
  name,
  mime,
  url,
  failed,
  fill,
}: {
  name: string;
  mime: string;
  url: string | null;
  failed: boolean;
  fill: boolean;
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
    <TextFrame url={url} mime={mime} name={name} fill={fill} />
  ) : (
    <DocFrame url={url} mime={mime} name={name} fill={fill} />
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
 * The transcript's own chip face — the very box, border, paper, ink and hover
 * that `Copy` wears in a tool result, so the card carries no foreign control.
 */
function DocChip({
  label,
  onClick,
  ariaLabel,
}: {
  label: string;
  onClick: () => void;
  ariaLabel: string;
}) {
  return (
    <button
      type="button"
      className="min-w-[6ch] shrink-0 border border-dialog-edge bg-button px-1.5 py-0.5 text-center font-mono text-chip text-button-foreground transition-colors hover:bg-hover"
      onClick={onClick}
      aria-label={ariaLabel}
    >
      {label}
    </button>
  );
}

/**
 * An opened document owns the WHOLE viewport — full height and full width —
 * with nothing above it but its own caption row and the Close chip.
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
    <div className="fixed inset-0 z-50 flex min-h-0 min-w-0 flex-col bg-panel">
      <DocCaption
        mime={mime}
        name={name}
        sizeLabel={sizeLabel}
        action={
          <DocChip
            label="Close"
            onClick={onClose}
            ariaLabel={`Close ${name}`}
          />
        }
      />
      <div className="flex min-h-0 min-w-0 flex-1 flex-col">
        {/* Opened, the artifact is not only READ: a note can be commented on and a
            PDF drawn on, and either one saves as the next version of the same
            filename. */}
        {annotate && url && !failed && isMarkdownMedia(mime, name) ? (
          <MarkdownArtifact
            client={annotate.client}
            sid={annotate.sid}
            iterationId={annotate.iterationId}
            name={name}
            mediaType={mime}
            url={url}
          />
        ) : annotate && url && !failed && isPdfMedia(mime) ? (
          <PdfAnnotator
            client={annotate.client}
            sid={annotate.sid}
            iterationId={annotate.iterationId}
            name={name}
            mediaType={mime}
            url={url}
            frame={<DocFrame url={url} mime={mime} name={name} fill />}
          />
        ) : (
          <DocBody name={name} mime={mime} url={url} failed={failed} fill />
        )}
      </div>
    </div>
  );
});

/**
 * A document attachment, shown in place: one caption row, the bytes, and the
 * single `Open` chip that throws the document over the whole screen. No draw
 * strip, no hide toggle, no new-tab escape.
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
          <DocChip label="Open" onClick={open} ariaLabel={`Open ${name}`} />
        }
      />
      {/* Every document preview in the transcript is the SAME box: a PDF in its
          frame and a note read by the app both stand 60vh tall and scroll inside
          themselves, so a long note no longer swallows the turn that made it. */}
      <div className="max-h-[60vh] min-w-0 overflow-y-auto">
        <DocBody
          name={name}
          mime={mime}
          url={url}
          failed={failed}
          fill={false}
        />
      </div>
      {opened && (
        <DocOverlay
          name={name}
          mime={mime}
          sizeLabel={sizeLabel}
          url={url}
          failed={failed}
          annotate={annotate}
          onClose={close}
        />
      )}
    </div>
  );
});
