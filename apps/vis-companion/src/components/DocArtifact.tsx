import { memo, useEffect } from 'react';
import { docKindLabel, isPdfMedia } from '../lib/artifacts';

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
  return isPdfMedia(mime) ? 'allow-scripts' : '';
}

/** Parse a `vis-doc` fence body: five header lines, no payload. */
export function parseDocBlock(body: string): DocArtifact {
  const lines = body.replace(/\n+$/, '').split('\n');
  const at = (index: number) => (lines[index] ?? '').trim();
  return {
    summary: at(0),
    path: at(1),
    mime: at(2),
    name: at(3) || at(1).split('/').pop() || 'document',
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
      className="h-[60vh] max-h-[34rem] w-full border-0 bg-input"
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
      className={`${compact ? 'my-2' : 'my-3'} flex w-full max-w-full min-w-0 flex-col overflow-hidden bg-input ${frameless ? '' : 'border border-code-edge'}`}
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
          {[artifact.mime, artifact.sizeLabel].filter(Boolean).join(' · ')}
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

/**
 * A document attachment, shown in place. The bytes are asked for as soon as the
 * artifact is on screen and land inside a sandboxed frame; there is no toggle,
 * no new-tab escape hatch and no capture strip — the document is simply read
 * where it sits.
 */
export const DocPreview = memo(function DocPreview({
  name,
  mime,
  sizeLabel,
  url,
  failed,
  onNeeded,
}: {
  name: string;
  mime: string;
  sizeLabel?: string;
  /** The artifact's object URL, or null while it is still being fetched. */
  url: string | null;
  failed: boolean;
  /** Asked for the bytes — the parent starts the fetch. */
  onNeeded: () => void;
}) {
  useEffect(() => {
    onNeeded();
  }, [onNeeded]);

  const kind = docKindLabel(mime);

  return (
    <div className="mt-2 min-w-0 border border-code-edge bg-input">
      <div className="flex flex-wrap items-center gap-2 border-b border-code-edge bg-panel px-2 py-1">
        <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-warn">
          {kind}
        </span>
        <span className="min-w-0 flex-1 truncate font-mono text-chip text-muted">
          {[name, sizeLabel].filter(Boolean).join(' · ')}
        </span>
      </div>
      <div className="min-w-0">
        {failed ? (
          <p className="px-2 py-3 text-meta text-footer-muted">
            This document could not be loaded from the gateway.
          </p>
        ) : url ? (
          <DocFrame url={url} mime={mime} name={name} />
        ) : (
          <p className="px-2 py-3 text-meta text-footer-muted">Loading…</p>
        )}
      </div>
    </div>
  );
});
