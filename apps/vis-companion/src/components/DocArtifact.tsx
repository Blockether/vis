import { memo, useEffect, useRef, useState } from 'react';
import { captureHtmlDocument } from '../lib/doc-capture';
import { pageCaptureFilename, viewCaptureFilename } from '../lib/image-file';
import { openPdfPages, type PdfPages } from '../lib/pdf-pages';
import { docKindLabel, isPdfMedia } from '../lib/artifacts';
import { useAttachImage } from '../lib/attach-image';
import { ImageViewer } from './ImageViewer';
import { Button } from './ui';
import { ChevronIcon } from './icons';

// A PDF or an HTML page is a DOCUMENT, not a picture and not data: nothing in it
// is worth spending a model's context on, so `vis_attach` clamps it to
// `audience: "user"` and emits a ````vis-doc` fence with five header lines
// (summary / host path / mime / name / size) and NO payload. The TUI opens the
// host file in the system viewer; this component is the app's half — a card in
// the transcript, a SANDBOXED frame for the artifact's own bytes, and a way
// back: any page of it can be rasterised, drawn on, and attached to the next
// message as an image, which is the only way its content ever reaches a model.
// See `lib/doc-capture.ts` for why that picture is a sanitised copy painted in
// a shadow root rather than a photograph of the frame on screen.

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
 * The strip under an opened artifact: which page, and "draw on it".
 *
 * Presentational on purpose — every byte of state lives in `DocPreview`, so this
 * is what the tests can render. The page number is a control rather than a
 * readout because the browser's own PDF viewer runs in an opaque origin: the app
 * cannot ask it which page the human is looking at, so the human says so here,
 * and that same number becomes the attachment's name.
 */
export function DocAnnotateBar({
  page,
  pageCount,
  busy,
  disabled,
  notice,
  onPage,
  onCapture,
}: {
  page: number;
  /** 0 until a PDF has been parsed; an HTML artifact has no pages at all. */
  pageCount: number;
  busy: boolean;
  disabled: boolean;
  notice: string;
  onPage: (page: number) => void;
  onCapture: () => void;
}) {
  const paged = pageCount > 0;
  return (
    <div className="flex flex-wrap items-center gap-2 border-t border-code-edge bg-panel px-2 py-1">
      {paged && (
        <div className="flex shrink-0 items-center">
          <Button
            variant="ghost"
            onClick={() => onPage(page - 1)}
            disabled={page <= 1}
            aria-label="Previous page"
          >
            {/* Two buttons, opposite directions, one glyph: the pager used to draw the
                same right-pointing chevron on both halves, so only the disabled state
                said which way you were going. */}
            <ChevronIcon back className="size-3" aria-hidden />
          </Button>
          <span
            className="flex min-h-7 min-w-24 items-center justify-center border-y border-edge-strong px-2 text-chip text-muted sm:min-h-8"
            aria-live="polite"
          >
            Page {page} of {pageCount}
          </span>
          <Button
            variant="ghost"
            onClick={() => onPage(page + 1)}
            disabled={page >= pageCount}
            aria-label="Next page"
          >
            <ChevronIcon className="size-3" aria-hidden />
          </Button>
        </div>
      )}
      <Button variant="ghost" onClick={onCapture} disabled={busy || disabled}>
        {busy ? 'Rendering…' : paged ? `Draw on page ${page}` : 'Draw on page'}
      </Button>
      <span
        className="min-w-0 flex-1 truncate text-chip text-footer-muted"
        aria-live="polite"
      >
        {notice}
      </span>
    </div>
  );
}

/**
 * A document attachment, opened on demand. Collapsed by default: a transcript
 * with a dozen reports in it must not fetch and decode a dozen PDFs to paint —
 * and pdf.js, the heaviest module in the app, is imported only once one is
 * actually opened.
 */
export const DocPreview = memo(function DocPreview({
  name,
  mime,
  sizeLabel,
  url,
  failed,
  onOpen,
}: {
  name: string;
  mime: string;
  sizeLabel?: string;
  /** The artifact's object URL, or null while it is still being fetched. */
  url: string | null;
  failed: boolean;
  /** Asked for the bytes — the parent starts the fetch. */
  onOpen: (open: boolean) => void;
}) {
  const [open, setOpen] = useState(false);
  const [page, setPage] = useState(1);
  const [pageCount, setPageCount] = useState(0);
  const [busy, setBusy] = useState(false);
  const [notice, setNotice] = useState('');
  const [capture, setCapture] = useState<{
    url: string;
    filename: string;
  } | null>(null);
  const pagesRef = useRef<PdfPages | null>(null);
  const attachImage = useAttachImage();
  const kind = docKindLabel(mime);
  const pdf = isPdfMedia(mime);

  // The page count is the document's own, so it has to be parsed to be known —
  // but only once the human opened this artifact, and it is released with it.
  useEffect(() => {
    if (!open || !pdf || !url) return;
    let alive = true;
    let opened: PdfPages | null = null;
    openPdfPages(url)
      .then((pages) => {
        if (!alive) {
          pages.close();
          return;
        }
        opened = pages;
        pagesRef.current = pages;
        setPageCount(pages.pageCount);
      })
      .catch(() => {
        if (alive) setNotice('This PDF could not be read for drawing.');
      });
    return () => {
      alive = false;
      pagesRef.current = null;
      opened?.close();
      setPageCount(0);
    };
  }, [open, pdf, url]);

  function closeCapture() {
    setCapture((current) => {
      if (current) URL.revokeObjectURL(current.url);
      return null;
    });
  }

  useEffect(() => closeCapture, []);

  async function captureDocument() {
    if (!url) return;
    setBusy(true);
    setNotice('');
    try {
      const pages = pagesRef.current;
      if (pdf && !pages) throw new Error('This PDF is still loading.');
      const filename = pdf
        ? pageCaptureFilename(name, page)
        : viewCaptureFilename(name);
      const image =
        pages && pdf
          ? await pages.renderPage(page)
          : await captureHtmlDocument(url);
      setCapture({ url: URL.createObjectURL(image), filename });
    } catch (cause) {
      setNotice(
        cause instanceof Error
          ? cause.message
          : 'This document could not be captured.',
      );
    } finally {
      setBusy(false);
    }
  }

  // The flattened picture is a NEW attachment rather than an edit of this one:
  // the document itself stays where it is (the model still may not read it), and
  // what travels is the page the human drew on, named after that page.
  async function attachCapture(edited: Blob) {
    if (!attachImage || !capture) return;
    await attachImage(edited, capture.filename);
    setNotice(`Attached ${capture.filename} to your message.`);
  }

  return (
    <div className="mt-2 min-w-0 border border-code-edge bg-input">
      <div className="flex flex-wrap items-center gap-2 border-b border-code-edge bg-panel px-2 py-1">
        <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-warn">
          {kind}
        </span>
        <span className="min-w-0 flex-1 truncate font-mono text-chip text-muted">
          {[name, sizeLabel].filter(Boolean).join(' · ')}
        </span>
        <Button
          variant="ghost"
          aria-expanded={open}
          onClick={() => {
            const next = !open;
            setOpen(next);
            onOpen(next);
          }}
        >
          {open ? 'Hide' : 'Open'}
        </Button>
        {url && (
          <Button
            variant="ghost"
            onClick={() => window.open(url, '_blank', 'noreferrer,noopener')}
          >
            New tab
          </Button>
        )}
      </div>
      {open && (
        <div className="min-w-0">
          {failed ? (
            <p className="px-2 py-3 text-meta text-footer-muted">
              This document could not be loaded from the gateway.
            </p>
          ) : url ? (
            <>
              <DocFrame url={url} mime={mime} name={name} />
              <DocAnnotateBar
                page={page}
                pageCount={pageCount}
                busy={busy}
                disabled={pdf && pageCount === 0}
                notice={notice}
                onPage={(next) =>
                  setPage(Math.max(1, Math.min(next, pageCount || 1)))
                }
                onCapture={captureDocument}
              />
            </>
          ) : (
            <p className="px-2 py-3 text-meta text-footer-muted">Loading…</p>
          )}
        </div>
      )}
      {capture && (
        <ImageViewer
          src={capture.url}
          name={capture.filename}
          onClose={closeCapture}
          onApply={attachImage ? attachCapture : undefined}
          applyLabel="Attach to message"
        />
      )}
    </div>
  );
});
