import { memo, useCallback, useEffect, useState } from "react";

import type { GatewayClient } from "../lib/gateway";
import {
  renderPdfPage,
  stampPdfPage,
  type RenderedPage,
} from "../lib/pdf-annotate";
import { ImageViewer } from "./ImageViewer";
import type { DocumentChrome } from "./MarkdownArtifact";
import { ChevronIcon } from "./icons";
import { BandButton } from "./ui";

/**
 * A PDF PAGE FITTED BY THE APP, WITH THE PAGER AND THE PEN IN ITS OWN BAND.
 *
 * The browser's viewer can be neither drawn on nor sized, so a page is rasterised
 * (`lib/pdf-annotate`), fitted to the overlay, and what the pen leaves is stamped
 * back onto that page as the next version of the same filename.
 *
 * Like a note, a PDF is always read inside somebody else's chrome, so it hands its
 * cells UP ({@link DocumentChrome}): `actions` are the band's, `note` is what the
 * band reports under the filename, and `body` is the page.
 */
export const PdfAnnotator = memo(function PdfAnnotator({
  client,
  sid,
  iterationId,
  name,
  mediaType,
  url,
  chrome,
}: {
  client: GatewayClient;
  sid: string;
  iterationId: string;
  name: string;
  mediaType: string;
  url: string;
  /** The band and the frame this page is read inside. */
  chrome: DocumentChrome;
}) {
  const [page, setPage] = useState(1);
  const [source, setSource] = useState<ArrayBuffer | null>(null);
  const [rendered, setRendered] = useState<RenderedPage | null>(null);
  const [drawn, setDrawn] = useState<string | null>(null);
  const [status, setStatus] = useState("");
  const pageCount = rendered?.pageCount ?? 0;

  // THE FILE IS DOWNLOADED ONCE PER URL; A PAGE TURN IS ONLY A RASTER.
  //
  // Both hung off one effect keyed on the page, so every ‹ › fetched the whole
  // document again before it could draw a page that was already in memory.
  useEffect(() => {
    let live = true;
    void (async () => {
      try {
        const bytes = await (await fetch(url)).arrayBuffer();
        if (live) setSource(bytes);
      } catch {
        if (live) setStatus("This PDF could not be opened.");
      }
    })();
    return () => {
      live = false;
    };
  }, [url]);

  useEffect(() => {
    if (!source) return;
    let live = true;
    void (async () => {
      try {
        const next = await renderPdfPage(source, page);
        // A fresh data URL paints as an EMPTY frame while it decodes, so the page
        // being left stays up until the next one can be shown whole.
        const ready = new Image();
        ready.src = next.src;
        try {
          await ready.decode();
        } catch {
          // No decoder on this platform — the <img> paints it regardless.
        }
        if (live) setRendered(next);
      } catch {
        if (live) setStatus("This page could not be rendered.");
      }
    })();
    return () => {
      live = false;
    };
  }, [source, page]);

  // A page turn drops what the band reported about the page being left: `Saved as
  // v4` belongs to the page it was stamped on.
  const turn = useCallback(
    (by: number) => {
      setStatus("");
      setPage((current) =>
        Math.max(
          1,
          pageCount ? Math.min(current + by, pageCount) : current + by,
        ),
      );
    },
    [pageCount],
  );

  const apply = useCallback(
    async (edited: Blob) => {
      if (!source) return;
      setStatus("");
      try {
        const stamped = await stampPdfPage(
          source,
          page,
          await edited.arrayBuffer(),
        );
        const saved = await client.saveArtifactBytes(
          sid,
          iterationId,
          name,
          mediaType || "application/pdf",
          stamped,
        );
        // The stamped bytes ARE the document now; re-rastering them is the render
        // effect's job, so the ink appears on the page it was left on.
        setSource(stamped.slice().buffer as ArrayBuffer);
        setDrawn(null);
        setStatus(saved.version ? `Saved as v${saved.version}` : "Saved");
      } catch {
        setStatus("Could not save this revision.");
      }
    },
    [client, sid, iterationId, name, mediaType, source, page],
  );

  const position = pageCount ? `Page ${page} of ${pageCount}` : `Page ${page}`;
  return chrome({
    // THE PAGER AND THE PEN ARE CELLS OF THE BAND THAT NAMES THE FILE.
    //
    // They stood in a docked strip under the page instead: three bordered boxes and
    // a counter on a second paper, at the opposite end of the screen from the ✕,
    // taking a band's worth of an 844px phone away from the page they act on. The
    // note settled this shape already (`MarkdownArtifact`) — what is done to the
    // WHOLE document is a cell of its own band, one hairline from the way out.
    //
    // A CELL IS BOUND BY THE DOCUMENT, NEVER BY THE WORK IN FLIGHT. All three were
    // disabled while a page rasterised, so every press dimmed the row it was aimed
    // at — and a disabled button drops the focus it was just handed, leaving a second
    // › to land on nothing. The bounds are the document's own: the first page, the
    // last page, and whether there is a page to draw on at all.
    actions: (
      <>
        <BandButton
          aria-label="Previous page"
          title="Previous page"
          disabled={page <= 1}
          onClick={() => turn(-1)}
        >
          <ChevronIcon back className="size-3" />
        </BandButton>
        <BandButton
          aria-label="Next page"
          title="Next page"
          disabled={pageCount === 0 || page >= pageCount}
          onClick={() => turn(1)}
        >
          <ChevronIcon className="size-3" />
        </BandButton>
        <BandButton
          aria-label={`Annotate page ${page}`}
          title={`Annotate page ${page}`}
          disabled={!rendered}
          onClick={() => rendered && setDrawn(rendered.src)}
        >
          Annotate
        </BandButton>
      </>
    ),
    // Where the reader stands in the document, and what just became of it: the two
    // things this band has to report under the filename.
    note: status ? `${position} · ${status}` : position,
    body: (
      <div className="flex min-h-0 min-w-0 flex-1 flex-col bg-input">
        <div className="flex min-h-0 flex-1 items-center justify-center overflow-hidden p-2 sm:p-4">
          {rendered ? (
            <img
              src={rendered.src}
              aria-label={`Page ${page} of ${name}`}
              className="block max-h-full max-w-full object-contain shadow-lg"
            />
          ) : (
            <p className="text-meta text-footer-muted">
              {status || "Rendering PDF…"}
            </p>
          )}
        </div>
        {/* The band's report is silent to a screen reader, so the outcome has a
            permanent, announced twin here. */}
        <span className="sr-only" role="status">
          {status}
        </span>
        {drawn ? (
          <ImageViewer
            src={drawn}
            name={`${name} — page ${page}`}
            onApply={apply}
            onClose={() => setDrawn(null)}
          />
        ) : null}
      </div>
    ),
  });
});
