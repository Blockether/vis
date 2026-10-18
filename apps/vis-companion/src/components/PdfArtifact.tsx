import { memo, useCallback, useEffect, useLayoutEffect, useRef, useState } from 'react';

import { EDGE_ZONE_PX } from '../lib/edge-back';
import type { GatewayClient } from '../lib/gateway';
import { openPdf, stampPdfPage, type PdfPages, type RenderedPage } from '../lib/pdf-annotate';
import { FIT_SCALE, type Transform } from '../lib/zoom-pan';
import { isKeyboardInputElement } from '../lib/viewport';
import { useZoomPan, ZOOM_FRAME_CLASS, ZOOMED_CLASS } from '../lib/zoom-surface';
import { ImageViewer } from './ImageViewer';
import type { DocumentChrome } from './MarkdownArtifact';
import { ChevronIcon } from './icons';
import { BandButton } from './ui';

/**
 * The density a page is rasterised at for a zoom. A zoomed page is drawn again more
 * sharply once the zoom stops, and only in two steps, so a pinch never re-renders it
 * frame by frame.
 */
export function rasterScale(zoom: number): number {
  return zoom > 1.5 ? 4 : 2;
}

/** How long a zoom has to rest before the page is drawn again at its new density. */
const SHARPEN_MS = 200;

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
  commentable = false,
}: {
  client: GatewayClient;
  sid: string;
  iterationId: string;
  name: string;
  mediaType: string;
  url: string;
  commentable?: boolean;
  /** The band and the frame this page is read inside. */
  chrome: DocumentChrome;
}) {
  const [page, setPage] = useState(1);
  const [raster, setRaster] = useState(rasterScale(FIT_SCALE));
  const [source, setSource] = useState<ArrayBuffer | null>(null);
  const [rendered, setRendered] = useState<RenderedPage | null>(null);
  const [drawn, setDrawn] = useState<string | null>(null);
  const [status, setStatus] = useState('');
  // The document opened once per file, so a page turn only draws a page.
  const [doc, setDoc] = useState<PdfPages | null>(null);
  const pageCount = doc?.pageCount ?? 0;

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
        if (live) setStatus('This PDF could not be opened.');
      }
    })();
    return () => {
      live = false;
    };
  }, [url]);

  useEffect(() => {
    if (!source) return;
    let live = true;
    let opened: PdfPages | null = null;
    void (async () => {
      try {
        opened = await openPdf(source);
        if (live) setDoc(opened);
        else opened.close();
      } catch {
        if (live) setStatus('This PDF could not be opened.');
      }
    })();
    return () => {
      live = false;
      opened?.close();
    };
  }, [source]);

  // THE PAGES NEXT TO THE ONE ON SCREEN ARE DRAWN WHILE IT IS READ.
  //
  // A page was drawn only once ‹ › asked for it, so every turn waited for a raster.
  // Now the page before and the page after are ready when the reader turns to them,
  // and only those three are kept.
  useEffect(() => {
    if (!doc) return;
    let live = true;
    void (async () => {
      try {
        const next = await doc.render(page, raster);
        if (!live) return;
        setRendered(next);
        doc.keep([page - 1, page, page + 1]);
        for (const near of [page + 1, page - 1]) {
          if (near >= 1 && near <= doc.pageCount) {
            doc.render(near, rasterScale(FIT_SCALE)).catch(() => undefined);
          }
        }
      } catch {
        if (live) setStatus('This page could not be rendered.');
      }
    })();
    return () => {
      live = false;
    };
  }, [doc, page, raster]);

  // A page turn drops what the band reported about the page being left: `Saved as
  // v4` belongs to the page it was stamped on.
  const turn = useCallback(
    (by: number) => {
      setStatus('');
      setRaster(rasterScale(FIT_SCALE));
      setPage((current) =>
        Math.max(1, pageCount ? Math.min(current + by, pageCount) : current + by),
      );
    },
    [pageCount],
  );

  // THE ARROW KEYS TURN THE PAGE, AS THE ‹ › OF THE BAND DO.
  //
  // A reader at a keyboard presses ← and → before reaching for a button. A shortcut
  // with a modifier, a key typed into a field, and the arrows of the drawing viewer
  // that is open over the page are left to their owners.
  useEffect(() => {
    if (drawn) return;
    const keyDown = (event: KeyboardEvent) => {
      if (event.defaultPrevented || event.altKey || event.ctrlKey || event.metaKey) return;
      const by = event.key === 'ArrowLeft' ? -1 : event.key === 'ArrowRight' ? 1 : 0;
      if (!by) return;
      const target = event.target instanceof Element ? event.target : null;
      if (isKeyboardInputElement(target) || target?.closest('select, input[type="range"]')) return;
      event.preventDefault();
      turn(by);
    };
    window.addEventListener('keydown', keyDown);
    return () => window.removeEventListener('keydown', keyDown);
  }, [drawn, turn]);

  // THE PAGE ZOOMS, PANS AND TURNS LIKE A PICTURE IN THE DRAWING VIEWER.
  //
  // Both take their gestures from one hook (`lib/zoom-surface`): a pinch, the wheel,
  // a trackpad pinch, a double tap or click, a drag of the zoomed page, and a sideways
  // swipe of the fitted page to the next one. A finger that lands in the edge strip
  // belongs to the back stroke of the dialog (`lib/edge-back`). The page is drawn
  // again more sharply once the zoom rests, and a page turn starts the next page fitted.
  const sharpenRef = useRef<ReturnType<typeof setTimeout> | undefined>(undefined);
  useEffect(() => () => clearTimeout(sharpenRef.current), []);
  const { viewportRef, transformedRef, handlers } = useZoomPan({
    resetKey: page,
    swipe: {
      back: page > 1,
      forward: pageCount > 0 && page < pageCount,
      onStep: turn,
    },
    skips: (at, frame) => at.x - frame.left <= EDGE_ZONE_PX,
    onChange: (transform: Transform) => {
      clearTimeout(sharpenRef.current);
      sharpenRef.current = setTimeout(
        () => setRaster(rasterScale(Math.max(FIT_SCALE, transform.scale))),
        SHARPEN_MS,
      );
    },
  });

  // The fitted size of the page, measured from the frame, so a tall page fits its
  // height and a wide one its width.
  const [fit, setFit] = useState<{ width: number; height: number } | null>(null);
  const aspect = rendered?.width && rendered.height ? rendered.width / rendered.height : 0;
  useLayoutEffect(() => {
    const surface = viewportRef.current;
    if (!surface || !aspect) {
      setFit(null);
      return;
    }
    const measure = () => {
      const style = getComputedStyle(surface);
      const pad = (value: string) => parseFloat(value) || 0;
      const across = surface.clientWidth - pad(style.paddingLeft) - pad(style.paddingRight);
      const down = surface.clientHeight - pad(style.paddingTop) - pad(style.paddingBottom);
      if (across <= 0 || down <= 0) return;
      const width = Math.min(across, down * aspect);
      setFit((current) =>
        current?.width === width ? current : { width, height: width / aspect },
      );
    };
    measure();
    const observer = new ResizeObserver(measure);
    observer.observe(surface);
    return () => observer.disconnect();
  }, [viewportRef, aspect]);

  const apply = useCallback(
    async (edited: Blob) => {
      if (!source) return;
      setStatus('');
      try {
        const stamped = await stampPdfPage(source, page, await edited.arrayBuffer());
        const saved = await client.saveArtifactBytes(
          sid,
          iterationId,
          name,
          mediaType || 'application/pdf',
          stamped,
        );
        // The stamped bytes ARE the document now; re-rastering them is the render
        // effect's job, so the ink appears on the page it was left on.
        setSource(stamped.slice().buffer as ArrayBuffer);
        setDrawn(null);
        setStatus(saved.version ? `Saved as v${saved.version}` : 'Saved');
      } catch {
        setStatus('Could not save this revision.');
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
        <BandButton label="Previous page" disabled={page <= 1} onClick={() => turn(-1)}>
          <ChevronIcon back className="size-3" />
        </BandButton>
        <BandButton
          label="Next page"
          disabled={pageCount === 0 || page >= pageCount}
          onClick={() => turn(1)}
        >
          <ChevronIcon className="size-3" />
        </BandButton>
        {commentable ? (
          <BandButton
            aria-label={`Annotate page ${page}`}
            title={`Annotate page ${page}`}
            disabled={!rendered}
            onClick={() => rendered && setDrawn(rendered.src)}
          >
            Annotate
          </BandButton>
        ) : null}
      </>
    ),
    // Where the reader stands in the document, and what just became of it: the two
    // things this band has to report under the filename.
    note: status ? `${position} · ${status}` : position,
    body: (
      <div className="flex min-h-0 min-w-0 flex-1 flex-col bg-input">
        <div
          ref={viewportRef}
          className={`relative grid min-h-0 flex-1 cursor-grab place-items-center p-2 active:cursor-grabbing sm:p-4 ${ZOOM_FRAME_CLASS}`}
          {...handlers}
        >
          {rendered ? (
            <div
              ref={transformedRef}
              className={`relative ${ZOOMED_CLASS}`}
              style={fit ? { width: fit.width, height: fit.height } : undefined}
            >
              <img
                src={rendered.src}
                aria-label={`Page ${page} of ${name}`}
                draggable={false}
                className={`block select-none ${fit ? 'size-full' : 'max-h-full max-w-full object-contain'}`}
              />
            </div>
          ) : (
            <p className="text-meta text-footer-muted">{status || 'Rendering PDF…'}</p>
          )}
        </div>
        {/* The band's report is silent to a screen reader, so the outcome has a
            permanent, announced twin here. */}
        <span className="sr-only" role="status">
          {status}
        </span>
        {commentable && drawn ? (
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
