import {
  memo,
  useCallback,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
  type ReactNode,
} from 'react';

import { EDGE_ZONE_PX } from '../lib/edge-back';
import { openPdf, type PdfPages, type RenderedPage } from '../lib/pdf-annotate';
import { FIT_SCALE, type Transform } from '../lib/zoom-pan';
import { isKeyboardInputElement } from '../lib/viewport';
import { useZoomPan, ZOOM_FRAME_CLASS, ZOOMED_CLASS } from '../lib/zoom-surface';
import type { DocumentChrome } from './MarkdownArtifact';
import { ChevronIcon, MinusIcon, PlusIcon } from './icons';

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
 * A PDF PAGE FITTED BY THE APP, WITH THE PAGER FLOATING ON IT.
 *
 * The browser's viewer cannot be sized, so a page is rasterised
 * (`lib/pdf-annotate`) and fitted to the overlay. The reader is read-only.
 *
 * Like a note, a PDF is always read inside somebody else's chrome, so it hands its
 * cells UP ({@link DocumentChrome}): `actions` are the band's, `note` is what the
 * band reports under the filename, and `body` is the page.
 */
export const PdfReader = memo(function PdfReader({
  name,
  url,
  chrome,
}: {
  name: string;
  url: string;
  /** The band and the frame this page is read inside. */
  chrome: DocumentChrome;
}) {
  const [page, setPage] = useState(1);
  const [raster, setRaster] = useState(rasterScale(FIT_SCALE));
  const [source, setSource] = useState<ArrayBuffer | null>(null);
  const [rendered, setRendered] = useState<RenderedPage | null>(null);
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

  // A page turn drops what the band reported about the page being left.
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
  // with a modifier and a key typed into a field are left to their owners.
  useEffect(() => {
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
  }, [turn]);

  // THE PAGE ZOOMS, PANS AND TURNS LIKE A PICTURE IN THE DRAWING VIEWER.
  //
  // Both take their gestures from one hook (`lib/zoom-surface`): a pinch, the wheel,
  // a trackpad pinch, a double tap or click, a drag of the zoomed page, and a sideways
  // swipe of the fitted page to the next one. A finger that lands in the edge strip
  // belongs to the back stroke of the dialog (`lib/edge-back`). The page is drawn
  // again more sharply once the zoom rests, and a page turn starts the next page fitted.
  const sharpenRef = useRef<ReturnType<typeof setTimeout> | undefined>(undefined);
  useEffect(() => () => clearTimeout(sharpenRef.current), []);
  const { viewportRef, transformedRef, handlers, labelRef, reset, zoomBy } = useZoomPan({
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

  const position = pageCount ? `${page} of ${pageCount}` : `${page}`;
  return chrome({
    // THE PAGER FLOATS OVER THE FOOT OF THE PAGE, AS IN EVERY PDF READER.
    //
    // It stood in the band, two cells from the ✕ and one from Share, so the band mixed
    // reading, sending and leaving in one run of look-alike squares. The band now holds
    // the file and its verbs. Turning and zooming are done to the page, so they sit on
    // it. A finger swipes instead, so a touch screen keeps only the counter.
    //
    // A CELL IS BOUND BY THE DOCUMENT, NEVER BY THE WORK IN FLIGHT. A button disabled
    // while a page rasterised drops the focus it was just handed. The bounds are the
    // document's own: the first page, the last page.
    actions: null,
    // A failure is the one thing the band has to report about the page.
    note: status,
    body: (
      <div className="flex min-h-0 min-w-0 flex-1 flex-col bg-input">
        <div
          ref={viewportRef}
          className={`relative grid min-h-0 flex-1 cursor-grab place-items-center p-2 pb-14 active:cursor-grabbing sm:p-4 sm:pb-16 ${ZOOM_FRAME_CLASS}`}
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
        <nav
          aria-label={`Pages of ${name}`}
          className="absolute bottom-3 left-1/2 z-10 flex -translate-x-1/2 items-center gap-0.5 border border-dialog-edge bg-panel p-0.5 font-mono text-chip text-white shadow-float"
        >
          <span className="hidden mouse:contents">
            <PagerButton label="Previous page" disabled={page <= 1} onClick={() => turn(-1)}>
              <ChevronIcon back className="size-3" />
            </PagerButton>
          </span>
          <span className="min-w-[9ch] px-2 py-1.5 text-center tabular-nums mouse:py-0">{position}</span>
          <span className="hidden mouse:contents">
            <PagerButton
              label="Next page"
              disabled={pageCount === 0 || page >= pageCount}
              onClick={() => turn(1)}
            >
              <ChevronIcon className="size-3" />
            </PagerButton>
            <span aria-hidden="true" className="mx-1 h-4 w-px bg-dialog-edge" />
            <PagerButton label="Zoom out" onClick={() => zoomBy(1 / 1.35)}>
              <MinusIcon className="size-3" />
            </PagerButton>
            <PagerButton label="Reset zoom" isWide onClick={reset}>
              <span ref={labelRef}>100%</span>
            </PagerButton>
            <PagerButton label="Zoom in" onClick={() => zoomBy(1.35)}>
              <PlusIcon className="size-3" />
            </PagerButton>
          </span>
        </nav>
      </div>
    ),
  });
});

/** One square of the floating pager: 32px, with the 44px reach a finger needs. */
function PagerButton({
  label,
  isWide = false,
  disabled = false,
  onClick,
  children,
}: {
  label: string;
  isWide?: boolean;
  disabled?: boolean;
  onClick: () => void;
  children: ReactNode;
}) {
  return (
    <button
      type="button"
      aria-label={label}
      title={label}
      disabled={disabled}
      onClick={onClick}
      className={`relative grid h-8 place-items-center bg-transparent font-bold text-current transition-colors duration-150 after:absolute after:-inset-1.5 after:content-[""] enabled:hover:bg-current/10 focus-visible:outline focus-visible:outline-2 focus-visible:-outline-offset-2 focus-visible:outline-accent disabled:cursor-not-allowed disabled:opacity-55 motion-reduce:transition-none ${
        isWide ? 'min-w-[6ch] px-2' : 'w-8'
      }`}
    >
      {children}
    </button>
  );
}
