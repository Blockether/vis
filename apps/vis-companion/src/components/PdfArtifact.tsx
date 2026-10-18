import { memo, useCallback, useEffect, useLayoutEffect, useRef, useState } from 'react';

import { EDGE_ZONE_PX } from '../lib/edge-back';
import type { GatewayClient } from '../lib/gateway';
import { renderPdfPage, stampPdfPage, type RenderedPage } from '../lib/pdf-annotate';
import {
  clamp,
  distance,
  FIT_SCALE,
  MAX_SCALE,
  midpoint,
  wheelFactor,
  type Point,
} from '../lib/zoom-pan';
import { ImageViewer } from './ImageViewer';
import type { DocumentChrome } from './MarkdownArtifact';
import { ChevronIcon } from './icons';
import { BandButton } from './ui';

/** How far a finger has to travel sideways before its lift turns the page. */
export const PAGE_SWIPE_PX = 48;

interface SwipePoint {
  readonly x: number;
  readonly y: number;
}

/**
 * The page turn that a finger stroke asks for: `1` for the next page, `-1` for the
 * previous one, `0` for none. A stroke to the left brings the next page in, as a
 * book does. A stroke that went mostly up or down, or not far enough, is no turn.
 */
export function swipeTurn(from: SwipePoint, to: SwipePoint): -1 | 0 | 1 {
  const across = to.x - from.x;
  if (Math.abs(across) < PAGE_SWIPE_PX || Math.abs(across) <= Math.abs(to.y - from.y)) return 0;
  return across < 0 ? 1 : -1;
}

/**
 * The point of the page, in px of the FITTED page, that stands under `at`. `at` is in
 * the scroller's own box, `scroll` is where it is scrolled to and `offset` is where the
 * page stands inside it.
 */
export function spotUnder(at: Point, scroll: Point, offset: Point, zoom: number): Point {
  return { x: (scroll.x + at.x - offset.x) / zoom, y: (scroll.y + at.y - offset.y) / zoom };
}

/** Where the scroller has to stand so that `spot` of the page lands under `at` again. */
export function scrollToKeep(spot: Point, zoom: number, at: Point, offset: Point): Point {
  return { x: spot.x * zoom + offset.x - at.x, y: spot.y * zoom + offset.y - at.y };
}

/**
 * The density a page is rasterised at for a zoom. A zoomed page is drawn again more
 * sharply once the zoom stops, and only in two steps, so a pinch never re-renders it
 * frame by frame.
 */
export function rasterScale(zoom: number): number {
  return zoom > 1.5 ? 4 : 2;
}

/** The zoom a double tap or double click goes to from the fitted page. */
const DOUBLE_TAP_ZOOM = 2;

/** Two taps this close in time and space are one double tap. */
const DOUBLE_TAP_MS = 300;
const DOUBLE_TAP_PX = 24;

/** How long a zoom has to rest before the page is drawn again at its new density. */
const SHARPEN_MS = 200;

/**
 * Safari's own trackpad and touch pinch (`ImageViewer` reads it too). TypeScript's DOM
 * library does not declare it.
 */
type SafariGestureEvent = Event & { scale: number; clientX: number; clientY: number };

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
    void (async () => {
      try {
        const next = await renderPdfPage(source, page, raster);
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
        if (live) setStatus('This page could not be rendered.');
      }
    })();
    return () => {
      live = false;
    };
  }, [source, page, raster]);

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

  // A SIDEWAYS STROKE ON THE PAGE TURNS IT, like the ‹ › of the band.
  //
  // The listeners stand on the page only, never on the pen's overlay: a stroke of ink
  // is not a page turn. A finger that lands in the edge strip belongs to the back
  // stroke of the dialog (`lib/edge-back`), and a second finger is a pinch. The turn
  // is decided on the lift, inside the document's own bounds.
  const [surface, setSurface] = useState<HTMLDivElement | null>(null);
  // How far the reader has zoomed into the page; 1 is the page fitted (below).
  const zoomRef = useRef(FIT_SCALE);
  const swipe = useRef((by: number) => turn(by));
  useEffect(() => {
    swipe.current = (by: number) => {
      if (by < 0 ? page > 1 : pageCount > 0 && page < pageCount) turn(by);
    };
  });
  useEffect(() => {
    if (!surface) return;
    let from: SwipePoint | null = null;
    let at: SwipePoint | null = null;
    const pointOf = (event: TouchEvent): SwipePoint | null => {
      const touch = event.touches[0];
      return touch ? { x: touch.clientX, y: touch.clientY } : null;
    };
    const onStart = (event: TouchEvent) => {
      const point = event.touches.length === 1 ? pointOf(event) : null;
      const left = surface.getBoundingClientRect().left;
      // A zoomed page is panned by the same stroke, never turned.
      const isFitted = zoomRef.current <= FIT_SCALE;
      from = point && isFitted && point.x - left > EDGE_ZONE_PX ? point : null;
      at = from;
    };
    const onMove = (event: TouchEvent) => {
      if (!from) return;
      if (event.touches.length !== 1) {
        from = null;
        return;
      }
      at = pointOf(event) ?? at;
    };
    const onEnd = (event: TouchEvent) => {
      const stroke = from && at && event.type === 'touchend' ? swipeTurn(from, at) : 0;
      from = null;
      at = null;
      if (stroke !== 0) swipe.current(stroke);
    };
    const watch = { passive: true };
    surface.addEventListener('touchstart', onStart, watch);
    surface.addEventListener('touchmove', onMove, watch);
    surface.addEventListener('touchend', onEnd, watch);
    surface.addEventListener('touchcancel', onEnd, watch);
    return () => {
      surface.removeEventListener('touchstart', onStart);
      surface.removeEventListener('touchmove', onMove);
      surface.removeEventListener('touchend', onEnd);
      surface.removeEventListener('touchcancel', onEnd);
    };
  }, [surface]);

  // THE PAGE SCROLLS AND ZOOMS LIKE A PAGE OF THE PHONE'S OWN READER.
  //
  // The page stands in a native scroller, so one finger, the wheel, the trackpad and
  // the scroll bars all move it the way they move anything else, momentum included.
  // A zoom only changes the size of the page and scrolls so that the point under the
  // fingers or the pointer stays there. The zoom is written to the page's style, not
  // to state: a pinch writes it every frame. The page is drawn again more sharply once
  // the zoom rests. A page turn starts the next page fitted.
  const sheetRef = useRef<HTMLDivElement | null>(null);
  const fitRef = useRef<{ width: number; height: number } | null>(null);
  const [fit, setFit] = useState<{ width: number; height: number } | null>(null);
  const aspect = rendered?.width && rendered.height ? rendered.width / rendered.height : 0;

  const paint = useCallback(() => {
    const sheet = sheetRef.current;
    const size = fitRef.current;
    if (!sheet || !size) return;
    sheet.style.width = `${size.width * zoomRef.current}px`;
    sheet.style.height = `${size.height * zoomRef.current}px`;
  }, []);

  useLayoutEffect(() => {
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
  }, [surface, aspect]);

  useLayoutEffect(() => {
    fitRef.current = fit;
    paint();
  }, [fit, paint]);

  useLayoutEffect(() => {
    zoomRef.current = FIT_SCALE;
    paint();
    surface?.scrollTo?.(0, 0);
  }, [page, surface, paint]);

  useEffect(() => {
    if (!surface) return;
    let sharpen: ReturnType<typeof setTimeout> | undefined;
    const box = (client: Point): Point => {
      const frame = surface.getBoundingClientRect();
      return { x: client.x - frame.left, y: client.y - frame.top };
    };
    const offset = (): Point => ({
      x: sheetRef.current?.offsetLeft ?? 0,
      y: sheetRef.current?.offsetTop ?? 0,
    });
    const spotAt = (client: Point): Point =>
      spotUnder(
        box(client),
        { x: surface.scrollLeft, y: surface.scrollTop },
        offset(),
        zoomRef.current,
      );
    // Zoom to `next`, with `spot` of the page under `client`.
    const zoomTo = (next: number, client: Point, spot = spotAt(client)) => {
      if (!fitRef.current) return;
      zoomRef.current = clamp(next, FIT_SCALE, MAX_SCALE);
      paint();
      const to = scrollToKeep(spot, zoomRef.current, box(client), offset());
      surface.scrollLeft = to.x;
      surface.scrollTop = to.y;
      clearTimeout(sharpen);
      sharpen = setTimeout(() => setRaster(rasterScale(zoomRef.current)), SHARPEN_MS);
    };
    const toggle = (client: Point) =>
      zoomTo(zoomRef.current > FIT_SCALE ? FIT_SCALE : DOUBLE_TAP_ZOOM, client);

    // Two fingers: the pinch. The page under the fingers' midpoint follows it.
    let pinch: { spot: Point; zoom: number; apart: number } | null = null;
    // One finger that lands and lifts without travelling: half of a double tap.
    let tap: { at: Point; time: number; isStill: boolean } | null = null;
    let lastTap: { at: Point; time: number } | null = null;
    let tappedAt = Number.NEGATIVE_INFINITY;
    const fingers = (event: TouchEvent): [Point, Point] | null => {
      const [a, b] = [event.touches[0], event.touches[1]];
      return a && b
        ? [
            { x: a.clientX, y: a.clientY },
            { x: b.clientX, y: b.clientY },
          ]
        : null;
    };
    const onTouchStart = (event: TouchEvent) => {
      const two = event.touches.length === 2 ? fingers(event) : null;
      if (two && fitRef.current) {
        const [a, b] = two;
        pinch = { spot: spotAt(midpoint(a, b)), zoom: zoomRef.current, apart: distance(a, b) };
      }
      const one = event.touches.length === 1 ? event.touches[0] : null;
      tap = one ? { at: { x: one.clientX, y: one.clientY }, time: event.timeStamp, isStill: true } : null;
    };
    const onTouchMove = (event: TouchEvent) => {
      const one = event.touches[0];
      if (tap && one && distance(tap.at, { x: one.clientX, y: one.clientY }) > DOUBLE_TAP_PX) {
        tap.isStill = false;
      }
      const two = pinch ? fingers(event) : null;
      if (!pinch || !two) return;
      // The browser must neither scroll nor zoom the whole app under a pinch.
      event.preventDefault();
      const [a, b] = two;
      zoomTo(pinch.zoom * (distance(a, b) / Math.max(1, pinch.apart)), midpoint(a, b), pinch.spot);
    };
    const onTouchEnd = (event: TouchEvent) => {
      if (event.touches.length < 2) pinch = null;
      if (event.touches.length > 0 || event.type !== 'touchend') return;
      const done = tap?.isStill ? { at: tap.at, time: event.timeStamp } : null;
      tap = null;
      const isDouble =
        done &&
        lastTap &&
        done.time - lastTap.time <= DOUBLE_TAP_MS &&
        distance(done.at, lastTap.at) <= DOUBLE_TAP_PX;
      lastTap = isDouble ? null : done;
      if (!done || !isDouble) return;
      tappedAt = event.timeStamp;
      toggle(done.at);
    };
    // A plain wheel scrolls. A ctrl+wheel is a trackpad pinch or a deliberate zoom.
    const onWheel = (event: WheelEvent) => {
      if (!event.ctrlKey) return;
      event.preventDefault();
      zoomTo(zoomRef.current * wheelFactor(event.deltaY, event.deltaMode, true), {
        x: event.clientX,
        y: event.clientY,
      });
    };
    // A Safari pinch reports its scale from the start of the gesture.
    let gesture: { spot: Point; zoom: number; scale: number } | null = null;
    const onGestureStart = (event: Event) => {
      event.preventDefault();
      const { scale, clientX, clientY } = event as SafariGestureEvent;
      // A touch pinch already drives the zoom; Safari reports it twice.
      gesture = pinch
        ? null
        : { spot: spotAt({ x: clientX, y: clientY }), zoom: zoomRef.current, scale: scale || 1 };
    };
    const onGestureChange = (event: Event) => {
      event.preventDefault();
      if (!gesture) return;
      const { scale, clientX, clientY } = event as SafariGestureEvent;
      zoomTo(
        gesture.zoom * ((scale || 1) / gesture.scale),
        { x: clientX, y: clientY },
        gesture.spot,
      );
    };
    const onGestureEnd = (event: Event) => {
      event.preventDefault();
      gesture = null;
    };
    // A double click zooms in and out on a desk. On a phone the double tap did it already.
    const onDoubleClick = (event: MouseEvent) => {
      if (event.timeStamp - tappedAt < 500) return;
      toggle({ x: event.clientX, y: event.clientY });
    };

    const watch = { passive: true };
    const owned = { passive: false };
    surface.addEventListener('touchstart', onTouchStart, watch);
    surface.addEventListener('touchmove', onTouchMove, owned);
    surface.addEventListener('touchend', onTouchEnd, watch);
    surface.addEventListener('touchcancel', onTouchEnd, watch);
    surface.addEventListener('wheel', onWheel, owned);
    surface.addEventListener('gesturestart', onGestureStart, owned);
    surface.addEventListener('gesturechange', onGestureChange, owned);
    surface.addEventListener('gestureend', onGestureEnd, owned);
    surface.addEventListener('dblclick', onDoubleClick);
    return () => {
      clearTimeout(sharpen);
      surface.removeEventListener('touchstart', onTouchStart);
      surface.removeEventListener('touchmove', onTouchMove);
      surface.removeEventListener('touchend', onTouchEnd);
      surface.removeEventListener('touchcancel', onTouchEnd);
      surface.removeEventListener('wheel', onWheel);
      surface.removeEventListener('gesturestart', onGestureStart);
      surface.removeEventListener('gesturechange', onGestureChange);
      surface.removeEventListener('gestureend', onGestureEnd);
      surface.removeEventListener('dblclick', onDoubleClick);
    };
  }, [surface, paint]);

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
          ref={setSurface}
          className={`relative flex min-h-0 flex-1 overscroll-contain p-2 [touch-action:pan-x_pan-y] sm:p-4 ${
            fit ? 'overflow-auto' : 'items-center justify-center overflow-hidden'
          }`}
        >
          {rendered && fit ? (
            // `m-auto` centres the page while it fits and lets it scroll once it does not.
            <div ref={sheetRef} className="m-auto shrink-0">
              <img
                src={rendered.src}
                aria-label={`Page ${page} of ${name}`}
                draggable={false}
                className="block size-full select-none"
              />
            </div>
          ) : rendered ? (
            <img
              src={rendered.src}
              aria-label={`Page ${page} of ${name}`}
              className="block max-h-full max-w-full object-contain"
            />
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
