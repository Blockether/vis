import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';

export interface SwipeAction {
  key: string;
  label: string;
  icon: ReactNode;
  /**
   * What the action MEANS, in colour. The SLAB carries the meaning and the INK
   * carries the caption: `accent` is the amber a star wears — a yellow-tinted
   * cell around a brand-yellow glyph, so "Star" can never read as one more
   * neutral verb beside "Rename" — and `danger` is the red one.
   *
   * Neither slab lends its own colour to the 9px caption on it. `--accent`
   * (#ffc420) and `--err` (#dc2626) are FILLS: as text on their own 15% tint
   * they measure 1.37:1 and 3.50:1, under the 4.5:1 a caption owes, and the
   * amber one arrived as a smear rather than a word. The palette splits each
   * one for exactly this — `accent-ink` reads 6.4:1 and `err-ink` 5.4:1 on the
   * same cells — and the full-strength fill is spent on the hover state, where
   * it becomes the background and takes its own foreground.
   */
  tone?: 'neutral' | 'accent' | 'danger';
  onSelect: () => void;
}

/**
 * A list row that hides its destructive controls behind a left swipe.
 *
 * The drawer is a horizontal SCROLL-SNAP track, not a transform driven by
 * pointer maths: the platform then owns the gesture (momentum, rubber banding,
 * axis locking against the vertical list, VoiceOver focus scrolling), and the
 * component stays free of inline style objects. Panel one is the row at full
 * width, panel two is the action strip.
 */
export function SwipeActions({
  actions,
  children,
  label,
}: {
  actions: SwipeAction[];
  children: ReactNode;
  label?: string;
}) {
  const scrollerRef = useRef<HTMLDivElement>(null);
  const [open, setOpen] = useState(false);

  const close = useCallback(() => {
    setOpen(false);
    scrollerRef.current?.scrollTo({ left: 0, behavior: 'smooth' });
  }, []);

  // An open drawer is a modal-ish state: Escape closes it, and so does scrolling
  // the list away from it — otherwise a forgotten row keeps a delete button armed
  // under the user's thumb.
  useEffect(() => {
    if (!open) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') close();
    };
    const onScroll = (event: Event) => {
      if (event.target !== scrollerRef.current) close();
    };
    window.addEventListener('keydown', onKey);
    window.addEventListener('scroll', onScroll, true);
    return () => {
      window.removeEventListener('keydown', onKey);
      window.removeEventListener('scroll', onScroll, true);
    };
  }, [open, close]);

  if (actions.length === 0) return <>{children}</>;

  return (
    <div
      ref={scrollerRef}
      onScroll={(event) => {
        const next = event.currentTarget.scrollLeft > 8;
        setOpen((current) => (current === next ? current : next));
      }}
      className="flex snap-x snap-mandatory overflow-x-auto overflow-y-hidden overscroll-x-contain [-ms-overflow-style:none] [scrollbar-width:none] [&::-webkit-scrollbar]:hidden"
    >
      {/* A GRID, not a plain block: the track is as tall as its TALLEST panel, and the
          action strip (a 16px icon over a 10px caption) stands 34px against a 32px
          desktop session row. This panel stretches to the track, so its single child
          has to stretch to IT — otherwise the row's hover slab stops short of the rule
          under it and the row reads as if it had lost two pixels of its own height. */}
      <div
        className="grid w-full shrink-0 snap-start bg-panel"
        onClickCapture={(event) => {
          // While the drawer is open the row itself is a dismiss target, never a
          // navigation: a thumb resting on it must not open the session.
          if (!open) return;
          event.preventDefault();
          event.stopPropagation();
          close();
        }}
      >
        {children}
      </div>
      <div
        className="flex shrink-0 snap-end"
        role="group"
        aria-label={label ? `${label} actions` : 'Row actions'}
      >
        {actions.map((action) => (
          <button
            key={action.key}
            type="button"
            aria-label={action.label}
            className={`flex w-[4.5rem] shrink-0 flex-col items-center justify-center gap-1 border-l font-mono text-chip font-bold uppercase tracking-[0.08em] transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 motion-reduce:transition-none ${
              action.tone === 'danger'
                ? 'border-err-edge bg-err-surface text-err-ink hover:bg-err hover:text-white'
                : action.tone === 'accent'
                  ? 'border-accent/40 bg-accent/15 text-accent-ink hover:bg-accent hover:text-accent-foreground'
                  : 'border-dialog-edge bg-panel-2 text-accent-ink hover:bg-hover'
            }`}
            onClick={() => {
              close();
              action.onSelect();
            }}
          >
            <span aria-hidden="true">{action.icon}</span>
            {action.label}
          </button>
        ))}
      </div>
    </div>
  );
}
