import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';

export interface SwipeAction {
  key: string;
  label: string;
  icon: ReactNode;
  /**
   * What the action MEANS, in colour. `accent` is the brand yellow a star wears —
   * the same amber the filled glyph is painted in — so "Star" can never read as
   * one more neutral verb beside "Rename".
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
      <div
        className="w-full shrink-0 snap-start bg-panel"
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
                ? 'border-err/40 bg-err/15 text-err hover:bg-err hover:text-white'
                : action.tone === 'accent'
                  ? 'border-accent/40 bg-accent/15 text-accent hover:bg-accent hover:text-accent-foreground'
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
