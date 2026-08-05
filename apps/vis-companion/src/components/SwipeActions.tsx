import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';

export interface SwipeAction {
  key: string;
  label: string;
  icon: ReactNode;
  tone?: 'neutral' | 'danger';
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

export function PencilIcon() {
  return (
    <svg viewBox="0 0 16 16" aria-hidden="true" className="size-4 fill-none stroke-current stroke-[1.4]">
      <path d="M10.6 2.4l3 3L5.9 13.1 2.4 13.6l.5-3.5z" strokeLinejoin="round" />
      <path d="M9.2 3.8l3 3" />
    </svg>
  );
}

export function TrashIcon() {
  return (
    <svg viewBox="0 0 16 16" aria-hidden="true" className="size-4 fill-none stroke-current stroke-[1.4]">
      <path d="M2.8 4.3h10.4M6.3 4.3V2.6h3.4v1.7M4.2 4.3l.7 9h6.2l.7-9" strokeLinejoin="round" />
      <path d="M6.6 6.6v4.4M9.4 6.6v4.4" />
    </svg>
  );
}

/**
 * The favorite mark. Filled is "starred": a filled star is an amber FILL, so it
 * wears the brand yellow (`accent`, #ffc420), not the legible amber ink a text
 * glyph would need. The outline stays adaptive (`stroke-current`) so it reads
 * among the other action glyphs; the fill alone is too quiet to spot at a
 * glance in a list, so the swipe action still shows both states.
 */
export function StarIcon({ filled = false }: { filled?: boolean }) {
  return (
    <svg
      viewBox="0 0 16 16"
      aria-hidden="true"
      className={`size-4 stroke-[1.4] ${filled ? 'fill-accent stroke-accent' : 'fill-none stroke-current'}`}
    >
      <path
        d="M8 1.9l1.9 3.9 4.3.6-3.1 3 .7 4.3L8 11.7l-3.8 2 .7-4.3-3.1-3 4.3-.6z"
        strokeLinejoin="round"
      />
    </svg>
  );
}
