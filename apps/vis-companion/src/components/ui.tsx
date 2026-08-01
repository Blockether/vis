import {
  forwardRef,
  type ButtonHTMLAttributes,
  type InputHTMLAttributes,
  type ReactNode,
} from 'react';

export function Button({
  variant = 'solid',
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  variant?: 'solid' | 'ghost' | 'quiet' | 'danger';
}) {
  // Disabled colours live PER VARIANT, not in the base class: `quiet` has to stay
  // frameless while it is busy, and a shared `disabled:border-edge` would fight it
  // on equal specificity (whoever Tailwind emits last wins).
  const dimmed = 'disabled:border-edge disabled:bg-panel-2 disabled:text-muted';
  const styles = {
    solid: `border-accent bg-accent text-accent-foreground hover:border-accent/85 hover:bg-accent/85 ${dimmed}`,
    ghost: `border-edge-strong bg-transparent text-white hover:border-accent hover:bg-hover hover:text-accent-ink ${dimmed}`,
    // For a SECONDARY action sitting next to a solid primary: two bordered boxes
    // side by side read as rivals, so this one keeps the button's box (transparent
    // border, identical metrics) and only draws a frame on hover/focus.
    quiet:
      'border-transparent bg-transparent text-dialog-hint hover:border-edge-strong hover:bg-hover hover:text-accent-ink disabled:border-transparent disabled:bg-transparent disabled:text-muted',
    danger: `border-err/40 bg-err/10 text-err hover:border-err hover:bg-err hover:text-white ${dimmed}`,
  }[variant];

  return (
    <button
      className={`min-h-7 rounded-none border px-2.5 py-0.5 text-meta font-bold transition-[background-color,border-color,color,opacity,transform,translate,scale,rotate] duration-150 active:scale-[0.98] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:cursor-not-allowed disabled:opacity-100 disabled:shadow-none disabled:active:scale-100 motion-reduce:transition-none sm:min-h-8 sm:px-3 sm:text-ui ${styles} ${className}`}
      {...props}
    />
  );
}

export const Input = forwardRef<HTMLInputElement, InputHTMLAttributes<HTMLInputElement>>(
  function Input({ className = '', ...props }, ref) {
    return (
      <input
        ref={ref}
        className={`min-h-7 w-full rounded-none border border-edge bg-input px-2.5 py-0.5 font-mono text-meta text-white transition-[border-color,box-shadow] duration-150 placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 motion-reduce:transition-none sm:min-h-8 sm:px-3 sm:text-ui ${className}`}
        {...props}
      />
    );
  },
);

export function Card({ children, className = '' }: { children: ReactNode; className?: string }) {
  return (
    <div className={`border border-dialog-edge bg-panel p-4 ${className}`}>{children}</div>
  );
}

export function Banner({ kind, children }: { kind: 'ok' | 'warn' | 'err'; children: ReactNode }) {
  const colors = {
    ok: 'border-ok/50 bg-ok/10 text-ok',
    warn: 'border-warn-strong/60 bg-warn-surface text-warn',
    err: 'border-err/50 bg-err/10 text-err',
  }[kind];

  return (
    <div className={`border px-3 py-2 font-mono text-body ${colors}`} role="status">
      {children}
    </div>
  );
}

export function Section({ title, children }: { title: string; children: ReactNode }) {
  return (
    <section className="space-y-3">
      <h2 className="border-l-2 border-accent px-2 font-mono text-body font-bold uppercase tracking-[0.1em] text-white/70">
        {title}
      </h2>
      {children}
    </section>
  );
}

export function DialogFrame({
  title,
  children,
  footer,
  onClose,
  className = '',
}: {
  title: string;
  children: ReactNode;
  footer?: ReactNode;
  onClose?: () => void;
  className?: string;
}) {
  return (
    <section
      className={`overflow-hidden border border-dialog-edge bg-panel shadow-none transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:shadow-[8px_8px_0_var(--dialog-shadow)] ${className}`}
      role="dialog"
      aria-modal="true"
      aria-label={title}
    >
      <header className="relative flex min-h-9 items-center justify-center bg-dialog-title px-12 py-1.5 text-dialog-title-foreground sm:min-h-8">
        <h2 className="truncate font-mono text-body font-bold tracking-wide">{title}</h2>
        {onClose && (
          <button
            type="button"
            className="absolute inset-y-0 right-0 grid min-w-9 place-items-center border-l border-dialog-title-foreground/20 font-mono text-title text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none sm:min-w-8"
            onClick={onClose}
            aria-label="Close dialog"
          >
            ✕
          </button>
        )}
      </header>
      <div className="border-t border-dialog-edge">{children}</div>
      {footer && (
        <footer className="border-t border-dialog-edge bg-panel-2 px-4 py-2 font-mono text-meta text-dialog-hint">
          {footer}
        </footer>
      )}
    </section>
  );
}
