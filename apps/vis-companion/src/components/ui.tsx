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
  variant?: 'solid' | 'ghost' | 'danger';
}) {
  const styles = {
    solid:
      'border-dialog-title bg-dialog-title text-dialog-title-foreground hover:bg-accent-2',
    ghost:
      'border-dialog-edge bg-panel text-dialog-hint-key hover:border-edge-strong hover:bg-hover hover:text-white',
    danger: 'border-err/50 bg-panel text-err hover:bg-warn-surface',
  }[variant];

  return (
    <button
      className={`min-h-11 rounded-none border px-3.5 py-2 text-xs font-semibold transition-[background-color,border-color,color,transform] duration-150 active:scale-[0.98] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:cursor-not-allowed disabled:border-dialog-edge disabled:bg-transparent disabled:text-dialog-hint disabled:shadow-none disabled:active:scale-100 motion-reduce:transition-none sm:min-h-9 ${styles} ${className}`}
      {...props}
    />
  );
}

export const Input = forwardRef<HTMLInputElement, InputHTMLAttributes<HTMLInputElement>>(
  function Input({ className = '', ...props }, ref) {
    return (
      <input
        ref={ref}
        className={`min-h-11 w-full rounded-none border border-dialog-edge bg-input px-3 py-2 font-mono text-base text-white transition-[border-color,box-shadow] duration-150 placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 motion-reduce:transition-none sm:min-h-9 sm:text-xs ${className}`}
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
    <div className={`border px-3 py-2 font-mono text-xs ${colors}`} role="status">
      {children}
    </div>
  );
}

export function Section({ title, children }: { title: string; children: ReactNode }) {
  return (
    <section className="space-y-3">
      <h2 className="border-l-2 border-accent px-2 font-mono text-xs font-bold uppercase tracking-[0.1em] text-white/70">
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
      className={`overflow-hidden border border-dialog-edge bg-panel shadow-none transition-[opacity,transform] duration-200 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:shadow-[8px_8px_0_var(--dialog-shadow)] ${className}`}
      role="dialog"
      aria-modal="true"
      aria-label={title}
    >
      <header className="relative flex min-h-11 items-center justify-center bg-dialog-title px-12 py-2 text-dialog-title-foreground sm:min-h-10">
        <h2 className="truncate font-mono text-xs font-bold tracking-wide">{title}</h2>
        {onClose && (
          <button
            type="button"
            className="absolute inset-y-0 right-0 grid min-w-11 place-items-center border-l border-dialog-title-foreground/20 font-mono text-base leading-none text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none sm:min-w-10"
            onClick={onClose}
            aria-label="Close dialog"
          >
            ✕
          </button>
        )}
      </header>
      <div className="border-t border-dialog-edge">{children}</div>
      {footer && (
        <footer className="border-t border-dialog-edge bg-panel-2 px-4 py-2 font-mono text-[10px] text-dialog-hint">
          {footer}
        </footer>
      )}
    </section>
  );
}
