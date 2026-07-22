import type { ButtonHTMLAttributes, InputHTMLAttributes, ReactNode } from 'react';

export function Button({
  variant = 'solid',
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  variant?: 'solid' | 'ghost' | 'danger';
}) {
  const styles = {
    solid: 'bg-accent text-ink font-medium hover:brightness-110',
    ghost: 'border border-edge text-white/80 hover:bg-panel-2',
    danger: 'border border-err/40 text-err hover:bg-err/10',
  }[variant];
  return (
    <button
      className={`rounded-lg px-4 py-2.5 text-sm transition disabled:opacity-40 ${styles} ${className}`}
      {...props}
    />
  );
}

export function Input({ className = '', ...props }: InputHTMLAttributes<HTMLInputElement>) {
  return (
    <input
      className={`w-full rounded-lg border border-edge bg-panel-2 px-3 py-2.5 font-mono text-sm text-white placeholder:text-white/30 focus:border-accent focus:outline-none ${className}`}
      {...props}
    />
  );
}

export function Card({ children, className = '' }: { children: ReactNode; className?: string }) {
  return (
    <div className={`rounded-xl border border-edge bg-panel p-4 ${className}`}>
      {children}
    </div>
  );
}

export function Banner({ kind, children }: { kind: 'ok' | 'warn' | 'err'; children: ReactNode }) {
  const c = { ok: 'text-ok border-ok/40', warn: 'text-warn border-warn/40', err: 'text-err border-err/40' }[kind];
  return (
    <div className={`rounded-lg border px-3 py-2 text-sm ${c} bg-white/[0.02]`}>{children}</div>
  );
}

export function Section({ title, children }: { title: string; children: ReactNode }) {
  return (
    <section className="space-y-3">
      <h2 className="px-1 text-xs font-semibold uppercase tracking-wide text-white/40">
        {title}
      </h2>
      {children}
    </section>
  );
}
