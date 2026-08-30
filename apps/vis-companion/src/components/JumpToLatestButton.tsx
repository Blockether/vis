import type { ButtonHTMLAttributes } from 'react';
import { ArrowDownIcon } from './icons';

/** The transcript's single floating action returns a reader to the newest turn. */
export function JumpToLatestButton({
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement>) {
  return (
    <button
      type="button"
      className={`inline-flex min-h-8 items-center gap-1.5 rounded-panel border border-dialog-edge bg-button px-3 font-mono text-ui font-bold text-button-foreground shadow-[4px_4px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate,background-color] duration-150 after:absolute after:inset-x-0 after:-top-1.5 after:-bottom-1.5 after:content-[""] hover:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 active:scale-[0.97] starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none mouse:min-h-7 mouse:px-2.5 mouse:text-meta mouse:after:content-none ${className}`}
      {...props}
    >
      <ArrowDownIcon className="size-3" />
      Latest
    </button>
  );
}
