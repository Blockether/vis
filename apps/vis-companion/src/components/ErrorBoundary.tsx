import { Component, type ErrorInfo, type ReactNode } from 'react';

import { recordDiagnostic } from '../lib/diagnostics';
import { Button } from './ui';

/**
 * The last line between a thrown render and a BLANK app.
 *
 * React unmounts the whole tree when a render throws, and this is a webview: an
 * unmounted tree is a white screen with no console, no back button and nothing
 * to tap. On iOS that reads exactly like a crash, and the session — unsent
 * message, streamed turn, everything — looks lost even though its durable data
 * survives. Catching it here keeps a surface on screen that says what happened
 * and can restart the app in one tap.
 *
 * It deliberately does NOT try to re-render the failed subtree in place: the
 * state that produced the throw is still there, so it would throw again. A
 * reload is the honest recovery: session data stays on the gateway and unsent
 * messages stay on this device.
 */
export class ErrorBoundary extends Component<{ children: ReactNode }, { error: Error | null }> {
  state: { error: Error | null } = { error: null };

  static getDerivedStateFromError(error: Error): { error: Error } {
    return { error };
  }

  componentDidCatch(error: Error, info: ErrorInfo): void {
    // The webview has no attached inspector in production, so leave the stack
    // where a remote-debug session can still find it.
    recordDiagnostic('error', 'react', 'render_failed', {
      error,
      componentStack: info.componentStack,
    });
    globalThis.console?.error?.('vis: render failed', error, info.componentStack);
  }

  render(): ReactNode {
    const { error } = this.state;
    if (!error) return this.props.children;
    return (
      <div className="flex min-h-svh flex-col items-center justify-center gap-4 bg-ink px-[max(1.25rem,env(safe-area-inset-left))] py-[max(1.25rem,env(safe-area-inset-top))] text-center text-white">
        <div className="grid size-9 place-items-center border border-dialog-edge bg-panel-2" aria-hidden="true">
          <img src="/vis-logo.png" alt="" className="h-5 w-6 object-contain" />
        </div>
        <p className="text-title">Vis hit an error</p>
        <p className="max-w-sm text-ui text-footer-muted">
          The screen stopped rendering. Your sessions remain safe on the gateway, and unsent messages stay on this device — reloading picks up where you were.
        </p>
        <p className="max-w-sm break-words font-mono text-chip text-footer-muted">{error.message}</p>
        <Button variant="secondary" onClick={() => window.location.reload()}>
          Reload Vis
        </Button>
      </div>
    );
  }
}
