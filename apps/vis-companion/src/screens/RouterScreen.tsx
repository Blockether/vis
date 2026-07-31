import { useCallback, useEffect, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { ModelPref, RouterProvider } from '../lib/types';
import { Banner, Button } from '../components/ui';
import {
  defaultFirstProviders,
  isProviderAuthed,
  preferredModelFirst,
  providerLimitsLine,
  providerStatusDot,
  providerStatusLine,
  useProviderFleet,
} from '../components/ProviderAuth';

interface Props {
  client: GatewayClient;
  /** Session whose model preference a pick writes. */
  sid?: string;
  onClose: () => void;
  onPicked?: (pref: ModelPref) => void;
  /** Jump to gateway settings, where provider accounts and OAuth live. */
  onManageProviders?: () => void;
}

/**
 * Pick the model this session runs on. Nothing here signs in or out: provider
 * ACCOUNTS — OAuth, API keys, sign-out, re-check — live in one place, the
 * gateway's settings, so a credential is never managed from two screens.
 *
 * This dialog reads the same cached `/v1/router` fleet those settings write, so
 * a sign-in there is reflected here without a refetch, and a signed-out
 * provider is shown as such with a direct way over to fix it.
 */
export function ProviderRouterDialog({ client, sid, onClose, onPicked, onManageProviders }: Props) {
  const fleet = useProviderFleet(client);
  const { providers, err, note, pending, refresh } = fleet;
  const [pref, setPref] = useState<ModelPref | null>(null);
  const [expanded, setExpanded] = useState<string | null>(null);
  const [picking, setPicking] = useState<string | null>(null);

  const loadPref = useCallback(
    async (signal?: AbortSignal) => {
      if (!sid) return;
      try {
        const current = await client.sessionModel(sid, signal);
        if (signal?.aborted) return;
        setPref(current);
      } catch {
        // The fleet is the point of this dialog: an unreadable pin paints as
        // "no model pinned" instead of blocking the picker.
      }
    },
    [client, sid],
  );

  useEffect(() => {
    const controller = new AbortController();
    void loadPref(controller.signal);
    return () => controller.abort();
  }, [loadPref]);

  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') onClose();
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);

  async function pick(provider: RouterProvider, model: string) {
    if (!sid) return;
    setPicking(`${provider.id}:${model}`);
    try {
      const next = await client.setSessionModel(sid, provider.id, model);
      setPref(next);
      if (next) onPicked?.(next);
      onClose();
    } catch (e) {
      fleet.setErr((e as Error).message);
    } finally {
      setPicking(null);
    }
  }

  function manageProviders() {
    onClose();
    onManageProviders?.();
  }

  return (
    <div
      className="fixed inset-0 z-50 flex items-end justify-center bg-ink/85 p-0 pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] backdrop-blur-[2px] transition-opacity duration-200 starting:opacity-0 motion-reduce:transition-none sm:items-center sm:p-5"
      onMouseDown={(event) => {
        if (event.target === event.currentTarget) onClose();
      }}
    >
      <section
        className="flex h-[92%] max-h-[calc(100%-env(safe-area-inset-top))] w-full max-w-3xl flex-col overflow-hidden border-x border-t border-dialog-edge bg-panel shadow-none transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-6 starting:opacity-0 motion-reduce:transition-none sm:h-auto sm:max-h-full sm:border sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:starting:translate-y-2"
        role="dialog"
        aria-modal="true"
        aria-labelledby="provider-router-title"
      >
        <header className="flex min-h-14 shrink-0 items-center gap-2 bg-dialog-title px-3 text-dialog-title-foreground sm:px-4">
          <div className="min-w-0 flex-1 py-2">
            <h2 id="provider-router-title" className="truncate font-mono text-title font-bold tracking-wide">
              Model
            </h2>
            <p className="truncate font-mono text-ui text-dialog-title-foreground/70">
              {pref?.model ? `${pref.provider ?? '?'} / ${pref.model}` : 'No model pinned'}
            </p>
          </div>
          <button
            type="button"
            className="min-h-10 shrink-0 border border-dialog-title-foreground/25 px-3 font-mono text-ui text-dialog-title-foreground/80 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none"
            onClick={onClose}
          >
            Close
          </button>
        </header>

        <div className="flex-1 touch-pan-y space-y-2 overflow-x-hidden overflow-y-auto overscroll-contain border-t border-dialog-edge p-3 pb-[max(0.75rem,env(safe-area-inset-bottom))] sm:p-4">
          {err && <Banner kind="err">{err}</Banner>}
          {note && <Banner kind="ok">{note}</Banner>}

          {providers === null && (
            <p className="py-8 text-center font-mono text-ui text-dialog-hint">Loading models…</p>
          )}

          {providers?.length === 0 && (
            <p className="py-8 text-center font-mono text-ui text-dialog-hint">
              No providers configured.
            </p>
          )}

          {defaultFirstProviders(providers ?? []).map((provider) => {
            const state = providerStatusDot(provider);
            const limits = providerLimitsLine(provider);
            const open = expanded === provider.id;
            const authed = isProviderAuthed(provider);
            const pinnedHere = pref?.provider === provider.id;

            return (
              <div
                key={provider.id}
                className={`border bg-panel-2 ${open ? 'border-dialog-hint-key' : 'border-dialog-edge'}`}
              >
                <button
                  type="button"
                  className="flex min-h-14 w-full items-center gap-3 px-3 py-2 text-left transition-colors hover:bg-hover focus-visible:bg-hover focus-visible:outline-none"
                  onClick={() => setExpanded(open ? null : provider.id)}
                  aria-expanded={open}
                >
                  <span className="min-w-0 flex-1">
                    <span className="flex min-w-0 items-center gap-2">
                      <span className="truncate font-mono text-body font-bold text-white">
                        {provider.label}
                      </span>
                      {pinnedHere && (
                        <span className="shrink-0 border border-accent-ink px-1 font-mono text-meta text-accent-ink">
                          in use
                        </span>
                      )}
                    </span>
                    <span className="mt-0.5 block truncate font-mono text-ui text-dialog-hint">
                      {limits ?? `${provider.models.length} models`}
                    </span>
                  </span>
                  <span className={`shrink-0 font-mono text-meta ${state.tone}`}>{state.label}</span>
                  <span className="shrink-0 font-mono text-meta text-dialog-hint">
                    {open ? 'Hide' : 'Show'}
                  </span>
                </button>

                {open && (
                  <div className="space-y-3 border-t border-dialog-edge p-3">
                    <p className="font-mono text-ui text-dialog-hint">
                      {providerStatusLine(provider)}
                    </p>

                    {!authed && (
                      <Button className="w-full" onClick={manageProviders}>
                        Sign in — machine settings
                      </Button>
                    )}

                    {sid && provider.models.length > 0 && (
                      <ul className="grid grid-cols-1 gap-2 sm:grid-cols-2">
                        {preferredModelFirst(provider.models, provider.default_model).map((model) => {
                          const active = pref?.provider === provider.id && pref?.model === model;
                          const busy = picking === `${provider.id}:${model}`;
                          return (
                            <li key={model} className="min-w-0">
                              <button
                                type="button"
                                className={`flex min-h-12 w-full items-center gap-2 border px-3 py-2 text-left font-mono text-ui transition-colors hover:bg-hover focus-visible:bg-hover focus-visible:outline-none ${
                                  active
                                    ? 'border-accent-ink bg-hover text-accent-ink'
                                    : 'border-dialog-edge text-white/85'
                                }`}
                                disabled={busy}
                                onClick={() => void pick(provider, model)}
                                aria-pressed={active}
                              >
                                <span className="min-w-0 flex-1 truncate">{model}</span>
                                {(active || busy) && (
                                  <span className="shrink-0 font-mono text-meta text-dialog-hint">
                                    {busy ? 'Picking…' : 'Current'}
                                  </span>
                                )}
                              </button>
                            </li>
                          );
                        })}
                      </ul>
                    )}
                  </div>
                )}
              </div>
            );
          })}
        </div>

        <footer className="flex shrink-0 flex-col gap-2 border-t border-dialog-edge p-3 pb-[max(0.75rem,env(safe-area-inset-bottom))] sm:flex-row sm:items-center sm:justify-between sm:p-4">
          <p className="font-mono text-ui text-dialog-hint">
            Sign-in, OAuth, and API keys live in machine settings.
          </p>
          <div className="flex gap-2">
            <Button
              variant="ghost"
              disabled={pending === 'reload'}
              onClick={() => void refresh()}
            >
              {pending === 'reload' ? 'Refreshing…' : 'Refresh'}
            </Button>
            <Button onClick={manageProviders}>Manage providers</Button>
          </div>
        </footer>
      </section>
    </div>
  );
}
