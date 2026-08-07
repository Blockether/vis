import { useCallback, useEffect, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { ModelPref, RouterProvider } from '../lib/types';
import { Banner, Button, DialogHeader, ListRow } from '../components/ui';
import { ChevronIcon } from '../components/icons';
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
        <DialogHeader
          titleId="provider-router-title"
          title="Model"
          subtitle={pref?.model ? `Current: ${pref.provider ?? '?'}/${pref.model}` : 'No model pinned'}
          closeLabel="Close model picker"
          onClose={onClose}
        />

        <div className="flex-1 touch-pan-y space-y-3 overflow-x-hidden overflow-y-auto overscroll-contain border-t border-dialog-edge p-3 pb-[max(0.75rem,env(safe-area-inset-bottom))] sm:p-4">
          {err && <Banner kind="err">{err.text}</Banner>}
          {note && <Banner kind="ok">{note.text}</Banner>}

          {providers === null && (
            <p className="py-8 text-center font-mono text-body text-dialog-hint">Loading models…</p>
          )}

          {providers?.length === 0 && (
            <p className="py-8 text-center font-mono text-body text-dialog-hint">
              No providers configured.
            </p>
          )}

          {defaultFirstProviders(providers ?? []).map((provider) => {
            const dot = providerStatusDot(provider);
            const limits = providerLimitsLine(provider);
            const open = expanded === provider.id;
            const authed = isProviderAuthed(provider);

            return (
              <div key={provider.id} className="border border-dialog-edge bg-panel-2">
                <ListRow
                  onClick={() => setExpanded(open ? null : provider.id)}
                  aria-expanded={open}
                >
                  <span className={`font-mono text-body ${dot.tone}`} aria-label={dot.label}>
                    {dot.glyph}
                  </span>
                  <span className="min-w-0 flex-1">
                    <span className="block truncate font-mono text-body font-bold text-white">
                      {provider.label}
                    </span>
                    <span className="block truncate font-mono text-meta text-dialog-hint">
                      {limits ?? `${provider.models.length} models`}
                    </span>
                  </span>
                  <ChevronIcon open={open} className="size-3.5 text-dialog-hint" />
                </ListRow>

                {open && (
                  <div className="space-y-2 border-t border-dialog-edge p-3">
                    <p className="font-mono text-meta text-dialog-hint">
                      {providerStatusLine(provider)}
                    </p>

                    {!authed && (
                      <Button className="w-full" onClick={manageProviders}>
                        Sign in — machine settings
                      </Button>
                    )}

                    {sid && provider.models.length > 0 && (
                      <ul className="grid grid-cols-2 gap-2 sm:grid-cols-3">
                        {preferredModelFirst(provider.models, provider.default_model).map((model) => {
                          const active = pref?.provider === provider.id && pref?.model === model;
                          return (
                            <li key={model} className="min-w-0">
                              <ListRow
                                isFramed
                                isSelected={active}
                                disabled={picking === `${provider.id}:${model}`}
                                onClick={() => void pick(provider, model)}
                                aria-pressed={active}
                              >
                                <span className="min-w-0 flex-1 truncate font-mono text-ui">
                                  {model}
                                </span>
                              </ListRow>
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
          <p className="font-mono text-meta text-dialog-hint">
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
