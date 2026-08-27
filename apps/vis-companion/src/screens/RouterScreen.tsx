import { useCallback, useEffect, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { ModelPref, RouterProvider } from '../lib/types';
import { BandButton, Banner, Button, DialogFrame, ListRow, Modal } from '../components/ui';
import { ChevronIcon, MARK_NUDGE } from '../components/icons';
import {
  defaultFirstProviders,
  isProviderAuthed,
  preferredModelFirst,
  providerLimitsLine,
  providerStatusMark,
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
    // A model pick is a QUESTION — six provider rows, and the models under one of
    // them — so it opens in the sheet a question opens in: up from the bottom edge,
    // as tall as it needs and no taller. It used to hand-roll its own scrim around a
    // panel pinned at 92% of the glass, which put half a phone of empty paper between
    // the last provider and the two verbs welded to its foot.
    <Modal size="fit" onDismiss={onClose}>
      <DialogFrame
        title="Model"
        // The pin, and only the pin: `Current: ` said what standing under the title of a
        // model picker already says, in 54px this band does not have to give.
        subtitle={pref?.model ? `${pref.provider ?? '?'}/${pref.model}` : 'No model pinned'}
        closeLabel="Close model picker"
        onClose={onClose}
        actions={
          <>
            <BandButton disabled={pending === 'reload'} onClick={() => void refresh()}>
              {pending === 'reload' ? 'Refreshing…' : 'Refresh'}
            </BandButton>
            {/* The word is the DESTINATION, and the sentence that used to stand in the
                footer is on it: sign-in, OAuth and API keys live in machine settings and
                nowhere else, and every signed-out provider already offers the same trip
                in its own row. Spelled out, the cell took 121px of a 390px band and left
                the pinned model — the one fact this band reports — truncating mid-token. */}
            <BandButton
              aria-label="Manage providers"
              title="Sign-in, OAuth and API keys live in machine settings"
              onClick={manageProviders}
            >
              Providers
            </BandButton>
          </>
        }
      >
        <div className="space-y-3 p-3 sm:p-4">
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
            const status = providerStatusMark(provider);
            const limits = providerLimitsLine(provider);
            const open = expanded === provider.id;
            const authed = isProviderAuthed(provider);

            return (
              <div key={provider.id} className="border border-dialog-edge bg-panel-2">
                <ListRow
                  onClick={() => setExpanded(open ? null : provider.id)}
                  aria-expanded={open}
                >
                  <span className="shrink-0" aria-label={status.label} title={status.label}>
                    <status.Mark className={`${MARK_NUDGE} ${status.tone}`} />
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
      </DialogFrame>
    </Modal>
  );
}
