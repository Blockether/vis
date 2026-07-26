import { useCallback, useEffect, useRef, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { AuthFlow, ModelPref, RouterProvider } from '../lib/types';
import { Banner, Button, Input } from '../components/ui';

interface Props {
  client: GatewayClient;
  /** Session whose model preference a pick writes. Omit for auth-only use. */
  sid?: string;
  onClose: () => void;
  onPicked?: (pref: ModelPref) => void;
}

/** How long to keep polling a device flow before giving up on our side. */
const DEVICE_POLL_CEILING_MS = 15 * 60 * 1000;

function isAuthed(provider: RouterProvider): boolean {
  return provider.status?.is_authenticated === true;
}

/**
 * Open an OAuth URL in the system browser. `window.open` is the one call that
 * works identically on web, iOS, and Android under Capacitor's WebView, so the
 * app pulls in no extra native plugin to sign a provider in.
 */
function openExternal(url: string): void {
  window.open(url, '_blank', 'noopener,noreferrer');
}

function statusDot(provider: RouterProvider) {
  return isAuthed(provider)
    ? { glyph: '●', tone: 'text-ok', label: 'Signed in' }
    : { glyph: '○', tone: 'text-dialog-hint', label: 'Signed out' };
}

function limitsLine(provider: RouterProvider): string | null {
  const rows = provider.limits?.rows;
  if (!rows?.length) return null;
  return rows
    .slice(0, 2)
    .map((row) => [row.label, row.value].filter(Boolean).join(' '))
    .join(' · ');
}

/**
 * The companion's provider router: every configured provider with its live
 * auth state, its models, and — new here — the ability to sign in and out
 * WITHOUT a terminal.
 *
 * The daemon runs the whole OAuth exchange. `device` providers (GitHub
 * Copilot) show a code and finish by polling, which is the only flow that
 * needs nothing pasted back and is therefore the best phone UX. `pkce`
 * providers (Anthropic, Codex) open a browser and take the final redirect URL
 * back. No token, verifier, or device code ever reaches this device.
 */
export function ProviderRouterDialog({ client, sid, onClose, onPicked }: Props) {
  const [providers, setProviders] = useState<RouterProvider[] | null>(null);
  const [pref, setPref] = useState<ModelPref | null>(null);
  const [expanded, setExpanded] = useState<string | null>(null);
  const [err, setErr] = useState<string | null>(null);
  const [note, setNote] = useState<string | null>(null);
  const [pending, setPending] = useState<string | null>(null);
  const [flow, setFlow] = useState<AuthFlow | null>(null);
  const [redirectUrl, setRedirectUrl] = useState('');
  const [apiKey, setApiKey] = useState('');
  const pollRef = useRef<number | null>(null);

  const stopPolling = useCallback(() => {
    if (pollRef.current !== null) {
      window.clearTimeout(pollRef.current);
      pollRef.current = null;
    }
  }, []);

  const load = useCallback(
    async (signal?: AbortSignal) => {
      try {
        const rows = await client.router(signal);
        if (signal?.aborted) return;
        setProviders(rows);
        setErr(null);
        if (sid) {
          const current = await client.sessionModel(sid, signal);
          if (signal?.aborted) return;
          setPref(current);
        }
      } catch (e) {
        if (signal?.aborted) return;
        setErr((e as Error).message);
        setProviders([]);
      }
    },
    [client, sid],
  );

  useEffect(() => {
    const controller = new AbortController();
    void load(controller.signal);
    return () => controller.abort();
  }, [load]);

  useEffect(() => stopPolling, [stopPolling]);

  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') onClose();
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);

  async function pick(provider: RouterProvider, model: string) {
    if (!sid) return;
    setPending(`${provider.id}:${model}`);
    try {
      const next = await client.setSessionModel(sid, provider.id, model);
      setPref(next);
      if (next) onPicked?.(next);
      onClose();
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  /**
   * Poll a device flow until the daemon reports a verdict.
   *
   * Self-scheduling: the next poll is armed only AFTER the previous one
   * settles, so a slow gateway can never stack overlapping requests the way a
   * fixed `setInterval` would. The cadence is the provider's own
   * `interval_ms` (GitHub rejects faster polling), floored at 2s.
   */
  function watchDeviceFlow(started: AuthFlow) {
    const deadline = Date.now() + DEVICE_POLL_CEILING_MS;
    const every = Math.max(2000, started.interval_ms ?? 5000);
    stopPolling();
    const tick = () => {
      pollRef.current = window.setTimeout(() => {
        void (async () => {
          if (Date.now() > deadline) {
            stopPolling();
            setFlow(null);
            setErr('Authorization timed out. Start again when ready.');
            return;
          }
          try {
            const verdict = await client.pollProviderAuth(started.provider_id, started.flow_id);
            if (pollRef.current === null) return;
            if (verdict.status === 'pending') {
              tick();
              return;
            }
            stopPolling();
            setFlow(null);
            if (verdict.status === 'ok') {
              setNote(`Signed in to ${started.provider_id}.`);
              await load();
            } else {
              setErr(verdict.message ?? 'Authorization failed.');
            }
          } catch (e) {
            stopPolling();
            setFlow(null);
            setErr((e as Error).message);
          }
        })();
      }, every);
    };
    tick();
  }

  async function signIn(provider: RouterProvider) {
    setPending(`auth:${provider.id}`);
    setErr(null);
    setNote(null);
    setRedirectUrl('');
    setApiKey('');
    try {
      const started = await client.startProviderAuth(provider.id);
      setFlow(started);
      if (started.url) await openExternal(started.url);
      if (started.kind === 'device') watchDeviceFlow(started);
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function finishPkce() {
    if (!flow || !redirectUrl.trim()) return;
    setPending('auth:complete');
    try {
      const verdict = await client.completeProviderAuth(
        flow.provider_id,
        flow.flow_id,
        redirectUrl.trim(),
      );
      if (verdict.status === 'ok') {
        setNote(`Signed in to ${flow.provider_id}.`);
        setFlow(null);
        setRedirectUrl('');
        await load();
      } else {
        setErr(verdict.message ?? 'Authorization failed.');
      }
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  /**
   * Finish an `api-key` flow. The key goes straight to the daemon, which
   * writes it into ITS config — this device never stores a credential.
   */
  async function finishApiKey() {
    if (!flow || !apiKey.trim()) return;
    setPending('auth:complete');
    try {
      const verdict = await client.submitProviderKey(
        flow.provider_id,
        flow.flow_id,
        apiKey.trim(),
      );
      if (verdict.status === 'ok') {
        setNote(`Signed in to ${flow.provider_id}.`);
        setFlow(null);
        setApiKey('');
        await load();
      } else {
        setErr(verdict.message ?? 'Authorization failed.');
      }
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function cancelFlow() {
    const current = flow;
    stopPolling();
    setFlow(null);
    setRedirectUrl('');
    setApiKey('');
    if (current) {
      try {
        await client.cancelProviderAuth(current.provider_id, current.flow_id);
      } catch {
        // A flow the daemon already forgot is exactly the state we want.
      }
    }
  }

  async function signOut(provider: RouterProvider) {
    setPending(`out:${provider.id}`);
    setErr(null);
    setNote(null);
    try {
      await client.logoutProvider(provider.id);
      setNote(`Signed out of ${provider.label}.`);
      await load();
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  return (
    <div
      className="fixed inset-0 z-50 flex items-end justify-center bg-ink/85 p-0 pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] backdrop-blur-[2px] transition-opacity duration-200 starting:opacity-0 motion-reduce:transition-none sm:items-center sm:p-5"
      onMouseDown={(event) => {
        if (event.target === event.currentTarget) onClose();
      }}
    >
      <section
        className="flex h-[92dvh] max-h-[calc(100dvh-env(safe-area-inset-top))] w-full max-w-3xl flex-col overflow-hidden border-x border-t border-dialog-edge bg-panel shadow-none transition-[opacity,transform] duration-200 starting:translate-y-6 starting:opacity-0 motion-reduce:transition-none sm:h-auto sm:max-h-[calc(100dvh-2.5rem)] sm:border sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:starting:translate-y-2"
        role="dialog"
        aria-modal="true"
        aria-labelledby="provider-router-title"
      >
        <header className="flex min-h-12 shrink-0 items-center bg-dialog-title text-dialog-title-foreground">
          <div className="min-w-0 flex-1 px-3 py-2 sm:px-4">
            <h2
              id="provider-router-title"
              className="truncate font-mono text-xs font-bold tracking-wide"
            >
              Router — providers &amp; models
            </h2>
            <p className="truncate font-mono text-[10px] text-dialog-title-foreground/70">
              {pref?.model ? `Current: ${pref.provider ?? '?'}/${pref.model}` : 'No model pinned'}
            </p>
          </div>
          <button
            type="button"
            className="grid min-h-12 min-w-12 place-items-center border-l border-dialog-title-foreground/20 font-mono text-base leading-none text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none"
            onClick={onClose}
            aria-label="Close router"
          >
            ✕
          </button>
        </header>

        <div className="flex-1 space-y-3 overflow-y-auto overscroll-contain border-t border-dialog-edge p-3 pb-[max(0.75rem,env(safe-area-inset-bottom))] sm:p-4">
          {err && <Banner kind="err">{err}</Banner>}
          {note && <Banner kind="ok">{note}</Banner>}

          {flow && (
            <div className="space-y-3 border border-accent/50 bg-panel-2 p-3">
              <p className="font-mono text-xs font-bold text-white">
                {flow.kind === 'device' ? 'Waiting for authorization…' : 'Finish sign-in'}
              </p>

              {flow.user_code && (
                <p className="select-all break-all border border-dialog-edge bg-input px-3 py-2 text-center font-mono text-2xl font-bold tracking-[0.2em] text-accent">
                  {flow.user_code}
                </p>
              )}

              {flow.instructions?.length ? (
                <ol className="list-inside list-decimal space-y-1 font-mono text-[11px] text-dialog-hint">
                  {flow.instructions.map((line) => (
                    <li key={line}>{line}</li>
                  ))}
                </ol>
              ) : null}

              {flow.url && (
                <Button
                  variant="ghost"
                  className="w-full"
                  onClick={() => void openExternal(flow.url as string)}
                >
                  Open sign-in page again
                </Button>
              )}

              {flow.kind === 'pkce' && (
                <div className="space-y-2">
                  <label
                    className="block font-mono text-[10px] uppercase tracking-[0.1em] text-dialog-hint"
                    htmlFor="redirect-url"
                  >
                    Paste the final redirect URL
                  </label>
                  <Input
                    id="redirect-url"
                    value={redirectUrl}
                    inputMode="url"
                    autoCapitalize="off"
                    autoCorrect="off"
                    spellCheck={false}
                    placeholder="https://…?code=…"
                    onChange={(event) => setRedirectUrl(event.target.value)}
                  />
                </div>
              )}

              {flow.kind === 'api-key' && (
                <div className="space-y-2">
                  <label
                    className="block font-mono text-[10px] uppercase tracking-[0.1em] text-dialog-hint"
                    htmlFor="provider-api-key"
                  >
                    Paste the provider API key
                  </label>
                  <Input
                    id="provider-api-key"
                    type="password"
                    value={apiKey}
                    autoCapitalize="off"
                    autoCorrect="off"
                    spellCheck={false}
                    placeholder="sk-…"
                    onChange={(event) => setApiKey(event.target.value)}
                  />
                </div>
              )}

              <div className="flex flex-col gap-2 sm:flex-row">
                {flow.kind === 'pkce' && (
                  <Button
                    className="flex-1"
                    disabled={!redirectUrl.trim() || pending === 'auth:complete'}
                    onClick={() => void finishPkce()}
                  >
                    {pending === 'auth:complete' ? 'Finishing…' : 'Finish sign-in'}
                  </Button>
                )}
                {flow.kind === 'api-key' && (
                  <Button
                    className="flex-1"
                    disabled={!apiKey.trim() || pending === 'auth:complete'}
                    onClick={() => void finishApiKey()}
                  >
                    {pending === 'auth:complete' ? 'Saving…' : 'Save key'}
                  </Button>
                )}
                <Button variant="ghost" className="flex-1" onClick={() => void cancelFlow()}>
                  Cancel
                </Button>
              </div>
            </div>
          )}

          {providers === null && (
            <p className="py-8 text-center font-mono text-xs text-dialog-hint">Loading router…</p>
          )}

          {providers?.length === 0 && (
            <p className="py-8 text-center font-mono text-xs text-dialog-hint">
              No providers configured.
            </p>
          )}

          {providers?.map((provider) => {
            const dot = statusDot(provider);
            const limits = limitsLine(provider);
            const open = expanded === provider.id;
            const authed = isAuthed(provider);

            return (
              <div key={provider.id} className="border border-dialog-edge bg-panel-2">
                <button
                  type="button"
                  className="flex w-full min-h-12 items-center gap-2 px-3 py-2 text-left transition-colors hover:bg-hover focus-visible:bg-hover focus-visible:outline-none"
                  onClick={() => setExpanded(open ? null : provider.id)}
                  aria-expanded={open}
                >
                  <span className={`font-mono text-xs ${dot.tone}`} aria-label={dot.label}>
                    {dot.glyph}
                  </span>
                  <span className="min-w-0 flex-1">
                    <span className="block truncate font-mono text-xs font-bold text-white">
                      {provider.label}
                    </span>
                    <span className="block truncate font-mono text-[10px] text-dialog-hint">
                      {limits ?? `${provider.models.length} models`}
                    </span>
                  </span>
                  <span className="font-mono text-[10px] text-dialog-hint">{open ? '▾' : '▸'}</span>
                </button>

                {open && (
                  <div className="space-y-2 border-t border-dialog-edge p-3">
                    <div className="flex flex-col gap-2 sm:flex-row">
                      {!authed && (
                        <Button
                          className="flex-1"
                          disabled={pending === `auth:${provider.id}`}
                          onClick={() => void signIn(provider)}
                        >
                          {pending === `auth:${provider.id}` ? 'Starting…' : 'Sign in'}
                        </Button>
                      )}
                      {authed && (
                        <Button
                          variant="danger"
                          className="flex-1"
                          disabled={pending === `out:${provider.id}`}
                          onClick={() => void signOut(provider)}
                        >
                          {pending === `out:${provider.id}` ? 'Signing out…' : 'Sign out'}
                        </Button>
                      )}
                    </div>

                    {sid && provider.models.length > 0 && (
                      <ul className="divide-y divide-dialog-edge border border-dialog-edge">
                        {provider.models.map((model) => {
                          const active =
                            pref?.provider === provider.id && pref?.model === model;
                          return (
                            <li key={model}>
                              <button
                                type="button"
                                className={`flex w-full min-h-11 items-center gap-2 px-3 py-2 text-left font-mono text-xs transition-colors hover:bg-hover focus-visible:bg-hover focus-visible:outline-none ${
                                  active ? 'bg-hover text-accent' : 'text-white/85'
                                }`}
                                disabled={pending === `${provider.id}:${model}`}
                                onClick={() => void pick(provider, model)}
                              >
                                <span className="w-3 shrink-0">{active ? '›' : ''}</span>
                                <span className="min-w-0 flex-1 truncate">{model}</span>
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
      </section>
    </div>
  );
}
