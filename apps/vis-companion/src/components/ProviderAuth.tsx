import { useCallback, useEffect, useRef, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { AuthFlow, ProviderLimitRow, ProviderPreset, RouterProvider } from '../lib/types';
import { Banner, Button, Input } from './ui';

/** How long to keep polling a device flow before giving up on our side. */
const DEVICE_POLL_CEILING_MS = 15 * 60 * 1000;

/** A live limits probe can invalidate credentials that still exist locally. */
function limitsAuthError(provider: RouterProvider): string | null {
  const report = provider.limits;
  if (report?.status !== 'unauthenticated') return null;
  return report.dynamic?.note ?? report.error?.message ?? 'Provider credentials were rejected.';
}

export function isProviderAuthed(provider: RouterProvider): boolean {
  return provider.status?.is_authenticated === true && !limitsAuthError(provider);
}

/** Present the explicit router default first without mutating the gateway fleet. */
export function defaultFirstProviders(providers: RouterProvider[]): RouterProvider[] {
  return [
    ...providers.filter((provider) => provider.is_default),
    ...providers.filter((provider) => !provider.is_default),
  ];
}

/** Present a provider's explicit default model first, preserving every other model's order. */
export function preferredModelFirst(models: string[], preferred?: string | null): string[] {
  if (!preferred) return [...models];
  return [...models.filter((model) => model === preferred), ...models.filter((model) => model !== preferred)];
}

/**
 * Open an OAuth URL in the system browser. `window.open` is the one call that
 * works identically on web, iOS, and Android under Capacitor's WebView, so the
 * app pulls in no extra native plugin to sign a provider in.
 */
export function openProviderUrl(url: string): void {
  window.open(url, '_blank', 'noopener,noreferrer');
}

export function providerStatusDot(provider: RouterProvider) {
  if (provider.status?.error || limitsAuthError(provider)) {
    return { glyph: '●', tone: 'text-err', label: 'Authentication error' };
  }
  return isProviderAuthed(provider)
    ? { glyph: '●', tone: 'text-ok', label: 'Signed in' }
    : { glyph: '○', tone: 'text-dialog-hint', label: 'Signed out' };
}

/** `12m`, `3h`, `6d` — coarse on purpose: this is a hint, not a countdown. */
function humanMs(ms: number): string {
  const minutes = Math.round(ms / 60_000);
  if (minutes < 60) return `${Math.max(1, minutes)}m`;
  const hours = Math.round(minutes / 60);
  if (hours < 48) return `${hours}h`;
  return `${Math.round(hours / 24)}d`;
}

/**
 * The daemon reports WHERE a credential came from as a machine token
 * (`auth-file`, `env-var`, …); say it in words instead.
 */
const SOURCE_LABELS: Record<string, string> = {
  'auth-file': 'signed-in session',
  keychain: 'system keychain',
  'env-var': 'environment key',
  config: 'config key',
  'api-key': 'API key',
};

/**
 * One line describing where the credential came from and how long it is good
 * for. Token previews (`oauth_token_preview`, `api_key_preview`) are
 * deliberately never rendered — the daemon owns the secret and this device has
 * no reason to echo even a fragment of it.
 */
export function providerStatusLine(provider: RouterProvider): string {
  const status = provider.status;
  const liveAuthError = limitsAuthError(provider);
  if (status?.error) return status.error;
  if (liveAuthError) return liveAuthError;
  if (!status?.is_authenticated) return status?.detail ?? 'Not signed in';
  const source = status.source ? (SOURCE_LABELS[status.source] ?? status.source) : undefined;
  const parts = [status.detail, source, status.account_type].filter(
    (part): part is string => !!part && part.length > 0,
  );
  if (typeof status.expires_in_ms === 'number' && status.expires_in_ms > 0) {
    parts.push(`expires in ${humanMs(status.expires_in_ms)}`);
  }
  return parts.length ? parts.join(' · ') : 'Signed in';
}

/**
 * A `note` only reads as a QUOTA when it is short (`100.0% remaining`). Longer
 * prose ("OpenAI Codex did not report this quota window.") is an explanation,
 * not a value, and would bury the real numbers in a one-line summary.
 */
const QUOTA_NOTE_MAX = 24;

function quotaNote(row: ProviderLimitRow): string | null {
  const note = row.note?.trim();
  return note && note.length <= QUOTA_NOTE_MAX ? note : null;
}

/**
 * A percentage window: the provider reports a 0-100 scale (`limit` 100) rather
 * than raw token counts. The gateway fills `remaining` for those, and the TUI
 * footer renders exactly that number — so this surface must read `remaining`
 * too, or the same account shows `6%` here and `94%` there.
 */
function percentRemaining(row: ProviderLimitRow): number | null {
  if (typeof row.limit !== 'number' || row.limit !== 100) return null;
  if (typeof row.remaining === 'number') return Math.round(row.remaining);
  if (typeof row.used === 'number') return Math.round(100 - row.used);
  return null;
}

function hasQuotaValue(row: ProviderLimitRow): boolean {
  if (row.is_unlimited) return true;
  if (percentRemaining(row) !== null) return true;
  if (typeof row.used === 'number' && typeof row.limit === 'number' && row.limit > 0) return true;
  return quotaNote(row) !== null;
}

/**
 * One quota window in words. Providers are inconsistent on purpose here: some
 * send `used`/`limit`, some only a short note, some neither (the window exists
 * but went unreported), so each case gets its own honest rendering rather than
 * a fabricated `0%`.
 */
function limitRowText(row: ProviderLimitRow): string | null {
  const label = row.label?.trim();
  if (!label) return null;
  if (row.is_unlimited) return `${label} unlimited`;
  const left = percentRemaining(row);
  if (left !== null) return `${label} ${left}% left`;
  if (typeof row.used === 'number' && typeof row.limit === 'number' && row.limit > 0) {
    return `${label} ${Math.round((row.used / row.limit) * 100)}% used`;
  }
  const note = quotaNote(row);
  return note ? `${label} ${note}` : label;
}

/**
 * The daemon's limits REPORT condensed to one line — `Claude 5h 16% · Claude
 * 7d 66%`. The report is the raw provider payload (`dynamic.limits`), so a
 * provider that reports nothing yields null rather than an empty shell.
 *
 * Windows that actually carry a number are shown FIRST: a provider that sends
 * one unreported window ahead of two real ones must not spend the summary on
 * the empty one.
 */
export function providerLimitsLine(provider: RouterProvider): string | null {
  const report = provider.limits;
  if (report?.error?.message) return report.error.message;
  const rows = report?.dynamic?.limits;
  if (!rows?.length) return null;
  const ranked = [...rows].sort((a, b) => Number(hasQuotaValue(b)) - Number(hasQuotaValue(a)));
  const text = ranked.slice(0, 2).map(limitRowText).filter(Boolean).join(' · ');
  return text || null;
}

/**
 * A banner and WHOSE it is.
 *
 * `providerId` names the provider whose card the message belongs INSIDE: the
 * status of a provider is part of that provider, never a line floating above
 * the list. `null` is fleet-wide — a reload that failed, presets that could not
 * be read, a provider that no longer exists.
 */
export interface ProviderMessage {
  text: string;
  providerId: string | null;
}

/**
 * The part of a message that has nowhere else to live: fleet-wide, or scoped to
 * a provider these rows do not contain (an add that failed, a provider that was
 * just removed). Everything else is painted by `ProviderNotice` inside its own
 * provider card, so nothing is ever silently swallowed.
 */
export function unscopedMessage(
  message: ProviderMessage | null,
  providers: RouterProvider[] | null,
): ProviderMessage | null {
  if (!message) return null;
  if (message.providerId === null) return message;
  return (providers ?? []).some((row) => row.id === message.providerId) ? null : message;
}

export interface ProviderFleet {
  providers: RouterProvider[] | null;
  /** Patch one row in place (a live status re-check) without a refetch. */
  setProviders: (update: (rows: RouterProvider[] | null) => RouterProvider[] | null) => void;
  err: ProviderMessage | null;
  note: ProviderMessage | null;
  /**
   * Surface a caller-side failure (a model pick, say) in the same banner.
   * `providerId` scopes it to that provider's own card; omitting it makes the
   * message the fleet's.
   */
  setErr: (text: string | null, providerId?: string | null) => void;
  setNote: (text: string | null, providerId?: string | null) => void;
  /** `auth:<id>` · `logout:<id>` · `status:<id>` · `auth:complete` · `reload`. */
  pending: string | null;
  setPending: (value: string | null) => void;
  reload: (signal?: AbortSignal, opts?: { force?: boolean }) => Promise<void>;
  refresh: () => Promise<void>;
}

export interface ProviderAuth extends ProviderFleet {
  flow: AuthFlow | null;
  redirectUrl: string;
  setRedirectUrl: (value: string) => void;
  apiKey: string;
  setApiKey: (value: string) => void;
  signIn: (provider: RouterProvider) => Promise<void>;
  signOut: (provider: RouterProvider) => Promise<void>;
  recheck: (provider: RouterProvider) => Promise<void>;
  finishPkce: () => Promise<void>;
  finishApiKey: () => Promise<void>;
  cancelFlow: () => Promise<void>;
  /**
   * Presets this machine can still add. `null` means nobody has asked the
   * gateway yet; an empty array is the daemon's answer that there is nothing
   * left to configure, and the picker must not exist at all.
   */
  presets: ProviderPreset[] | null;
  loadPresets: () => Promise<void>;
  addProvider: (preset: ProviderPreset, baseUrl?: string) => Promise<void>;
  removeProvider: (provider: RouterProvider) => Promise<void>;
}

/**
 * The gateway's provider fleet, READ-only: every provider with its live auth
 * verdict, models, and quota report, plus the banners and the reload that keep
 * them honest. This is all a model picker needs — signing in is a different
 * job, and lives in `useProviderAuth` below.
 */
export function useProviderFleet(client: GatewayClient): ProviderFleet {
  // Paint whatever the shared router cache already holds (prefetched at
  // connect time) so a screen opens instantly; `reload` revalidates under it.
  const [providers, setProviders] = useState<RouterProvider[] | null>(() => client.cachedRouter());
  const [err, setErrMessage] = useState<ProviderMessage | null>(null);
  const [note, setNoteMessage] = useState<ProviderMessage | null>(null);
  const [pending, setPending] = useState<string | null>(null);

  const setErr = useCallback(
    (text: string | null, providerId: string | null = null) =>
      setErrMessage(text === null ? null : { text, providerId }),
    [],
  );
  const setNote = useCallback(
    (text: string | null, providerId: string | null = null) =>
      setNoteMessage(text === null ? null : { text, providerId }),
    [],
  );

  const reload = useCallback(
    async (signal?: AbortSignal, opts?: { force?: boolean }) => {
      try {
        const rows = await client.router(signal, opts);
        if (signal?.aborted) return;
        setProviders(rows);
        setErr(null);
      } catch (e) {
        if (signal?.aborted) return;
        setErr((e as Error).message);
        setProviders([]);
      }
    },
    [client, setErr, setProviders],
  );

  const refresh = useCallback(async () => {
    setPending('reload');
    setNote(null);
    try {
      await reload(undefined, { force: true });
    } finally {
      setPending(null);
    }
  }, [reload, setNote, setPending]);

  useEffect(() => {
    const controller = new AbortController();
    void reload(controller.signal);
    return () => controller.abort();
  }, [reload]);

  return {
    providers,
    setProviders,
    err,
    note,
    setErr,
    setNote,
    pending,
    setPending,
    reload,
    refresh,
  };
}

/**
 * The whole headless OAuth exchange on top of the fleet, used by the ONE
 * surface that owns provider accounts: gateway settings.
 *
 * The daemon runs the exchange end to end. `device` providers (GitHub Copilot)
 * show a code and finish by polling — the best phone UX, since nothing has to
 * be pasted back. `pkce` providers (Anthropic, Codex) open a browser and take
 * the final redirect URL back. `api-key` providers take the key. No token,
 * verifier, or device code ever lands on this device.
 */
export function useProviderAuth(client: GatewayClient): ProviderAuth {
  const fleet = useProviderFleet(client);
  const { setProviders, setErr, setNote, setPending, reload } = fleet;
  const [flow, setFlow] = useState<AuthFlow | null>(null);
  const [redirectUrl, setRedirectUrl] = useState('');
  const [apiKey, setApiKey] = useState('');
  const [presets, setPresets] = useState<ProviderPreset[] | null>(null);
  const pollRef = useRef<number | null>(null);

  const stopPolling = useCallback(() => {
    if (pollRef.current !== null) {
      window.clearTimeout(pollRef.current);
      pollRef.current = null;
    }
  }, []);

  useEffect(() => stopPolling, [stopPolling]);

  /**
   * Poll a device flow until the daemon reports a verdict.
   *
   * Self-scheduling: the next poll is armed only AFTER the previous one
   * settles, so a slow gateway can never stack overlapping requests the way a
   * fixed `setInterval` would. The cadence is the provider's own
   * `interval_ms` (GitHub rejects faster polling), floored at 2s.
   */
  const watchDeviceFlow = useCallback(
    (started: AuthFlow) => {
      const deadline = Date.now() + DEVICE_POLL_CEILING_MS;
      const every = Math.max(2000, started.interval_ms ?? 5000);
      stopPolling();
      const tick = () => {
        pollRef.current = window.setTimeout(() => {
          void (async () => {
            if (Date.now() > deadline) {
              stopPolling();
              setFlow(null);
              setErr('Authorization timed out. Start again when ready.', started.provider_id);
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
                setNote(`Signed in to ${started.provider_id}.`, started.provider_id);
                await reload(undefined, { force: true });
              } else {
                setErr(verdict.message ?? 'Authorization failed.', started.provider_id);
              }
            } catch (e) {
              stopPolling();
              setFlow(null);
              setErr((e as Error).message, started.provider_id);
            }
          })();
        }, every);
      };
      tick();
    },
    [client, reload, stopPolling],
  );

  const signIn = useCallback(
    async (provider: RouterProvider) => {
      setPending(`auth:${provider.id}`);
      setErr(null);
      setNote(null);
      setRedirectUrl('');
      setApiKey('');
      try {
        const started = await client.startProviderAuth(provider.id);
        setFlow(started);
        if (started.url) openProviderUrl(started.url);
        if (started.kind === 'device') watchDeviceFlow(started);
      } catch (e) {
        setErr((e as Error).message, provider.id);
      } finally {
        setPending(null);
      }
    },
    [client, watchDeviceFlow],
  );

  /**
   * Clear the provider's credentials ON THE DAEMON. Nothing is deleted here —
   * the gateway owns the credential file — and the forced reload makes the
   * next paint show the real post-logout verdict rather than a guess.
   */
  const signOut = useCallback(
    async (provider: RouterProvider) => {
      setPending(`logout:${provider.id}`);
      setErr(null);
      setNote(null);
      try {
        const verdict = await client.logoutProvider(provider.id);
        if (verdict.status === 'error') {
          setErr(verdict.message ?? 'Sign-out failed.', provider.id);
        } else {
          setNote(`Signed out of ${provider.label}.`, provider.id);
        }
        await reload(undefined, { force: true });
      } catch (e) {
        setErr((e as Error).message, provider.id);
      } finally {
        setPending(null);
      }
    },
    [client, reload],
  );

  /** Re-probe ONE provider's auth + limits, live, without re-probing the fleet. */
  const recheck = useCallback(
    async (provider: RouterProvider) => {
      setPending(`status:${provider.id}`);
      setErr(null);
      setNote(null);
      try {
        const [status, limits] = await Promise.all([
          client.providerStatus(provider.id),
          client.providerLimits(provider.id).catch(() => null),
        ]);
        setProviders(
          (rows) =>
            rows?.map((row) =>
              row.id === provider.id ? { ...row, status, ...(limits ? { limits } : {}) } : row,
            ) ?? rows,
        );
        setNote(
          `${provider.label}: ${status.is_authenticated ? 'signed in' : 'signed out'}.`,
          provider.id,
        );
      } catch (e) {
        setErr((e as Error).message, provider.id);
      } finally {
        setPending(null);
      }
    },
    [client],
  );

  const finishPkce = useCallback(async () => {
    if (!flow || !redirectUrl.trim()) return;
    setPending('auth:complete');
    try {
      const verdict = await client.completeProviderAuth(
        flow.provider_id,
        flow.flow_id,
        redirectUrl.trim(),
      );
      if (verdict.status === 'ok') {
        setNote(`Signed in to ${flow.provider_id}.`, flow.provider_id);
        setFlow(null);
        setRedirectUrl('');
        await reload(undefined, { force: true });
      } else {
        setErr(verdict.message ?? 'Authorization failed.', flow.provider_id);
      }
    } catch (e) {
      setErr((e as Error).message, flow.provider_id);
    } finally {
      setPending(null);
    }
  }, [client, flow, redirectUrl, reload]);

  /**
   * Finish an `api-key` flow. The key goes straight to the daemon, which
   * writes it into ITS config — this device never stores a credential.
   */
  const finishApiKey = useCallback(async () => {
    if (!flow || !apiKey.trim()) return;
    setPending('auth:complete');
    try {
      const verdict = await client.submitProviderKey(flow.provider_id, flow.flow_id, apiKey.trim());
      if (verdict.status === 'ok') {
        setNote(`Signed in to ${flow.provider_id}.`, flow.provider_id);
        setFlow(null);
        setApiKey('');
        await reload(undefined, { force: true });
      } else {
        setErr(verdict.message ?? 'Authorization failed.', flow.provider_id);
      }
    } catch (e) {
      setErr((e as Error).message, flow.provider_id);
    } finally {
      setPending(null);
    }
  }, [apiKey, client, flow, reload]);

  const cancelFlow = useCallback(async () => {
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
  }, [client, flow, stopPolling]);

  /**
   * What this machine can still add. Read on demand: the daemon answers with
   * the presets it does NOT already carry,
   * so the picker can never offer a duplicate.
   */
  const loadPresets = useCallback(async () => {
    setPending('presets');
    setErr(null);
    try {
      setPresets(await client.providerPresets());
    } catch (e) {
      setErr((e as Error).message);
      setPresets([]);
    } finally {
      setPending(null);
    }
  }, [client, setErr, setPending]);

  /**
   * Put a preset into THIS machine's fleet and walk straight into its sign-in.
   *
   * The daemon owns config, so the phone only names the preset (and, for a
   * local runtime, where it listens) and repaints from the fleet the gateway
   * answers with. A provider without a credential is useless, so the OAuth /
   * API-key flow starts in the same tap; a local runtime is done the moment it
   * exists.
   */
  const addProvider = useCallback(
    async (preset: ProviderPreset, baseUrl?: string) => {
      setPending(`add:${preset.id}`);
      setErr(null);
      setNote(null);
      try {
        const rows = await client.addProvider(preset.id, baseUrl);
        setProviders(() => rows);
        setPresets((current) => current?.filter((row) => row.id !== preset.id) ?? null);
        setNote(`Added ${preset.label}.`, preset.id);
        const added = rows.find((row) => row.id === preset.id);
        if (added && preset.auth_kind !== 'none') {
          await signIn(added);
          return;
        }
      } catch (e) {
        setErr((e as Error).message, preset.id);
      } finally {
        setPending(null);
      }
    },
    [client, setErr, setNote, setPending, setProviders, signIn],
  );

  /**
   * Remove a provider from this machine. The daemon drops the config entry AND
   * runs the provider's logout, so nothing is left behind; the preset becomes
   * addable again, which is why the picker is dropped rather than patched.
   */
  const removeProvider = useCallback(
    async (provider: RouterProvider) => {
      setPending(`remove:${provider.id}`);
      setErr(null);
      setNote(null);
      try {
        const rows = await client.removeProvider(provider.id);
        setProviders(() => rows);
        setPresets(null);
        setNote(`Removed ${provider.label}.`, provider.id);
      } catch (e) {
        setErr((e as Error).message, provider.id);
      } finally {
        setPending(null);
      }
    },
    [client, setErr, setNote, setPending, setProviders],
  );

  return {
    ...fleet,
    flow,
    redirectUrl,
    setRedirectUrl,
    apiKey,
    setApiKey,
    signIn,
    signOut,
    recheck,
    finishPkce,
    finishApiKey,
    cancelFlow,
    presets,
    loadPresets,
    addProvider,
    removeProvider,
  };
}

/**
 * The live sign-in step: a device code to type into the browser, a redirect URL
 * to paste back, or an API key to enter. Only ever rendered by
 * `ProviderNotice`, INSIDE the card of the provider the flow belongs to.
 */
function ProviderFlowPanel({ auth }: { auth: ProviderAuth }) {
  const { flow } = auth;
  if (!flow) return null;
  const busy = auth.pending === 'auth:complete';

  return (
    <div className="space-y-3 border border-accent/50 bg-panel-2 p-3">
      <p className="font-mono text-body font-bold text-white">
        {flow.kind === 'device' ? 'Waiting for authorization…' : 'Finish sign-in'}
      </p>

      {flow.user_code && (
        <p className="select-all break-all border border-dialog-edge bg-input px-3 py-2 text-center font-mono text-display font-bold tracking-[0.2em] text-accent-ink">
          {flow.user_code}
        </p>
      )}

      {flow.instructions?.length ? (
        <ol className="list-inside list-decimal space-y-1 font-mono text-ui text-dialog-hint">
          {flow.instructions.map((line) => (
            <li key={line}>{line}</li>
          ))}
        </ol>
      ) : null}

      {flow.url && (
        <Button
          variant="ghost"
          className="w-full"
          onClick={() => openProviderUrl(flow.url as string)}
        >
          Open sign-in page again
        </Button>
      )}

      {flow.kind === 'pkce' && (
        <div className="space-y-2">
          <label
            className="block font-mono text-meta uppercase tracking-[0.1em] text-dialog-hint"
            htmlFor="provider-redirect-url"
          >
            Paste the final redirect URL
          </label>
          <Input
            id="provider-redirect-url"
            value={auth.redirectUrl}
            inputMode="url"
            autoCapitalize="off"
            autoCorrect="off"
            spellCheck={false}
            placeholder="https://…?code=…"
            onChange={(event) => auth.setRedirectUrl(event.target.value)}
          />
        </div>
      )}

      {flow.kind === 'api-key' && (
        <div className="space-y-2">
          <label
            className="block font-mono text-meta uppercase tracking-[0.1em] text-dialog-hint"
            htmlFor="provider-api-key"
          >
            Paste the provider API key
          </label>
          <Input
            id="provider-api-key"
            type="password"
            value={auth.apiKey}
            autoCapitalize="off"
            autoCorrect="off"
            spellCheck={false}
            placeholder="sk-…"
            onChange={(event) => auth.setApiKey(event.target.value)}
          />
        </div>
      )}

      <div className="flex flex-col gap-2 sm:flex-row">
        {flow.kind === 'pkce' && (
          <Button
            className="flex-1"
            disabled={!auth.redirectUrl.trim() || busy}
            onClick={() => void auth.finishPkce()}
          >
            {busy ? 'Finishing…' : 'Finish sign-in'}
          </Button>
        )}
        {flow.kind === 'api-key' && (
          <Button
            className="flex-1"
            disabled={!auth.apiKey.trim() || busy}
            onClick={() => void auth.finishApiKey()}
          >
            {busy ? 'Saving…' : 'Save key'}
          </Button>
        )}
        <Button variant="ghost" className="flex-1" onClick={() => void auth.cancelFlow()}>
          Cancel
        </Button>
      </div>
    </div>
  );
}

/**
 * Everything the gateway has to SAY about one provider, painted inside that
 * provider's own card: its last verdict and the sign-in step it is in the
 * middle of.
 *
 * This is the whole point of scoping a `ProviderMessage`. A banner at the top
 * of the panel announced "Z.ai (Coding Plan): signed out." above a list where
 * that provider already carries its own red dot, and the API-key form for ONE
 * provider sat above EVERY provider's row — so the screen asked for a key
 * without saying whose. It sits outside the collapsible body on purpose: a
 * running sign-in must not be hideable, and a verdict must be readable without
 * expanding anything.
 */
export function ProviderNotice({
  auth,
  provider,
}: {
  auth: ProviderAuth;
  provider: RouterProvider;
}) {
  const err = auth.err?.providerId === provider.id ? auth.err : null;
  const note = auth.note?.providerId === provider.id ? auth.note : null;
  const isFlowing = auth.flow?.provider_id === provider.id;
  if (!err && !note && !isFlowing) return null;

  return (
    <div className="space-y-2 border-t border-dialog-edge p-3">
      {err && <Banner kind="err">{err.text}</Banner>}
      {note && <Banner kind="ok">{note.text}</Banner>}
      {isFlowing && <ProviderFlowPanel auth={auth} />}
    </div>
  );
}

/**
 * Sign-out, two-step. It deletes a credential ON THE DAEMON that every client
 * of that gateway shares, so a stray tap on a phone must not be able to drop
 * it — the first press only arms the confirmation.
 */
export function ProviderSignOutButton({
  auth,
  provider,
  className = '',
}: {
  auth: ProviderAuth;
  provider: RouterProvider;
  className?: string;
}) {
  const [isConfirming, setIsConfirming] = useState(false);
  const busy = auth.pending === `logout:${provider.id}`;

  if (!isConfirming) {
    return (
      <Button
        variant="ghost"
        className={className}
        disabled={busy}
        onClick={() => setIsConfirming(true)}
      >
        {busy ? 'Signing out…' : 'Sign out'}
      </Button>
    );
  }

  return (
    <span className={`flex min-w-0 gap-2 ${className}`}>
      <Button
        variant="danger"
        className="min-w-0 flex-1"
        disabled={busy}
        onClick={() => {
          setIsConfirming(false);
          void auth.signOut(provider);
        }}
      >
        {busy ? 'Signing out…' : 'Yes, sign out'}
      </Button>
      <Button
        variant="ghost"
        className="min-w-0 flex-1"
        disabled={busy}
        onClick={() => setIsConfirming(false)}
      >
        Cancel
      </Button>
    </span>
  );
}

/** What adding this preset will ask for next, in the user's words. */
export function presetHint(preset: ProviderPreset): string {
  if (preset.is_local) return `Local runtime · ${preset.base_url ?? 'address on that machine'}`;
  if (preset.auth_kind === 'oauth') return 'Sign in with your account';
  if (preset.auth_kind === 'command') return 'Credential minted on that machine';
  return 'Needs an API key';
}

/**
 * Add a provider TO THE GATEWAY'S MACHINE.
 *
 * A provider is not a client setting: the daemon writes it into its own config
 * next to its own credentials, so this picker only offers what that machine
 * reported as missing, and the add is finished by the very same flow panel a
 * sign-in uses. A local runtime is the one preset that asks a question first —
 * LM Studio and Ollama listen wherever that machine put them, so the address is
 * editable before the add, and resolved THERE, not on this device.
 *
 * A machine with every provider already configured has no picker and no button:
 * the panel asks the gateway BEFORE it paints, and renders nothing until the
 * answer is a non-empty list.
 */
export function AddProviderPanel({
  auth,
  className = '',
}: {
  auth: ProviderAuth;
  className?: string;
}) {
  const { presets, loadPresets, pending } = auth;
  const [isPicking, setIsPicking] = useState(false);
  const [chosen, setChosen] = useState<ProviderPreset | null>(null);
  const [baseUrl, setBaseUrl] = useState('');

  // Only the daemon knows what is still addable, so ask on mount — and again
  // whenever a removal puts a preset back in play (`presets` drops to `null`).
  // A failed probe answers `[]`, which is why this can never loop.
  useEffect(() => {
    if (presets === null && pending !== 'presets') void loadPresets();
  }, [presets, pending, loadPresets]);

  // Unasked, or every provider this machine knows is already configured.
  if (presets === null || presets.length === 0) return null;

  if (!isPicking) {
    return (
      <Button
        className={className}
        onClick={() => {
          setIsPicking(true);
          setChosen(null);
        }}
      >
        Add provider
      </Button>
    );
  }

  if (chosen) {
    const busy = auth.pending === `add:${chosen.id}`;
    return (
      <div className={`space-y-3 border border-accent/50 bg-panel-2 p-3 ${className}`}>
        <p className="font-mono text-body font-bold text-white">Add {chosen.label}</p>
        <div className="space-y-2">
          <label
            className="block font-mono text-meta uppercase tracking-[0.1em] text-dialog-hint"
            htmlFor="add-provider-base-url"
          >
            Where it listens on that machine
          </label>
          <Input
            id="add-provider-base-url"
            value={baseUrl}
            inputMode="url"
            autoCapitalize="off"
            autoCorrect="off"
            spellCheck={false}
            placeholder={chosen.base_url ?? 'http://localhost:1234/v1'}
            onChange={(event) => setBaseUrl(event.target.value)}
          />
          <p className="break-words font-mono text-chip text-dialog-hint">
            Resolved by the gateway, not by this device. Leave it blank for {chosen.base_url ?? 'the default'}.
          </p>
        </div>
        <div className="flex flex-col gap-2 sm:flex-row">
          <Button
            className="flex-1"
            disabled={busy}
            onClick={() => {
              void (async () => {
                await auth.addProvider(chosen, baseUrl.trim() || undefined);
                setIsPicking(false);
                setChosen(null);
                setBaseUrl('');
              })();
            }}
          >
            {busy ? 'Adding…' : `Add ${chosen.label}`}
          </Button>
          <Button variant="ghost" className="flex-1" disabled={busy} onClick={() => setChosen(null)}>
            Back
          </Button>
        </div>
      </div>
    );
  }

  return (
    <div className={`space-y-2 border border-accent/50 bg-panel-2 p-3 ${className}`}>
      <p className="font-mono text-body font-bold text-white">Add a provider to this machine</p>

      {presets.map((preset) => {
        const busy = auth.pending === `add:${preset.id}`;
        return (
          <button
            key={preset.id}
            type="button"
            disabled={busy}
            className="flex min-h-12 w-full items-center gap-2 border border-dialog-edge bg-panel px-3 py-2 text-left transition-colors hover:bg-hover focus-visible:bg-hover focus-visible:outline-none disabled:opacity-50"
            onClick={() => {
              if (preset.is_local) {
                setBaseUrl('');
                setChosen(preset);
                return;
              }
              void (async () => {
                await auth.addProvider(preset);
                setIsPicking(false);
              })();
            }}
          >
            <span className="min-w-0 flex-1">
              <span className="block truncate font-mono text-ui font-bold text-white">{preset.label}</span>
              <span className="block truncate font-mono text-meta text-dialog-hint">
                {busy ? 'Adding…' : presetHint(preset)}
              </span>
            </span>
            <span className="shrink-0 font-mono text-meta text-dialog-hint" aria-hidden="true">
              {preset.is_local ? '▸' : '+'}
            </span>
          </button>
        );
      })}

      <Button
        variant="ghost"
        className="w-full"
        onClick={() => {
          setIsPicking(false);
          setChosen(null);
        }}
      >
        Cancel
      </Button>
    </div>
  );
}

/**
 * Remove, two-step. Unlike sign-out this also deletes the provider from the
 * machine's config — every client of that gateway loses it — so the first press
 * only arms the confirmation.
 */
export function ProviderRemoveButton({
  auth,
  provider,
  className = '',
}: {
  auth: ProviderAuth;
  provider: RouterProvider;
  className?: string;
}) {
  const [isConfirming, setIsConfirming] = useState(false);
  const busy = auth.pending === `remove:${provider.id}`;

  if (!isConfirming) {
    return (
      <Button variant="ghost" className={className} disabled={busy} onClick={() => setIsConfirming(true)}>
        {busy ? 'Removing…' : 'Remove'}
      </Button>
    );
  }

  return (
    <span className={`flex min-w-0 gap-2 ${className}`}>
      <Button
        variant="danger"
        className="min-w-0 flex-1"
        disabled={busy}
        onClick={() => {
          setIsConfirming(false);
          void auth.removeProvider(provider);
        }}
      >
        {busy ? 'Removing…' : 'Yes, remove'}
      </Button>
      <Button variant="ghost" className="min-w-0 flex-1" disabled={busy} onClick={() => setIsConfirming(false)}>
        Cancel
      </Button>
    </span>
  );
}
