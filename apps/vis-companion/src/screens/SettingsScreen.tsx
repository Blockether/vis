import { useCallback, useEffect, useState, type ReactNode } from 'react';
import { GatewayError, type GatewayClient } from '../lib/gateway';
import type {
  GatewayConn,
  GatewayTheme,
  PushDevice,
  PushStatus,
  ThemePref,
  Toggle,
  ToggleGroup,
} from '../lib/types';
import {
  acquirePushToken,
  cachedPushToken,
  deviceRegistration,
  isPushSupported,
  maskToken,
  pushPermission,
  pushPlatform,
  type PushPermission,
} from '../lib/push';
import { applyGatewayTheme, resolveTheme } from '../lib/theme';
import { getThemePref, setThemePref } from '../lib/storage';
import { Banner, Button, Input } from '../components/ui';
import {
  ProviderFlowPanel,
  ProviderSignOutButton,
  isProviderAuthed,
  providerLimitsLine,
  providerStatusDot,
  providerStatusLine,
  useProviderAuth,
} from '../components/ProviderAuth';

interface Props {
  client: GatewayClient;
  gateway: GatewayConn;
  isActive: boolean;
  onActivate?: () => void;
  onRename?: (label: string | undefined) => void | Promise<void>;
  onRemove?: () => void | Promise<void>;
  onClose: () => void;
}

export function GatewaySettingsDialog({
  client,
  gateway,
  isActive,
  onActivate,
  onRename,
  onRemove,
  onClose,
}: Props) {
  // Reopening the dialog paints the gateway's last known toggles immediately;
  // `load` below refreshes them (and `setSetting` patches the cache in place).
  const [groups, setGroups] = useState<ToggleGroup[] | null>(
    () => client.cachedSettings()?.groups ?? null,
  );
  const [theme, setTheme] = useState<GatewayTheme | null>(null);
  const [pref, setPref] = useState<ThemePref>('light');
  const [err, setErr] = useState<string | null>(null);
  const [pending, setPending] = useState<string | null>(null);
  const [unreachable, setUnreachable] = useState(false);
  const [unauthorized, setUnauthorized] = useState(false);
  const [labelDraft, setLabelDraft] = useState(gateway.label ?? '');
  const [confirmRemove, setConfirmRemove] = useState(false);

  const load = useCallback(
    async (signal?: AbortSignal) => {
      // Status flags are assigned only after the request settles, and never once the
      // caller has been torn down — so mounting this loader writes no state
      // synchronously and none after unmount.
      try {
        const settings = await client.settings();
        if (signal?.aborted) return;
        setErr(null);
        setUnreachable(false);
        setUnauthorized(false);
        setGroups(settings.groups ?? []);
        try {
          const activeTheme = await client.theme();
          const themePref = await getThemePref();
          if (signal?.aborted) return;
          setTheme(activeTheme);
          setPref(themePref);
          if (isActive) applyGatewayTheme(resolveTheme(activeTheme, themePref));
        } catch (e) {
          if (signal?.aborted) return;
          setTheme(null);
          setErr(`Theme sync unavailable: ${(e as Error).message}`);
        }
      } catch (e) {
        if (signal?.aborted) return;
        // A token-gated gateway that's actually up answers /healthz (so the list
        // reads Online) but 401s on /v1/settings. Surface that as "unauthorized",
        // NOT "offline" — otherwise the dialog contradicts the reachable list.
        if (e instanceof GatewayError && e.status === 401) {
          setErr(null);
          setUnreachable(false);
          setUnauthorized(true);
          setGroups(null);
          return;
        }
        setErr((e as Error).message);
        setUnreachable(true);
        setUnauthorized(false);
        setGroups(null);
      }
    },
    [client, isActive],
  );

  useEffect(() => {
    const controller = new AbortController();
    // Mount-time settings fetch: `load` writes state only after it settles, and
    // never once the signal aborts.
    void load(controller.signal);
    return () => controller.abort();
  }, [load]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === 'Escape') onClose();
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [onClose]);

  function patch(updated: Toggle) {
    setGroups(
      (current) =>
        current?.map((group) => ({
          ...group,
          toggles: group.toggles.map((toggle) => (toggle.id === updated.id ? updated : toggle)),
        })) ?? null,
    );
  }

  async function chooseTheme(next: ThemePref) {
    setPending(`theme:${next}`);
    try {
      await setThemePref(next);
      setPref(next);
      if (isActive && theme) applyGatewayTheme(resolveTheme(theme, next));
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function flip(toggle: Toggle) {
    setPending(toggle.id);
    try {
      patch(await client.setSetting(toggle.id, 'toggle'));
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function pick(toggle: Toggle, value: string) {
    setPending(toggle.id);
    try {
      patch(await client.setSetting(toggle.id, 'value', value));
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  const settingCount = groups?.reduce((total, group) => total + group.toggles.length, 0) ?? 0;
  const status = unreachable
    ? { dot: '○', label: 'Offline', tone: 'text-err' }
    : unauthorized
      ? { dot: '●', label: 'Unauthorized', tone: 'text-warn-strong' }
      : isActive
        ? { dot: '●', label: 'Active', tone: 'text-ok' }
        : { dot: '○', label: 'Saved', tone: 'text-dialog-hint' };

  return (
    <div
      className="fixed inset-0 z-50 flex items-end justify-center bg-ink/85 p-0 pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] backdrop-blur-[2px] transition-opacity duration-200 starting:opacity-0 motion-reduce:transition-none sm:items-center sm:p-5"
      onMouseDown={(event) => {
        if (event.target === event.currentTarget) onClose();
      }}
    >
      <section
        className="flex h-[92dvh] max-h-[calc(100dvh-env(safe-area-inset-top))] w-full max-w-3xl flex-col overflow-hidden border-x border-t border-dialog-edge bg-panel shadow-none transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-6 starting:opacity-0 motion-reduce:transition-none sm:h-auto sm:max-h-[calc(100dvh-2.5rem)] sm:border sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:starting:translate-y-2"
        role="dialog"
        aria-modal="true"
        aria-labelledby="gateway-settings-title"
      >
        <header className="flex min-h-12 shrink-0 items-center bg-dialog-title text-dialog-title-foreground">
          <div className="min-w-0 flex-1 px-3 py-2 sm:px-4">
            <h2
              id="gateway-settings-title"
              className="shrink-0 font-mono text-body font-black uppercase tracking-[0.12em]"
            >
              Gateway settings
            </h2>
            <p className="truncate font-mono text-meta opacity-65">{gateway.url}</p>
          </div>
          <button
            type="button"
            className="grid min-w-10 self-stretch place-items-center border-l border-dialog-title-foreground/20 font-mono text-title text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none"
            onClick={onClose}
            aria-label="Close gateway settings"
          >
            ✕
          </button>
        </header>

        <div className="shrink-0 border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4">
          <p className="text-ui text-dialog-hint">
            Providers, notifications and appearance live on the gateway — shared with
            its TUI and every other client.
          </p>
        </div>

        <div className="min-h-0 flex-1 touch-pan-y space-y-3 overflow-x-hidden overflow-y-auto overscroll-contain p-3 sm:p-4">
          {err && <Banner kind="err">{err}</Banner>}

          <SettingsPanel
            title="Saved connection"
            meta={
              <span className={`font-black ${status.tone}`}>
                {status.dot} {status.label}
              </span>
            }
          >
            <div className="space-y-2 p-2.5">
              <Input
                value={labelDraft}
                placeholder="Name this gateway"
                aria-label="Name this gateway"
                autoCapitalize="none"
                autoCorrect="off"
                className="w-full"
                onChange={(event) => setLabelDraft(event.target.value)}
                onBlur={() => {
                  if ((labelDraft.trim() || undefined) !== (gateway.label ?? undefined))
                    void onRename?.(labelDraft.trim() || undefined);
                }}
                onKeyDown={(event) => {
                  if (event.key === 'Enter') event.currentTarget.blur();
                  if (event.key === 'Escape') {
                    setLabelDraft(gateway.label ?? '');
                    event.currentTarget.blur();
                  }
                }}
              />

              <p className="font-mono text-meta text-dialog-hint">
                This device remembers{' '}
                <span className="text-white">{gatewayHost(gateway.url)}</span> and its
                access token. The name is only shown in your gateway list — the gateway
                never sees it.
              </p>

              <div className="flex flex-wrap items-center gap-x-2 gap-y-1 border-t border-dialog-edge pt-2">
                {!isActive && (
                  <Button
                    onClick={() => {
                      onActivate?.();
                      onClose();
                    }}
                  >
                    Use this gateway
                  </Button>
                )}

                {!confirmRemove && <span className="flex-1" />}

                {confirmRemove ? (
                  <>
                    <span className="min-w-0 flex-1 font-mono text-meta text-dialog-hint">
                      Deletes the address and token from this device. You&apos;ll need
                      the QR code again.
                    </span>
                    <Button variant="ghost" onClick={() => setConfirmRemove(false)}>
                      Cancel
                    </Button>
                    <Button
                      variant="danger"
                      onClick={async () => {
                        await onRemove?.();
                        onClose();
                      }}
                    >
                      Forget
                    </Button>
                  </>
                ) : (
                  <Button variant="danger" onClick={() => setConfirmRemove(true)}>
                    Forget this gateway
                  </Button>
                )}
              </div>
            </div>
          </SettingsPanel>

          {!unreachable && !unauthorized && <ProvidersPanel client={client} />}

          {!unreachable && !unauthorized && <NotificationsPanel client={client} />}

          {!unreachable && !unauthorized && theme && (
            <SettingsPanel
              title="Theme"
              meta={pref === 'gateway' ? `gateway · ${theme.display_name}` : 'saved on this device'}
            >
              {(() => {
                const resolved = resolveTheme(theme, pref);
                const options: { key: ThemePref; name: string; sub: string }[] = [
                  {
                    key: 'gateway',
                    name: 'Follow gateway',
                    sub: theme.display_name,
                  },
                  ...theme.themes.map((t) => ({
                    key: t.id,
                    name: t.display_name,
                    sub: t.mode,
                  })),
                ];
                return (
                  <div className="grid grid-cols-1 gap-px bg-dialog-edge min-[420px]:grid-cols-2">
                    {options.map((choice) => {
                      const selected =
                        choice.key === pref ||
                        (choice.key !== 'gateway' &&
                          pref !== 'gateway' &&
                          choice.key === resolved.id);
                      return (
                        <button
                          type="button"
                          key={choice.key}
                          disabled={pending?.startsWith('theme:') ?? false}
                          onClick={() => chooseTheme(choice.key)}
                          className={`flex min-h-10 min-w-0 items-center justify-between gap-3 px-3 py-1.5 text-left transition-[background-color,color,transform,translate,scale,rotate] duration-150 active:scale-[0.99] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent disabled:opacity-45 motion-reduce:transition-none sm:min-h-9 ${
                            selected
                              ? 'bg-accent text-accent-foreground'
                              : 'bg-input text-white hover:bg-hover'
                          }`}
                          aria-pressed={selected}
                        >
                          <span className="min-w-0">
                            <span className="block truncate font-mono text-ui font-bold">
                              {choice.name}
                            </span>
                            <span className="block font-mono text-chip uppercase tracking-wider opacity-65">
                              {choice.sub}
                            </span>
                          </span>
                          <span
                            className="shrink-0 font-mono text-meta font-black"
                            aria-hidden="true"
                          >
                            {selected ? '●' : '○'}
                          </span>
                        </button>
                      );
                    })}
                  </div>
                );
              })()}
            </SettingsPanel>
          )}

          {unreachable ? (
            <SettingsPanel title="Settings">
              <div className="flex flex-col items-center gap-3 px-4 py-8 text-center">
                <p className="font-mono text-body font-bold text-err">Gateway unreachable</p>
                <p className="font-mono text-meta text-dialog-hint">
                  Can't load settings — the gateway isn't responding.
                </p>
                <button
                  type="button"
                  onClick={() => void load()}
                  className="border border-dialog-edge bg-input px-3 py-1.5 font-mono text-meta font-bold text-white hover:bg-hover"
                >
                  Retry
                </button>
              </div>
            </SettingsPanel>
          ) : unauthorized ? (
            <SettingsPanel title="Settings" meta="unauthorized">
              <div className="flex flex-col items-center gap-3 px-4 py-8 text-center">
                <p className="font-mono text-body font-bold text-warn-strong">
                  Token missing or invalid
                </p>
                <p className="max-w-sm font-mono text-meta text-dialog-hint">
                  The gateway is online, but rejected this token. Re-pair from{' '}
                  <code className="text-accent-ink">vis gateway pair</code> and paste the fresh link
                  to load its settings.
                </p>
                <button
                  type="button"
                  onClick={() => void load()}
                  className="border border-dialog-edge bg-input px-3 py-1.5 font-mono text-meta font-bold text-white hover:bg-hover"
                >
                  Retry
                </button>
              </div>
            </SettingsPanel>
          ) : groups === null ? (
            <SettingsPanel title="Loading">
              <div className="space-y-px bg-dialog-edge" aria-label="Loading settings">
                {[0, 1, 2].map((item) => (
                  <div key={item} className="h-12 animate-pulse bg-panel-2" />
                ))}
              </div>
            </SettingsPanel>
          ) : groups.length === 0 ? (
            <SettingsPanel title="Settings">
              <p className="px-4 py-6 text-center font-mono text-body text-dialog-hint">
                No settings exposed by this gateway.
              </p>
            </SettingsPanel>
          ) : (
            groups.map((group) => (
              <SettingsPanel
                key={group.id}
                title={group.title}
                meta={`${group.toggles.length} ${group.toggles.length === 1 ? 'option' : 'options'}`}
              >
                <div className="divide-y divide-dialog-edge">
                  {group.toggles.map((toggle) => {
                    const busy = pending === toggle.id;
                    return (
                      <div
                        key={toggle.id}
                        className="grid min-w-0 grid-cols-[auto_minmax(0,1fr)_auto] items-start gap-x-4 gap-y-2 px-3 py-3 transition-colors hover:bg-hover sm:px-4 sm:py-2.5"
                      >
                        <span
                          className={`pt-0.5 font-mono text-body ${
                            toggle.type === 'boolean' && toggle.enabled
                              ? 'text-ok'
                              : 'text-dialog-hint'
                          }`}
                          aria-hidden="true"
                        >
                          {toggle.type === 'boolean' ? (toggle.enabled ? '●' : '○') : '◆'}
                        </span>

                        <div className="min-w-0">
                          <p className="break-words font-mono text-ui font-bold text-white">
                            {toggle.label}
                          </p>
                          {toggle.description && (
                            <p className="mt-0.5 hyphens-auto break-words text-justify text-meta text-dialog-hint">
                              {toggle.description}
                            </p>
                          )}
                        </div>

                        {toggle.type === 'boolean' && (
                          <Switch
                            label={toggle.label}
                            on={!!toggle.enabled}
                            busy={busy}
                            disabled={busy}
                            onClick={() => flip(toggle)}
                          />
                        )}

                        {toggle.type === 'enum' && toggle.choices && (
                          <div className="col-span-full col-start-2 flex min-w-0 flex-wrap gap-1.5">
                            {toggle.choices.map((choice) => {
                              const selected = toggle.value === choice;
                              return (
                                <button
                                  type="button"
                                  key={choice}
                                  disabled={busy}
                                  onClick={() => pick(toggle, choice)}
                                  className={`min-h-8 border px-2 py-0.5 font-mono text-chip font-bold transition-[background-color,border-color,color,transform,translate,scale,rotate] active:scale-[0.98] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:opacity-45 motion-reduce:transition-none sm:min-h-6 ${
                                    selected
                                      ? 'border-transparent bg-accent text-accent-foreground'
                                      : 'border-transparent bg-panel-2 text-dialog-hint hover:bg-hover hover:text-white'
                                  }`}
                                  aria-pressed={selected}
                                >
                                  {choice}
                                </button>
                              );
                            })}
                          </div>
                        )}
                      </div>
                    );
                  })}
                </div>
              </SettingsPanel>
            ))
          )}
        </div>

        <footer className="flex shrink-0 items-center border-t border-dialog-edge bg-panel-2 px-3 pt-2 pb-[max(0.5rem,env(safe-area-inset-bottom))] font-mono text-chip text-dialog-hint sm:px-4 sm:py-2">
          <span>
            {settingCount} {settingCount === 1 ? 'option' : 'options'}
          </span>
        </footer>
      </section>
    </div>
  );
}

/**
 * Provider accounts ON THIS GATEWAY: live auth status, sign-in, a manual
 * re-check, and sign-out — the whole terminal-free equivalent of
 * `vis auth login/logout/status`.
 *
 * Every credential lives on the daemon: this panel starts flows, polls them,
 * and asks for verdicts, but never holds a token, verifier, or device code.
 * The exchange itself is `useProviderAuth`, shared with the router dialog.
 */
function ProvidersPanel({ client }: { client: GatewayClient }) {
  const auth = useProviderAuth(client);
  const { providers, err, note, pending } = auth;
  const [expanded, setExpanded] = useState<string | null>(null);
  const signedIn = providers?.filter(isProviderAuthed).length ?? 0;

  return (
    <SettingsPanel
      title="Providers"
      meta={providers ? `${signedIn}/${providers.length} signed in` : 'checking…'}
    >
      <div className="space-y-2 p-3">
        {err && <Banner kind="err">{err}</Banner>}
        {note && <Banner kind="ok">{note}</Banner>}
        <ProviderFlowPanel auth={auth} />

        {providers === null && (
          <p className="py-4 text-center font-mono text-meta text-dialog-hint">
            Checking provider sign-in…
          </p>
        )}

        {providers?.length === 0 && (
          <p className="py-4 text-center font-mono text-meta text-dialog-hint">
            No providers configured on this gateway.
          </p>
        )}

        {providers?.map((provider) => {
          const dot = providerStatusDot(provider);
          const authed = isProviderAuthed(provider);
          const limits = providerLimitsLine(provider);
          const open = expanded === provider.id;

          return (
            <div key={provider.id} className="border border-dialog-edge bg-panel-2">
              <button
                type="button"
                className="flex min-h-12 w-full items-center gap-2 px-3 py-2 text-left transition-colors hover:bg-hover focus-visible:bg-hover focus-visible:outline-none"
                onClick={() => setExpanded(open ? null : provider.id)}
                aria-expanded={open}
              >
                <span className={`font-mono text-body ${dot.tone}`} aria-label={dot.label}>
                  {dot.glyph}
                </span>
                <span className="min-w-0 flex-1">
                  <span className="block truncate font-mono text-ui font-bold text-white">
                    {provider.label}
                  </span>
                  <span className="block truncate font-mono text-meta text-dialog-hint">
                    {providerStatusLine(provider)}
                  </span>
                </span>
                <span className="shrink-0 font-mono text-meta text-dialog-hint" aria-hidden="true">
                  {open ? '▾' : '▸'}
                </span>
              </button>

              {open && (
                <div className="space-y-2 border-t border-dialog-edge p-3">
                  {limits && (
                    <p className="break-words font-mono text-meta text-dialog-hint">{limits}</p>
                  )}
                  <p className="break-words font-mono text-chip text-dialog-hint">
                    {provider.id} · {provider.models.length}{' '}
                    {provider.models.length === 1 ? 'model' : 'models'}
                  </p>

                  <div className="flex flex-col gap-2 sm:flex-row">
                    <Button
                      className="flex-1"
                      variant={authed ? 'ghost' : 'solid'}
                      disabled={pending === `auth:${provider.id}`}
                      onClick={() => void auth.signIn(provider)}
                    >
                      {pending === `auth:${provider.id}`
                        ? 'Starting…'
                        : authed
                          ? 'Sign in again'
                          : 'Sign in'}
                    </Button>
                    <Button
                      variant="ghost"
                      className="flex-1"
                      disabled={pending === `status:${provider.id}`}
                      onClick={() => void auth.recheck(provider)}
                    >
                      {pending === `status:${provider.id}` ? 'Checking…' : 'Check status'}
                    </Button>
                    {authed && (
                      <ProviderSignOutButton auth={auth} provider={provider} className="flex-1" />
                    )}
                  </div>
                </div>
              )}
            </div>
          );
        })}
      </div>
    </SettingsPanel>
  );
}

/**
 * Native push ON THIS GATEWAY: whether it can push at all, whether THIS device
 * is registered, and a live test that proves the whole push chain (APNs key and
 * topic on iOS, the Firebase service account on Android) without waiting for a
 * real turn to finish.
 *
 * The token itself never round-trips through the UI — the gateway masks every
 * token it stores, and the app matches its own row by computing the same mask.
 */
function NotificationsPanel({ client }: { client: GatewayClient }) {
  const [push, setPush] = useState<PushStatus | null>(null);
  const [devices, setDevices] = useState<PushDevice[] | null>(null);
  const [perm, setPerm] = useState<PushPermission>('unsupported');
  const [err, setErr] = useState<string | null>(null);
  const [note, setNote] = useState<string | null>(null);
  const [busy, setBusy] = useState<'enable' | 'disable' | 'test' | null>(null);
  // An OLDER gateway simply has no /v1/devices route. That is not an error the
  // user can act on — it is a missing capability upstream — so the whole panel
  // (and every button in it) disappears instead of offering calls that 404.
  const [unsupported, setUnsupported] = useState(false);

  const load = useCallback(
    async (signal?: AbortSignal) => {
      try {
        const [state, permission] = await Promise.all([client.devices(signal), pushPermission()]);
        if (signal?.aborted) return;
        setPush(state.push);
        setDevices(state.devices);
        setPerm(permission);
        setErr(null);
      } catch (e) {
        if (signal?.aborted) return;
        if (e instanceof GatewayError && (e.status === 404 || e.status === 501)) {
          setUnsupported(true);
          setDevices([]);
          setErr(null);
          return;
        }
        setDevices([]);
        setErr(e instanceof GatewayError ? e.message : String(e));
      }
    },
    [client],
  );

  useEffect(() => {
    const ctrl = new AbortController();
    void load(ctrl.signal);
    return () => ctrl.abort();
  }, [load]);

  const token = cachedPushToken();
  const mask = token ? maskToken(token) : null;
  const mine = mask ? (devices ?? []).find((d) => d.token_preview === mask) : undefined;
  const registered = Boolean(mine);
  const supported = isPushSupported();

  const enable = useCallback(async () => {
    setBusy('enable');
    setErr(null);
    setNote(null);
    try {
      const fresh = await acquirePushToken();
      await client.registerDevice(deviceRegistration(fresh));
      setNote('This device will be notified when a turn finishes.');
      await load();
    } catch (e) {
      setErr(e instanceof GatewayError ? e.message : (e as Error).message);
    } finally {
      setBusy(null);
    }
  }, [client, load]);

  const disable = useCallback(async () => {
    const current = cachedPushToken();
    if (!current) return;
    setBusy('disable');
    setErr(null);
    setNote(null);
    try {
      await client.unregisterDevice(current);
      setNote('This device will no longer be notified.');
      await load();
    } catch (e) {
      setErr(e instanceof GatewayError ? e.message : (e as Error).message);
    } finally {
      setBusy(null);
    }
  }, [client, load]);

  const test = useCallback(async () => {
    setBusy('test');
    setErr(null);
    setNote(null);
    try {
      const { results } = await client.testPush();
      const ok = results.filter((r) => r.is_delivered).length;
      setNote(
        results.length === 0
          ? 'No devices registered yet.'
          : `Sent to ${ok}/${results.length} device${results.length === 1 ? '' : 's'}` +
              (ok < results.length
                ? ` — ${results.find((r) => !r.is_delivered)?.reason ?? 'rejected'}`
                : '.'),
      );
      await load();
    } catch (e) {
      setErr(e instanceof GatewayError ? e.message : (e as Error).message);
    } finally {
      setBusy(null);
    }
  }, [client, load]);

  // Push has two independent halves; this device only cares about its own. An
  // iOS-only gateway is "available" to an iPhone and "not configured" to a
  // Pixel, and the missing-credentials banner must name the right ones.
  const provider = pushPlatform() === 'android' ? push?.fcm : push?.apns;
  const available = provider ? provider.is_available : (push?.is_available ?? false);
  const missing = provider?.missing ?? push?.missing;

  // Gateway too old to know about push at all: render nothing.
  if (unsupported) return null;

  return (
    <SettingsPanel
      title="Notifications"
      meta={
        push
          ? available
            ? `${push.devices} device${push.devices === 1 ? '' : 's'} · ${pushPlatform() === 'android' ? (push.fcm?.project_id ?? 'fcm') : (push.environment ?? 'production')}`
            : 'not configured'
          : 'checking…'
      }
    >
      <div className="space-y-2 p-3">
        {err && <Banner kind="err">{err}</Banner>}
        {note && <Banner kind="ok">{note}</Banner>}

        <p className="font-mono text-meta text-dialog-hint">
          The gateway sends one alert when a turn finishes or fails, to every device you
          register with it.
        </p>

        {push && !available && (
          <Banner kind="warn">
            This gateway cannot push to {pushPlatform() === 'android' ? 'Android' : 'iOS'} yet — missing{' '}
            {(missing ?? ['push credentials']).join(', ')}.
          </Banner>
        )}

        {!supported && (
          <Banner kind="warn">
            Native alerts need the iOS or Android app. The web build can stay open instead.
          </Banner>
        )}

        {supported && perm === 'denied' && (
          <Banner kind="warn">
            Notifications are turned off for Vis in system Settings — enable them there first.
          </Banner>
        )}

        <div className="flex flex-wrap gap-2">
          {supported && !registered && (
            <Button
              className="min-h-9 flex-1 px-3 font-mono text-meta"
              disabled={busy !== null || !available}
              onClick={() => void enable()}
            >
              {busy === 'enable' ? 'Registering…' : 'Notify this device'}
            </Button>
          )}
          {supported && registered && (
            <Button
              variant="danger"
              className="min-h-9 flex-1 px-3 font-mono text-meta"
              disabled={busy !== null}
              onClick={() => void disable()}
            >
              {busy === 'disable' ? 'Removing…' : 'Stop notifying this device'}
            </Button>
          )}
          <Button
            variant="ghost"
            className="min-h-9 flex-1 px-3 font-mono text-meta"
            disabled={busy !== null || !available || (push?.devices ?? 0) === 0}
            onClick={() => void test()}
          >
            {busy === 'test' ? 'Sending…' : 'Send a test'}
          </Button>
        </div>

        {devices === null && (
          <p className="py-4 text-center font-mono text-meta text-dialog-hint">
            Checking registered devices…
          </p>
        )}

        {devices?.map((device) => (
          <div
            key={device.token_preview}
            className="flex min-h-12 items-center gap-2 border border-dialog-edge bg-panel-2 px-3 py-2"
          >
            <span className="min-w-0 flex-1">
              <span className="block truncate font-mono text-ui text-white">
                {device.label ?? device.platform ?? 'device'}
                {device.token_preview === mask && (
                  <span className="ml-2 text-chip font-bold uppercase tracking-wider text-accent">
                    this device
                  </span>
                )}
              </span>
              <span className="block truncate font-mono text-meta text-dialog-hint">
                {device.token_preview} · {device.environment ?? 'production'}
                {device.client_version ? ` · v${device.client_version}` : ''}
              </span>
            </span>
          </div>
        ))}

        {devices?.length === 0 && (
          <p className="py-4 text-center font-mono text-meta text-dialog-hint">
            No devices registered with this gateway.
          </p>
        )}
      </div>
    </SettingsPanel>
  );
}

function SettingsPanel({
  title,
  meta,
  children,
}: {
  title: string;
  meta?: ReactNode;
  children: ReactNode;
}) {
  return (
    <section className="min-w-0 overflow-hidden border border-dialog-edge bg-panel transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
      <header className="flex min-h-8 items-center justify-between gap-3 border-b border-dialog-edge bg-panel-2 px-3 py-1.5">
        <h3 className="min-w-0 truncate border-l-2 border-accent pl-2 font-mono text-meta font-black uppercase tracking-[0.12em] text-white">
          {title}
        </h3>
        {meta && (
          <span className="shrink-0 font-mono text-chip font-bold uppercase tracking-wider text-dialog-hint">
            {meta}
          </span>
        )}
      </header>
      <div>{children}</div>
    </section>
  );
}

function Switch({
  label,
  on,
  busy,
  disabled,
  onClick,
}: {
  label: string;
  on: boolean;
  busy?: boolean;
  disabled?: boolean;
  onClick: () => void;
}) {
  return (
    <button
      type="button"
      role="switch"
      aria-label={`${label}: ${on ? 'on' : 'off'}`}
      aria-checked={on}
      aria-busy={busy}
      disabled={disabled}
      onClick={onClick}
      className={`mt-0.5 inline-flex h-8 w-[3.25rem] shrink-0 items-center justify-center border font-mono text-chip font-black tracking-[0.08em] transition-colors duration-150 ease-out active:scale-[0.97] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:opacity-45 motion-reduce:transition-none motion-reduce:active:scale-100 sm:h-6 ${
        on
          ? 'border-transparent bg-accent text-accent-foreground'
          : 'border-transparent bg-panel-2 text-dialog-hint hover:bg-hover hover:text-white'
      }`}
    >
      <span aria-hidden className={busy ? 'animate-pulse' : ''}>
        {busy ? '··' : on ? 'ON' : 'OFF'}
      </span>
    </button>
  );
}

function gatewayHost(url: string): string {
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}
