import { useCallback, useEffect, useState, type ReactNode } from 'react';
import { GatewayError, type GatewayClient } from '../lib/gateway';
import type { GatewayConn, GatewayTheme, ThemePref, Toggle, ToggleGroup } from '../lib/types';
import { applyGatewayTheme, resolveTheme } from '../lib/theme';
import { getThemePref, setThemePref } from '../lib/storage';
import { Banner, Input } from '../components/ui';

interface Props {
  client: GatewayClient;
  gateway: GatewayConn;
  isActive: boolean;
  onActivate?: () => void;
  onDeactivate?: () => void;
  onRename?: (label: string | undefined) => void | Promise<void>;
  onRemove?: () => void | Promise<void>;
  onClose: () => void;
}

export function GatewaySettingsDialog({
  client,
  gateway,
  isActive,
  onActivate,
  onDeactivate,
  onRename,
  onRemove,
  onClose,
}: Props) {
  const [groups, setGroups] = useState<ToggleGroup[] | null>(null);
  const [theme, setTheme] = useState<GatewayTheme | null>(null);
  const [pref, setPref] = useState<ThemePref>('light');
  const [err, setErr] = useState<string | null>(null);
  const [pending, setPending] = useState<string | null>(null);
  const [unreachable, setUnreachable] = useState(false);
  const [unauthorized, setUnauthorized] = useState(false);
  const [labelDraft, setLabelDraft] = useState(gateway.label ?? '');
  const [confirmRemove, setConfirmRemove] = useState(false);

  const load = useCallback(async () => {
    setErr(null);
    setUnreachable(false);
    setUnauthorized(false);
    try {
      const settings = await client.settings();
      setGroups(settings.groups ?? []);
      try {
        const activeTheme = await client.theme();
        const themePref = await getThemePref();
        setTheme(activeTheme);
        setPref(themePref);
        if (isActive) applyGatewayTheme(resolveTheme(activeTheme, themePref));
      } catch (e) {
        setTheme(null);
        setErr(`Theme sync unavailable: ${(e as Error).message}`);
      }
    } catch (e) {
      // A token-gated gateway that's actually up answers /healthz (so the list
      // reads Online) but 401s on /v1/settings. Surface that as "unauthorized",
      // NOT "offline" — otherwise the dialog contradicts the reachable list.
      if (e instanceof GatewayError && e.status === 401) {
        setUnauthorized(true);
        setGroups(null);
        return;
      }
      setErr((e as Error).message);
      setUnreachable(true);
      setGroups(null);
    }
  }, [client, isActive]);

  useEffect(() => {
    void load();
  }, [load]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === 'Escape') onClose();
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [onClose]);

  function patch(updated: Toggle) {
    setGroups((current) =>
      current?.map((group) => ({
        ...group,
        toggles: group.toggles.map((toggle) =>
          toggle.id === updated.id ? updated : toggle,
        ),
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
        className="flex h-[92dvh] max-h-[calc(100dvh-env(safe-area-inset-top))] w-full max-w-3xl flex-col overflow-hidden border-x border-t border-dialog-edge bg-panel shadow-none transition-[opacity,transform] duration-200 starting:translate-y-6 starting:opacity-0 motion-reduce:transition-none sm:h-auto sm:max-h-[calc(100dvh-2.5rem)] sm:border sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:starting:translate-y-2"
        role="dialog"
        aria-modal="true"
        aria-labelledby="gateway-settings-title"
      >
        <header className="flex min-h-12 shrink-0 items-center bg-dialog-title text-dialog-title-foreground">
          <div className="min-w-0 flex-1 px-3 py-2 sm:px-4">
            <h2
              id="gateway-settings-title"
              className="shrink-0 font-mono text-xs font-black uppercase tracking-[0.12em]"
            >
              Gateway settings
            </h2>
            <p className="truncate font-mono text-[10px] opacity-65">{gateway.url}</p>
          </div>
          <button
            type="button"
            className="grid min-w-12 self-stretch place-items-center border-l border-dialog-title-foreground/20 font-mono text-base leading-none text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none"
            onClick={onClose}
            aria-label="Close gateway settings"
          >
            ✕
          </button>
        </header>

        <div className="shrink-0 border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4">
          <p className="text-[11px] leading-4 text-dialog-hint">
            Stored on this gateway and shared with its connected TUI and clients.
          </p>
        </div>

        <div className="min-h-0 flex-1 space-y-3 overflow-y-auto overscroll-contain p-3 sm:p-4">
          {err && <Banner kind="err">{err}</Banner>}

          <SettingsPanel
            title="Gateway"
            meta={<span className={`font-black ${status.tone}`}>{status.dot} {status.label}</span>}
          >
            <div className="space-y-3 p-3">
              <label className="block">
                <span className="mb-1 block font-mono text-[9px] font-bold uppercase tracking-wider text-dialog-hint">
                  Display name
                </span>
                <div className="flex gap-2">
                  <Input
                    value={labelDraft}
                    placeholder={gatewayHost(gateway.url)}
                    autoCapitalize="none"
                    autoCorrect="off"
                    className="h-10 min-w-0 flex-1 text-xs sm:h-9"
                    onChange={(event) => setLabelDraft(event.target.value)}
                    onKeyDown={(event) => {
                      if (event.key === 'Enter') void onRename?.(labelDraft.trim() || undefined);
                    }}
                  />
                  <button
                    type="button"
                    disabled={(labelDraft.trim() || undefined) === (gateway.label ?? undefined)}
                    onClick={() => void onRename?.(labelDraft.trim() || undefined)}
                    className="h-10 shrink-0 border border-dialog-edge bg-panel px-4 font-mono text-[10px] font-bold uppercase tracking-[0.12em] text-dialog-hint-key transition-colors hover:border-edge-strong hover:bg-hover hover:text-white disabled:border-dialog-edge disabled:bg-panel disabled:text-dialog-hint disabled:opacity-45 disabled:hover:bg-panel disabled:hover:text-dialog-hint sm:h-9"
                  >
                    Save
                  </button>
                </div>
              </label>

              <div className="flex flex-wrap items-center gap-2 border-t border-dialog-edge pt-3">
                {isActive ? (
                  <button
                    type="button"
                    onClick={() => onDeactivate?.()}
                    className="h-9 border border-dialog-edge bg-panel px-4 font-mono text-[10px] font-bold uppercase tracking-[0.12em] text-dialog-hint-key transition-colors hover:border-edge-strong hover:bg-hover hover:text-white"
                  >
                    Deactivate
                  </button>
                ) : (
                  <button
                    type="button"
                    onClick={() => {
                      onActivate?.();
                      onClose();
                    }}
                    className="h-9 border border-accent bg-accent px-4 font-mono text-[10px] font-bold uppercase tracking-[0.12em] text-accent-foreground transition-opacity hover:opacity-90"
                  >
                    Activate
                  </button>
                )}
                {confirmRemove ? (
                  <span className="flex items-center gap-2">
                    <span className="font-mono text-[10px] font-bold uppercase tracking-[0.12em] text-err">Remove?</span>
                    <button
                      type="button"
                      onClick={async () => {
                        await onRemove?.();
                        onClose();
                      }}
                      className="h-9 border border-err bg-err px-4 font-mono text-[10px] font-bold uppercase tracking-[0.12em] text-white transition-opacity hover:opacity-90"
                    >
                      Yes, remove
                    </button>
                    <button
                      type="button"
                      onClick={() => setConfirmRemove(false)}
                      className="h-9 border border-dialog-edge bg-panel px-4 font-mono text-[10px] font-bold uppercase tracking-[0.12em] text-dialog-hint-key transition-colors hover:border-edge-strong hover:bg-hover hover:text-white"
                    >
                      Cancel
                    </button>
                  </span>
                ) : (
                  <button
                    type="button"
                    onClick={() => setConfirmRemove(true)}
                    className="h-9 border border-err/40 bg-panel px-4 font-mono text-[10px] font-bold uppercase tracking-[0.12em] text-err transition-colors hover:border-err hover:bg-err hover:text-white"
                  >
                    Remove
                  </button>
                )}
              </div>
            </div>
          </SettingsPanel>

          {!unreachable && !unauthorized && theme && (
            <SettingsPanel
              title="Theme"
              meta={pref === 'gateway' ? `gateway · ${theme.display_name}` : 'saved on this device'}
            >
              {(() => {
                const resolved = resolveTheme(theme, pref);
                const options: { key: ThemePref; name: string; sub: string }[] = [
                  { key: 'gateway', name: 'Follow gateway', sub: theme.display_name },
                  ...theme.themes.map((t) => ({ key: t.id, name: t.display_name, sub: t.mode })),
                ];
                return (
                  <div className="grid grid-cols-1 gap-px bg-dialog-edge min-[420px]:grid-cols-2">
                    {options.map((choice) => {
                      const selected =
                        choice.key === pref ||
                        (choice.key !== 'gateway' && pref !== 'gateway' && choice.key === resolved.id);
                      return (
                        <button
                          type="button"
                          key={choice.key}
                          disabled={pending?.startsWith('theme:') ?? false}
                          onClick={() => chooseTheme(choice.key)}
                          className={`flex min-h-12 min-w-0 items-center justify-between gap-3 px-3 py-2 text-left transition-[background-color,color,transform] duration-150 active:scale-[0.99] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent disabled:opacity-45 motion-reduce:transition-none sm:min-h-11 ${
                            selected
                              ? 'bg-accent text-accent-foreground'
                              : 'bg-input text-white hover:bg-hover'
                          }`}
                          aria-pressed={selected}
                        >
                          <span className="min-w-0">
                            <span className="block truncate font-mono text-[11px] font-bold">
                              {choice.name}
                            </span>
                            <span className="block font-mono text-[8px] uppercase tracking-wider opacity-65">
                              {choice.sub}
                            </span>
                          </span>
                          <span className="shrink-0 font-mono text-[10px] font-black" aria-hidden="true">
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
                <p className="font-mono text-xs font-bold text-err">Gateway unreachable</p>
                <p className="font-mono text-[10px] leading-relaxed text-dialog-hint">
                  Can't load settings — the gateway isn't responding.
                </p>
                <button
                  type="button"
                  onClick={() => void load()}
                  className="border border-dialog-edge bg-input px-3 py-1.5 font-mono text-[10px] font-bold text-white hover:bg-hover"
                >
                  Retry
                </button>
              </div>
            </SettingsPanel>
          ) : unauthorized ? (
            <SettingsPanel title="Settings" meta="unauthorized">
              <div className="flex flex-col items-center gap-3 px-4 py-8 text-center">
                <p className="font-mono text-xs font-bold text-warn-strong">Token missing or invalid</p>
                <p className="max-w-sm font-mono text-[10px] leading-relaxed text-dialog-hint">
                  The gateway is online, but rejected this token. Re-pair from{' '}
                  <code className="text-accent-ink">vis gateway pair</code> and paste the fresh
                  link to load its settings.
                </p>
                <button
                  type="button"
                  onClick={() => void load()}
                  className="border border-dialog-edge bg-input px-3 py-1.5 font-mono text-[10px] font-bold text-white hover:bg-hover"
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
              <p className="px-4 py-6 text-center font-mono text-xs text-dialog-hint">
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
                          className={`pt-0.5 font-mono text-xs ${
                            toggle.type === 'boolean' && toggle.enabled ? 'text-ok' : 'text-dialog-hint'
                          }`}
                          aria-hidden="true"
                        >
                          {toggle.type === 'boolean' ? (toggle.enabled ? '●' : '○') : '◆'}
                        </span>

                        <div className="min-w-0">
                          <p className="break-words font-mono text-[11px] font-bold leading-5 text-white">
                            {toggle.label}
                          </p>
                          {toggle.description && (
                            <p className="mt-0.5 hyphens-auto break-words text-justify text-[10px] leading-[1.45] text-dialog-hint">
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
                                  className={`min-h-11 border px-2.5 py-1 font-mono text-[9px] font-bold transition-[background-color,border-color,color,transform] active:scale-[0.98] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:opacity-45 motion-reduce:transition-none sm:min-h-7 ${
                                    selected
                                      ? 'border-accent bg-accent text-accent-foreground'
                                      : 'border-dialog-edge bg-input text-dialog-hint hover:border-edge-strong hover:text-white'
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

        <footer className="flex shrink-0 items-center border-t border-dialog-edge bg-panel-2 px-3 pt-2 pb-[max(0.5rem,env(safe-area-inset-bottom))] font-mono text-[9px] text-dialog-hint sm:px-4 sm:py-2">
          <span>{settingCount} {settingCount === 1 ? 'option' : 'options'}</span>
        </footer>
      </section>
    </div>
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
    <section className="min-w-0 overflow-hidden border border-dialog-edge bg-panel transition-[opacity,transform] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
      <header className="flex min-h-8 items-center justify-between gap-3 border-b border-dialog-edge bg-panel-2 px-3 py-1.5">
        <h3 className="min-w-0 truncate border-l-2 border-accent pl-2 font-mono text-[10px] font-black uppercase tracking-[0.12em] text-white">
          {title}
        </h3>
        {meta && (
          <span className="shrink-0 font-mono text-[8px] font-bold uppercase tracking-wider text-dialog-hint">
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
      className={`mt-0.5 inline-flex h-8 w-[3.25rem] shrink-0 items-center justify-center border font-mono text-[9px] font-black leading-none tracking-[0.08em] transition-colors duration-150 ease-out active:scale-[0.97] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:opacity-45 motion-reduce:transition-none motion-reduce:active:scale-100 sm:h-6 ${
        on
          ? 'border-accent bg-accent text-accent-foreground'
          : 'border-dialog-edge bg-input text-dialog-hint hover:border-edge-strong'
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