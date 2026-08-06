import {
  useCallback,
  useEffect,
  useMemo,
  useState,
  type ReactNode,
} from "react";
import {
  GatewayClient,
  GatewayError,
  cachedThemeCatalogs,
} from "../lib/gateway";
import { ChevronIcon, CloseIcon } from "../components/icons";
import type {
  GatewayConn,
  PushDevice,
  PushStatus,
  ThemePref,
  ThemeSummary,
  Toggle,
  ToggleGroup,
  McpAuthFlow,
  McpServer,
  McpServerInput,
  McpTestResult,
} from "../lib/types";
import {
  acquirePushToken,
  cachedPushToken,
  deviceRegistration,
  isPushSupported,
  maskToken,
  pushPermission,
  pushPlatform,
  type PushPermission,
} from "../lib/push";
import { applyTheme, dedupeThemes, resolveLocalTheme } from "../lib/theme";
import { applyGatewayNotify } from "../lib/notify";
import {
  registerForPush,
  registeredIds,
  refusedRelayUrl,
  relayHost,
  relayUrlFor,
  unregisterFromPush,
} from "../lib/relay";
import {
  DEFAULT_SESSION_PAGE_SIZE,
  getGatewayNotify,
  getOfferDrafts,
  getSessionsPerPage,
  getThemePalette,
  getThemePref,
  loadConnections,
  setOfferDrafts as setOfferDraftsPref,
  setSessionsPerPage,
  setThemePalette,
  setThemePref,
} from "../lib/storage";
import { BUNDLED_THEMES } from "../lib/palettes";
import { Banner, Button, Input } from "../components/ui";
import {
  REACH_HINT,
  REACH_LABEL,
  bestAddress,
  hostOf,
  mergeAddresses,
  reachOf,
} from "../lib/endpoints";
import { onWake } from "../lib/wake";
import {
  ProviderNotice,
  ProviderSignOutButton,
  ProviderRemoveButton,
  AddProviderPanel,
  defaultFirstProviders,
  isProviderAuthed,
  preferredModelFirst,
  providerLimitsLine,
  providerStatusDot,
  providerStatusLine,
  unscopedMessage,
  useProviderAuth,
} from "../components/ProviderAuth";

interface Props {
  client: GatewayClient;
  gateway: GatewayConn;
  isPrimary: boolean;
  onMakePrimary?: () => void | Promise<void>;
  onRename?: (label: string | undefined) => void | Promise<void>;
  onRemove?: () => void | Promise<void>;
  onSelectAddress?: (url: string, pinned: boolean) => void | Promise<void>;
  onClose: () => void;
}

export function GatewaySettingsDialog({
  client,
  gateway,
  isPrimary,
  onMakePrimary,
  onRename,
  onRemove,
  onSelectAddress,
  onClose,
}: Props) {
  // Reopening the dialog paints the gateway's last known toggles immediately;
  // `load` below refreshes them (and `setSetting` patches the cache in place).
  const [groups, setGroups] = useState<ToggleGroup[] | null>(
    () => client.cachedSettings()?.groups ?? null,
  );
  const [err, setErr] = useState<string | null>(null);
  const [pending, setPending] = useState<string | null>(null);
  const [unreachable, setUnreachable] = useState(false);
  const [unauthorized, setUnauthorized] = useState(false);
  const [labelDraft, setLabelDraft] = useState(gateway.label ?? "");
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
    [client],
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
      if (event.key === "Escape") onClose();
    };
    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [onClose]);

  function patch(updated: Toggle) {
    setGroups(
      (current) =>
        current?.map((group) => ({
          ...group,
          toggles: group.toggles.map((toggle) =>
            toggle.id === updated.id ? updated : toggle,
          ),
        })) ?? null,
    );
  }

  async function flip(toggle: Toggle) {
    setPending(toggle.id);
    try {
      patch(await client.setSetting(toggle.id, "toggle"));
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function pick(toggle: Toggle, value: string) {
    setPending(toggle.id);
    try {
      patch(await client.setSetting(toggle.id, "value", value));
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  const settingCount =
    groups?.reduce((total, group) => total + group.toggles.length, 0) ?? 0;
  const status = unreachable
    ? { dot: "○", label: "Offline", tone: "text-err" }
    : unauthorized
      ? { dot: "●", label: "Unauthorized", tone: "text-warn-strong" }
      : isPrimary
        ? { dot: "●", label: "Primary", tone: "text-ok" }
        : { dot: "○", label: "Saved", tone: "text-dialog-hint" };

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
        aria-labelledby="gateway-settings-title"
      >
        <header className="flex min-h-12 shrink-0 items-center bg-dialog-title text-dialog-title-foreground">
          <div className="min-w-0 flex-1 px-3 py-2 sm:px-4">
            <h2
              id="gateway-settings-title"
              className="shrink-0 font-mono text-body font-black uppercase tracking-[0.12em]"
            >
              Machine settings
            </h2>
            <p className="truncate font-mono text-meta opacity-65">
              {gateway.url}
            </p>
          </div>
          <button
            type="button"
            className="grid min-w-10 self-stretch place-items-center border-l border-dialog-title-foreground/20 text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none"
            onClick={onClose}
            aria-label="Close machine settings"
          >
            <CloseIcon />
          </button>
        </header>

        <div className="shrink-0 border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4">
          <p className="text-pretty text-justify text-ui text-dialog-hint">
            These settings are stored by this gateway and shared with its TUI
            and every other client.
          </p>
        </div>

        <div className="min-h-0 flex-1 touch-pan-y space-y-3 overflow-x-hidden overflow-y-auto overscroll-contain p-3 sm:p-4">
          {err && <Banner kind="err">{err}</Banner>}

          <SettingsPanel
            title="Saved connection"
            description="Reconnect to this machine without re-scanning its QR code."
            meta={
              <span className={`font-black ${status.tone}`}>
                {status.dot} {status.label}
              </span>
            }
          >
            <div className="space-y-2 p-2.5">
              <Input
                value={labelDraft}
                placeholder="Name this machine"
                aria-label="Name this machine"
                autoCapitalize="none"
                autoCorrect="off"
                className="w-full"
                onChange={(event) => setLabelDraft(event.target.value)}
                onBlur={() => {
                  if (
                    (labelDraft.trim() || undefined) !==
                    (gateway.label ?? undefined)
                  )
                    void onRename?.(labelDraft.trim() || undefined);
                }}
                onKeyDown={(event) => {
                  if (event.key === "Enter") event.currentTarget.blur();
                  if (event.key === "Escape") {
                    setLabelDraft(gateway.label ?? "");
                    event.currentTarget.blur();
                  }
                }}
              />

              <p className="font-mono text-meta text-dialog-hint">
                This device remembers{" "}
                <span className="text-white">{gatewayHost(gateway.url)}</span>{" "}
                and its access token. The name is only shown in your machine
                list — the machine never sees it.
              </p>

              <div className="flex flex-wrap items-center gap-x-2 gap-y-1 border-t border-dialog-edge pt-2">
                {!isPrimary && (
                  <Button
                    onClick={() => {
                      void onMakePrimary?.();
                      onClose();
                    }}
                  >
                    Make primary
                  </Button>
                )}

                {!confirmRemove && <span className="flex-1" />}

                {confirmRemove ? (
                  <>
                    <span className="min-w-0 flex-1 font-mono text-meta text-dialog-hint">
                      Deletes the address and token from this device.
                      You&apos;ll need the QR code again.
                    </span>
                    <Button
                      variant="ghost"
                      onClick={() => setConfirmRemove(false)}
                    >
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
                  <Button
                    variant="danger"
                    onClick={() => setConfirmRemove(true)}
                  >
                    Forget this machine
                  </Button>
                )}
              </div>
            </div>
          </SettingsPanel>

          {onSelectAddress && (
            <AddressPanel gateway={gateway} onSelect={onSelectAddress} />
          )}

          {!unreachable && !unauthorized && <ProvidersPanel client={client} />}

          {!unreachable && !unauthorized && (
            <NotificationsPanel client={client} gateway={gateway} />
          )}

          {!unreachable && !unauthorized && <McpServersPanel client={client} />}

          {unreachable ? (
            <SettingsPanel title="Settings">
              <div className="flex flex-col items-center gap-3 px-4 py-8 text-center">
                <p className="font-mono text-body font-bold text-err">
                  Machine unreachable
                </p>
                <p className="font-mono text-meta text-dialog-hint">
                  Can't load settings — vis isn't responding on this machine.
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
                  The machine is online, but rejected this token. Re-pair from{" "}
                  <code className="text-accent-ink">vis gateway pair</code> and
                  paste the fresh link to load its settings.
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
              {/* `bg-panel-2` equals `bg-panel` in the shipped themes, so plain
                  tinted blocks were an invisible skeleton — a blank hole where
                  the settings should be. Bars are drawn in `--color-muted`. */}
              <div
                className="space-y-px bg-dialog-edge"
                role="status"
                aria-live="polite"
                aria-label="Loading settings"
              >
                <p className="bg-panel px-4 py-2 font-mono text-ui text-dialog-hint">
                  Loading settings…
                </p>
                {["w-1/2", "w-2/3", "w-2/5"].map((width) => (
                  <div
                    key={width}
                    className="animate-pulse bg-panel px-4 py-3.5 motion-reduce:animate-none"
                  >
                    <span className={`block h-2.5 bg-muted/30 ${width}`} />
                    <span className="mt-2 block h-1.5 w-1/4 bg-muted/20" />
                  </div>
                ))}
              </div>
            </SettingsPanel>
          ) : groups.length === 0 ? (
            <SettingsPanel title="Settings">
              <p className="px-4 py-6 text-center font-mono text-body text-dialog-hint">
                No settings exposed by this machine.
              </p>
            </SettingsPanel>
          ) : (
            groups.map((group) => (
              <SettingsPanel
                key={group.id}
                title={group.title}
                meta={`${group.toggles.length} ${group.toggles.length === 1 ? "option" : "options"}`}
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
                            toggle.type === "boolean" && toggle.enabled
                              ? "text-ok"
                              : "text-dialog-hint"
                          }`}
                          aria-hidden="true"
                        >
                          {toggle.type === "boolean"
                            ? toggle.enabled
                              ? "●"
                              : "○"
                            : "◆"}
                        </span>

                        <div className="min-w-0">
                          <p className="break-words font-mono text-ui font-bold text-white">
                            {toggle.label}
                          </p>
                          {toggle.description && (
                            <p className="mt-0.5 hyphens-auto break-words text-pretty text-justify text-meta text-dialog-hint">
                              {toggle.description}
                            </p>
                          )}
                        </div>

                        {toggle.type === "boolean" && (
                          <Switch
                            label={toggle.label}
                            on={!!toggle.enabled}
                            busy={busy}
                            disabled={busy}
                            onClick={() => flip(toggle)}
                          />
                        )}

                        {toggle.type === "enum" && toggle.choices && (
                          <div className="col-span-full col-start-2 flex min-w-0 flex-wrap gap-1.5">
                            {toggle.choices.map((choice) => {
                              const selected = toggle.value === choice;
                              return (
                                <button
                                  type="button"
                                  key={choice}
                                  disabled={busy}
                                  onClick={() => pick(toggle, choice)}
                                  className={`min-h-8 border px-2 py-0.5 font-mono text-chip font-bold transition-[background-color,border-color,color,transform,translate,scale,rotate] active:scale-[0.98] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:opacity-45 motion-reduce:transition-none mouse:min-h-6 ${
                                    selected
                                      ? "border-transparent bg-accent text-accent-foreground"
                                      : "border-transparent bg-panel-2 text-dialog-hint hover:bg-hover hover:text-white"
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
            {settingCount} {settingCount === 1 ? "option" : "options"}
          </span>
        </footer>
      </section>
    </div>
  );
}

function McpServersPanel({ client }: { client: GatewayClient }) {
  const [servers, setServers] = useState<McpServer[] | null>(null);
  const [showForm, setShowForm] = useState(false);
  const [transport, setTransport] = useState<"stdio" | "streamable_http">(
    "stdio",
  );
  const [name, setName] = useState("");
  const [command, setCommand] = useState("");
  const [args, setArgs] = useState("");
  const [cwd, setCwd] = useState("");
  const [url, setUrl] = useState("");
  const [env, setEnv] = useState("");
  const [headers, setHeaders] = useState("");
  const [busy, setBusy] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [test, setTest] = useState<McpTestResult | null>(null);
  const [authFlow, setAuthFlow] = useState<McpAuthFlow | null>(null);
  const [authInput, setAuthInput] = useState("");
  // The server being edited, or null while adding. Editing keys the save by the
  // ORIGINAL name: `POST /v1/mcp/servers` replaces by name, so a renamed field
  // would fork a second server instead of updating this one.
  const [editing, setEditing] = useState<McpServer | null>(null);

  const load = useCallback(async () => {
    try {
      setServers(await client.mcpServers());
      setError(null);
    } catch (e) {
      setError((e as Error).message);
    }
  }, [client]);

  useEffect(() => {
    void load();
  }, [load]);

  // A browser that can reach the gateway's loopback listener finishes the flow
  // by itself, and the pasted-URL leg is then never used. Poll so the UI notices.
  useEffect(() => {
    if (!authFlow) return;
    const timer = window.setInterval(() => {
      void (async () => {
        try {
          const verdict = await client.mcpAuthPoll(
            authFlow.server,
            authFlow.flow_id,
          );
          if (verdict.status === "ok") {
            setAuthFlow(null);
            setAuthInput("");
            await load();
          } else if (verdict.status === "error") {
            setAuthFlow(null);
            setError(verdict.error ?? "Authorization failed.");
          }
        } catch {
          // Expired or already swept; the Finish button reports it in context.
        }
      })();
    }, 2000);
    return () => window.clearInterval(timer);
  }, [authFlow, client, load]);

  // Editing loads the sanitized row back into the form. `env` and `headers` come
  // back BLANK on purpose: the gateway never sends secret values, and a save that
  // omits those keys keeps the ones it already stores.
  function openForm(server: McpServer | null) {
    setError(null);
    setTest(null);
    setEditing(server);
    setTransport(server?.transport ?? "stdio");
    setName(server?.name ?? "");
    setCommand(server?.command ?? "");
    setArgs((server?.args ?? []).join("\n"));
    setCwd(server?.cwd ?? "");
    setUrl(server?.url ?? "");
    setEnv("");
    setHeaders("");
    setShowForm(true);
  }

  function closeForm() {
    setShowForm(false);
    setEditing(null);
    setName("");
    setCommand("");
    setArgs("");
    setCwd("");
    setUrl("");
    setEnv("");
    setHeaders("");
  }

  const spec = (): McpServerInput => {
    const keyValues = (text: string) =>
      Object.fromEntries(
        text
          .split("\n")
          .map((line) => line.trim())
          .filter(Boolean)
          .map((line) => {
            const index = line.indexOf("=");
            return [line.slice(0, index).trim(), line.slice(index + 1)];
          })
          .filter(([key]) => key),
      );
    // An edit must not silently re-enable a disabled server or drop its timeout:
    // the row carries the whole non-secret spec, so both are carried back.
    const kept = {
      ...(editing ? { enabled: editing.enabled } : {}),
      ...(editing?.timeout_ms ? { timeout_ms: editing.timeout_ms } : {}),
    };
    return transport === "stdio"
      ? {
          ...kept,
          transport,
          command: command.trim(),
          args: args
            .split("\n")
            .map((arg) => arg.trim())
            .filter(Boolean),
          ...(cwd.trim() ? { cwd: cwd.trim() } : {}),
          ...(env.trim() ? { env: keyValues(env) } : {}),
        }
      : {
          ...kept,
          transport,
          url: url.trim(),
          ...(headers.trim() ? { headers: keyValues(headers) } : {}),
        };
  };

  const valid = () => {
    if (!name.trim()) return "Server name is required.";
    if (transport === "stdio" && !command.trim())
      return "An executable is required.";
    if (transport === "streamable_http" && !url.trim())
      return "An MCP endpoint is required.";
    return null;
  };

  async function validateAndSave() {
    const message = valid();
    if (message) return setError(message);
    // Keyed by the name the gateway already knows when editing.
    const target = editing ? editing.name : name.trim();
    const candidate = spec();
    setBusy("save");
    try {
      const result = await client.testMcpServer(target, candidate);
      setTest(result);
      await client.saveMcpServer(target, candidate);
      closeForm();
      await load();
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(null);
    }
  }

  async function toggle(server: McpServer) {
    setBusy(server.name);
    try {
      await client.setMcpServerEnabled(server.name, !server.enabled);
      await load();
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(null);
    }
  }

  // Kill / start are RUNTIME ops: they stop or revive the child process without
  // touching anybody's config, so they stay available for hand-written servers
  // too. A kill holds until Start — the gateway will not silently reconnect it.
  async function setRunning(server: McpServer, running: boolean) {
    setBusy(server.name);
    try {
      await (running
        ? client.startMcpServer(server.name)
        : client.killMcpServer(server.name));
      await load();
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(null);
    }
  }

  // OAuth is headless: the gateway mints the flow and keeps the PKCE verifier and
  // the token. This device only opens the URL and hands back where the browser
  // landed — which is the only thing a phone away from that gateway can do.
  async function authorize(server: McpServer) {
    setBusy(server.name);
    try {
      const flow = await client.mcpAuthStart(server.name);
      setError(null);
      setAuthInput("");
      setAuthFlow(flow);
      window.open(flow.url, "_blank", "noopener,noreferrer");
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(null);
    }
  }

  async function finishAuth() {
    if (!authFlow) return;
    setBusy(authFlow.server);
    try {
      const verdict = await client.mcpAuthComplete(
        authFlow.server,
        authFlow.flow_id,
        authInput.trim(),
      );
      if (verdict.status === "error") {
        setError(verdict.error ?? "Authorization failed.");
        return;
      }
      setAuthFlow(null);
      setAuthInput("");
      await load();
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(null);
    }
  }

  async function cancelAuth() {
    const flow = authFlow;
    setAuthFlow(null);
    setAuthInput("");
    if (!flow) return;
    try {
      await client.mcpAuthCancel(flow.server, flow.flow_id);
    } catch {
      // Already swept on the gateway — nothing left to release.
    }
  }

  async function signOut(server: McpServer) {
    if (
      !window.confirm(`Forget ${server.name}'s OAuth tokens on this gateway?`)
    )
      return;
    setBusy(server.name);
    try {
      await client.mcpAuthLogout(server.name);
      await load();
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(null);
    }
  }

  async function remove(server: McpServer) {
    if (!window.confirm(`Remove ${server.name} from this gateway?`)) return;
    setBusy(server.name);
    try {
      await client.deleteMcpServer(server.name);
      await load();
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(null);
    }
  }

  return (
    <SettingsPanel
      title="MCP servers"
      description="Tools run on this gateway and are shared with every client. Commands, tokens, and environment values never leave this machine."
      meta={servers === null ? "loading" : `${servers.length} configured`}
    >
      <div className="divide-y divide-dialog-edge">
        {error && <Banner kind="err">{error}</Banner>}
        {servers?.map((server) => (
          <div
            key={server.name}
            className="grid grid-cols-[minmax(0,1fr)_auto] gap-x-3 px-3 py-3 sm:px-4 sm:py-2.5"
          >
            <div className="min-w-0">
              <p className="truncate font-mono text-ui font-bold text-white">
                {server.name}
              </p>
              <p className="mt-0.5 truncate font-mono text-meta text-dialog-hint">
                {server.transport === "stdio" ? server.command : server.url} ·{" "}
                {server.tools} tools ·{" "}
                {server.is_killed
                  ? "killed"
                  : server.is_connected
                    ? "connected"
                    : server.enabled
                      ? "connecting"
                      : "disabled"}
                {server.url
                  ? server.is_authorized
                    ? " · signed in"
                    : " · not signed in"
                  : null}
              </p>
            </div>
            <div className="flex flex-wrap items-center justify-end gap-1.5">
              {server.url && (
                <Button
                  variant="ghost"
                  disabled={busy !== null}
                  onClick={() => void authorize(server)}
                >
                  {server.is_authorized ? "Re-auth" : "Sign in"}
                </Button>
              )}
              {server.url && server.is_authorized && (
                <Button
                  variant="ghost"
                  disabled={busy !== null}
                  onClick={() => void signOut(server)}
                >
                  Sign out
                </Button>
              )}
              <Button
                variant="ghost"
                disabled={busy !== null}
                onClick={() => void setRunning(server, server.is_killed)}
              >
                {server.is_killed ? "Start" : "Kill"}
              </Button>
              {server.is_managed ? (
                <>
                  <Switch
                    label={`${server.name} MCP server`}
                    on={server.enabled}
                    busy={busy === server.name}
                    disabled={busy !== null}
                    onClick={() => void toggle(server)}
                  />
                  <Button
                    variant="ghost"
                    disabled={busy !== null}
                    onClick={() => openForm(server)}
                  >
                    Edit
                  </Button>
                  <Button
                    variant="ghost"
                    disabled={busy !== null}
                    onClick={() => void remove(server)}
                  >
                    Remove
                  </Button>
                </>
              ) : (
                <p className="font-mono text-chip text-dialog-hint">
                  config file
                </p>
              )}
            </div>
            {authFlow?.server === server.name && (
              <div className="col-span-2 mt-2 space-y-2 border-t border-dialog-edge pt-2">
                <p className="font-mono text-meta text-dialog-hint">
                  Approve the sign-in in the browser tab we opened. If it stayed
                  shut, use this link — then paste the URL the browser lands on.
                  Nothing secret ever reaches this device.
                </p>
                <a
                  className="block truncate font-mono text-meta text-accent underline"
                  href={authFlow.url}
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  {authFlow.url}
                </a>
                <Input
                  value={authInput}
                  onChange={(event) => setAuthInput(event.target.value)}
                  placeholder="http://127.0.0.1:…/callback?code=…"
                  inputMode="url"
                  autoCapitalize="none"
                  autoCorrect="off"
                />
                <div className="flex flex-wrap justify-end gap-2">
                  <Button
                    variant="ghost"
                    disabled={busy !== null}
                    onClick={() => void cancelAuth()}
                  >
                    Cancel
                  </Button>
                  <Button
                    disabled={busy !== null || !authInput.trim()}
                    onClick={() => void finishAuth()}
                  >
                    Finish sign-in
                  </Button>
                </div>
              </div>
            )}
          </div>
        ))}
        {servers?.length === 0 && !showForm && (
          <p className="px-3 py-5 font-mono text-meta text-dialog-hint sm:px-4">
            No MCP servers on this gateway.
          </p>
        )}
        {!showForm ? (
          <div className="p-2.5">
            <Button onClick={() => openForm(null)}>Add MCP server</Button>
          </div>
        ) : (
          <div className="space-y-3 p-2.5">
            <div
              className="grid grid-cols-2 gap-1"
              role="group"
              aria-label="MCP transport"
            >
              {(["stdio", "streamable_http"] as const).map((kind) => (
                <button
                  key={kind}
                  type="button"
                  onClick={() => setTransport(kind)}
                  className={`min-h-8 border px-2 font-mono text-chip font-bold uppercase focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 ${transport === kind ? "border-transparent bg-accent text-accent-foreground" : "border-transparent bg-panel-2 text-dialog-hint hover:bg-hover hover:text-white"}`}
                >
                  {kind === "stdio" ? "Local command" : "Streamable HTTP"}
                </button>
              ))}
            </div>
            <FormLabel label="Server name">
              {editing ? (
                <p className="font-mono text-ui text-white">{editing.name}</p>
              ) : (
                <Input
                  value={name}
                  onChange={(event) => setName(event.target.value)}
                  placeholder="filesystem"
                  autoCapitalize="none"
                  autoCorrect="off"
                />
              )}
            </FormLabel>
            {transport === "stdio" ? (
              <>
                <FormLabel label="Executable">
                  <Input
                    value={command}
                    onChange={(event) => setCommand(event.target.value)}
                    placeholder="npx"
                    autoCapitalize="none"
                    autoCorrect="off"
                  />
                </FormLabel>
                <FormLabel
                  label="Arguments — one per line"
                  hint="Arguments are passed directly, never through a shell."
                >
                  <textarea
                    value={args}
                    onChange={(event) => setArgs(event.target.value)}
                    placeholder={
                      "-y\n@modelcontextprotocol/server-filesystem\n/path"
                    }
                    className="min-h-24 w-full resize-y border border-dialog-edge bg-input px-2.5 py-2 font-mono text-meta text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none"
                  />
                </FormLabel>
                <FormLabel label="Working directory (optional)">
                  <Input
                    value={cwd}
                    onChange={(event) => setCwd(event.target.value)}
                    placeholder="/workspace"
                    autoCapitalize="none"
                    autoCorrect="off"
                  />
                </FormLabel>
                <FormLabel
                  label="Environment variables (optional)"
                  hint={
                    editing
                      ? "One NAME=value per line. Leave blank to keep the values already stored."
                      : "One NAME=value per line. Values are write-only after saving."
                  }
                >
                  <textarea
                    value={env}
                    onChange={(event) => setEnv(event.target.value)}
                    placeholder="API_TOKEN=…"
                    className="min-h-20 w-full resize-y border border-dialog-edge bg-input px-2.5 py-2 font-mono text-meta text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none"
                  />
                </FormLabel>
              </>
            ) : (
              <>
                <FormLabel label="Streamable HTTP endpoint">
                  <Input
                    value={url}
                    onChange={(event) => setUrl(event.target.value)}
                    placeholder="https://mcp.example.com/mcp"
                    inputMode="url"
                    autoCapitalize="none"
                    autoCorrect="off"
                  />
                </FormLabel>
                <FormLabel
                  label="Headers (optional)"
                  hint={
                    editing
                      ? "One NAME=value per line. Leave blank to keep the values already stored."
                      : "One NAME=value per line. Values are write-only after saving."
                  }
                >
                  <textarea
                    value={headers}
                    onChange={(event) => setHeaders(event.target.value)}
                    placeholder="Authorization=Bearer …"
                    className="min-h-20 w-full resize-y border border-dialog-edge bg-input px-2.5 py-2 font-mono text-meta text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none"
                  />
                </FormLabel>
              </>
            )}
            {test && (
              <Banner kind="ok">
                Validated {test.name}: {test.tools.length} tools discovered.
              </Banner>
            )}
            <div className="flex flex-wrap justify-end gap-2 border-t border-dialog-edge pt-2">
              <Button
                variant="ghost"
                disabled={busy !== null}
                onClick={() => closeForm()}
              >
                Cancel
              </Button>
              <Button
                disabled={busy !== null}
                onClick={() => void validateAndSave()}
              >
                {busy === "save"
                  ? "Validating…"
                  : editing
                    ? "Validate & update"
                    : "Validate & save"}
              </Button>
            </div>
          </div>
        )}
      </div>
    </SettingsPanel>
  );
}

function FormLabel({
  label,
  hint,
  children,
}: {
  label: string;
  hint?: string;
  children: ReactNode;
}) {
  return (
    <label className="block space-y-1">
      <span className="block font-mono text-chip font-bold text-white">
        {label}
      </span>
      {children}
      {hint && (
        <span className="block font-mono text-chip text-dialog-hint">
          {hint}
        </span>
      )}
    </label>
  );
}

/** Settings owned by this companion installation, never by a gateway. */
export function ApplicationSettingsDialog({
  onClose,
}: {
  onClose: () => void;
}) {
  const [pref, setPref] = useState<ThemePref>("blockether-light");
  // Seeded from the catalogs already cached for the paired gateways (persisted
  // across a cold start), so the FIRST frame is the finished list. Anything
  // fetched below lands on top of the same rows instead of replacing them.
  const [themes, setThemes] = useState<ThemeSummary[]>(() =>
    dedupeThemes(...cachedThemeCatalogs(), BUNDLED_THEMES),
  );
  const [pageSize, setPageSize] = useState(DEFAULT_SESSION_PAGE_SIZE);
  // A draft is an expert move, so the sessions list only asks about one when it was
  // told to. Off, "New session" is a single verb that starts in the project itself.
  const [offerDrafts, setOfferDrafts] = useState(false);
  useEffect(() => {
    void getOfferDrafts().then(setOfferDrafts);
  }, []);
  const [pending, setPending] = useState<string | null>(null);
  const [err, setErr] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    void (async () => {
      const [theme, sessions, cachedPalette, connections] = await Promise.all([
        getThemePref(),
        getSessionsPerPage(),
        getThemePalette(),
        loadConnections(),
      ]);
      if (cancelled) return;
      setPref(theme);
      setPageSize(sessions);
      const paint = () =>
        setThemes(
          dedupeThemes(
            ...cachedThemeCatalogs(),
            cachedPalette ? [cachedPalette] : [],
            BUNDLED_THEMES,
          ),
        );
      paint();

      // Each gateway advertises its complete catalog, and that catalog only moves
      // when someone installs a theme — so this is a CACHED read (THEME_TTL_MS).
      // Reopening settings inside the window touches the network zero times and
      // repaints the identical list; a cold-but-cached machine paints instantly
      // and revalidates underneath.
      const clients = connections.map(
        (connection) => new GatewayClient(connection),
      );
      if (clients.every((client) => client.isThemeFresh())) return;
      await Promise.allSettled(clients.map((client) => client.themeCatalog()));
      if (cancelled) return;
      // Unreachable gateways keep contributing their last known catalog, so a
      // machine that is merely asleep never makes palettes vanish from the list.
      paint();
    })();
    return () => {
      cancelled = true;
    };
  }, []);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") onClose();
    };
    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [onClose]);

  async function chooseTheme(next: ThemeSummary) {
    setPending(`theme:${next.id}`);
    try {
      await Promise.all([setThemePref(next.id), setThemePalette(next)]);
      setPref(next.id);
      applyTheme(resolveLocalTheme(next.id, next));
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function choosePageSize(next: number) {
    setPending(`pageSize:${next}`);
    try {
      await setSessionsPerPage(next);
      setPageSize(next);
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function chooseOfferDrafts(next: boolean) {
    setPending("offerDrafts");
    try {
      await setOfferDraftsPref(next);
      setOfferDrafts(next);
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
        className="flex h-[92%] max-h-[calc(100%-env(safe-area-inset-top))] w-full max-w-xl flex-col overflow-hidden border-x border-t border-dialog-edge bg-panel shadow-none transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-6 starting:opacity-0 motion-reduce:transition-none sm:h-auto sm:max-h-full sm:border sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:starting:translate-y-2"
        role="dialog"
        aria-modal="true"
        aria-labelledby="application-settings-title"
      >
        <header className="flex min-h-12 shrink-0 items-center bg-dialog-title text-dialog-title-foreground">
          <div className="min-w-0 flex-1 px-3 py-2 sm:px-4">
            <h2
              id="application-settings-title"
              className="font-mono text-body font-black uppercase tracking-[0.12em]"
            >
              Application settings
            </h2>
            <p className="font-mono text-meta opacity-65">This device</p>
          </div>
          <button
            type="button"
            className="grid min-w-10 self-stretch place-items-center border-l border-dialog-title-foreground/20 text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none"
            onClick={onClose}
            aria-label="Close application settings"
          >
            <CloseIcon />
          </button>
        </header>

        <div className="shrink-0 border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4">
          <p className="text-pretty text-justify text-ui text-dialog-hint">
            These choices affect this copy of Vis only. They are never sent to a
            gateway.
          </p>
        </div>

        <div className="min-h-0 flex-1 space-y-3 overflow-y-auto overscroll-contain p-3 sm:p-4">
          {err && <Banner kind="err">{err}</Banner>}
          <SettingsPanel
            title="Theme"
            description="All themes advertised by your paired gateways. Duplicate theme ids appear once; the choice is saved on this device."
            meta={`${themes.length} available`}
          >
            <div className="grid grid-cols-1 gap-px bg-dialog-edge min-[420px]:grid-cols-2">
              {themes.map((choice) => {
                const selected = pref === choice.id;
                return (
                  <button
                    type="button"
                    key={choice.id}
                    disabled={pending?.startsWith("theme:") ?? false}
                    onClick={() => void chooseTheme(choice)}
                    className={`flex min-h-10 items-center justify-between gap-3 px-3 py-1.5 text-left transition-[background-color,color,transform,translate,scale,rotate] duration-150 active:scale-[0.99] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent disabled:opacity-45 motion-reduce:transition-none mouse:min-h-9 ${selected ? "bg-accent text-accent-foreground" : "bg-input text-white hover:bg-hover"}`}
                    aria-pressed={selected}
                  >
                    <span className="min-w-0">
                      <span className="block truncate font-mono text-ui font-bold">
                        {choice.display_name}
                      </span>
                      <span className="block font-mono text-chip uppercase tracking-wider opacity-65">
                        {choice.mode}
                      </span>
                    </span>
                    <span
                      className="shrink-0 font-mono text-meta font-black"
                      aria-hidden="true"
                    >
                      {selected ? "●" : "○"}
                    </span>
                  </button>
                );
              })}
            </div>
          </SettingsPanel>

          <SettingsPanel
            title="Sessions per project"
            description="How many sessions each project lists before paging. Collapsed projects show this many live sessions; expanding pages the rest in steps of the same size."
            meta="saved on this device"
          >
            <div className="grid grid-cols-1 gap-px bg-dialog-edge min-[420px]:grid-cols-3">
              {[
                { size: 5, label: "compact" },
                { size: 10, label: "balanced" },
                { size: 15, label: "detailed" },
              ].map(({ size, label }) => {
                const selected = size === pageSize;
                return (
                  <button
                    type="button"
                    key={size}
                    disabled={pending?.startsWith("pageSize:") ?? false}
                    onClick={() => void choosePageSize(size)}
                    className={`flex min-h-10 min-w-0 items-center justify-between gap-2 px-3 py-1.5 text-left transition-[background-color,color,transform,translate,scale,rotate] duration-150 active:scale-[0.99] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent disabled:opacity-45 motion-reduce:transition-none mouse:min-h-9 ${selected ? "bg-accent text-accent-foreground" : "bg-input text-white hover:bg-hover"}`}
                    aria-pressed={selected}
                  >
                    <span className="min-w-0">
                      <span className="block truncate font-mono text-ui font-bold">
                        {size}
                      </span>
                      <span className="block font-mono text-chip uppercase tracking-wider opacity-65">
                        {label}
                      </span>
                    </span>
                    <span
                      className="shrink-0 font-mono text-meta font-black"
                      aria-hidden="true"
                    >
                      {selected ? "●" : "○"}
                    </span>
                  </button>
                );
              })}
            </div>
          </SettingsPanel>

          <SettingsPanel
            title="Offer drafts"
            description="A draft is a private copy of a project, so an agent can work without touching the repo. With this off, a new session always starts in the project itself and the question is never asked."
            meta={offerDrafts ? "on" : "off"}
          >
            <button
              type="button"
              aria-pressed={offerDrafts}
              disabled={pending === "offerDrafts"}
              onClick={() => void chooseOfferDrafts(!offerDrafts)}
              className={`flex min-h-11 w-full items-center justify-between gap-3 px-3 py-1.5 text-left transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent disabled:opacity-45 motion-reduce:transition-none ${offerDrafts ? "bg-accent text-accent-foreground" : "bg-input text-white hover:bg-hover"}`}
            >
              <span className="min-w-0">
                <span className="block truncate font-mono text-ui font-bold">
                  Ask where a session starts
                </span>
                <span className="block font-mono text-chip uppercase tracking-wider opacity-65">
                  {offerDrafts
                    ? "a machine's menu offers a draft too"
                    : "a machine's menu starts in the project"}
                </span>
              </span>
              <span className="shrink-0 font-mono text-chip uppercase tracking-wider">
                {offerDrafts ? "on" : "off"}
              </span>
            </button>
          </SettingsPanel>
        </div>
      </section>
    </div>
  );
}

/**
 * Provider accounts ON THIS GATEWAY: live auth status, sign-in, a manual
 * re-check, and sign-out — the whole terminal-free equivalent of
 * `vis-agent auth login/logout/status`.
 *
 * Every credential lives on the daemon: this panel starts flows, polls them,
 * and asks for verdicts, but never holds a token, verifier, or device code.
 * The exchange itself is `useProviderAuth`, shared with the router dialog.
 */
function ProvidersPanel({ client }: { client: GatewayClient }) {
  const auth = useProviderAuth(client);
  const { providers, err, note, pending } = auth;
  const [expanded, setExpanded] = useState<string | null>(null);
  const [modelDrafts, setModelDrafts] = useState<Record<string, string>>({});
  const signedIn = providers?.filter(isProviderAuthed).length ?? 0;
  // A message that names a provider is painted inside THAT provider's card by
  // `ProviderNotice`; only what has no card left to live in surfaces here.
  const fleetErr = unscopedMessage(err, providers);
  const fleetNote = unscopedMessage(note, providers);

  const tagModel = async (
    role: "default" | "fallback",
    providerId: string,
    model: string,
  ) => {
    if (!model) return;
    auth.setPending(`${role}:${providerId}`);
    auth.setErr(null);
    auth.setNote(null);
    try {
      if (role === "fallback") {
        await client.setFallbackModel(providerId, model);
      } else {
        await client.setDefaultModel(providerId, model);
      }
      await auth.reload(undefined, { force: true });
      auth.setNote(
        `${role === "fallback" ? "Fallback" : "Default"} set to ${providerId} / ${model}.`,
        providerId,
      );
    } catch (e) {
      auth.setErr(
        e instanceof GatewayError ? e.message : String(e),
        providerId,
      );
    } finally {
      auth.setPending(null);
    }
  };

  const setDefault = (providerId: string, model: string) =>
    tagModel("default", providerId, model);
  const setFallback = (providerId: string, model: string) =>
    tagModel("fallback", providerId, model);

  const clearFallback = async (providerId: string) => {
    auth.setPending(`fallback:${providerId}`);
    auth.setErr(null);
    auth.setNote(null);
    try {
      await client.clearFallbackModel();
      await auth.reload(undefined, { force: true });
      auth.setNote("Fallback cleared.", providerId);
    } catch (e) {
      auth.setErr(
        e instanceof GatewayError ? e.message : String(e),
        providerId,
      );
    } finally {
      auth.setPending(null);
    }
  };

  return (
    <SettingsPanel
      title="Providers"
      description="Sign in to model providers so this machine can reach them, then tag the default model and a fallback on another provider."
      meta={
        providers ? `${signedIn}/${providers.length} signed in` : "checking…"
      }
    >
      <div className="space-y-2 p-3">
        {fleetErr && <Banner kind="err">{fleetErr.text}</Banner>}
        {fleetNote && <Banner kind="ok">{fleetNote.text}</Banner>}

        {providers === null && (
          <p className="py-4 text-center font-mono text-meta text-dialog-hint">
            Checking provider sign-in…
          </p>
        )}

        {providers?.length === 0 && (
          <p className="py-4 text-center font-mono text-meta text-dialog-hint">
            No providers configured on this machine.
          </p>
        )}

        {defaultFirstProviders(providers ?? []).map((provider) => {
          const dot = providerStatusDot(provider);
          const authed = isProviderAuthed(provider);
          const limits = providerLimitsLine(provider);
          const open = expanded === provider.id;
          const orderedModels = preferredModelFirst(
            provider.models,
            provider.default_model ?? provider.fallback_model,
          );
          const selectedModel =
            modelDrafts[provider.id] ??
            provider.default_model ??
            provider.fallback_model ??
            orderedModels[0] ??
            "";
          const settingDefault = pending === `default:${provider.id}`;
          const settingFallback = pending === `fallback:${provider.id}`;
          const tagging = settingDefault || settingFallback;
          const isDefaultModel =
            provider.is_default && provider.default_model === selectedModel;
          const isFallbackModel =
            provider.is_fallback && provider.fallback_model === selectedModel;

          return (
            <div
              key={provider.id}
              className="border border-dialog-edge bg-panel-2"
            >
              <button
                type="button"
                className="flex min-h-12 w-full items-center gap-2 px-3 py-2 text-left transition-colors hover:bg-hover focus-visible:bg-hover focus-visible:outline-none"
                onClick={() => setExpanded(open ? null : provider.id)}
                aria-expanded={open}
              >
                <span
                  className={`font-mono text-body ${dot.tone}`}
                  aria-label={dot.label}
                >
                  {dot.glyph}
                </span>
                <span className="min-w-0 flex-1">
                  <span className="flex min-w-0 items-center gap-2">
                    <span className="truncate font-mono text-ui font-bold text-white">
                      {provider.label}
                    </span>
                  </span>
                  <span className="block truncate font-mono text-meta text-dialog-hint">
                    {provider.is_default && provider.default_model
                      ? `${provider.default_model} · default · ${providerStatusLine(provider)}`
                      : provider.is_fallback && provider.fallback_model
                        ? `${provider.fallback_model} · fallback · ${providerStatusLine(provider)}`
                        : providerStatusLine(provider)}
                  </span>
                </span>
                <ChevronIcon open={open} className="size-3.5 text-dialog-hint" />
              </button>

              <ProviderNotice auth={auth} provider={provider} />

              {open && (
                <div className="space-y-3 border-t border-dialog-edge p-3">
                  {limits && (
                    <p className="break-words font-mono text-meta text-dialog-hint">
                      {limits}
                    </p>
                  )}
                  <p className="break-words font-mono text-chip text-dialog-hint">
                    {provider.id} · {provider.models.length}{" "}
                    {provider.models.length === 1 ? "model" : "models"}{" "}
                    available
                  </p>

                  <div className="space-y-2">
                    <label
                      htmlFor={`model-${provider.id}`}
                      className="block font-mono text-meta font-bold text-dialog-hint"
                    >
                      {settingDefault
                        ? "Model · saving default…"
                        : settingFallback
                          ? "Model · saving fallback…"
                          : "Model"}
                    </label>
                    <select
                      id={`model-${provider.id}`}
                      value={selectedModel}
                      disabled={provider.models.length === 0 || tagging}
                      onChange={(event) => {
                        const model = event.target.value;
                        setModelDrafts((drafts) => ({
                          ...drafts,
                          [provider.id]: model,
                        }));
                      }}
                      className="min-h-10 w-full min-w-0 border border-dialog-edge bg-input px-3 font-mono text-ui text-white outline-none transition-colors focus:border-accent disabled:opacity-50"
                    >
                      {orderedModels.map((model) => (
                        <option key={model} value={model}>
                          {model}
                        </option>
                      ))}
                    </select>
                    <div className="flex flex-col gap-2 sm:flex-row">
                      <Button
                        className="flex-1"
                        variant={isDefaultModel ? "ghost" : "solid"}
                        disabled={!selectedModel || tagging || isDefaultModel}
                        onClick={() =>
                          void setDefault(provider.id, selectedModel)
                        }
                      >
                        {isDefaultModel ? "Default" : "Set as default"}
                      </Button>
                      <Button
                        className="flex-1"
                        variant="ghost"
                        disabled={
                          !selectedModel ||
                          tagging ||
                          provider.is_default ||
                          isFallbackModel
                        }
                        onClick={() =>
                          void setFallback(provider.id, selectedModel)
                        }
                      >
                        {isFallbackModel ? "Fallback" : "Set as fallback"}
                      </Button>
                      {provider.is_fallback && (
                        <Button
                          className="flex-1"
                          variant="ghost"
                          disabled={tagging}
                          onClick={() => void clearFallback(provider.id)}
                        >
                          Clear fallback
                        </Button>
                      )}
                    </div>
                    <p className="break-words font-mono text-chip text-dialog-hint">
                      {provider.is_default
                        ? "This is the default provider, so it cannot also hold the fallback — tag another provider instead."
                        : "The default runs every turn; the fallback takes over on another provider when it cannot."}
                    </p>
                  </div>

                  <div className="flex flex-col gap-2 sm:flex-row">
                    <Button
                      className="flex-1"
                      variant={authed ? "ghost" : "solid"}
                      disabled={pending === `auth:${provider.id}`}
                      onClick={() => void auth.signIn(provider)}
                    >
                      {pending === `auth:${provider.id}`
                        ? "Starting…"
                        : authed
                          ? "Sign in again"
                          : "Sign in"}
                    </Button>
                    <Button
                      variant="ghost"
                      className="flex-1"
                      disabled={pending === `status:${provider.id}`}
                      onClick={() => void auth.recheck(provider)}
                    >
                      {pending === `status:${provider.id}`
                        ? "Checking…"
                        : "Check status"}
                    </Button>
                    {authed && (
                      <ProviderSignOutButton
                        auth={auth}
                        provider={provider}
                        className="flex-1"
                      />
                    )}
                    <ProviderRemoveButton
                      auth={auth}
                      provider={provider}
                      className="flex-1"
                    />
                  </div>
                </div>
              )}
            </div>
          );
        })}

        {providers !== null && (
          <AddProviderPanel auth={auth} className="w-full" />
        )}
      </div>
    </SettingsPanel>
  );
}

/**
 * Native push ON THIS GATEWAY: whether it can push at all, and whether THIS
 * device is registered.
 *
 * The token itself never round-trips through the UI — the gateway masks every
 * token it stores, and the app matches its own row by computing the same mask.
 */
function NotificationsPanel({
  client,
  gateway,
}: {
  client: GatewayClient;
  gateway: GatewayConn;
}) {
  const [push, setPush] = useState<PushStatus | null>(null);
  const [devices, setDevices] = useState<PushDevice[] | null>(null);
  const [perm, setPerm] = useState<PushPermission>("unsupported");
  const [err, setErr] = useState<string | null>(null);
  const [note, setNote] = useState<string | null>(null);
  const [busy, setBusy] = useState<"enable" | "disable" | null>(null);
  // An OLDER gateway simply has no /v1/devices route. That is not an error the
  // user can act on — it is a missing capability upstream — so the whole panel
  // (and every button in it) disappears instead of offering calls that 404.
  const [unsupported, setUnsupported] = useState(false);
  // The switch itself, remembered per gateway: a machine you silenced stays
  // silenced across relaunches, and a machine you want stays registered even
  // while another gateway is the one you have open.
  const [notify, setNotify] = useState(true);

  const load = useCallback(
    async (signal?: AbortSignal) => {
      try {
        const [state, permission, wanted] = await Promise.all([
          client.devices(signal),
          pushPermission(),
          getGatewayNotify(gateway.url),
        ]);
        if (signal?.aborted) return;
        setPush(state.push);
        setDevices(state.devices);
        setPerm(permission);
        setNotify(wanted);
        setErr(null);
      } catch (e) {
        if (signal?.aborted) return;
        if (
          e instanceof GatewayError &&
          (e.status === 404 || e.status === 501)
        ) {
          setUnsupported(true);
          setDevices([]);
          setErr(null);
          return;
        }
        setDevices([]);
        setErr(e instanceof GatewayError ? e.message : String(e));
      }
    },
    [client, gateway.url],
  );

  useEffect(() => {
    const ctrl = new AbortController();
    void load(ctrl.signal);
    return () => ctrl.abort();
  }, [load]);

  const token = cachedPushToken();
  // This device can appear in the list under either of its names: its push token,
  // or the relay grant a machine without a signing key was handed instead.
  const [masks, setMasks] = useState<string[]>([]);
  useEffect(() => {
    let stale = false;
    void (async () => {
      const ids = await registeredIds(token ?? "");
      if (!stale) setMasks(ids.map(maskToken));
    })();
    return () => {
      stale = true;
    };
  }, [token, devices]);
  const mine = (devices ?? []).find((d) => masks.includes(d.token_preview));
  const registered = Boolean(mine);
  // Both halves have to agree: this machine holds the token AND this device
  // still wants alerts from it.
  const notifying = registered && notify;
  const supported = isPushSupported();

  const enable = useCallback(async () => {
    setBusy("enable");
    setErr(null);
    setNote(null);
    try {
      const fresh = await acquirePushToken();
      await applyGatewayNotify(gateway.url, true, () =>
        registerForPush(deviceRegistration(fresh), client.pushTarget()),
      );
      setNotify(true);
      setNote(
        "This device will be notified when a turn finishes on this machine.",
      );
      await load();
    } catch (e) {
      // The switch may already be stored even though the machine refused the
      // call, so show what this device WILL do once it can reach it again.
      setNotify(await getGatewayNotify(gateway.url));
      setErr(e instanceof GatewayError ? e.message : (e as Error).message);
    } finally {
      setBusy(null);
    }
  }, [client, gateway.url, load]);

  const disable = useCallback(async () => {
    const current = cachedPushToken();
    if (!current) return;
    setBusy("disable");
    setErr(null);
    setNote(null);
    try {
      await applyGatewayNotify(gateway.url, false, () =>
        unregisterFromPush(current, client.pushTarget()),
      );
      setNotify(false);
      setNote("This device will no longer be notified by this machine.");
      await load();
    } catch (e) {
      setNotify(await getGatewayNotify(gateway.url));
      setErr(e instanceof GatewayError ? e.message : (e as Error).message);
    } finally {
      setBusy(null);
    }
  }, [client, gateway.url, load]);

  // Push has two independent halves; this device only cares about its own. An
  // iOS-only gateway can sign for an iPhone and not for a Pixel, so the verdict
  // is per platform, never the summary flag.
  const provider = pushPlatform() === "android" ? push?.fcm : push?.apns;
  // A machine holding no signing key is not silent: it reaches this device
  // through a relay, which needs nothing configured on either side — the app
  // was built naming one, and so was the gateway.
  const relayUrl = relayUrlFor(push ?? undefined, pushPlatform());
  // The one way that breaks is an operator who named an address we refuse. That
  // is a MISCONFIGURED machine, not a machine without credentials — and the
  // address is the only part its operator can fix.
  const refusedRelay = refusedRelayUrl(push ?? undefined, pushPlatform());
  const available =
    Boolean(relayUrl) ||
    (provider ? provider.is_available : (push?.is_available ?? false));

  // Gateway too old to know about push at all: render nothing.
  if (unsupported) return null;

  return (
    <SettingsPanel
      title="Notifications"
      description="Alerts from THIS machine only — every paired machine has its own switch."
      meta={
        push
          ? available
            ? `${push.devices} device${push.devices === 1 ? "" : "s"} · ${relayUrl ? `via ${relayHost(relayUrl)}` : pushPlatform() === "android" ? (push.fcm?.project_id ?? "fcm") : (push.environment ?? "production")}`
            : "relay not https"
          : "checking…"
      }
    >
      <div className="space-y-2 p-3">
        {err && <Banner kind="err">{err}</Banner>}
        {note && <Banner kind="ok">{note}</Banner>}

        {push && !available && refusedRelay && (
          <Banner kind="warn">
            This machine relays notifications through {refusedRelay}, which is
            not https — this device will not hand a push grant to an address on
            the wire. Unset VIS_PUSH_RELAY_URL there and it goes back to the
            relay this app was built with; point it at an https address to keep
            your own.
          </Banner>
        )}

        {!supported && (
          <Banner kind="warn">
            Native alerts need the iOS or Android app. The web build can stay
            open instead.
          </Banner>
        )}

        {supported && perm === "denied" && (
          <Banner kind="warn">
            Notifications are turned off for Vis in system Settings — enable
            them there first.
          </Banner>
        )}

        <div className="flex flex-wrap gap-2">
          {supported && !notifying && (
            <Button
              className="min-h-9 flex-1 px-3 font-mono text-meta"
              disabled={busy !== null || !available}
              onClick={() => void enable()}
            >
              {busy === "enable"
                ? "Registering…"
                : "Notify me from this machine"}
            </Button>
          )}
          {supported && notifying && (
            <Button
              variant="danger"
              className="min-h-9 flex-1 px-3 font-mono text-meta"
              disabled={busy !== null}
              onClick={() => void disable()}
            >
              {busy === "disable"
                ? "Removing…"
                : "Stop notifying me from this machine"}
            </Button>
          )}
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
                {device.label ?? device.platform ?? "device"}
                {masks.includes(device.token_preview) && (
                  <span className="ml-2 text-chip font-bold uppercase tracking-wider text-accent">
                    this device
                  </span>
                )}
              </span>
              <span className="block truncate font-mono text-meta text-dialog-hint">
                {device.token_preview} · {device.environment ?? "production"}
                {device.client_version ? ` · v${device.client_version}` : ""}
              </span>
            </span>
          </div>
        ))}

        {devices?.length === 0 && (
          <p className="py-4 text-center font-mono text-meta text-dialog-hint">
            No devices registered with this machine.
          </p>
        )}
      </div>
    </SettingsPanel>
  );
}

function SettingsPanel({
  title,
  description,
  meta,
  children,
}: {
  title: string;
  description?: string;
  meta?: ReactNode;
  children: ReactNode;
}) {
  return (
    <section className="min-w-0 overflow-hidden border border-dialog-edge bg-panel transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
      <header
        className={`flex min-h-8 gap-3 border-b border-dialog-edge bg-panel-2 px-3 py-1.5 ${
          description ? "items-start" : "items-center"
        }`}
      >
        <div className="min-w-0 flex-1">
          <h3 className="min-w-0 truncate border-l-2 border-accent pl-2 font-mono text-meta font-black uppercase tracking-[0.12em] text-white">
            {title}
          </h3>
          {description && (
            <p className="mt-0.5 pl-2 text-pretty text-justify font-mono text-chip text-dialog-hint">
              {description}
            </p>
          )}
        </div>
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
      aria-label={`${label}: ${on ? "on" : "off"}`}
      aria-checked={on}
      aria-busy={busy}
      disabled={disabled}
      onClick={onClick}
      className={`mt-0.5 inline-flex h-8 w-[3.25rem] shrink-0 items-center justify-center border font-mono text-chip font-black tracking-[0.08em] transition-colors duration-150 ease-out active:scale-[0.97] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:opacity-45 motion-reduce:transition-none motion-reduce:active:scale-100 mouse:h-6 ${
        on
          ? "border-transparent bg-accent text-accent-foreground"
          : "border-transparent bg-panel-2 text-dialog-hint hover:bg-hover hover:text-white"
      }`}
    >
      <span aria-hidden className={busy ? "animate-pulse" : ""}>
        {busy ? "··" : on ? "ON" : "OFF"}
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

/**
 * How long ONE probe attempt gets before it counts as unreachable.
 *
 * Without a deadline the probe inherits the platform's TCP timeout — over a
 * minute on iOS — so a row can sit red long after the address came back. The
 * budget is generous on purpose: a tailnet address is cold, and its first
 * packet has to wake the peer, punch NAT or fall back to a relay, which
 * routinely outlasts the sub-second a LAN address needs.
 */
const PROBE_TIMEOUT_MS = 9000;

/**
 * Which address this device actually talks to.
 *
 * A gateway answers on several at once — Tailscale, LAN, loopback — and pairing
 * happens standing next to the machine, where the LAN address always wins the
 * race to reply. That address stops resolving the moment the phone leaves the
 * house, so the choice has to be visible, probeable and pinnable instead of
 * being whatever answered first months ago.
 */
function AddressPanel({
  gateway,
  onSelect,
}: {
  gateway: GatewayConn;
  onSelect: (url: string, pinned: boolean) => void | Promise<void>;
}) {
  // Key on the contents, not on the array identity: `gateway.alts` is a fresh
  // array on every reload, and depending on it re-ran the probe forever.
  const altsKey = (gateway.alts ?? []).join(" ");
  // Content-keyed, never url-keyed: choosing another address rewrites
  // `gateway.url` but yields the SAME address set. Re-deriving the array on the
  // url handed the probe effect below a fresh identity, so every dot fell back
  // to a pulsing "checking" and the rows re-flowed for no reason.
  const addressKey = mergeAddresses(
    [gateway.url],
    altsKey ? altsKey.split(" ") : [],
  ).join(" ");
  const addresses = useMemo(
    () => (addressKey ? addressKey.split(" ") : []),
    [addressKey],
  );
  const [reach, setReach] = useState<
    Record<string, "checking" | "online" | "offline">
  >({});
  const [probeNonce, setProbeNonce] = useState(0);
  const [busy, setBusy] = useState<string | null>(null);
  const [err, setErr] = useState<string | null>(null);

  const token = gateway.token;
  useEffect(() => {
    let cancelled = false;
    const inFlight = new Set<AbortController>();
    setReach(
      Object.fromEntries(addresses.map((url) => [url, "checking" as const])),
    );

    // One controller PER address, never one shared across the batch: a single
    // deadline let the slowest address abort every probe still in flight and
    // paint reachable rows red by association.
    const probe = async (url: string): Promise<boolean> => {
      const ctrl = new AbortController();
      const deadline = setTimeout(() => ctrl.abort(), PROBE_TIMEOUT_MS);
      inFlight.add(ctrl);
      try {
        return await new GatewayClient({ url, token }).ping(ctrl.signal);
      } catch (cause) {
        // Reachable-but-unauthorized still proves the address routes here;
        // only a network failure means the address is unusable from here.
        return cause instanceof GatewayError;
      } finally {
        clearTimeout(deadline);
        inFlight.delete(ctrl);
      }
    };

    void Promise.all(
      addresses.map(async (url) => {
        // Retry once before declaring an address dead. The first attempt is
        // what BRINGS a tailnet path up (handshake, relay fallback); judging
        // the address on that one cold attempt marks a working gateway red
        // for the rest of the session.
        let ok = await probe(url);
        if (!ok && !cancelled) ok = await probe(url);
        if (!cancelled)
          setReach((current) => ({
            ...current,
            [url]: ok ? "online" : "offline",
          }));
      }),
    );

    return () => {
      cancelled = true;
      for (const ctrl of inFlight) ctrl.abort();
    };
  }, [addresses, token, probeNonce]);

  // A tailnet address is routable seconds *after* the phone wakes, and a probe
  // taken while the interface was still down would otherwise stay red for the
  // rest of the session.
  useEffect(() => onWake(() => setProbeNonce((n) => n + 1)), []);

  const choose = async (url: string, pinned: boolean) => {
    setBusy(url);
    setErr(null);
    try {
      await onSelect(url, pinned);
    } catch (cause) {
      setErr((cause as Error).message);
    } finally {
      setBusy(null);
    }
  };

  // Nothing to choose between: one address and no pin is simply "the address",
  // and a panel offering a single disabled row is noise.
  if (addresses.length < 2 && !gateway.pinned) return null;

  return (
    <SettingsPanel
      title="Address"
      description="Which network path this device uses to reach the machine — pin one, or let the app pick the most durable route."
      meta={gateway.pinned ? "pinned" : "automatic"}
    >
      <div className="space-y-2 p-3">
        {err && <Banner kind="err">{err}</Banner>}

        <ul className="space-y-1">
          {addresses.map((url) => {
            const inUse = url === gateway.url;
            const kind = reachOf(url);
            const state = reach[url] ?? "checking";
            return (
              <li key={url}>
                <button
                  type="button"
                  disabled={inUse || busy !== null}
                  onClick={() => void choose(url, true)}
                  className={`flex w-full min-w-0 items-center gap-2 border px-2 py-1.5 text-left transition-colors disabled:cursor-default ${
                    inUse
                      ? "border-accent bg-panel-2"
                      : "border-dialog-edge hover:border-accent hover:bg-hover"
                  }`}
                >
                  <span
                    className={`size-1.5 shrink-0 rounded-full ${
                      state === "online"
                        ? "bg-ok"
                        : state === "offline"
                          ? "bg-err"
                          : "animate-pulse bg-dialog-hint motion-reduce:animate-none"
                    }`}
                    aria-hidden="true"
                  />
                  <span className="min-w-0 flex-1 truncate font-mono text-ui text-white">
                    {hostOf(url)}
                  </span>
                  <span className="shrink-0 font-mono text-chip font-bold uppercase tracking-wider text-dialog-hint">
                    {REACH_LABEL[kind]}
                  </span>
                  <span className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-accent-ink">
                    {inUse ? "in use" : busy === url ? "switching" : "use"}
                  </span>
                </button>
                {inUse && (
                  <p className="px-2 pt-1 font-mono text-meta text-dialog-hint">
                    {REACH_HINT[kind]}
                  </p>
                )}
              </li>
            );
          })}
        </ul>

        <div className="flex flex-wrap items-center gap-2 border-t border-dialog-edge pt-2">
          <span className="min-w-0 flex-1 font-mono text-meta text-dialog-hint">
            {gateway.pinned
              ? "Pinned: this device always uses the address above and never switches on its own."
              : "Automatic: this device prefers the most durable address that answers — Tailscale over Wi-Fi, Wi-Fi over anything local."}
          </span>
          {gateway.pinned && (
            <Button
              variant="ghost"
              onClick={() =>
                void choose(bestAddress(addresses) ?? gateway.url, false)
              }
            >
              Automatic
            </Button>
          )}
        </div>
      </div>
    </SettingsPanel>
  );
}
