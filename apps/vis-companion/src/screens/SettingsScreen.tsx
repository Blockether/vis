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
} from "../lib/gateway";
import { ChevronIcon } from "../components/icons";
import type {
  GatewayConn,
  PushDevice,
  PushStatus,
  ThemePref,
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
  canOpenSystemNotificationSettings,
  deviceRegistration,
  isPushSupported,
  maskToken,
  openSystemNotificationSettings,
  pushPermission,
  pushPlatform,
  type PushPermission,
} from "../lib/push";
import {
  ensureWebPushSubscription,
  getExistingWebPushSubscription,
  isWebNotificationsPlatform,
  isWebPushSupported,
  registerWebPushForGateway,
  requestWebPushPermission,
  unregisterWebPushForGateway,
  webPushApplicationServerKey,
  webPushPermission,
} from "../lib/web-push";
import { applyTheme } from "../lib/theme";
import { applyGatewayNotify, applyWebGatewayNotify } from "../lib/notify";
import {
  registerForPush,
  registeredIds,
  refusedRelayUrl,
  relayUrlFor,
  unregisterFromPush,
} from "../lib/relay";
import {
  DEFAULT_SESSION_PAGE_SIZE,
  getGatewayNotify,
  getSessionsPerPage,
  getThemePref,
  setSessionsPerPage,
  setThemePref,
} from "../lib/storage";
import {
  DEFAULT_THEME,
  THEMES,
  type ThemeChoice,
} from "../lib/themes.generated";
import {
  Banner,
  Button,
  ChoiceCell,
  Chip,
  DialogFrame,
  Input,
  ListRow,
  Modal,
  NotifyConnectionRow,
  PROSE,
  Switch,
} from "../components/ui";
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
  ProviderQuota,
  ProviderRemoveButton,
  AddProviderPanel,
  defaultFirstProviders,
  isProviderAuthed,
  preferredModelFirst,
  providerStatusDot,
  providerStatusLine,
  unscopedMessage,
  useProviderAuth,
} from "../components/ProviderAuth";
import {
  AddMachine,
  MachineRows,
  useFleetHealth,
} from "../components/Machines";

/**
 * ONE MACHINE'S OWN SETTINGS, as a column inside `SettingsDialog`.
 *
 * These panels used to be a dialog of their own — `Machine settings`, opened from a
 * machine's `⋯` — so the two halves of one question ("where do I change this?") stood
 * behind two different doors that could not be open at once. The panels are unchanged;
 * what left is the frame around them, and the dialog now owns Escape, the title and
 * the way out.
 */
function GatewayPanels({
  client,
  gateway,
  onSelectAddress,
}: {
  client: GatewayClient;
  gateway: GatewayConn;
  onSelectAddress?: (url: string, pinned: boolean) => void | Promise<void>;
}) {
  // Reopening the dialog paints the gateway's last known toggles immediately;
  // `load` below refreshes them (and `setSetting` patches the cache in place).
  const [groups, setGroups] = useState<ToggleGroup[] | null>(
    () => client.cachedSettings()?.groups ?? null,
  );
  const [err, setErr] = useState<string | null>(null);
  const [pending, setPending] = useState<string | null>(null);
  const [unreachable, setUnreachable] = useState(false);
  const [unauthorized, setUnauthorized] = useState(false);

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

  // Escape belongs to the dialog that frames these panels.

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

  return (
    // Groups run FULL BLEED and are divided by one rule, so the dialog's own frame is
    // the only box on the screen. A banner still needs air, so it brings its own
    // rather than padding every group to get it.
    <div className="min-w-0 touch-pan-y divide-y divide-dialog-edge overflow-x-hidden">
          {err && (
            <div className="p-3 sm:p-4">
              <Banner kind="err">{err}</Banner>
            </div>
          )}

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
                <Button variant="secondary" onClick={() => void load()}>
                  Retry
                </Button>
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
                <Button variant="secondary" onClick={() => void load()}>
                  Retry
                </Button>
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
                            <p className={`mt-0.5 break-words ${PROSE} text-meta text-dialog-hint`}>
                              {toggle.description}
                            </p>
                          )}
                        </div>

                        {toggle.type === "boolean" && (
                          <Switch
                            label={toggle.label}
                            isOn={!!toggle.enabled}
                            isBusy={busy}
                            disabled={busy}
                            onClick={() => flip(toggle)}
                          />
                        )}

                        {toggle.type === "enum" && toggle.choices && (
                          <div className="col-span-full col-start-2 flex min-w-0 flex-wrap gap-1.5">
                            {toggle.choices.map((choice) => {
                              const selected = toggle.value === choice;
                              return (
                                <Chip
                                  key={choice}
                                  isOn={selected}
                                  disabled={busy}
                                  onClick={() => pick(toggle, choice)}
                                >
                                  {choice}
                                </Chip>
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
                  variant="secondary"
                  disabled={busy !== null}
                  onClick={() => void authorize(server)}
                >
                  {server.is_authorized ? "Re-auth" : "Sign in"}
                </Button>
              )}
              {server.url && server.is_authorized && (
                <Button
                  variant="secondary"
                  disabled={busy !== null}
                  onClick={() => void signOut(server)}
                >
                  Sign out
                </Button>
              )}
              <Button
                variant="secondary"
                disabled={busy !== null}
                onClick={() => void setRunning(server, server.is_killed)}
              >
                {server.is_killed ? "Start" : "Kill"}
              </Button>
              {server.is_managed ? (
                <>
                  <Switch
                    label={`${server.name} MCP server`}
                    isOn={server.enabled}
                    isBusy={busy === server.name}
                    disabled={busy !== null}
                    onClick={() => void toggle(server)}
                  />
                  <Button
                    variant="secondary"
                    disabled={busy !== null}
                    onClick={() => openForm(server)}
                  >
                    Edit
                  </Button>
                  <Button
                    variant="secondary"
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
                    variant="secondary"
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
                <Chip
                  key={kind}
                  isOn={transport === kind}
                  onClick={() => setTransport(kind)}
                  className="w-full uppercase"
                >
                  {kind === "stdio" ? "Local command" : "Streamable HTTP"}
                </Chip>
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
                variant="secondary"
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
/**
 * ONE COLUMN OF SETTINGS, and the dialog has two of them.
 *
 * A column is the level ABOVE a `SettingsPanel`: it says whose settings these are —
 * this copy of Vis, or the machine — and every band under it belongs to that owner.
 * It is the sentence the two dialogs used to spend a whole header band saying.
 */
function SettingsColumn({
  title,
  description,
  meta,
  action,
  children,
}: {
  title: string;
  description?: string;
  meta?: ReactNode;
  /** The column's ONE verb, at the end of its band: `Add a machine`. */
  action?: ReactNode;
  children: ReactNode;
}) {
  return (
    <section className="flex min-w-0 flex-col sm:min-h-0">
      {/* A BAND NAMES THE COLUMN IN ONE LINE, and its verb is a WORD.
          The title, a meta and the ＋ used to share one wrapping flex line whose
          height was the button's: the two words sat at the top of it on their
          baseline while the ＋ centred itself in the rest, 8px lower than the title
          it stands beside. Then the reader asked what the meta and the sentence
          under it were FOR — a column that lists every machine does not need to
          name one of them in its own header, and "tap a row" is not news — and a
          bare ＋ is the mark this app already spends on a new session, so an amber
          slab of it here was one glyph meaning two things. The name and its meta
          wrap inside their own cell, the verb is the band's trailing cell centred
          against whatever that cell grows to, and the band never pads around it:
          the row is `min-h-12` because a finger lands there. */}
      <header className="min-w-0 shrink-0 border-b border-dialog-edge bg-level-machine">
        <div className="flex min-h-12 min-w-0 items-center gap-3 px-3 py-1 sm:px-4 mouse:min-h-9">
          <div className="flex min-w-0 flex-auto flex-wrap items-baseline gap-x-3 gap-y-1">
            <h3 className="min-w-0 flex-auto truncate font-mono text-ui font-black uppercase tracking-[0.12em] text-white">
              {title}
            </h3>
            {meta && (
              <span className="ms-auto min-w-0 max-w-full break-words text-right font-mono text-chip font-bold uppercase tracking-wider text-dialog-hint">
                {meta}
              </span>
            )}
          </div>
          {action && <span className="flex shrink-0 items-center">{action}</span>}
        </div>
        {/* One LINE, not a paragraph. This column's own said what a machine stores,
            how to swipe a row and what it shares with the TUI — 310 characters, six
            lines on a 320px phone, a band 137px tall introducing a 48px row. */}
        {description && (
          <p
            className={`px-3 pb-2 sm:px-4 ${PROSE} font-mono text-chip text-dialog-hint`}
          >
            {description}
          </p>
        )}
      </header>
      <div className="min-w-0 divide-y divide-dialog-edge sm:min-h-0 sm:flex-1 sm:overflow-y-auto sm:overscroll-contain">
        {children}
      </div>
    </section>
  );
}

/**
 * SETTINGS IS ONE PLACE: this device on the left, the machines on the right.
 *
 * There used to be two settings dialogs that could never be open at once —
 * `Application settings` behind the cog in the bar, `Machine settings` behind a
 * machine's `⋯` three screens away — so "where do I change this?" was answered by
 * remembering which of two doors a choice lived behind, and pairing a machine was
 * filed under the device while the machine it produced was filed somewhere else.
 *
 * One dialog, two columns, one rule between them. MACHINES owns the fleet — which
 * machines this device is paired with, how to add another, and what the machine being
 * read decides — and APPLICATION owns what this copy of Vis decides (theme, page size).
 * Machines leads, because the cog is opened to reach a machine far more often than to
 * repaint the app, and below `sm:` the columns stack in that same order.
 */
export function SettingsDialog({
  gateways,
  gateway,
  gatewayKey,
  client,
  activeUrl,
  primaryUrl,
  onSelectGateway,
  onAddMachine,
  onMakePrimary,
  onRename,
  onRemove,
  onSelectAddress,
  onClose,
}: {
  gateways: GatewayConn[];
  /** The machine the Gateways column is showing; `null` only when none is paired. */
  gateway: GatewayConn | null;
  /**
   * The selected machine's REMOUNT identity, captured once — never its current URL.
   * Switching address rewrites the URL, and keying on that tore the column down.
   */
  gatewayKey: string;
  client: GatewayClient | null;
  /** The machine the APP is talking to, which is not always the one being read. */
  activeUrl?: string | null;
  primaryUrl?: string | null;
  onSelectGateway: (conn: GatewayConn) => void;
  /** Pairing is setup, and setup happens HERE — never by leaving this dialog. */
  onAddMachine: (conn: GatewayConn, makeActive?: boolean) => Promise<void>;
  /**
   * A machine's own verbs act on the ROW they came out of, and every one of them
   * names its machine. They used to act on whichever machine the column happened
   * to be READING, because they were controls in that machine's own panel — so a
   * fleet's verbs all pointed at one row, and the row under the thumb was not it.
   */
  onMakePrimary?: (conn: GatewayConn) => void | Promise<void>;
  onRename?: (conn: GatewayConn, label: string | undefined) => void | Promise<void>;
  onRemove?: (conn: GatewayConn) => void | Promise<void>;
  onSelectAddress?: (url: string, pinned: boolean) => void | Promise<void>;
  onClose: () => void;
}) {
  const [pref, setPref] = useState<ThemePref>(DEFAULT_THEME.id);
  const [pageSize, setPageSize] = useState(DEFAULT_SESSION_PAGE_SIZE);
  const [pending, setPending] = useState<string | null>(null);
  const [err, setErr] = useState<string | null>(null);
  // Pairing opens over this dialog rather than inside it: see the sheet at the
  // foot of the return, and the band’s + that is its only door.
  const [isAdding, setIsAdding] = useState(false);

  useEffect(() => {
    let cancelled = false;
    void (async () => {
      const [theme, sessions] = await Promise.all([
        getThemePref(),
        getSessionsPerPage(),
      ]);
      if (cancelled) return;
      setPref(theme);
      setPageSize(sessions);
    })();
    return () => {
      cancelled = true;
    };
  }, []);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key !== "Escape") return;
      // One Escape, one surface: the pairing sheet standing over this dialog
      // leaves first, or adding a machine and reading its settings ended on the
      // same keystroke.
      if (isAdding) {
        setIsAdding(false);
        return;
      }
      onClose();
    };
    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [isAdding, onClose]);

  async function chooseTheme(next: ThemeChoice) {
    setPending(`theme:${next.id}`);
    try {
      await setThemePref(next.id);
      setPref(next.id);
      applyTheme(next);
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

  const health = useFleetHealth(gateways);

  return (
    // The app's ONE dialog: `Modal` + `DialogFrame`, the same outer component
    // "Manage projects" and every ask already open in. `wide` is the one size that
    // holds two columns of settings side by side; the height is every dialog's.
    <Modal size="wide" onDismiss={onClose}>
      <DialogFrame
        title="Settings"
        subtitle={`This device · ${gateways.length} ${
          gateways.length === 1 ? "machine" : "machines"
        }`}
        onClose={onClose}
      >
        {/* Each column scrolls ITSELF on desktop. One shared scroller made the short
            column a 1500px empty gutter: scrolling to a machine's Sandbox panel dragged
            Theme off the top of the screen for no reason. Below `sm:` the halves stack
            and the dialog body is the one scroller again. */}
        <div className="grid min-w-0 grid-cols-1 divide-y divide-dialog-edge sm:min-h-0 sm:flex-1 sm:grid-cols-2 sm:divide-x sm:divide-y-0 sm:overflow-hidden">
          <SettingsColumn
            title="Machines"
            action={
              <Button
                variant="primary"
                density="compact"
                onClick={() => setIsAdding(true)}
              >
                Add a machine
              </Button>
            }
          >
            {/* THE COG'S FIRST ANSWER IS THE FLEET. Reported over the machines screen:
                this should open when I click the cog. It did not — this column held a
                strip of bare machine NAMES and a `Pair machine` button whose only job
                was to CLOSE the dialog and navigate to a screen the app bar has no door
                to, so "which machines does this app know, and how do I add one?" was
                answered nowhere the cog could reach. The list and both ways to pair are
                now the very components that screen is made of: one object, and nothing
                leaves this dialog to reach it. It leads the dialog because it is what
                the cog was opened FOR — below `sm:` the columns stack in that order. */}
            {gateways.length > 0 && (
              <MachineRows
                conns={gateways}
                selectedUrl={gateway?.url}
                activeUrl={activeUrl}
                primaryUrl={primaryUrl}
                health={health}
                onPick={onSelectGateway}
                onMakePrimary={onMakePrimary}
                onRename={onRename}
                onForget={onRemove}
              />
            )}

            {gateway && client ? (
              <GatewayPanels
                key={gatewayKey}
                client={client}
                gateway={gateway}
                onSelectAddress={onSelectAddress}
              />
            ) : (
              <SettingsPanel title="No machine yet">
                <p className="px-4 py-6 text-center font-mono text-body text-dialog-hint">
                  Add a machine above and its settings appear here.
                </p>
              </SettingsPanel>
            )}
          </SettingsColumn>

          <SettingsColumn
            title="Application"
            description="These choices affect this copy of Vis only. They are never sent to a gateway."
            meta="this device"
          >
            {err && (
              <div className="p-3 sm:p-4">
                <Banner kind="err">{err}</Banner>
              </div>
            )}

            <SettingsPanel
              title="Theme"
              description="Every palette Vis ships, rendered from the same theme definitions the TUI paints with. The choice is saved on this device."
              meta={`${THEMES.length} available`}
            >
              <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                {THEMES.map((choice) => (
                  <ChoiceCell
                    key={choice.id}
                    title={choice.label}
                    sub={choice.mode}
                    isSelected={pref === choice.id}
                    disabled={pending?.startsWith("theme:") ?? false}
                    onClick={() => void chooseTheme(choice)}
                  />
                ))}
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
                ].map(({ size, label }) => (
                  <ChoiceCell
                    key={size}
                    title={String(size)}
                    sub={label}
                    isSelected={size === pageSize}
                    disabled={pending?.startsWith("pageSize:") ?? false}
                    onClick={() => void choosePageSize(size)}
                  />
                ))}
              </div>
            </SettingsPanel>
          </SettingsColumn>

        </div>
      </DialogFrame>

      {/* PAIRING IS A SHEET OVER SETTINGS, not a panel standing open inside it.
          Both ways in — the link (or its QR) and a typed address — used to sit
          permanently expanded under the machine list, so the column opened on
          two forms for a machine that does not exist yet and the fleet the cog
          was pressed FOR started below them. The band's ＋ is the door now, and
          `fit` means the sheet is as tall as the two cards and no taller. */}
      {isAdding && (
        <Modal size="fit" onDismiss={() => setIsAdding(false)}>
          <DialogFrame
            title="Add a machine"
            subtitle="Paste the pairing link printed by ‘vis gateway pair’, scan its QR, or type the address."
            onClose={() => setIsAdding(false)}
          >
            <div className="p-3 sm:p-4">
              <AddMachine
                onAdd={async (conn, makeActive) => {
                  await onAddMachine(conn, makeActive);
                  setIsAdding(false);
                }}
                isStacked
              />
            </div>
          </DialogFrame>
        </Modal>
      )}
    </Modal>
  );
}

/**
 * Provider accounts ON THIS GATEWAY: live auth status, the quota each account
 * has left, sign-in, and removal — the whole terminal-free equivalent of
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
              <ListRow
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
                <ChevronIcon
                  open={open}
                  className="size-3.5 text-dialog-hint"
                />
              </ListRow>

              <ProviderNotice auth={auth} provider={provider} />

              {open && (
                <div className="space-y-3 border-t border-dialog-edge p-3">
                  <ProviderQuota auth={auth} provider={provider} />
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
                        variant={isDefaultModel ? "secondary" : "primary"}
                        disabled={!selectedModel || tagging || isDefaultModel}
                        onClick={() =>
                          void setDefault(provider.id, selectedModel)
                        }
                      >
                        {isDefaultModel ? "Default" : "Set as default"}
                      </Button>
                      <Button
                        className="flex-1"
                        variant="secondary"
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
                          variant="secondary"
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
                    {!authed && (
                      <Button
                        className="flex-1"
                        disabled={pending === `auth:${provider.id}`}
                        onClick={() => void auth.signIn(provider)}
                      >
                        {pending === `auth:${provider.id}`
                          ? "Starting…"
                          : "Sign in"}
                      </Button>
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
  if (isWebNotificationsPlatform())
    return <WebNotificationsPanel gateway={gateway} />;
  return <NativeNotificationsPanel client={client} gateway={gateway} />;
}

function WebNotificationsPanel({ gateway }: { gateway: GatewayConn }) {
  const [perm, setPerm] = useState<PushPermission>(webPushPermission());
  const [subscription, setSubscription] = useState<PushSubscription | null>(
    null,
  );
  const [notify, setNotify] = useState(true);
  // Nothing may be reported until the browser has answered: "Not connected"
  // rendered before the first read is a verdict about a question not yet asked.
  const [loaded, setLoaded] = useState(false);
  const [busy, setBusy] = useState<"enable" | "disable" | null>(null);
  const [err, setErr] = useState<string | null>(null);
  const supported = isWebPushSupported();

  useEffect(() => {
    let cancelled = false;
    void Promise.all([
      getGatewayNotify(gateway.url),
      getExistingWebPushSubscription(gateway.url),
    ]).then(([wanted, current]) => {
      if (cancelled) return;
      setNotify(wanted);
      setSubscription(current);
      setPerm(webPushPermission());
      setLoaded(true);
    });
    return () => {
      cancelled = true;
    };
  }, [gateway.url]);

  const enable = useCallback(async () => {
    setBusy("enable");
    setErr(null);
    try {
      if (!supported)
        throw new Error("This browser does not support background Web Push.");
      const permission = await requestWebPushPermission();
      setPerm(permission);
      if (permission !== "granted")
        throw new Error(
          "Notifications are blocked in this browser. Allow them in browser settings first.",
        );
      const target = new GatewayClient(gateway).pushTarget();
      const status = await target.status();
      const next = await ensureWebPushSubscription(
        gateway.url,
        webPushApplicationServerKey(status),
      );
      await registerWebPushForGateway(gateway, next);
      await applyWebGatewayNotify(gateway.url, true);
      setSubscription(next);
      setNotify(true);
    } catch (cause) {
      setErr(cause instanceof Error ? cause.message : String(cause));
    } finally {
      setBusy(null);
    }
  }, [gateway, supported]);

  const disable = useCallback(async () => {
    setBusy("disable");
    setErr(null);
    try {
      const current =
        subscription ?? (await getExistingWebPushSubscription(gateway.url));
      if (current) await unregisterWebPushForGateway(gateway, current);
      await applyWebGatewayNotify(gateway.url, false);
      setNotify(false);
    } catch (cause) {
      setErr(cause instanceof Error ? cause.message : String(cause));
    } finally {
      setBusy(null);
    }
  }, [gateway, subscription]);

  const notifying =
    supported && notify && perm === "granted" && subscription !== null;
  const machine = gateway.label ?? gatewayHost(gateway.url);
  const blocked = supported && perm === "denied";
  const hasBanner = Boolean(err) || !supported || blocked;

  return (
    <SettingsPanel title="Notifications" meta={machine}>
      {hasBanner && (
        <div className="space-y-2 p-3 pb-0">
          {err && <Banner kind="err">{err}</Banner>}

          {!supported && (
            <Banner kind="warn">
              This browser does not support background Web Push.
            </Banner>
          )}

          {blocked && (
            <Banner kind="warn">
              Notifications are blocked in this browser — allow them in browser
              settings and this device can connect again.
            </Banner>
          )}
        </div>
      )}

      <NotifyConnectionRow
        machine={machine}
        isOn={notifying}
        isBusy={busy !== null}
        isChecking={!loaded}
        disabled={!supported || blocked || !loaded || busy !== null}
        onClick={() => void (notifying ? disable() : enable())}
      />
    </SettingsPanel>
  );
}

function NativeNotificationsPanel({
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
  const [busy, setBusy] = useState<"enable" | "disable" | null>(null);
  // An OLDER gateway simply has no /v1/devices route. That is not an error the
  // user can act on — it is a missing capability upstream — so the whole panel
  // (and every button in it) disappears instead of offering calls that 404.
  const [unsupported, setUnsupported] = useState(false);
  // This device's own answer, remembered per gateway: a machine you disconnected
  // from stays silent across relaunches, and a machine you connected to stays
  // registered even while another gateway is the one you have open.
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
    try {
      const fresh = await acquirePushToken();
      await applyGatewayNotify(gateway.url, true, () =>
        registerForPush(deviceRegistration(fresh), client.pushTarget()),
      );
      setNotify(true);
      await load();
    } catch (e) {
      // This device's answer may already be stored even though the machine refused
      // the call, so show what this device WILL do once it can reach it again.
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
    try {
      await applyGatewayNotify(gateway.url, false, () =>
        unregisterFromPush(current, client.pushTarget()),
      );
      setNotify(false);
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

  const machine = gateway.label ?? gatewayHost(gateway.url);
  // The OS outranks everything else: a machine can hold this device's token and
  // still reach nobody, so a blocked permission is never reported as connected.
  const blocked = supported && perm === "denied";
  const checking = devices === null;
  const hasBanner =
    Boolean(err) ||
    !supported ||
    blocked ||
    Boolean(push && !available);

  return (
    <SettingsPanel title="Notifications" meta={machine}>
      {hasBanner && (
        <div className="space-y-2 p-3 pb-0">
          {err && <Banner kind="err">{err}</Banner>}

          {push && !available && refusedRelay && (
            <Banner kind="warn">
              This machine relays notifications through {refusedRelay}, which is
              not https — this device will not hand a push grant to an address on
              the wire. Unset VIS_PUSH_RELAY_URL there and it goes back to the
              relay this app was built with; point it at an https address to keep
              your own.
            </Banner>
          )}

          {push && !available && !refusedRelay && (
            <Banner kind="warn">
              This machine cannot send notifications — it holds no push
              credentials and no relay.
            </Banner>
          )}

          {!supported && (
            <Banner kind="warn">
              Native alerts need the iOS or Android app. The web build can stay
              open instead.
            </Banner>
          )}

          {blocked && (
            <Banner kind="warn">
              Notifications are turned off for Vis in system Settings — turn them
              on there and this device can connect again.
            </Banner>
          )}
        </div>
      )}

      <NotifyConnectionRow
        machine={machine}
        isOn={notifying && !blocked}
        isBusy={busy !== null}
        isChecking={checking}
        disabled={
          !supported || !available || blocked || checking || busy !== null
        }
        onClick={() => void (notifying ? disable() : enable())}
      />

      {blocked && canOpenSystemNotificationSettings() && (
        <div className="px-3 pb-3">
          <Button
            variant="secondary"
            density="panel"
            className="w-full"
            onClick={() => openSystemNotificationSettings()}
          >
            Open system Settings
          </Button>
        </div>
      )}
    </SettingsPanel>
  );
}

export function SettingsPanel({
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
    // A BAND, not a card. This section used to carry its own frame inside the
    // dialog's frame, so every settings group sat in a box inside a box — two
    // concentric hairlines 16px apart, and a third around each control inside it.
    // The dialog is the only box; a group is separated from the next by the one
    // rule its container divides on, exactly as a project is separated from the
    // next in the sessions list.
    <section className="min-w-0 overflow-hidden bg-panel transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
      {/* A HEADER LINE IS NOT A COMPETITION FOR ONE ROW. The status used to be
          `shrink-0` beside the name, so it took its whole intrinsic width first
          and the name lived on what was left: measured on a 390px iPhone,
          "0 devices · via <relay host>" claimed 339 of 390, the title box
          collapsed to 15px and clipped to one syllable, the sentence under it
          wrapped one word per line, and the band grew 213px tall. The row WRAPS
          instead — the name is measured at its own width so a status that does
          not fit beside it drops to its own line, and the description always
          spans the whole band. */}
      <header className="flex min-h-8 min-w-0 flex-wrap content-center items-baseline gap-x-3 gap-y-1 border-b border-dialog-edge bg-panel-2 px-3 py-1.5">
        <h3 className="min-w-0 flex-auto truncate border-l-2 border-accent pl-2 font-mono text-meta font-black uppercase tracking-[0.12em] text-white">
          {title}
        </h3>
        {meta && (
          <span className="ms-auto min-w-0 max-w-full break-words text-right font-mono text-chip font-bold uppercase tracking-wider text-dialog-hint">
            {meta}
          </span>
        )}
        {description && (
          <p className={`w-full pl-2 ${PROSE} font-mono text-chip text-dialog-hint`}>
            {description}
          </p>
        )}
      </header>
      <div>{children}</div>
    </section>
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
                <ListRow
                  isFramed
                  isSelected={inUse}
                  disabled={inUse || busy !== null}
                  onClick={() => void choose(url, true)}
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
                </ListRow>
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
              variant="secondary"
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
