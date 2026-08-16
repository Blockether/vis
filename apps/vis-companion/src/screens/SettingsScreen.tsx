import {
  useCallback,
  useEffect,
  useRef,
  useMemo,
  useState,
  type ReactNode,
} from "react";
import { GatewayClient, GatewayError } from "../lib/gateway";
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
  SpeechPrefs,
  SpeechRoute,
  SpeechVoice,
  SpeechVoices,
  VoiceEngineAbsence,
  VoiceModelState,
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
  DEFAULT_SPEECH_RATE,
  DEFAULT_SPEECH_ROUTE,
  SPEECH_RATES,
  SPEECH_ROUTES,
  getGatewayNotify,
  getSessionsPerPage,
  getThemePref,
  getSpeechPrefs,
  setSessionsPerPage,
  setSpeechDeviceVoice,
  setSpeechGatewayVoice,
  setSpeechRate,
  setSpeechRoute,
  setThemePref,
} from "../lib/storage";
import { speechOutput } from "../lib/speech";
import { deviceVoices, type DeviceVoice } from "../lib/speech-voices";
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
  ConfirmRow,
  DialogFrame,
  Input,
  Modal,
  NotifyConnectionRow,
  PROSE,
  Switch,
} from "../components/ui";
import {
  AddProviderButton,
  ProviderRows,
  unscopedMessage,
  useProviderAuth,
} from "../components/ProviderAuth";
import {
  AddMachine,
  MachineRows,
  useFleetHealth,
} from "../components/Machines";

/**
 * ONE MACHINE'S OWN SETTINGS, standing under that machine's own row in `SettingsDialog`.
 *
 * These panels used to be a dialog of their own — `Machine settings`, opened from a
 * machine's `⋯` — so the two halves of one question ("where do I change this?") stood
 * behind two different doors that could not be open at once. The panels are unchanged;
 * what left is the frame around them, and the dialog now owns Escape, the title and
 * the way out.
 */
function GatewayPanels({ gateway }: { gateway: GatewayConn }) {
  // ONE CLIENT PER MACHINE, and the transport pair is its whole identity. A fresh
  // `new GatewayClient(...)` per render re-fired every panel's `load` on every
  // unrelated re-render of the dialog; renaming a machine — a field no client reads
  // — must not rebuild it either. The panels mount only while this machine's row is
  // open, so nothing here talks to a gateway the reader has not opened.
  const client = useMemo(
    () => new GatewayClient({ url: gateway.url, token: gateway.token }),
    [gateway.url, gateway.token],
  );
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
    // rather than padding every group to get it. The stack also RULES ITS OWN TOP:
    // `divide-y` draws only BETWEEN groups, so the first band opened straight onto
    // the machine row that owns it with nothing between them, and Providers read as
    // part of that row instead of the first thing under it.
    <div className="min-w-0 touch-pan-y divide-y divide-dialog-edge overflow-x-hidden border-t border-dialog-edge">
      {err && (
        <div className="p-3 sm:p-4">
          <Banner kind="err">{err}</Banner>
        </div>
      )}

      {!unreachable && !unauthorized && <ProvidersPanel client={client} />}

      {!unreachable && !unauthorized && (
        <NotificationsPanel client={client} gateway={gateway} />
      )}

      {!unreachable && !unauthorized && <McpServersPanel client={client} />}

      {!unreachable && !unauthorized && <VoiceEnginesPanel client={client} />}

      {!unreachable && !unauthorized && <VoicesPanel client={client} />}

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
              <code className="text-accent-ink">vis-agent gateway pair</code>{" "}
              and paste the fresh link to load its settings.
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
                        <p
                          className={`mt-0.5 break-words ${PROSE} text-meta text-dialog-hint`}
                        >
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
      description="Tools shared with every client; secrets stay on this machine."
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

/**
 * THE MACHINE'S VOICES, and the one way to add another.
 *
 * A cloning engine speaks by imitating a reference recording, so a voice IS a clip and
 * "create a voice" is an upload and nothing else. The clip is stored on the machine that
 * imported it and every session there speaks with the same catalogue, so this band stands
 * beside that machine's other inventories rather than inside whichever session was open.
 *
 * A machine with no speaking engine renders NOTHING. Speech is an extension and most
 * installs do not carry it; a band explaining a feature that is not there is noise on
 * every one of them.
 */
function VoicesPanel({ client }: { client: GatewayClient }) {
  const [catalogue, setCatalogue] = useState<SpeechVoices | null>(null);
  const [isAbsent, setIsAbsent] = useState(false);
  const [err, setErr] = useState<string | null>(null);
  const [note, setNote] = useState<string | null>(null);
  const [clip, setClip] = useState<File | null>(null);
  const [voiceName, setVoiceName] = useState("");
  const [language, setLanguage] = useState("");
  const [says, setSays] = useState("");
  const [pending, setPending] = useState<string | null>(null);
  const [confirming, setConfirming] = useState<string | null>(null);
  // Which of THIS machine's voices this device asks for. Device-local, like every other
  // audio choice, and stored by id: a machine that no longer has it speaks in its own
  // default rather than falling silent.
  const [prefs, setPrefs] = useState<SpeechPrefs | null>(null);
  const fileRef = useRef<HTMLInputElement>(null);

  const load = useCallback(
    async (signal?: AbortSignal) => {
      try {
        const answer = await client.speechVoices(signal);
        if (signal?.aborted) return;
        setCatalogue(answer);
        setIsAbsent(false);
        setErr(null);
      } catch (e) {
        if (signal?.aborted) return;
        // 501 is a machine with no voice extension installed — the ordinary Vis. That
        // is not a failure worth a red banner, it is a feature this machine does not
        // have, so the whole band goes away.
        if (e instanceof GatewayError && e.status === 501) {
          setIsAbsent(true);
          setCatalogue(null);
          setErr(null);
          return;
        }
        setErr((e as Error).message);
      }
    },
    [client],
  );

  useEffect(() => {
    const controller = new AbortController();
    void load(controller.signal);
    return () => controller.abort();
  }, [load]);

  useEffect(() => {
    let isLive = true;
    void speechOutput.settings().then((current) => {
      if (isLive) setPrefs(current);
    });
    return () => {
      isLive = false;
    };
  }, []);

  async function chooseVoice(id: string | null) {
    await setSpeechGatewayVoice(id);
    const next = await getSpeechPrefs();
    speechOutput.apply(next);
    setPrefs(next);
  }

  function chooseClip(file: File | null) {
    setClip(file);
    setNote(null);
    setErr(null);
    if (fileRef.current && !file) fileRef.current.value = "";
    if (file && !voiceName.trim()) {
      setVoiceName(
        file.name
          .replace(/\.[^.]+$/, "")
          .replace(/[_-]+/g, " ")
          .trim(),
      );
    }
  }

  async function importClip() {
    if (!clip || !voiceName.trim()) return;
    setPending("import");
    try {
      const voice = await client.importSpeechVoice(clip, {
        name: voiceName.trim(),
        lang: language.trim() || undefined,
        text: says.trim() || undefined,
      });
      setNote(`${voice.label ?? voice.id} can speak on this machine now.`);
      chooseClip(null);
      setVoiceName("");
      setLanguage("");
      setSays("");
      await load();
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function forget(voice: SpeechVoice) {
    setPending(voice.id);
    try {
      await client.forgetSpeechVoice(voice.id);
      setConfirming(null);
      setNote(null);
      await load();
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  if (isAbsent) return null;

  const voices = catalogue?.voices ?? [];
  const canImport = catalogue?.engine?.is_voice_import === true;

  return (
    <SettingsPanel
      title="Voices"
      description="How this machine speaks. A cloning engine learns a voice from one clip."
      meta={
        catalogue
          ? `${voices.length} ${voices.length === 1 ? "voice" : "voices"}`
          : "checking…"
      }
    >
      <div className="space-y-2 p-3">
        {err && <Banner kind="err">{err}</Banner>}
        {note && <Banner kind="ok">{note}</Banner>}

        {catalogue === null && !err && (
          <p className="py-4 text-center font-mono text-meta text-dialog-hint">
            Reading this machine's voices…
          </p>
        )}

        {catalogue && voices.length === 0 && (
          <p className="py-4 text-center font-mono text-meta text-dialog-hint">
            {canImport
              ? "No voice yet — import a recording and it becomes one."
              : "This engine speaks in no named voice."}
          </p>
        )}

        {voices.length > 0 && (
          <>
            <p className="font-mono text-chip text-dialog-hint">
              A reply this device sends here is spoken in the voice marked ●.
              Whether replies are spoken at all is in Application → Spoken
              replies.
            </p>
            <div className="border border-dialog-edge bg-panel-2">
              <ChoiceCell
                title="Engine default"
                sub="whatever this machine picks"
                isSelected={prefs?.gatewayVoice == null}
                onClick={() => void chooseVoice(null)}
              />
            </div>
          </>
        )}

        {voices.map((voice) => (
          <div key={voice.id} className="border border-dialog-edge bg-panel-2">
            <div className="grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3 pr-3">
              <ChoiceCell
                className="min-w-0"
                title={voice.label ?? voice.id}
                sub={[
                  voice.language,
                  voice.is_imported ? "imported here" : "ships with the engine",
                ]
                  .filter(Boolean)
                  .join(" · ")}
                isSelected={prefs?.gatewayVoice === voice.id}
                onClick={() => void chooseVoice(voice.id)}
              />
              {voice.is_imported && confirming !== voice.id && (
                <Button
                  variant="secondary"
                  onClick={() => setConfirming(voice.id)}
                >
                  Forget
                </Button>
              )}
            </div>
            {confirming === voice.id && (
              <ConfirmRow
                question={`Forget ${voice.label ?? voice.id}?`}
                confirmLabel="Forget"
                isBusy={pending === voice.id}
                onKeep={() => setConfirming(null)}
                onConfirm={() => void forget(voice)}
              />
            )}
          </div>
        ))}

        {canImport && (
          <div className="space-y-2 border border-dialog-edge bg-panel-2 p-3">
            <input
              ref={fileRef}
              type="file"
              accept="audio/*"
              className="hidden"
              aria-label="Recording to import as a voice"
              onChange={(event) => chooseClip(event.target.files?.[0] ?? null)}
            />
            {clip === null ? (
              <Button
                variant="secondary"
                onClick={() => fileRef.current?.click()}
              >
                Import a voice…
              </Button>
            ) : (
              <div className="space-y-2">
                <FormLabel
                  label="Recording"
                  hint="Ten to thirty seconds of clear speech is plenty."
                >
                  <p className="truncate font-mono text-meta text-white">
                    {clip.name}
                  </p>
                </FormLabel>
                <FormLabel label="Name">
                  <Input
                    value={voiceName}
                    placeholder="What to call this voice"
                    onChange={(event) => setVoiceName(event.target.value)}
                  />
                </FormLabel>
                <FormLabel
                  label="Language"
                  hint="Optional — the tag this clip speaks in, like en or en-GB."
                >
                  <Input
                    value={language}
                    placeholder="en"
                    onChange={(event) => setLanguage(event.target.value)}
                  />
                </FormLabel>
                <FormLabel
                  label="What the clip says"
                  hint="Optional, and worth typing: the model is TOLD these words, so the clone tracks the voice instead of guessing them."
                >
                  <Input
                    value={says}
                    placeholder="Transcript of the recording"
                    onChange={(event) => setSays(event.target.value)}
                  />
                </FormLabel>
                <div className="flex flex-wrap gap-2">
                  <Button
                    variant="primary"
                    disabled={!voiceName.trim() || pending === "import"}
                    onClick={() => void importClip()}
                  >
                    {pending === "import" ? "Importing…" : "Import"}
                  </Button>
                  <Button variant="quiet" onClick={() => chooseClip(null)}>
                    Cancel
                  </Button>
                </div>
              </div>
            )}
          </div>
        )}
      </div>
    </SettingsPanel>
  );
}

/** How far one engine has got, in the fewest words that are still true. */
const ENGINE_POLL_MS = 1200;

type EngineReading = {
  state: VoiceModelState | null;
  /** Set when the direction has NO engine at all — 501, with whatever failed to load. */
  absence: VoiceEngineAbsence | null;
  error: string | null;
};

function engineWord(reading: EngineReading | null): string {
  if (reading === null) return "checking…";
  if (reading.absence) return "not installed";
  if (reading.error) return "cannot be read";
  switch (reading.state?.status) {
    case "ready":
      return "ready";
    case "downloading":
      return reading.state.phase === "extracting" ? "unpacking" : "downloading";
    case "failed":
      return "failed";
    case "absent":
      return "not downloaded yet";
    default:
      return "unavailable";
  }
}

/**
 * ONE direction's readiness, as a row: what it is doing, which engine is doing it, and the
 * one verb that can change it. Presentational on purpose — the panel owns every read, so a
 * download is polled once for both rows instead of twice.
 */
function EngineRow({
  title,
  hint,
  reading,
  isBusy,
  onPrepare,
}: {
  title: string;
  hint: string;
  reading: EngineReading | null;
  isBusy: boolean;
  onPrepare: () => void;
}) {
  const state = reading?.state ?? null;
  const percent =
    typeof state?.progress === "number" ? Math.round(state.progress) : null;
  const word = engineWord(reading);
  const canPrepare =
    reading !== null &&
    !reading.absence &&
    (state?.status === "absent" ||
      state?.status === "failed" ||
      !!reading.error);

  return (
    <div className="space-y-2 border border-dialog-edge bg-panel-2 p-3">
      <div className="flex flex-wrap items-baseline justify-between gap-x-3 gap-y-1">
        <p className="font-mono text-ui text-white">{title}</p>
        <p className="font-mono text-meta text-dialog-hint">
          {word}
          {percent !== null && state?.status === "downloading"
            ? ` ${percent}%`
            : ""}
          {state?.engine ? ` · ${state.engine}` : ""}
        </p>
      </div>
      <p className="font-mono text-chip text-dialog-hint">{hint}</p>

      {/* A machine that never carried a voice engine and one whose engine FAILED to load
          are different machines, and only the second is something a human can fix. */}
      {reading?.absence && (
        <p className="font-mono text-chip text-dialog-hint">
          {reading.absence.reasons?.length
            ? reading.absence.reasons.join(" · ")
            : "This machine has no engine for this direction installed."}
        </p>
      )}

      {state?.status === "failed" && state.error && (
        <Banner kind="err">{state.error}</Banner>
      )}
      {reading?.error && <Banner kind="err">{reading.error}</Banner>}

      {canPrepare && (
        <Button variant="primary" disabled={isBusy} onClick={onPrepare}>
          {isBusy
            ? "Asking…"
            : state?.status === "absent"
              ? "Download the model"
              : "Try again"}
        </Button>
      )}
    </div>
  );
}

/**
 * WHETHER this machine can listen and speak, and what it is doing about it.
 *
 * Regression, user report: with the model already installed, voice still failed and the only
 * cure anyone found was restarting Vis — while no screen said which half was broken, whether
 * a download had died, or that the engine had failed to load at all. Both directions report
 * themselves here, the panel polls itself while bytes are moving, and a failure carries the
 * reason plus the one button that retries it.
 */
export function VoiceEnginesPanel({ client }: { client: GatewayClient }) {
  const [listening, setListening] = useState<EngineReading | null>(null);
  const [speaking, setSpeaking] = useState<EngineReading | null>(null);
  const [busy, setBusy] = useState<"listen" | "speak" | null>(null);

  const readOne = useCallback(
    async (
      ask: (start: boolean, signal?: AbortSignal) => Promise<VoiceModelState>,
      signal?: AbortSignal,
    ): Promise<EngineReading> => {
      try {
        return { state: await ask(false, signal), absence: null, error: null };
      } catch (e) {
        if (e instanceof GatewayError && e.status === 501) {
          return {
            state: null,
            absence: {
              error: e.message,
              reasons: (e.body as VoiceEngineAbsence | undefined)?.reasons,
            },
            error: null,
          };
        }
        return { state: null, absence: null, error: (e as Error).message };
      }
    },
    [],
  );

  const load = useCallback(
    async (signal?: AbortSignal) => {
      const [heard, spoken] = await Promise.all([
        readOne((start, sig) => client.voiceModel(start, sig), signal),
        readOne((start, sig) => client.speechModel(start, sig), signal),
      ]);
      if (signal?.aborted) return;
      setListening(heard);
      setSpeaking(spoken);
    },
    [client, readOne],
  );

  useEffect(() => {
    const controller = new AbortController();
    void load(controller.signal);
    return () => controller.abort();
  }, [load]);

  // A download is the one state where a screen that never refreshes is a screen that lies.
  const isMoving =
    listening?.state?.status === "downloading" ||
    speaking?.state?.status === "downloading";
  useEffect(() => {
    if (!isMoving) return;
    const timer = window.setInterval(() => {
      void load();
    }, ENGINE_POLL_MS);
    return () => window.clearInterval(timer);
  }, [isMoving, load]);

  async function prepare(which: "listen" | "speak") {
    setBusy(which);
    try {
      const state =
        which === "listen"
          ? await client.voiceModel(true)
          : await client.speechModel(true);
      const reading: EngineReading = { state, absence: null, error: null };
      if (which === "listen") setListening(reading);
      else setSpeaking(reading);
    } catch (e) {
      const failed: EngineReading = {
        state: null,
        absence: null,
        error: (e as Error).message,
      };
      if (which === "listen") setListening(failed);
      else setSpeaking(failed);
    } finally {
      setBusy(null);
    }
  }

  // An ordinary Vis carries no voice extension at all, and a panel that only ever says so is
  // clutter. One that has something to REPORT — a model to fetch, a download in flight, an
  // engine that failed to load — is the whole point.
  const isSilentMachine =
    listening?.absence != null &&
    speaking?.absence != null &&
    !listening.absence.reasons?.length &&
    !speaking.absence.reasons?.length;
  if (isSilentMachine) return null;

  const meta =
    listening === null || speaking === null
      ? "checking…"
      : `${engineWord(listening)} · ${engineWord(speaking)}`;

  return (
    <SettingsPanel
      title="Speech engines"
      description="Whether this machine can listen and speak. Models stay on it."
      meta={meta}
    >
      <div className="space-y-2 p-3">
        <EngineRow
          title="Listening"
          hint="Turns a recording into text — the microphone in the composer, and Ctrl+B in the terminal."
          reading={listening}
          isBusy={busy === "listen"}
          onPrepare={() => void prepare("listen")}
        />
        <EngineRow
          title="Speaking"
          hint="Reads a reply out loud when this device asks the machine to."
          reading={speaking}
          isBusy={busy === "speak"}
          onPrepare={() => void prepare("speak")}
        />
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
/** The three answers to "where is a reply spoken", in the order the band offers them. */
const SPEECH_ROUTE_FACES: Record<SpeechRoute, { title: string; sub: string }> =
  {
    off: { title: "Off", sub: "answers stay on the page" },
    device: { title: "This device", sub: "the phone reads them" },
    gateway: { title: "The machine", sub: "its own voice speaks" },
  };

/** What each speed sounds like, so the number is not the only thing on the cell. */
const SPEECH_RATE_WORDS: Record<string, string> = {
  "0.85": "unhurried",
  "1": "natural",
  "1.2": "brisk",
};

/**
 * WHERE A REPLY IS SPOKEN, and in which voice.
 *
 * This device is the only thing that can answer it - it owns the speaker - so the whole
 * band lives in the Application column and nothing here is ever sent to a gateway. Three
 * choices, and each is a real one: `Off` is SILENCE, not a broken feature; `This device`
 * is the phone's own engine, which works with no gateway reachable at all; `The machine`
 * sends the line to whichever machine answered it and plays what comes back, which is the
 * only way to hear a voice that was cloned from a recording.
 *
 * A machine that cannot speak right now does not cost the reader the reply: the router in
 * `speech.ts` falls back to this device and says so once. WHICH of a machine's voices to
 * ask for is picked in that machine's own Voices band, beside the catalogue it belongs to.
 */
export function SpokenRepliesPanel() {
  const [prefs, setPrefs] = useState<SpeechPrefs | null>(null);
  const [voices, setVoices] = useState<DeviceVoice[] | null>(null);
  const [err, setErr] = useState<string | null>(null);

  useEffect(() => {
    let isLive = true;
    void speechOutput.settings().then((current) => {
      if (isLive) setPrefs(current);
    });
    void deviceVoices()
      .then((list) => {
        if (isLive) setVoices(list);
      })
      .catch(() => {
        if (isLive) setVoices([]);
      });
    return () => {
      isLive = false;
    };
  }, []);

  async function save(change: () => Promise<void>) {
    try {
      await change();
      const next = await getSpeechPrefs();
      // The router reads its settings once per app run, so the next reply obeys this
      // without a reload.
      speechOutput.apply(next);
      setPrefs(next);
      setErr(null);
    } catch (e) {
      setErr((e as Error).message);
    }
  }

  const route = prefs?.route ?? DEFAULT_SPEECH_ROUTE;
  const rate = prefs?.rate ?? DEFAULT_SPEECH_RATE;
  const deviceList = voices ?? [];

  return (
    <SettingsPanel
      title="Spoken replies"
      description="Whether an answer is read out loud, and by what."
      meta={SPEECH_ROUTE_FACES[route].sub}
    >
      <div className="space-y-2 p-3">
        {err && <Banner kind="err">{err}</Banner>}

        <div className="grid grid-cols-1 gap-px bg-dialog-edge min-[420px]:grid-cols-3">
          {SPEECH_ROUTES.map((choice) => (
            <ChoiceCell
              key={choice}
              title={SPEECH_ROUTE_FACES[choice].title}
              sub={SPEECH_ROUTE_FACES[choice].sub}
              isSelected={route === choice}
              onClick={() => void save(() => setSpeechRoute(choice))}
            />
          ))}
        </div>

        {route === "device" && (
          <>
            <div className="grid grid-cols-1 gap-px bg-dialog-edge min-[420px]:grid-cols-3">
              {SPEECH_RATES.map((choice) => (
                <ChoiceCell
                  key={choice}
                  title={`${choice}×`}
                  sub={SPEECH_RATE_WORDS[String(choice)] ?? "speed"}
                  isSelected={rate === choice}
                  onClick={() => void save(() => setSpeechRate(choice))}
                />
              ))}
            </div>

            {voices === null && (
              <p className="py-2 text-center font-mono text-meta text-dialog-hint">
                Asking this device what it can speak in…
              </p>
            )}
            {voices !== null && deviceList.length === 0 && (
              <p className="py-2 text-center font-mono text-meta text-dialog-hint">
                This device has no speech engine installed, so nothing can be
                read out loud here.
              </p>
            )}
            {deviceList.length > 0 && (
              <div className="grid max-h-64 grid-cols-1 gap-px overflow-y-auto bg-dialog-edge">
                <ChoiceCell
                  title="System default"
                  sub="whatever this device prefers"
                  isSelected={prefs?.deviceVoice == null}
                  onClick={() => void save(() => setSpeechDeviceVoice(null))}
                />
                {deviceList.map((voice) => (
                  <ChoiceCell
                    key={voice.id}
                    title={voice.label}
                    sub={[
                      voice.language,
                      voice.isDefault ? "device default" : null,
                    ]
                      .filter(Boolean)
                      .join(" · ")}
                    isSelected={prefs?.deviceVoice === voice.id}
                    onClick={() =>
                      void save(() => setSpeechDeviceVoice(voice.id))
                    }
                  />
                ))}
              </div>
            )}
          </>
        )}
      </div>
    </SettingsPanel>
  );
}

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
          {action && (
            <span className="flex shrink-0 items-center">{action}</span>
          )}
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

/** A machine's identity across an address change: a URL is a property of it, not it. */
function machineId(conn: GatewayConn): string {
  return conn.id ?? conn.url;
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
 * machines this device is paired with, how to add another, and what each of them
 * decides — and APPLICATION owns what this copy of Vis decides (theme, page size).
 * Machines leads, because the cog is opened to reach a machine far more often than to
 * repaint the app, and below `sm:` the columns stack in that same order.
 *
 * A MACHINE'S SETTINGS ARE HIDDEN UNDER THAT MACHINE. Every row is a disclosure and
 * its panels stand under its own row, opened by the chevron the rest of this app
 * opens things with. They used to be ONE column body under the whole list, showing
 * whichever machine was pressed last: pressing a machine opened nothing, it swapped
 * the settings already on screen for another machine's — reported as a press that
 * changes the view instead of opening the row — and the machine that column happened
 * to be reading wore the word `CURRENT`, which named no choice the reader had made.
 * Opening one machine leaves every other machine exactly as it was.
 */
export function SettingsDialog({
  gateways,
  gateway,
  primaryUrl,
  onAddMachine,
  onMakePrimary,
  onRename,
  onRemove,
  onSelectAddress,
  onClose,
}: {
  gateways: GatewayConn[];
  /**
   * The machine this dialog was OPENED on, and the only row that starts open: the cog
   * lands on the machine the app is using, a session's `Manage providers` on its own
   * machine. `null` only when none is paired.
   */
  gateway: GatewayConn | null;
  primaryUrl?: string | null;
  /** Pairing is setup, and setup happens HERE — never by leaving this dialog. */
  onAddMachine: (conn: GatewayConn, makeActive?: boolean) => Promise<void>;
  /**
   * A machine's own verbs act on the ROW they came out of, and every one of them
   * names its machine. They used to act on whichever machine the column happened
   * to be READING, because they were controls in that machine's own panel — so a
   * fleet's verbs all pointed at one row, and the row under the thumb was not it.
   */
  onMakePrimary?: (conn: GatewayConn) => void | Promise<void>;
  onRename?: (
    conn: GatewayConn,
    label: string | undefined,
  ) => void | Promise<void>;
  onRemove?: (conn: GatewayConn) => void | Promise<void>;
  /**
   * Bind one machine to a different address. It acts on the ROW it came out of —
   * the machine's own address line — and never on another machine's.
   */
  onSelectAddress?: (
    conn: GatewayConn,
    url: string,
    pinned: boolean,
  ) => void | Promise<void>;
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

  // WHICH MACHINES STAND OPEN, held by the identity that survives an address change.
  // Several may: opening a machine is never a reason to close another one.
  const [openIds, setOpenIds] = useState<ReadonlySet<string>>(
    () => new Set(gateway ? [machineId(gateway)] : []),
  );
  const toggleMachine = useCallback((conn: GatewayConn) => {
    setOpenIds((open) => {
      const next = new Set(open);
      if (!next.delete(machineId(conn))) next.add(machineId(conn));
      return next;
    });
  }, []);
  const openUrls = new Set(
    gateways
      .filter((conn) => openIds.has(machineId(conn)))
      .map((conn) => conn.url),
  );

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
                aria-label="Add a machine"
                onClick={() => setIsAdding(true)}
              >
                Add
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
            {gateways.length > 0 ? (
              <MachineRows
                conns={gateways}
                openUrls={openUrls}
                primaryUrl={primaryUrl}
                health={health}
                onPick={toggleMachine}
                onMakePrimary={onMakePrimary}
                onRename={onRename}
                onForget={onRemove}
                onSelectAddress={onSelectAddress}
                renderPanel={(conn) => (
                  <GatewayPanels key={machineId(conn)} gateway={conn} />
                )}
              />
            ) : (
              <SettingsPanel title="No machine yet">
                <p className="px-4 py-6 text-center font-mono text-body text-dialog-hint">
                  Add a machine above, and its settings live under its own row.
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
              description="Every palette Vis ships, saved on this device."
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
              description="How many sessions a project lists before paging."
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

            <SpokenRepliesPanel />
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
            subtitle="Paste the pairing link printed by ‘vis-agent gateway pair’, scan its QR, or type the address."
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
  const { providers, err, note } = auth;
  // A message that names a provider is painted inside THAT provider's row by
  // `ProviderNotice`; only what has no row left to live in surfaces here.
  const fleetErr = unscopedMessage(err, providers);
  const fleetNote = unscopedMessage(note, providers);

  return (
    <SettingsPanel
      title="Providers"
      action={<AddProviderButton auth={auth} />}
    >
      {(fleetErr || fleetNote) && (
        <div className="space-y-2 p-3">
          {fleetErr && <Banner kind="err">{fleetErr.text}</Banner>}
          {fleetNote && <Banner kind="ok">{fleetNote.text}</Banner>}
        </div>
      )}

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

      <ProviderRows auth={auth} />
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
  const [notify, setNotify] = useState(false);
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

export function NativeNotificationsPanel({
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
  // Nothing is claimed before that machine's own answer is read back; a machine
  // this device never connected to answers no.
  const [notify, setNotify] = useState(false);

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
    // Never gated on holding the OS token: this machine may know this device by
    // the relay GRANT instead, and a token this run was not given is no reason
    // to drop the user's answer on the floor. `unregisterFromPush` names every
    // id the machine could have filed it under, and the answer is stored first
    // so an unreachable machine is still silenced by the next sweep.
    const current = cachedPushToken() ?? "";
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
    Boolean(err) || !supported || blocked || Boolean(push && !available);

  return (
    <SettingsPanel title="Notifications" meta={machine}>
      {hasBanner && (
        <div className="space-y-2 p-3 pb-0">
          {err && <Banner kind="err">{err}</Banner>}

          {push && !available && refusedRelay && (
            <Banner kind="warn">
              This machine relays notifications through {refusedRelay}, which is
              not https — this device will not hand a push grant to an address
              on the wire. Unset VIS_PUSH_RELAY_URL there and it goes back to
              the relay this app was built with; point it at an https address to
              keep your own.
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
              Notifications are turned off for Vis in system Settings — turn
              them on there and this device can connect again.
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
  action,
  children,
}: {
  title: string;
  description?: string;
  meta?: ReactNode;
  /** One verb for the whole band, sitting in it — the panel's own ＋. */
  action?: ReactNode;
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
        {action && (
          <span className="flex shrink-0 items-center self-center">
            {action}
          </span>
        )}
        {description && (
          <p
            className={`w-full pl-2 ${PROSE} font-mono text-chip text-dialog-hint`}
          >
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
