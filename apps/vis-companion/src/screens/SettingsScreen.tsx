import {
  useCallback,
  useEffect,
  useRef,
  useMemo,
  useState,
  type ReactNode,
} from "react";
import { GatewayClient, GatewayError, INCOMPATIBLE_STATUS } from "../lib/gateway";
import type {
  GatewayCapabilities,
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
  cachedNotifyVerdict,
  isHeldBy,
  notifyVerdict,
  rememberNotifyVerdict,
} from "../lib/notify-verdict";
import {
  registerForPush,
  registeredIds,
  refusedRelayUrl,
  relayUrlFor,
  unregisterFromPush,
} from "../lib/relay";
import {
  DEFAULT_SPEECH_PREFS,
  SPEECH_RATES,
  getGatewayNotify,
  getThemePref,
  getSpeechPrefs,
  setSpeechAsrEngine,
  setSpeechDeviceVoice,
  setSpeechGatewayVoice,
  setSpeechRate,
  setSpeechTtsEngine,
  setThemePref,
} from "../lib/storage";
import { speechOutput } from "../lib/speech";
import {
  CircleCheckIcon,
  CircleDashedIcon,
  CircleDotIcon,
  DownloadIcon,
  MARK_NUDGE,
  PlayIcon,
  PlusIcon,
  StopIcon,
} from "../components/icons";
import {
  bestDeviceVoices,
  deviceVoices,
  iosVoiceDownloadGuidance,
  type DeviceVoice,
} from "../lib/speech-voices";
import { onWake } from "../lib/wake";
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
  IconButton,
  Input,
  Modal,
  NotifyConnectionButton,
  PROSE,
  SettingsChoiceDisclosure,
  SettingsChoiceGroup,
  SettingsDisclosure,
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
import { APP_BUILD_COMMIT, APP_BUILD_NUMBER } from "../lib/build-info";
import {
  APP_MIN_GATEWAY_PROTOCOL,
  APP_PROTOCOL,
  APP_VERSION,
} from "../lib/compat";
import { exportDiagnostics } from "../lib/diagnostics";

/**
 * ONE MACHINE'S OWN SETTINGS, standing under that machine's own row in `SettingsDialog`.
 *
 * These panels used to be a dialog of their own — `Machine settings`, opened from a
 * machine's `⋯` — so the two halves of one question ("where do I change this?") stood
 * behind two different doors that could not be open at once. The panels are unchanged;
 * what left is the frame around them, and the dialog now owns Escape, the title and
 * the way out.
 */
function GatewayPanels({
  gateway,
  speechPrefs,
  onSpeechChange,
}: {
  gateway: GatewayConn;
  speechPrefs: SpeechPrefs;
  onSpeechChange: SaveSpeechPrefs;
}) {
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
  const [failure, setFailure] = useState<
    "unreachable" | "unauthorized" | "incompatible" | null
  >(null);

  const load = useCallback(
    async (signal?: AbortSignal) => {
      // Status flags are assigned only after the request settles, and never once the
      // caller has been torn down — so mounting this loader writes no state
      // synchronously and none after unmount.
      try {
        const settings = await client.settings();
        if (signal?.aborted) return;
        setErr(null);
        setFailure(null);
        setGroups(settings.groups ?? []);
      } catch (e) {
        if (signal?.aborted) return;
        // A token-gated gateway that's actually up answers /healthz (so the list
        // reads Online) but 401s on /v1/settings. Surface that as "unauthorized",
        // NOT "offline" — otherwise the dialog contradicts the reachable list.
        if (e instanceof GatewayError && e.status === 401) {
          setErr(null);
          setFailure("unauthorized");
          setGroups(null);
          return;
        }
        setErr((e as Error).message);
        setGroups(null);
        if (e instanceof GatewayError && e.status === INCOMPATIBLE_STATUS) {
          setFailure("incompatible");
          return;
        }
        setFailure("unreachable");
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

      {failure === null && (
        <>
          <ProvidersPanel client={client} />
          <NotificationsPanel client={client} gateway={gateway} />
          <McpServersPanel client={client} />
          <SpeechEnginesPanel
            client={client}
            prefs={speechPrefs}
            onChange={onSpeechChange}
          />
        </>
      )}

      {failure === "incompatible" ? null : failure === "unreachable" ? (
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
      ) : failure === "unauthorized" ? (
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
                // THE STATE IS THE SHAPE, and the colour only agrees with it: the
                // ticked ring is on, the dashed ring is off, and a setting that
                // holds a VALUE rather than a switch carries the dot — one ring,
                // three interiors, the same set the live view paints.
                const StateMark =
                  toggle.type !== "boolean"
                    ? CircleDotIcon
                    : toggle.enabled
                      ? CircleCheckIcon
                      : CircleDashedIcon;
                return (
                  <div
                    key={toggle.id}
                    className="grid min-w-0 grid-cols-[auto_minmax(0,1fr)_auto] items-start gap-x-4 gap-y-2 px-3 py-3 transition-colors hover:bg-hover sm:px-4 sm:py-2.5"
                  >
                    <StateMark
                      className={`${MARK_NUDGE} ${
                        toggle.type === "boolean" && toggle.enabled
                          ? "text-ok"
                          : "text-dialog-hint"
                      }`}
                    />

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
  // The rows this machine gave last time are the first frame; `load` below
  // revalidates them underneath. Opening on `null` flashed an empty band and
  // then moved every panel under it down (see `cachedMcpServers`).
  const [servers, setServers] = useState<McpServer[] | null>(() =>
    client.cachedMcpServers(),
  );
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
      action={
        showForm ? null : (
          <IconButton
            variant="quiet"
            label="Add an MCP server"
            title="Add an MCP server"
            onClick={() => openForm(null)}
          >
            <PlusIcon className="size-4" />
          </IconButton>
        )
      }
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
        {showForm && (
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
 * ONE ENGINE'S VOICES, immediately under the engine that owns them.
 *
 * A cloning engine speaks by imitating a reference recording, so a voice IS a clip and
 * "create a voice" is an upload and nothing else. The clip is stored on the machine that
 * imported it and every session there speaks with the same catalogue.
 *
 * A machine with no speaking engine renders NOTHING. Speech is an extension and most
 * installs do not carry it; a group explaining a feature that is not there is noise.
 */
export function VoicesPanel({
  client,
  prefs,
  engine = prefs.ttsEngine,
  onChange,
}: {
  client: GatewayClient;
  prefs: SpeechPrefs;
  engine?: string | null;
  onChange: SaveSpeechPrefs;
}) {
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
  const [confirmingInstall, setConfirmingInstall] = useState<string | null>(null);
  // Which of THIS machine's voices this device asks for. Stored by id: a machine
  // that no longer has it speaks in its selected engine's default instead.
  const fileRef = useRef<HTMLInputElement>(null);
  // Which voice this device is auditioning right now. One at a time on purpose: the
  // player has one output, so a second press replaces the sound instead of layering it.
  const [playing, setPlaying] = useState<string | null>(null);
  const auditionRef = useRef<AbortController | null>(null);
  const cancelAudition = useCallback((resetControl = true) => {
    const controller = auditionRef.current;
    if (!controller) return;
    auditionRef.current = null;
    controller.abort();
    speechOutput.stop();
    if (resetControl) setPlaying(null);
  }, []);
  const load = useCallback(
    async (signal?: AbortSignal) => {
      try {
        const answer = await client.speechVoices({
          signal,
          engine,
        });
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
    [client, engine],
  );

  useEffect(() => {
    const controller = new AbortController();
    void load(controller.signal);
    return () => controller.abort();
  }, [load]);

  useEffect(() => () => cancelAudition(false), [cancelAudition]);

  useEffect(() => {
    if (!catalogue?.voices?.some((voice) => voice.model?.status === "downloading")) return;
    const timer = window.setTimeout(() => void load(), ENGINE_POLL_MS);
    return () => window.clearTimeout(timer);
  }, [catalogue, load]);

  async function chooseVoice(id: string | null) {
    await onChange(() => setSpeechGatewayVoice(id));
  }

  /**
   * The audition: bytes for ONE voice, played on this device.
   *
   * Nothing is stored by listening and no preference is spent, so a catalogue can be
   * heard before it is chosen — which is the whole point, since choosing a voice that
   * has to be downloaded first costs 60-100 MB.
   */
  async function playSample(voice: SpeechVoice) {
    cancelAudition();
    const controller = new AbortController();
    auditionRef.current = controller;
    setPlaying(voice.id);
    setErr(null);
    try {
      const audio = await client.speechVoiceSample(voice.id, {
        signal: controller.signal,
        engine,
      });
      if (controller.signal.aborted || auditionRef.current !== controller) return;
      await speechOutput.playSample(audio);
    } catch (e) {
      if (controller.signal.aborted || auditionRef.current !== controller) return;
      setErr((e as Error).message);
    } finally {
      if (auditionRef.current === controller) {
        auditionRef.current = null;
        setPlaying(null);
      }
    }
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
      const voice = await client.importSpeechVoice(
        clip,
        {
          name: voiceName.trim(),
          lang: language.trim() || undefined,
          text: says.trim() || undefined,
        },
        { engine },
      );
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
      await client.forgetSpeechVoice(voice.id, { engine });
      setConfirming(null);
      setNote(null);
      await load();
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function prepareVoice(voice: SpeechVoice, isLicenseAccepted: boolean) {
    setPending(`install:${voice.id}`);
    setErr(null);
    try {
      const state = await client.speechModel({
        start: true,
        engine,
        voice: voice.id,
        isLicenseAccepted,
      });
      setConfirmingInstall(null);
      if (state.status === "failed") {
        setErr(state.error ?? `Could not download ${voice.label ?? voice.id}.`);
        setNote(null);
      } else {
        setNote(
          state.status === "ready"
            ? `${voice.label ?? voice.id} is ready.`
            : `${voice.label ?? voice.id} is downloading.`,
        );
      }
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
    <SettingsChoiceGroup label="Voices" isNested>
      {/* A VOICE IS A ROW, NOT A CARD. Reported over this screen: every voice sat in
          its own hairline box inside a padded box inside the panel, three frames
          deep, and the band spent two lines saying where a DIFFERENT band lives.
          The list divides on one rule like every other list here, and the panel's
          one verb is its last row. */}
      <div className="divide-y divide-dialog-edge">
        {(err || note) && (
          <div className="space-y-2 p-3">
            {err && <Banner kind="err">{err}</Banner>}
            {note && <Banner kind="ok">{note}</Banner>}
          </div>
        )}

        {catalogue === null && !err && (
          <p className="px-3 py-5 font-mono text-meta text-dialog-hint sm:px-4">
            Reading this machine's voices…
          </p>
        )}

        {catalogue && voices.length === 0 && (
          <p className="px-3 py-5 font-mono text-meta text-dialog-hint sm:px-4">
            {canImport
              ? "No voice yet — import a recording and it becomes one."
              : "This engine speaks in no named voice."}
          </p>
        )}

        {voices.length > 0 && (
          <ChoiceCell
            className="w-full"
            isLeaf
            title="Engine default"
            sub="whatever this machine picks"
            isSelected={prefs?.gatewayVoice == null}
            onClick={() => void chooseVoice(null)}
          />
        )}

        {voices.map((voice) => {
          const name = voice.label ?? voice.id;
          const isPlaying = playing === voice.id;
          const model = voice.model;
          const canPrepare = model?.status === "absent" || model?.status === "failed";
          const isDownloading = model?.status === "downloading";
          const hasDownloadAction = canPrepare || isDownloading;
          const modelWord =
            model?.status === "downloading"
              ? `${model.phase === "extracting" ? "unpacking" : "downloading"}${
                  typeof model.progress === "number" ? ` ${Math.round(model.progress)}%` : ""
                }`
              : model?.status === "absent"
                ? "not downloaded yet"
                : model?.status === "failed"
                  ? "failed"
                  : model?.status === "ready"
                    ? "ready"
                    : null;
          // Once the model is local, a ready or cheap-to-prepare sample can be heard without
          // changing the choice. Until then, the model download is the row's leading action.
          const canHear = !!(voice.is_sample_ready || voice.is_sample_preparable);
          const hasTrailing = !!voice.is_imported;
          const leadingAction = hasDownloadAction
            ? {
                label: isDownloading
                  ? `Downloading ${name}`
                  : model?.status === "failed"
                    ? `Retry download ${name}`
                    : `Download ${name}`,
                icon: <DownloadIcon className="size-3" />,
                disabled: isDownloading || pending === `install:${voice.id}`,
                onClick: () => {
                  cancelAudition();
                  if (!canPrepare) return;
                  if (voice.is_opt_in) setConfirmingInstall(voice.id);
                  else void prepareVoice(voice, false);
                },
              }
            : canHear
              ? {
                  label: isPlaying
                    ? `Stop the sample of ${name}`
                    : `Play a sample of ${name}`,
                  icon: isPlaying ? (
                    <StopIcon className="size-3" />
                  ) : (
                    <PlayIcon className="size-3" />
                  ),
                  onClick: () => {
                    if (isPlaying) cancelAudition();
                    else void playSample(voice);
                  },
                }
              : undefined;
          return (
            <div key={voice.id}>
              <div
                className={
                  hasTrailing
                    ? "grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3 pr-3"
                    : "min-w-0"
                }
              >
                <ChoiceCell
                  className="w-full min-w-0"
                  isLeaf
                  title={name}
                  sub={[
                    voice.language,
                    voice.is_imported ? "imported here" : modelWord ?? "ships with the engine",
                  ]
                    .filter(Boolean)
                    .join(" · ")}
                  isSelected={prefs?.gatewayVoice === voice.id}
                  showSelectionMark={!model || model.status === "ready"}
                  leadingAction={leadingAction}
                  disabled={!!model && model.status !== "ready"}
                  onClick={() => {
                    void chooseVoice(voice.id);
                    if (canHear) void playSample(voice);
                  }}
                />
                {hasTrailing && (
                  <div className="flex shrink-0 items-center gap-2">
                    {voice.is_imported && confirming !== voice.id && (
                      <Button variant="secondary" onClick={() => setConfirming(voice.id)}>
                        Forget
                      </Button>
                    )}
                  </div>
                )}
              </div>
              {model?.status === "failed" && model.error && (
                <div className="border-t border-dialog-edge p-3">
                  <Banner kind="err">{model.error}</Banner>
                </div>
              )}
              {confirmingInstall === voice.id && (
                <div className="border-t border-dialog-edge">
                  <div className="space-y-2 px-3 pt-3 font-mono text-meta text-dialog-hint sm:px-4">
                    <p>{voice.notice ?? `This voice requires acceptance of ${voice.license}.`}</p>
                    {voice.license && <p className="font-bold text-white">{voice.license}</p>}
                    {voice.source_url && (
                      <a
                        className="block truncate text-accent underline"
                        href={voice.source_url}
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        Read the source and licence
                      </a>
                    )}
                  </div>
                  <ConfirmRow
                    question={`Accept ${voice.license ?? "these terms"} and download ${voice.label ?? voice.id}?`}
                    confirmLabel="Accept and download"
                    isBusy={pending === `install:${voice.id}`}
                    onKeep={() => setConfirmingInstall(null)}
                    onConfirm={() => void prepareVoice(voice, true)}
                  />
                </div>
              )}
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
          );
        })}
      </div>

      {canImport && (
        <>
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
              variant="primary"
              density="panel"
              className="w-full justify-center"
              onClick={() => fileRef.current?.click()}
            >
              Import a voice…
            </Button>
          ) : (
            <div className="space-y-2 p-3">
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
        </>
      )}
    </SettingsChoiceGroup>
  );
}

/** One persisted speech preference write, shared by every open machine panel. */
type SaveSpeechPrefs = (write: () => Promise<void>) => Promise<SpeechPrefs>;

/** How often a moving model refreshes its own progress. */
const ENGINE_POLL_MS = 1200;

/** What each device speed sounds like, so the number is not the only thing on the cell. */
const SPEECH_RATE_WORDS: Record<string, string> = {
  "0.85": "unhurried",
  "1": "natural",
  "1.2": "brisk",
};

/** One stable line lets the ear compare device voices rather than compare wording. */
const DEVICE_VOICE_SAMPLE = "This is what this voice sounds like.";

type EngineReading = {
  state: VoiceModelState | null;
  /** Set when the direction has NO engine at all — 501, with whatever failed to load. */
  absence: VoiceEngineAbsence | null;
  error: string | null;
};

type EngineReadings = Record<string, EngineReading>;

type EngineChoice = { id: string; label?: string };

function selectedEngine(
  requested: string | null,
  fallback: string | null | undefined,
  engines: EngineChoice[],
): string | null {
  if (requested && engines.some((engine) => engine.id === requested)) return requested;
  if (fallback && engines.some((engine) => engine.id === fallback)) return fallback;
  return engines[0]?.id ?? null;
}

/** A machine engine is local TO THE GATEWAY, which the Companion names explicitly. */
function gatewayEngineName(engine: EngineChoice): string {
  return (engine.label?.trim() || engine.id)
    .replace(/\s*\((?:local|gateway)\)\s*$/i, "")
    .replace(/[-_\s]+local$/i, "")
    .trim();
}

function gatewayEngineLabel(engine: EngineChoice): string {
  return `${gatewayEngineName(engine)} (gateway)`;
}
function engineWord(reading: EngineReading | null): string {
  if (reading === null) return "checking…";
  if (reading.absence) return "not installed";
  if (reading.error) return "cannot be read";
  switch (reading.state?.status) {
    case "ready":
      return "ready";
    case "downloading": {
      const action = reading.state.phase === "extracting" ? "unpacking" : "downloading";
      return typeof reading.state.progress === "number"
        ? `${action} ${Math.round(reading.state.progress)}%`
        : action;
    }
    case "failed":
      return "failed";
    case "absent":
      return "not downloaded yet";
    default:
      return "unavailable";
  }
}

/** One engine's preparation action and exceptional detail, directly under its own row. */
function EngineProblem({
  engineName,
  reading,
  isBusy,
  onPrepare,
}: {
  engineName: string;
  reading: EngineReading | null;
  isBusy: boolean;
  onPrepare: () => void;
}) {
  const state = reading?.state ?? null;
  const canPrepare =
    reading !== null &&
    !reading.absence &&
    (state?.status === "absent" || state?.status === "failed" || !!reading.error);
  if (!reading?.absence && !reading?.error && state?.status !== "failed" && !canPrepare) {
    return null;
  }
  const isDownload = state?.status === "absent";
  return (
    <div className="space-y-2 border-t border-dialog-edge px-3 py-3 sm:px-4">
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
        <Button
          variant="primary"
          disabled={isBusy}
          aria-label={`${isDownload ? "Download" : "Retry"} ${engineName} model`}
          onClick={onPrepare}
        >
          {isBusy ? "Asking…" : isDownload ? "Download model" : "Try again"}
        </Button>
      )}
    </div>
  );
}

/**
 * THE TWO SPEECH DIRECTIONS, each opening onto the concrete engines that can do it.
 *
 * Selection belongs to this device and travels on each request. ASR names one machine
 * engine. TTS puts this device's system engine in the same list as every engine the
 * machine advertised, so there is one choice instead of a separate reply-routing panel.
 */
export function SpeechEnginesPanel({
  client,
  prefs,
  onChange,
}: {
  client: GatewayClient;
  prefs: SpeechPrefs;
  onChange: SaveSpeechPrefs;
}) {
  const [capabilities, setCapabilities] = useState<GatewayCapabilities | null>(() =>
    client.cachedCapabilities(),
  );
  const [open, setOpen] = useState<"asr" | "tts" | null>(null);
  const [listening, setListening] = useState<EngineReadings>({});
  const [speaking, setSpeaking] = useState<EngineReadings>({});
  const [voices, setVoices] = useState<DeviceVoice[] | null>(null);
  const [busy, setBusy] = useState<string | null>(null);
  const [openTtsSettings, setOpenTtsSettings] = useState<ReadonlySet<string>>(
    () => new Set(),
  );
  const [err, setErr] = useState<string | null>(null);
  // `undefined` is silence; `null` is the system default actively speaking.
  const [playingDeviceVoice, setPlayingDeviceVoice] = useState<
    string | null | undefined
  >(undefined);
  const deviceAuditionRef = useRef<object | null>(null);
  const cancelDeviceAudition = useCallback((resetControl = true) => {
    if (!deviceAuditionRef.current) return;
    deviceAuditionRef.current = null;
    speechOutput.stop();
    if (resetControl) setPlayingDeviceVoice(undefined);
  }, []);

  const voiceFeature = capabilities?.features?.voice;
  const speechFeature = capabilities?.features?.speech;
  const asrEngines = useMemo(() => voiceFeature?.engines ?? [], [voiceFeature?.engines]);
  const ttsEngines = useMemo(() => speechFeature?.engines ?? [], [speechFeature?.engines]);
  const asrEngine = selectedEngine(
    prefs.asrEngine,
    voiceFeature?.selected,
    asrEngines,
  );
  const chosenTtsEngine =
    prefs.ttsEngine && ttsEngines.some((engine) => engine.id === prefs.ttsEngine)
      ? prefs.ttsEngine
      : null;

  useEffect(() => {
    const controller = new AbortController();
    void client
      .capabilities(controller.signal)
      .then((answer) => {
        if (!controller.signal.aborted) {
          setCapabilities(answer);
          setErr(null);
        }
      })
      .catch((cause: unknown) => {
        if (!controller.signal.aborted) setErr((cause as Error).message);
      });
    return () => controller.abort();
  }, [client]);

  useEffect(() => {
    let isLive = true;
    const refresh = () => {
      void deviceVoices()
        .then((list) => {
          if (isLive) setVoices(list);
        })
        .catch(() => {
          if (isLive) setVoices([]);
        });
    };
    refresh();
    const stopRefreshingOnWake = onWake(refresh);
    return () => {
      isLive = false;
      stopRefreshingOnWake();
    };
  }, []);

  useEffect(
    () => () => cancelDeviceAudition(false),
    [cancelDeviceAudition],
  );

  const readOne = useCallback(
    async (
      ask: () => Promise<VoiceModelState>,
    ): Promise<EngineReading> => {
      try {
        return { state: await ask(), absence: null, error: null };
      } catch (cause) {
        if (cause instanceof GatewayError && cause.status === 501) {
          return {
            state: null,
            absence: {
              error: cause.message,
              reasons: (cause.body as VoiceEngineAbsence | undefined)?.reasons,
            },
            error: null,
          };
        }
        return { state: null, absence: null, error: (cause as Error).message };
      }
    },
    [],
  );

  const readDirection = useCallback(
    async (
      engines: EngineChoice[],
      ask: (engine: string | null) => Promise<VoiceModelState>,
    ): Promise<EngineReadings> => {
      const ids: Array<string | null> = engines.length > 0 ? engines.map(({ id }) => id) : [null];
      const answers = await Promise.all(
        ids.map(async (engine) => [engine ?? "", await readOne(() => ask(engine))] as const),
      );
      return Object.fromEntries(answers);
    },
    [readOne],
  );

  const loadModels = useCallback(
    async (signal?: AbortSignal) => {
      const [heard, spoken] = await Promise.all([
        readDirection(asrEngines, (engine) => client.voiceModel({ signal, engine })),
        readDirection(ttsEngines, (engine) => client.speechModel({ signal, engine })),
      ]);
      if (signal?.aborted) return;
      setListening(heard);
      setSpeaking(spoken);
    },
    [asrEngines, client, readDirection, ttsEngines],
  );

  useEffect(() => {
    const controller = new AbortController();
    void loadModels(controller.signal);
    return () => controller.abort();
  }, [loadModels]);

  const isMoving = [...Object.values(listening), ...Object.values(speaking)].some(
    (reading) => reading.state?.status === "downloading",
  );
  useEffect(() => {
    if (!isMoving) return;
    const timer = window.setInterval(() => void loadModels(), ENGINE_POLL_MS);
    return () => window.clearInterval(timer);
  }, [isMoving, loadModels]);

  async function choose(direction: "asr" | "tts", engine: string | null) {
    try {
      await onChange(() =>
        direction === "asr"
          ? setSpeechAsrEngine(engine)
          : setSpeechTtsEngine(engine),
      );
      setErr(null);
    } catch (cause) {
      setErr((cause as Error).message);
    }
  }

  async function chooseDeviceSetting(write: () => Promise<void>) {
    try {
      await onChange(write);
      setErr(null);
    } catch (cause) {
      setErr((cause as Error).message);
    }
  }

  async function playDeviceVoice(voiceId: string | null) {
    cancelDeviceAudition();
    const audition = {};
    deviceAuditionRef.current = audition;
    setPlayingDeviceVoice(voiceId);
    setErr(null);
    try {
      await speechOutput.playDeviceSample(DEVICE_VOICE_SAMPLE, voiceId, prefs.rate);
    } catch (cause) {
      if (deviceAuditionRef.current !== audition) return;
      setErr((cause as Error).message);
    } finally {
      if (deviceAuditionRef.current === audition) {
        deviceAuditionRef.current = null;
        setPlayingDeviceVoice(undefined);
      }
    }
  }

  function toggleTtsSettings(engine: string) {
    setOpenTtsSettings((current) => {
      const next = new Set(current);
      if (next.has(engine)) next.delete(engine);
      else next.add(engine);
      return next;
    });
  }

  async function prepare(direction: "asr" | "tts", engine: string) {
    const busyKey = `${direction}:${engine}`;
    setBusy(busyKey);
    try {
      const state =
        direction === "asr"
          ? await client.voiceModel({ start: true, engine })
          : await client.speechModel({ start: true, engine });
      const reading: EngineReading = { state, absence: null, error: null };
      if (direction === "asr") {
        setListening((current) => ({ ...current, [engine]: reading }));
      } else {
        setSpeaking((current) => ({ ...current, [engine]: reading }));
      }
    } catch (cause) {
      const failed: EngineReading = {
        state: null,
        absence: null,
        error: (cause as Error).message,
      };
      if (direction === "asr") {
        setListening((current) => ({ ...current, [engine]: failed }));
      } else {
        setSpeaking((current) => ({ ...current, [engine]: failed }));
      }
    } finally {
      setBusy(null);
    }
  }

  const asrChoice = asrEngines.find((engine) => engine.id === asrEngine);
  const asrLabel = asrChoice
    ? gatewayEngineLabel(asrChoice)
    : asrEngine ?? (capabilities ? "Not installed" : "Checking…");
  const ttsChoice = ttsEngines.find((engine) => engine.id === chosenTtsEngine);
  const deviceList = bestDeviceVoices(voices ?? [], prefs.deviceVoice);
  const voiceDownloadGuidance = iosVoiceDownloadGuidance();
  const chosenDeviceVoice = deviceList.find((voice) => voice.id === prefs.deviceVoice);
  const ttsLabel = ttsChoice
    ? gatewayEngineLabel(ttsChoice)
    : chosenDeviceVoice
      ? `This device · ${chosenDeviceVoice.label}`
      : "This device";

  return (
    <SettingsPanel title="Speech engines">
      <div className="divide-y divide-dialog-edge">
        {err && (
          <div className="p-3 sm:p-4">
            <Banner kind="err">{err}</Banner>
          </div>
        )}

        <div>
          <SettingsDisclosure
            label="ASR"
            value={asrLabel}
            isOpen={open === "asr"}
            aria-controls="speech-asr-engines"
            onClick={() => setOpen((current) => (current === "asr" ? null : "asr"))}
          />
          {open === "asr" && (
            <div id="speech-asr-engines" className="border-t border-dialog-edge">
              {asrEngines.length > 0 ? (
                <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                  {asrEngines.map((engine) => {
                    const reading = listening[engine.id] ?? null;
                    return (
                      <div key={engine.id} className="grid bg-input">
                        <ChoiceCell
                          title={gatewayEngineLabel(engine)}
                          sub={engineWord(reading)}
                          isSelected={engine.id === asrEngine}
                          isLeaf
                          onClick={() => void choose("asr", engine.id)}
                        />
                        <EngineProblem
                          engineName={gatewayEngineName(engine)}
                          reading={reading}
                          isBusy={busy === `asr:${engine.id}`}
                          onPrepare={() => void prepare("asr", engine.id)}
                        />
                      </div>
                    );
                  })}
                </div>
              ) : (
                <>
                  <p className="px-3 py-4 font-mono text-chip text-dialog-hint sm:px-4">
                    No ASR engine is registered on this machine.
                  </p>
                  <EngineProblem
                    engineName="ASR"
                    reading={listening[""] ?? null}
                    isBusy={false}
                    onPrepare={() => undefined}
                  />
                </>
              )}
            </div>
          )}
        </div>

        <div>
          <SettingsDisclosure
            label="TTS"
            value={ttsLabel}
            isOpen={open === "tts"}
            aria-controls="speech-tts-engines"
            onClick={() => setOpen((current) => (current === "tts" ? null : "tts"))}
          />
          {open === "tts" && (
            <div id="speech-tts-engines" className="border-t border-dialog-edge">
              <SettingsChoiceGroup label="TTS engines">
                <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                  <div data-speech-engine="device" className="grid bg-input">
                    <SettingsChoiceDisclosure
                      title="This device"
                      sub={chosenDeviceVoice?.label ?? "system TTS"}
                      isSelected={chosenTtsEngine === null}
                      isOpen={openTtsSettings.has("device")}
                      controls="speech-tts-settings-device"
                      onSelect={() => void choose("tts", null)}
                      onToggle={() => toggleTtsSettings("device")}
                    />
                    {openTtsSettings.has("device") && (
                      <div id="speech-tts-settings-device" className="grid">
                        {voices === null && (
                          <p className="border-t border-dialog-edge px-3 py-4 font-mono text-chip text-dialog-hint sm:px-4">
                            Asking this device what it can speak in…
                          </p>
                        )}
                        {voices !== null && voices.length === 0 && (
                          <p className="border-t border-dialog-edge px-3 py-4 font-mono text-chip text-dialog-hint sm:px-4">
                            This device has no system TTS engine installed.
                          </p>
                        )}
                        {voices !== null && voices.length > 0 && (
                          <SettingsChoiceGroup label="Voices" isNested>
                            <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                              <ChoiceCell
                                title="System default"
                                sub="the voice this device prefers"
                                isSelected={prefs.deviceVoice === null}
                                isLeaf
                                leadingAction={{
                                  label:
                                    playingDeviceVoice === null
                                      ? "Stop the sample of System default"
                                      : "Play a sample of System default",
                                  icon:
                                    playingDeviceVoice === null ? (
                                      <StopIcon className="size-3" />
                                    ) : (
                                      <PlayIcon className="size-3" />
                                    ),
                                  onClick: () => {
                                    if (playingDeviceVoice === null) cancelDeviceAudition();
                                    else void playDeviceVoice(null);
                                  },
                                }}
                                onClick={() =>
                                  void chooseDeviceSetting(() => setSpeechDeviceVoice(null))
                                }
                              />
                              {deviceList.map((voice) => {
                                const isPlaying = playingDeviceVoice === voice.id;
                                return (
                                  <ChoiceCell
                                    key={voice.id}
                                    title={voice.label}
                                    sub={[
                                      voice.language,
                                      voice.isDefault ? "device default" : null,
                                    ]
                                      .filter(Boolean)
                                      .join(" · ")}
                                    isSelected={prefs.deviceVoice === voice.id}
                                    isLeaf
                                    leadingAction={{
                                      label: isPlaying
                                        ? `Stop the sample of ${voice.label}`
                                        : `Play a sample of ${voice.label}`,
                                      icon: isPlaying ? (
                                        <StopIcon className="size-3" />
                                      ) : (
                                        <PlayIcon className="size-3" />
                                      ),
                                      onClick: () => {
                                        if (isPlaying) cancelDeviceAudition();
                                        else void playDeviceVoice(voice.id);
                                      },
                                    }}
                                    onClick={() =>
                                      void chooseDeviceSetting(() =>
                                        setSpeechDeviceVoice(voice.id),
                                      )
                                    }
                                  />
                                );
                              })}
                            </div>
                            {voiceDownloadGuidance && (
                              <p className="border-t border-dialog-edge p-3 font-mono text-meta text-dialog-hint sm:p-4">
                                {voiceDownloadGuidance}
                              </p>
                            )}
                          </SettingsChoiceGroup>
                        )}
                        <SettingsChoiceGroup label="Speech rate" isNested>
                          <div className="grid grid-cols-3 gap-px bg-dialog-edge">
                            {SPEECH_RATES.map((rate) => (
                              <ChoiceCell
                                key={rate}
                                title={`${rate}×`}
                                sub={SPEECH_RATE_WORDS[String(rate)] ?? "speed"}
                                isSelected={prefs.rate === rate}
                                onClick={() =>
                                  void chooseDeviceSetting(() => setSpeechRate(rate))
                                }
                              />
                            ))}
                          </div>
                        </SettingsChoiceGroup>
                        {ttsEngines.length === 0 && (
                          <EngineProblem
                            engineName="TTS"
                            reading={speaking[""] ?? null}
                            isBusy={false}
                            onPrepare={() => undefined}
                          />
                        )}
                      </div>
                    )}
                  </div>
                  {ttsEngines.map((engine) => {
                    const reading = speaking[engine.id] ?? null;
                    const isSelected = engine.id === chosenTtsEngine;
                    const isSettingsOpen = openTtsSettings.has(engine.id);
                    const settingsId = `speech-tts-settings-${engine.id}`;
                    return (
                      <div
                        key={engine.id}
                        data-speech-engine={engine.id}
                        className="grid bg-input"
                      >
                        <SettingsChoiceDisclosure
                          title={gatewayEngineLabel(engine)}
                          sub={engineWord(reading)}
                          isSelected={isSelected}
                          isOpen={isSettingsOpen}
                          controls={settingsId}
                          onSelect={() => void choose("tts", engine.id)}
                          onToggle={() => toggleTtsSettings(engine.id)}
                        />
                        {isSettingsOpen && (
                          <div id={settingsId} className="grid">
                            <EngineProblem
                              engineName={gatewayEngineName(engine)}
                              reading={reading}
                              isBusy={busy === `tts:${engine.id}`}
                              onPrepare={() => void prepare("tts", engine.id)}
                            />
                            <VoicesPanel
                              client={client}
                              prefs={prefs}
                              engine={engine.id}
                              onChange={onChange}
                            />
                          </div>
                        )}
                      </div>
                    );
                  })}
                </div>
              </SettingsChoiceGroup>
            </div>
          )}
        </div>
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
 *
 * A BAND NAMES ITS GROUP AND NEVER EXPLAINS IT. Both levels took a `description`
 * under the title, and every one of them said what the rows under it already say
 * — "every palette Vis ships" over the list of palettes, "how many sessions a
 * project lists" over 5/10/15 — so a group of 48px rows opened with two lines of
 * grey prose nobody reads twice. Reported over this screen as pointless; the prop
 * went with the last three call sites that used it.
 */

function SettingsColumn({
  title,
  meta,
  action,
  children,
}: {
  title: string;
  meta?: ReactNode;
  /** The column's ONE verb, at the end of its band: the amber ＋. */
  action?: ReactNode;
  children: ReactNode;
}) {
  return (
    <section className="flex min-w-0 flex-col sm:min-h-0">
      {/* A BAND NAMES THE COLUMN IN ONE LINE, and its verb is a MARK.
          The title, a meta and the ＋ used to share one wrapping flex line whose
          height was the button's: the two words sat at the top of it on their
          baseline while the ＋ centred itself in the rest, 8px lower than the title
          it stands beside. Then the reader asked what the meta and the sentence
          under it were FOR — a column that lists every machine does not need to
          name one of them in its own header, and "tap a row" is not news. The verb
          spelled itself out in WORDS for a while, which is how a band ends up as
          wide as its longest verb; it is the disc again, and the ＋ adds the thing
          the band is NAMED after — so the mark opening a session in the project
          band of the list and the mark adding a machine here are one rule, not one
          glyph meaning two things. The name and its meta wrap inside their own
          cell, the verb is the band's trailing cell centred against whatever that
          cell grows to, and the band never pads around it: the row is `min-h-12`
          because a finger lands there. */}
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
            <span className="flex shrink-0 items-center empty:hidden">{action}</span>
          )}
        </div>
      </header>
      {/* A COLUMN CLOSES ITS OWN LAST GROUP. Reported over this screenshot: the
          dialog's last panel simply stopped. On a phone the frame is full-bleed
          and carries no bottom edge, so the selected amber cell of the last
          choice ran out into paper with no hairline under it — measured at 390px,
          the column body ended at the cell's own 2655px — and the two stacked
          halves were told apart by the GRID rather than by the column that ends.
          The body draws the rule it owes below itself; on `sm:` the columns stand
          side by side and the frame's own 1px bottom border is that edge, so it
          is dropped there rather than doubled. */}
      <div className="min-w-0 divide-y divide-dialog-edge border-b border-dialog-edge sm:min-h-0 sm:flex-1 sm:overflow-y-auto sm:overscroll-contain sm:border-b-0">
        {children}
      </div>
    </section>
  );
}

function DiagnosticFact({ label, value }: { label: string; value: string }) {
  return (
    <div className="flex min-h-12 min-w-0 items-center gap-3 px-3 py-2 mouse:min-h-9 mouse:py-1.5">
      <dt className="min-w-0 flex-1 text-body text-white">{label}</dt>
      <dd className="max-w-[65%] break-words text-right font-mono text-ui text-dialog-hint">
        {value}
      </dd>
    </div>
  );
}

/** Source identity, wire compatibility and the one deliberate way out for app-private logs. */
export function DiagnosticsPanel() {
  const [isExporting, setIsExporting] = useState(false);
  const [exported, setExported] = useState("");
  const [exportError, setExportError] = useState("");

  async function exportLogs() {
    setIsExporting(true);
    setExported("");
    setExportError("");
    try {
      setExported(await exportDiagnostics());
    } catch (cause) {
      setExportError(
        cause instanceof Error && cause.message
          ? cause.message
          : "The app logs could not be exported.",
      );
    } finally {
      setIsExporting(false);
    }
  }

  return (
    <SettingsPanel title="Diagnostics" meta="app logs">
      <dl className="divide-y divide-dialog-edge">
        <DiagnosticFact label="Version" value={APP_VERSION} />
        <DiagnosticFact label="Build" value={APP_BUILD_NUMBER} />
        <DiagnosticFact label="Commit" value={APP_BUILD_COMMIT} />
        <DiagnosticFact
          label="Gateway compatibility"
          value={`Protocol ${APP_MIN_GATEWAY_PROTOCOL}+ · must accept client ${APP_PROTOCOL}`}
        />
      </dl>
      <div className="space-y-2 px-3 py-3">
        <p className="text-body text-dialog-hint">
          App events stay in app-private files for seven days, capped at 8 MB.
          Export uses the system share sheet or saves a file. Gateway credentials
          and request bodies are not recorded.
        </p>
        {exported && <Banner kind="ok">{exported}</Banner>}
        {exportError && <Banner kind="err">{exportError}</Banner>}
        <Button
          variant="secondary"
          density="panel"
          className="w-full"
          disabled={isExporting}
          aria-busy={isExporting}
          onClick={() => void exportLogs()}
        >
          {isExporting ? "Preparing logs…" : "Export app logs"}
        </Button>
      </div>
    </SettingsPanel>
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
 * decides — and APPLICATION owns what this copy of Vis decides (its theme).
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
  primaryUrl,
  onAddMachine,
  onMakePrimary,
  onRename,
  onRemove,
  onSelectAddress,
  onClose,
}: {
  gateways: GatewayConn[];
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
  const [speechPrefs, setSpeechPrefs] = useState<SpeechPrefs>(DEFAULT_SPEECH_PREFS);
  const [pending, setPending] = useState<string | null>(null);
  const [err, setErr] = useState<string | null>(null);
  // Pairing opens over this dialog rather than inside it: see the sheet at the
  // foot of the return, and the band’s + that is its only door.
  const [isAdding, setIsAdding] = useState(false);

  useEffect(() => {
    let cancelled = false;
    void (async () => {
      const [theme, speech] = await Promise.all([getThemePref(), getSpeechPrefs()]);
      if (cancelled) return;
      setPref(theme);
      setSpeechPrefs(speech);
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

  async function changeSpeech(write: () => Promise<void>): Promise<SpeechPrefs> {
    await write();
    const next = await getSpeechPrefs();
    speechOutput.apply(next);
    setSpeechPrefs(next);
    return next;
  }

  // Settings opens on the fleet, not inside one machine. The reader chooses which
  // disclosures to open; opening one is never a reason to close another one.
  const [openIds, setOpenIds] = useState<ReadonlySet<string>>(
    () => new Set<string>(),
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
        <div className="grid min-w-0 grid-cols-1 divide-dialog-edge sm:min-h-0 sm:flex-1 sm:grid-cols-2 sm:divide-x sm:overflow-hidden">
          <SettingsColumn
            title="Machines"
            action={
              /* THE COLUMN'S ONE VERB, AND IT WEARS THE ONLY FILL IN THE DIALOG.
                 This is the top level here — the bands nested under a machine
                 (Providers, Notifications, MCP servers) carry the same disc in the
                 quiet face, so a panel inside a machine can never read as a second
                 MACHINES band. */
              <IconButton
                variant="primary"
                label="Add a machine"
                title="Add a machine"
                onClick={() => setIsAdding(true)}
              >
                <PlusIcon className="size-4" />
              </IconButton>
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
                  <GatewayPanels
                    key={machineId(conn)}
                    gateway={conn}
                    speechPrefs={speechPrefs}
                    onSpeechChange={changeSpeech}
                  />
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
            meta="this device"
          >
            {err && (
              <div className="p-3 sm:p-4">
                <Banner kind="err">{err}</Banner>
              </div>
            )}

            <SettingsPanel
              title="Theme"
              meta={`${THEMES.length} available`}
            >
              <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                {/* NO MODE COLUMN. Every theme is named `Blockether Light`, `Solarized
                    Dark`, `Vis Light`, so a trailing `light`/`dark` restated the last word
                    of its own row six times down the list. The name is the whole answer. */}
                {THEMES.map((choice) => (
                  <ChoiceCell
                    key={choice.id}
                    title={choice.label}
                    isSelected={pref === choice.id}
                    isLeaf
                    disabled={pending?.startsWith("theme:") ?? false}
                    onClick={() => void chooseTheme(choice)}
                  />
                ))}
              </div>
            </SettingsPanel>
            <DiagnosticsPanel />
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
      /* THE VERB RIDES THE BAND THAT NAMES WHAT IT ADDS, and it renders nothing
         until the gateway has said something is addable — so the band asks for it
         unconditionally and `AddProviderButton` answers with its own silence. */
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
  // Same rule as the native panel: the verdict this browser settled on last time
  // is the honest first frame, so reopening Settings does not flash `Checking…`.
  const live = loaded ? notifying : null;
  const remembered = useMemo(
    () => cachedNotifyVerdict(gateway.url),
    [gateway.url],
  );
  useEffect(() => {
    if (live !== null) rememberNotifyVerdict(gateway.url, live);
  }, [live, gateway.url]);
  const shown = live ?? remembered;
  const hasBanner = Boolean(err) || !supported || blocked;

  return (
    <SettingsPanel
      title="Notifications"
      meta={machine}
      action={
        <NotifyConnectionButton
          machine={machine}
          isOn={shown ?? false}
          isBusy={busy !== null}
          isChecking={shown === null}
          disabled={!supported || blocked || shown === null || busy !== null}
          // The mark on the control is what the press must do, so a band painted
          // from the remembered verdict acts on THAT, not on a load still in flight.
          onClick={() => void (shown ? disable() : enable())}
        />
      }
    >
      {hasBanner && (
        <div className="space-y-2 p-3">
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
  // Reopening Settings must not re-ask a question this device already has the
  // answer to: the last device list this machine gave is painted first and the
  // fetch below revalidates it underneath.
  const seed = useMemo(() => client.cachedDevices(), [client]);
  const [push, setPush] = useState<PushStatus | null>(seed?.push ?? null);
  const [devices, setDevices] = useState<PushDevice[] | null>(
    seed?.devices ?? null,
  );
  const [perm, setPerm] = useState<PushPermission>("unsupported");
  const [err, setErr] = useState<string | null>(null);
  const [busy, setBusy] = useState<"enable" | "disable" | null>(null);
  // An OLDER gateway simply has no /v1/devices route. That is not an error the
  // user can act on — it is a missing capability upstream — so the whole panel
  // (and every button in it) disappears instead of offering calls that 404.
  // The refusal is remembered per machine, because a panel that paints itself
  // and then deletes itself takes everything below it up the screen with it.
  const [unsupported, setUnsupported] = useState(() =>
    client.isDevicesUnsupported(),
  );
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
        // A machine that was upgraded since the last visit answers now: take
        // the remembered refusal back off rather than staying hidden until the
        // app is relaunched.
        setUnsupported(false);
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
  const [areMasksRead, setAreMasksRead] = useState(false);
  useEffect(() => {
    let stale = false;
    void (async () => {
      const ids = await registeredIds(token ?? "");
      if (stale) return;
      setMasks(ids.map(maskToken));
      setAreMasksRead(true);
    })();
    return () => {
      stale = true;
    };
  }, [token, devices]);
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

  // The OS outranks everything else: a machine can hold this device's token and
  // still reach nobody, so a blocked permission is never reported as connected.
  const blocked = supported && perm === "denied";
  // THE ROW NEVER FLASHES, AND OPENING IT COSTS NOTHING. Its verdict is
  // assembled from asynchronous answers, so its honest first frame used to be
  // `Checking…` on every open — an amber `Connect` that turned into a quiet
  // `Disconnect` a moment later, on a question whose answer had not changed
  // since the last time this dialog was opened. Reported as: the settings
  // screen flickers, and every paired machine is asked the same thing four or
  // five times over. So the launch/wake sweep settles this verdict for the
  // WHOLE fleet at one request each (`lib/notify.ts`), the row paints from
  // there, and the revalidating read below is answered by that same request
  // (`gateway.ts`).
  const isSettled = devices !== null && areMasksRead;
  const live = isSettled
    ? notifyVerdict({
        isHeld: isHeldBy(devices ?? [], masks),
        isWanted: notify,
        isBlocked: blocked,
      })
    : null;
  const remembered = useMemo(
    () => cachedNotifyVerdict(gateway.url),
    [gateway.url],
  );
  useEffect(() => {
    if (live !== null) rememberNotifyVerdict(gateway.url, live);
  }, [live, gateway.url]);
  const shown = live ?? remembered;

  // Gateway too old to know about push at all: render nothing.
  if (unsupported) return null;

  const machine = gateway.label ?? gatewayHost(gateway.url);
  const checking = shown === null;
  const hasBanner =
    Boolean(err) || !supported || blocked || Boolean(push && !available);

  return (
    <SettingsPanel
      title="Notifications"
      meta={machine}
      action={
        <NotifyConnectionButton
          machine={machine}
          isOn={shown ?? false}
          isBusy={busy !== null}
          isChecking={checking}
          disabled={
            !supported || !available || blocked || checking || busy !== null
          }
          // The mark on the control is what the press must do, so a band painted
          // from the remembered verdict acts on THAT, not on a load still in flight.
          onClick={() => void (shown ? disable() : enable())}
        />
      }
    >
      {hasBanner && (
        <div className="space-y-2 p-3">
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
  meta,
  action,
  children,
}: {
  title: string;
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
          not fit beside it drops to its own line. */}
      {/* A NESTED BAND IS NOT A COLUMN BAND. Reported over this screen: Providers,
          Notifications and MCP servers did not read as parts OF a machine — each
          wore the same slab, the same white and nearly the same weight as the
          `MACHINES` band above them, so four peers stood where there are two
          levels. The column keeps the paper, the size and the white; a panel
          inside a machine keeps neither, and speaks in the hint colour one step
          smaller. Reported since (paraphrased: bin that rail on the left, line the
          borders up): the 2px accent tick it wore was the only label on the screen
          standing 10px right of every other one. */}
      <header className="flex min-h-8 min-w-0 flex-wrap content-center items-baseline gap-x-3 gap-y-1 border-b border-dialog-edge px-3 py-1.5">
        <h3 className="min-w-0 flex-auto truncate font-mono text-chip font-bold uppercase tracking-[0.14em] text-dialog-hint">
          {title}
        </h3>
        {meta && (
          <span className="ms-auto min-w-0 max-w-full break-words text-right font-mono text-chip font-bold uppercase tracking-wider text-dialog-hint">
            {meta}
          </span>
        )}
        {action && (
          <span className="flex shrink-0 items-center self-center empty:hidden">
            {action}
          </span>
        )}
      </header>
      {/* A PANEL BODY DIVIDES ITS OWN PARTS. `divide-y` draws only BETWEEN
          siblings, so a panel holding one list is unchanged, and a panel whose
          last child is a verb gets the hairline that verb needs to be a row. */}
      <div className="divide-y divide-dialog-edge">{children}</div>
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
