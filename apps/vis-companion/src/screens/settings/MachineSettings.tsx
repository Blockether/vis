import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { clientCallbackMode, startGatewayAuth, watchAuth, type AuthWatch } from "../../lib/oauth";
import { McpAuth } from "../../components/McpAuth";

import {
  GatewayClient,
  GatewayError,
  GatewayOAuthError,
  INCOMPATIBLE_STATUS,
} from "../../lib/gateway";
import type {
  GatewayConn,
  McpAuthFlow,
  McpServer,
  McpServerInput,
  McpTestResult,
  SpeechPrefs,
  Toggle,
  ToggleGroup,
} from "../../lib/types";
import { PlusIcon } from "../../components/icons";
import {
  Banner,
  Button,
  Chip,
  IconButton,
  Input,
  PROSE,
  Switch,
} from "../../components/ui";
import {
  AddProviderButton,
  ProviderRows,
  unscopedMessage,
  useProviderAuth,
} from "../../components/ProviderAuth";
import { NotificationsPanel } from "./NotificationSettings";
import { SpeechEnginesPanel, type SaveSpeechPrefs } from "./SpeechSettings";
import { FormLabel, SettingsPanel } from "./SettingsLayout";

/**
 * ONE MACHINE'S OWN SETTINGS, standing under that machine's own row in `SettingsDialog`.
 *
 * These panels used to be a dialog of their own — `Machine settings`, opened from a
 * machine's `⋯` — so the two halves of one question ("where do I change this?") stood
 * behind two different doors that could not be open at once. The panels are unchanged;
 * what left is the frame around them, and the dialog now owns Escape, the title and
 * the way out.
 */
export function MachineSettings({
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
        // A band's meta says what the list itself CANNOT — `unauthorized`, `app
        // logs`, `this device`. A tally of the rows you are already looking at is
        // not that, and it said the same nothing over every group.
        groups.map((group) => (
          <SettingsPanel key={group.id} title={group.title}>
            <div className="divide-y divide-dialog-edge">
              {group.toggles.map((toggle) => {
                const busy = pending === toggle.id;
                // Regression, user report (paraphrased: now that the row ends in a
                // real toggle, what is the mark on the left for): the row said its
                // state twice — a ticked ring in one alphabet and, a column away, the
                // switch that already says it — and every value row wore the same dot,
                // which changed for nothing. One row, one anchor (anti-slop 3 and 5).
                // The marks that stayed are the ones nothing else in their row can
                // say: a machine's health, a provider's session, an MCP server's
                // reach.
                return (
                  <div
                    key={toggle.id}
                    className="grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-start gap-x-4 gap-y-2 px-3 py-2 transition-colors hover:bg-hover sm:px-4 sm:py-2"
                  >
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
                      // A CONTROL ANSWERS THE WHOLE ROW, not its first line. The mark
                      // beside the name rides the title's baseline, but a 28px switch
                      // pinned to `items-start` sat 6px above the centre of the two
                      // lines it governs, and every wrapped description tilted the
                      // column further; it centres against the cell, the way a setting
                      // row does everywhere a finger expects one.
                      <Switch
                        className="self-center"
                        label={toggle.label}
                        isOn={!!toggle.enabled}
                        isBusy={busy}
                        disabled={busy}
                        onClick={() => flip(toggle)}
                      />
                    )}

                    {toggle.type === "enum" && toggle.choices && (
                      <div className="col-span-full flex min-w-0 flex-wrap gap-1.5">
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

export function McpServersPanel({ client }: { client: GatewayClient }) {
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
  const [auth, setAuth] = useState<{ flow: McpAuthFlow; client: GatewayClient } | null>(null);
  const authFlow = auth?.client === client ? auth.flow : null;
  const authEpoch = useRef(0);
  const stopAuth = useRef<AuthWatch | null>(null);
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

  useEffect(() => () => { authEpoch.current += 1; }, [client]);

  useEffect(() => {
    if (!auth || auth.client !== client) return;
    const { client: paired, flow } = auth;
    const verdictOf = (value: McpAuthFlow) => ({ status: value.status, message: value.error });
    const watcher = watchAuth({ ...flow, expires_at: flow.expires_at_ms }, {
      complete: input => paired.mcpAuthComplete(flow.server, flow.flow_id, input).then(verdictOf),
      poll: () => paired.mcpAuthPoll(flow.server, flow.flow_id).then(verdictOf),
      cancel: () => paired.mcpAuthCancel(flow.server, flow.flow_id),
    }, verdict => {
      authEpoch.current += 1;
      setAuth(null); setBusy(null);
      setAuthInput("");
      if (verdict.status === "ok") void load();
      else setError(verdict.message ?? "Authorization failed. Start sign-in again.");
    });
    stopAuth.current = watcher;
    return () => { watcher.stop(); if (stopAuth.current === watcher) stopAuth.current = null; };
  }, [auth, client, load]);

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

  // Bind every flow to the paired client that initiated it, never the currently
  // selected machine after a switch. A late start is cancelled without opening a URL.
  async function authorize(server: McpServer) {
    const epoch = ++authEpoch.current;
    stopAuth.current?.stop();
    setAuth(null);
    setBusy(server.name);
    setError(null);
    try {
      const flow = await startGatewayAuth(client,
        () => client.mcpAuthStart(server.name, clientCallbackMode()),
        () => authEpoch.current === epoch);
      if (!flow) return;
      if (authEpoch.current !== epoch) {
        await client.mcpAuthCancel(flow.server, flow.flow_id).catch(() => {});
        return;
      }
      setAuthInput("");
      setAuth({ flow, client });
    } catch (error) {
      if (authEpoch.current === epoch) setError(error instanceof GatewayOAuthError ? error.message
        : "Cannot start sign-in. Check the gateway and MCP server settings, then try again.");
    } finally {
      if (authEpoch.current === epoch) setBusy(null);
    }
  }

  async function finishAuth() {
    if (!authFlow) return;
    const epoch = authEpoch.current;
    setBusy(authFlow.server);
    try {
      await stopAuth.current?.complete(authInput.trim());
    } catch {
      if (authEpoch.current === epoch) setError("Cannot finish sign-in. Check the callback and try again.");
    } finally {
      if (authEpoch.current === epoch) setBusy(null);
    }
  }

  function cancelAuth() {
    authEpoch.current += 1;
    stopAuth.current?.stop();
    setAuth(null);
    setAuthInput("");
    setBusy(null);
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
            edge
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
              <McpAuth flow={authFlow} input={authInput} busy={busy !== null}
                onInput={setAuthInput} onFinish={() => void finishAuth()} onCancel={cancelAuth} onOpen={() => stopAuth.current?.open()} />
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
