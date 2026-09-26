import { Fragment, useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { reserveAuthTab, watchAuth, type AuthTab, type AuthWatch } from '../../lib/oauth';
import { homeifyPath } from '../../lib/path';
import { SwipeActions, type SwipeAction } from '../../components/SwipeActions';

import {
  GatewayClient,
  GatewayError,
  GatewayOAuthError,
  INCOMPATIBLE_STATUS,
} from '../../lib/gateway';
import type {
  GatewayConn,
  McpAuthFlow,
  McpServer,
  McpServerInput,
  McpTestResult,
  SpeechPrefs,
  Toggle,
  ToggleGroup,
} from '../../lib/types';
import {
  ArrowOutIcon,
  ChevronIcon,
  CircleSlashIcon,
  PencilIcon,
  PlayIcon,
  PlusIcon,
  StopIcon,
  TrashIcon,
} from '../../components/icons';
import {
  Banner,
  Button,
  Chip,
  ConfirmRow,
  IconButton,
  Input,
  ListRow,
  Select,
  Switch,
  Text,
} from '../../components/ui';
import {
  AddProviderButton,
  AddProviderPicker,
  ProviderRows,
  unscopedMessage,
  useProviderAuth,
} from '../../components/ProviderAuth';
import { NotificationsPanel } from './NotificationSettings';
import { SpeechEnginesPanel, type SaveSpeechPrefs } from './SpeechSettings';
import { FormLabel, SettingsPanel } from './SettingsLayout';
import { IMPROVE_MODE_LABELS, type ImproveMode } from '../../lib/improve';

/** Closed-choice setting: one shared picker, with saving disabling input. */
export function EnumSetting({
  toggle,
  busy = false,
  onPick,
}: {
  toggle: Toggle;
  busy?: boolean;
  onPick: (value: string) => void;
}) {
  return (
    <Select
      className="self-center"
      aria-label={toggle.label}
      aria-busy={busy}
      value={toggle.value ?? ''}
      disabled={busy}
      onValueChange={onPick}
      options={(toggle.choices ?? []).map((choice) => ({
        value: choice,
        label: toggle.id === 'improve_mode'
          ? (IMPROVE_MODE_LABELS[choice as ImproveMode] ?? choice)
          : choice,
      }))}
    />
  );
}

/** Gateway-backed text setting. Drafts remain local until explicit Save/Enter. */
export function StringSetting({
  toggle,
  busy,
  onSave,
}: {
  toggle: Toggle;
  busy: boolean;
  onSave: (value: string) => Promise<boolean>;
}) {
  const [draft, setDraft] = useState(toggle.value ?? '');
  const changed = draft !== (toggle.value ?? '');
  return (
    <form
      className="flex min-w-0 flex-col gap-2 px-3 py-2 sm:px-4"
      onSubmit={(event) => {
        event.preventDefault();
        if (!busy && changed && draft.trim()) {
          void onSave(draft).then((saved) => {
            if (saved) setDraft(draft.trim());
          });
        }
      }}
    >
      <div>
        <Text as="p" variant="label">
          {toggle.label}
        </Text>
        {toggle.description && (
          <Text as="p" variant="description" className="mt-0.5">
            {toggle.description}
          </Text>
        )}
      </div>
      <div className="flex min-w-0 flex-wrap items-center gap-2">
        <Input
          aria-label={toggle.label}
          value={draft}
          maxLength={toggle.max_length}
          disabled={busy}
          required
          className="flex-1"
          onChange={(event) => setDraft(event.target.value)}
        />
        {(changed || busy) && (
          <Button
            type="submit"
            density="panel"
            disabled={busy || !draft.trim()}
            aria-busy={busy}
          >
            {busy ? 'Saving…' : 'Save'}
          </Button>
        )}
        {changed && (
          <Button
            type="button"
            variant="secondary"
            density="panel"
            disabled={busy}
            onClick={() => setDraft(toggle.value ?? '')}
          >
            Cancel
          </Button>
        )}
      </div>
    </form>
  );
}

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
  const [failure, setFailure] = useState<'unreachable' | 'unauthorized' | 'incompatible' | null>(
    null,
  );

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
          setFailure('unauthorized');
          setGroups(null);
          return;
        }
        setErr((e as Error).message);
        setGroups(null);
        if (e instanceof GatewayError && e.status === INCOMPATIBLE_STATUS) {
          setFailure('incompatible');
          return;
        }
        setFailure('unreachable');
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
          toggles: group.toggles.map((toggle) => (toggle.id === updated.id ? updated : toggle)),
        })) ?? null,
    );
  }

  async function flip(toggle: Toggle) {
    setPending(toggle.id);
    try {
      patch(await client.setSetting(toggle.id, 'toggle'));
      // Feature flags can reveal or hide their dependent settings.
      if (toggle.is_experimental) await load();
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function pick(toggle: Toggle, value: string) {
    setErr(null);
    setPending(toggle.id);
    try {
      patch(await client.setSetting(toggle.id, 'value', value));
      return true;
    } catch (e) {
      setErr((e as Error).message);
      return false;
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
          <SpeechEnginesPanel client={client} prefs={speechPrefs} onChange={onSpeechChange} />
        </>
      )}

      {failure === 'incompatible' ? null : failure === 'unreachable' ? (
        <SettingsPanel title="Settings">
          <div className="flex flex-col items-center gap-3 px-4 py-8 text-center">
            <p className="text-err">
              <Text variant="label" tone="inherit">
                Machine unreachable
              </Text>
            </p>
            <Text as="p" variant="description">
              Can't load settings — vis isn't responding on this machine.
            </Text>
            <Button variant="secondary" onClick={() => void load()}>
              Retry
            </Button>
          </div>
        </SettingsPanel>
      ) : failure === 'unauthorized' ? (
        <SettingsPanel title="Settings" meta="unauthorized">
          <div className="flex flex-col items-center gap-3 px-4 py-8 text-center">
            <p className="text-warn-strong">
              <Text variant="label" tone="inherit">
                Token missing or invalid
              </Text>
            </p>
            <Text as="p" variant="description" className="max-w-sm">
              The machine is online, but rejected this token. Re-pair from{' '}
              <code className="text-accent-ink">vis-agent gateway pair</code> and paste the fresh
              link to load its settings.
            </Text>
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
            <div className="bg-panel px-4 py-2">
              <Text as="p" variant="description">
                Loading settings…
              </Text>
            </div>
            {['w-1/2', 'w-2/3', 'w-2/5'].map((width) => (
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
          <p className="px-4 py-6 text-center">
            <Text variant="description">No settings exposed by this machine.</Text>
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
                if (toggle.type === 'string')
                  return (
                    <StringSetting
                      key={`${toggle.id}:${toggle.value}`}
                      toggle={toggle}
                      busy={busy}
                      onSave={(value) => pick(toggle, value)}
                    />
                  );
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
                    className="grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-start gap-x-4 gap-y-2 px-3 py-2 sm:px-4 sm:py-2"
                  >
                    <div className="min-w-0">
                      <div className="flex flex-wrap items-center gap-2">
                        <Text as="p" variant="label" className="break-words">
                          {toggle.label}
                        </Text>
                        {toggle.is_experimental && (
                          <span className="bg-thinking-surface px-1 font-mono text-ui text-warn">
                            Experimental
                          </span>
                        )}
                      </div>
                      {toggle.description && (
                        <Text as="p" variant="description" className="mt-0.5 break-words">
                          {toggle.description}
                        </Text>
                      )}
                    </div>

                    {toggle.type === 'boolean' && (
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

                    {toggle.type === 'enum' && toggle.choices && (
                      <EnumSetting
                        toggle={toggle}
                        busy={busy}
                        onPick={(value) => void pick(toggle, value)}
                      />
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

/**
 * A server that answers "connecting" may still be mid-handshake after a start or
 * sign-in. Poll until it settles, but give each server its own 30-second deadline:
 * a stalled peer must neither stay "connecting" forever nor postpone another
 * server's verdict. The gateway continues its own health retries.
 */
const MCP_SETTLE_POLL_MS = 1500;
const MCP_SETTLE_WINDOW_MS = 30_000;

export function McpServersPanel({ client }: { client: GatewayClient }) {
  // The rows this machine gave last time are the first frame; `load` below
  // revalidates them underneath. Opening on `null` flashed an empty band and
  // then moved every panel under it down (see `cachedMcpServers`).
  const [servers, setServers] = useState<McpServer[] | null>(() => client.cachedMcpServers());
  const [showForm, setShowForm] = useState(false);
  const [transport, setTransport] = useState<'stdio' | 'streamable_http'>('stdio');
  const [name, setName] = useState('');
  const [command, setCommand] = useState('');
  const [args, setArgs] = useState('');
  const [cwd, setCwd] = useState('');
  const [url, setUrl] = useState('');
  const [env, setEnv] = useState('');
  const [headers, setHeaders] = useState('');
  const [busy, setBusy] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [test, setTest] = useState<McpTestResult | null>(null);
  const [auth, setAuth] = useState<{
    flow: McpAuthFlow;
    client: GatewayClient;
    tab?: AuthTab;
  } | null>(null);
  const authFlow = auth?.client === client ? auth.flow : null;
  const authEpoch = useRef(0);
  const stopAuth = useRef<AuthWatch | null>(null);
  // The server being edited, or null while adding. Editing keys the save by the
  // ORIGINAL name: `POST /v1/mcp/servers` replaces by name, so a renamed field
  // would fork a second server instead of updating this one.
  const [editing, setEditing] = useState<McpServer | null>(null);
  // Rows standing open to show the whole spec: the arguments and working
  // directory the one-line meta cannot carry.
  const [expanded, setExpanded] = useState<Set<string>>(() => new Set());
  // The one destructive question open at a time, asked in the row itself.
  const [confirming, setConfirming] = useState<{
    name: string;
    kind: 'remove' | 'signout';
  } | null>(null);

  // Escape unwinds the row's own question first, before the dialog hears it.
  useEffect(() => {
    if (confirming === null) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.stopPropagation();
      setConfirming(null);
    };
    window.addEventListener('keydown', onKey, true);
    return () => window.removeEventListener('keydown', onKey, true);
  }, [confirming]);

  const settlingSince = useRef(new Map<string, number>());
  const [timedOut, setTimedOut] = useState<Set<string>>(() => new Set());
  const load = useCallback(
    async (isPoll = false) => {
      // A user action opens a fresh settle window; a poll only spends the open one.
      if (!isPoll) {
        settlingSince.current.clear();
        setTimedOut(new Set());
      }
      try {
        setServers(await client.mcpServers());
        setError(null);
      } catch (e) {
        setError((e as Error).message);
      }
    },
    [client],
  );

  useEffect(() => {
    void load();
  }, [load]);

  useEffect(() => {
    const pending = servers?.filter((server) => mcpServerState(server).isSettling) ?? [];
    const names = new Set(pending.map((server) => server.name));
    for (const name of settlingSince.current.keys()) {
      if (!names.has(name)) settlingSince.current.delete(name);
    }
    if ([...timedOut].some((name) => !names.has(name))) {
      setTimedOut((previous) => new Set([...previous].filter((name) => names.has(name))));
    }
    const now = Date.now();
    for (const { name } of pending) {
      if (!settlingSince.current.has(name)) settlingSince.current.set(name, now);
    }
    const waiting = pending.filter(({ name }) => !timedOut.has(name));
    if (!waiting.length) return;
    const untilDeadline = Math.min(
      ...waiting.map(({ name }) => MCP_SETTLE_WINDOW_MS - (now - settlingSince.current.get(name)!)),
    );
    const deadlineTimer = window.setTimeout(() => {
      const expired = waiting
        .filter(({ name }) => Date.now() - settlingSince.current.get(name)! >= MCP_SETTLE_WINDOW_MS)
        .map(({ name }) => name);
      if (expired.length) setTimedOut((previous) => new Set([...previous, ...expired]));
    }, Math.max(0, untilDeadline));
    const pollTimer = window.setTimeout(() => void load(true), MCP_SETTLE_POLL_MS);
    return () => {
      window.clearTimeout(deadlineTimer);
      window.clearTimeout(pollTimer);
    };
  }, [servers, timedOut, load]);

  useEffect(
    () => () => {
      authEpoch.current += 1;
    },
    [client],
  );

  useEffect(() => {
    if (!auth || auth.client !== client) return;
    const { client: paired, flow, tab } = auth;
    const verdictOf = (value: McpAuthFlow) => ({ status: value.status, message: value.error });
    const watcher = watchAuth(
      { ...flow, expires_at: flow.expires_at_ms },
      {
        complete: (input) =>
          paired.mcpAuthComplete(flow.server, flow.flow_id, input).then(verdictOf),
        poll: () => paired.mcpAuthPoll(flow.server, flow.flow_id).then(verdictOf),
        cancel: () => paired.mcpAuthCancel(flow.server, flow.flow_id),
      },
      (verdict) => {
        authEpoch.current += 1;
        setAuth(null);
        setBusy(null);
        if (verdict.status === 'ok') void load();
        else setError(verdict.message ?? 'Authorization failed. Start sign-in again.');
      },
      undefined,
      tab,
    );
    stopAuth.current = watcher;
    return () => {
      watcher.stop();
      if (stopAuth.current === watcher) stopAuth.current = null;
    };
  }, [auth, client, load]);

  // Editing loads the sanitized row back into the form. `env` and `headers` come
  // back BLANK on purpose: the gateway never sends secret values, and a save that
  // omits those keys keeps the ones it already stores.
  function openForm(server: McpServer | null) {
    setError(null);
    setTest(null);
    setEditing(server);
    setTransport(server?.transport ?? 'stdio');
    setName(server?.name ?? '');
    setCommand(server?.command ?? '');
    setArgs((server?.args ?? []).join('\n'));
    setCwd(server?.cwd ?? '');
    setUrl(server?.url ?? '');
    setEnv('');
    setHeaders('');
    setShowForm(true);
  }

  function closeForm() {
    setShowForm(false);
    setEditing(null);
    setName('');
    setCommand('');
    setArgs('');
    setCwd('');
    setUrl('');
    setEnv('');
    setHeaders('');
  }

  const spec = (): McpServerInput => {
    const keyValues = (text: string) =>
      Object.fromEntries(
        text
          .split('\n')
          .map((line) => line.trim())
          .filter(Boolean)
          .map((line) => {
            const index = line.indexOf('=');
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
    return transport === 'stdio'
      ? {
          ...kept,
          transport,
          command: command.trim(),
          args: args
            .split('\n')
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
    if (!name.trim()) return 'Server name is required.';
    if (transport === 'stdio' && !command.trim()) return 'An executable is required.';
    if (transport === 'streamable_http' && !url.trim()) return 'An MCP endpoint is required.';
    return null;
  };

  async function validateAndSave() {
    const message = valid();
    if (message) return setError(message);
    // Keyed by the name the gateway already knows when editing.
    const target = editing ? editing.name : name.trim();
    const candidate = spec();
    setBusy('save');
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
      await (running ? client.startMcpServer(server.name) : client.killMcpServer(server.name));
      await load();
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(null);
    }
  }

  // Bind every flow to the paired client that initiated it, never the currently
  // selected machine after a switch. A late start is cancelled without opening a URL.
  // The gateway always issues a loopback callback: on a phone the native receiver
  // binds that port and opens the system browser itself. Elsewhere the browser only
  // honours an open made inside the tap, so the tab is claimed before the fetch that
  // produces the URL and navigated once it arrives.
  async function authorize(server: McpServer) {
    const epoch = ++authEpoch.current;
    stopAuth.current?.stop();
    setAuth(null);
    setBusy(server.name);
    setError(null);
    const tab = reserveAuthTab();
    try {
      const flow = await client.mcpAuthStart(server.name);
      if (authEpoch.current !== epoch) {
        tab?.close();
        await client.mcpAuthCancel(flow.server, flow.flow_id).catch(() => {});
        return;
      }
      setAuth({ flow, client, tab });
    } catch (error) {
      tab?.close();
      if (authEpoch.current === epoch)
        setError(
          error instanceof GatewayOAuthError
            ? error.message
            : 'Cannot start sign-in. Check the gateway and MCP server settings, then try again.',
        );
    } finally {
      if (authEpoch.current === epoch) setBusy(null);
    }
  }

  function cancelAuth() {
    authEpoch.current += 1;
    stopAuth.current?.stop();
    setAuth(null);
    setBusy(null);
  }

  async function signOut(server: McpServer) {
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

  function toggleOpen(name: string) {
    setExpanded((current) => {
      const next = new Set(current);
      if (next.has(name)) next.delete(name);
      else next.add(name);
      return next;
    });
  }

  // Editing owns its divider; an add form uses the enclosing list or panel boundary.
  const form = showForm && (
    <div className={`space-y-3 bg-panel-2 p-3 ${editing ? 'border-t border-dialog-edge' : ''}`}>
      {!editing && (
        <div className="grid grid-cols-2 gap-1" role="group" aria-label="MCP transport">
          {(['stdio', 'streamable_http'] as const).map((kind) => (
            <Chip
              key={kind}
              isOn={transport === kind}
              onClick={() => setTransport(kind)}
              className="w-full"
            >
              {kind === 'stdio' ? 'Local command' : 'Streamable HTTP'}
            </Chip>
          ))}
        </div>
      )}
      {!editing && (
        <FormLabel label="Server name">
          <Input
            value={name}
            onChange={(event) => setName(event.target.value)}
            placeholder="filesystem"
            autoCapitalize="none"
            autoCorrect="off"
          />
        </FormLabel>
      )}
      {transport === 'stdio' ? (
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
              placeholder={'-y\n@modelcontextprotocol/server-filesystem\n/path'}
              className="min-h-24 w-full resize-y border border-dialog-edge bg-input px-2.5 py-2 font-mono text-meta text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none mouse:text-ui"
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
                ? 'One NAME=value per line. Leave blank to keep the values already stored.'
                : 'One NAME=value per line. Values are write-only after saving.'
            }
          >
            <textarea
              value={env}
              onChange={(event) => setEnv(event.target.value)}
              placeholder="API_TOKEN=…"
              className="min-h-20 w-full resize-y border border-dialog-edge bg-input px-2.5 py-2 font-mono text-meta text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none mouse:text-ui"
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
                ? 'One NAME=value per line. Leave blank to keep the values already stored.'
                : 'One NAME=value per line. Values are write-only after saving.'
            }
          >
            <textarea
              value={headers}
              onChange={(event) => setHeaders(event.target.value)}
              placeholder="Authorization=Bearer …"
              className="min-h-20 w-full resize-y border border-dialog-edge bg-input px-2.5 py-2 font-mono text-meta text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none mouse:text-ui"
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
        <Button variant="secondary" disabled={busy !== null} onClick={() => closeForm()}>
          Cancel
        </Button>
        <Button disabled={busy !== null} onClick={() => void validateAndSave()}>
          {busy === 'save' ? 'Validating…' : editing ? 'Validate & update' : 'Validate & save'}
        </Button>
      </div>
    </div>
  );

  return (
    <SettingsPanel
      title="MCP servers"
      action={
        showForm ? null : (
          <IconButton
            variant="quiet"
            align="trailing"
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
        {error && (
          <div className="p-3">
            <Banner kind="err">{error}</Banner>
          </div>
        )}
        {servers?.map((server) => {
          const isSigningIn = authFlow?.server === server.name;
          const state = mcpServerState(server, isSigningIn, timedOut.has(server.name));
          const isOpen = expanded.has(server.name);
          const panelId = `mcp-server-${server.name}`;
          const idle = busy === null;

          // The confirm IS the row, at the row's own height — the way a session's
          // delete asks. A cost sentence made it taller than the row it replaces.
          if (confirming?.name === server.name)
            return confirming.kind === 'remove' ? (
              <ConfirmRow
                key={server.name}
                question={`Remove ${server.name}?`}
                confirmLabel="Yes, remove"
                isBusy={busy === server.name}
                onKeep={() => setConfirming(null)}
                onConfirm={() => {
                  setConfirming(null);
                  void remove(server);
                }}
              />
            ) : (
              <ConfirmRow
                key={server.name}
                question={`Sign out of ${server.name}?`}
                confirmLabel="Yes, sign out"
                isBusy={busy === server.name}
                onKeep={() => setConfirming(null)}
                onConfirm={() => {
                  setConfirming(null);
                  void signOut(server);
                }}
              />
            );

          // THE VERBS OF THIS SERVER, waiting under its own row's trailing edge,
          // the way a provider's and a machine's do. Kill and start are runtime
          // verbs, so a config-file server carries them too; only the edits that
          // would rewrite somebody's `vis.yml` are missing from it.
          const actions: SwipeAction[] = [];
          if (server.url)
            actions.push(
              // While the browser holds the sign-in, the same slot takes it back.
              isSigningIn
                ? {
                    key: 'auth',
                    label: 'Cancel',
                    name: `Cancel signing in to ${server.name}`,
                    icon: <CircleSlashIcon className="size-4" />,
                    onSelect: cancelAuth,
                  }
                : {
                    key: 'auth',
                    label: server.is_authorized ? 'Re-auth' : 'Sign in',
                    name: server.is_authorized
                      ? `Sign in to ${server.name} again`
                      : `Sign in to ${server.name}`,
                    icon: <ArrowOutIcon className="size-4" />,
                    // The one verb a server cannot work without wears the accent.
                    tone: server.is_authorized ? 'neutral' : 'accent',
                    onSelect: () => {
                      if (idle) void authorize(server);
                    },
                  },
            );
          if (server.url && server.is_authorized)
            actions.push({
              key: 'signout',
              label: 'Sign out',
              name: `Sign out of ${server.name}`,
              icon: <CircleSlashIcon className="size-4" />,
              onSelect: () => setConfirming({ name: server.name, kind: 'signout' }),
            });
          actions.push({
            key: 'run',
            label: server.is_killed ? 'Start' : 'Kill',
            name: server.is_killed
              ? `Start ${server.name}`
              : `Kill ${server.name} until it is started again`,
            icon: server.is_killed ? (
              <PlayIcon className="size-4" />
            ) : (
              <StopIcon className="size-4" />
            ),
            onSelect: () => {
              if (idle) void setRunning(server, server.is_killed);
            },
          });
          if (server.is_managed) {
            actions.push({
              key: 'edit',
              label: 'Edit',
              name: `Edit ${server.name}`,
              icon: <PencilIcon className="size-4" />,
              onSelect: () => {
                if (idle) openForm(server);
              },
            });
            actions.push({
              key: 'remove',
              label: 'Remove',
              name: `Remove ${server.name} from this machine`,
              icon: <TrashIcon className="size-4" />,
              tone: 'danger',
              onSelect: () => setConfirming({ name: server.name, kind: 'remove' }),
            });
          }

          return (
            <div key={server.name} className="min-w-0">
              <SwipeActions label={server.name} actions={actions} alignMenuWithHeader>
                <div className="flex min-h-13 min-w-0 items-center gap-2 pl-3 sm:pl-4 mouse:min-h-0">
                  {/* Enablement leads the row; connection status remains in its
                      trailing text. Config-file settings are visible but read-only. */}
                  <Switch
                    label={`${server.name} MCP server`}
                    isOn={server.enabled}
                    isBusy={busy === server.name}
                    disabled={!server.is_managed || !idle}
                    title={
                      server.is_managed
                        ? state.label
                        : 'Listed from a hand-written config file; edit it there.'
                    }
                    aria-description={
                      server.is_managed
                        ? state.label
                        : `${state.label}. Listed from a hand-written config file; edit it there.`
                    }
                    onClick={() => void toggle(server)}
                  />
                  <ListRow
                    className="min-w-0 flex-1 gap-3"
                    aria-expanded={isOpen}
                    aria-controls={isOpen ? panelId : undefined}
                    aria-description={state.label}
                    onClick={() => toggleOpen(server.name)}
                  >
                    <span className="min-w-0 flex-1">
                      <span className="flex min-w-0 items-center gap-2">
                        <Text variant="label" className="truncate">
                          {server.name}
                        </Text>
                        {!server.is_managed && (
                          <Text
                            variant="meta"
                            className="shrink-0"
                            title="Listed from a hand-written config file; edit it there."
                          >
                            Config file
                          </Text>
                        )}
                      </span>
                      <Text
                        variant="meta"
                        className="block truncate"
                        title={server.transport === 'stdio' ? server.command : server.url}
                      >
                        {server.transport === 'stdio' ? homeifyPath(server.command) : server.url}
                      </Text>
                    </span>
                    <span
                      className={`shrink-0 ${state.tone === 'text-ok' ? 'text-dialog-hint' : state.tone}`}
                      title={state.label}
                    >
                      <Text variant="meta" tone="inherit">
                        {server.enabled ? state.word : ''}
                      </Text>
                    </span>
                    <ChevronIcon
                      open={isOpen}
                      className="size-3 shrink-0 text-dialog-hint"
                      aria-hidden
                    />
                  </ListRow>
                </div>
              </SwipeActions>
              {isOpen && !(showForm && editing?.name === server.name) && (
                <McpServerDetails id={panelId} server={server} />
              )}
              {showForm && editing?.name === server.name && form}
            </div>
          );
        })}
        {servers === null && (
          <p className="py-4 text-center">
            <Text variant="description">Checking MCP servers…</Text>
          </p>
        )}
        {servers?.length === 0 && !showForm && (
          <p className="py-4 text-center">
            <Text variant="description">No MCP servers on this gateway.</Text>
          </p>
        )}
        {showForm && !editing && form}
      </div>
    </SettingsPanel>
  );
}

/** Connection status is separate from the enable switch, which only reports configuration. */
function mcpServerState(
  server: McpServer,
  isSigningIn = false,
  timedOut = false,
): {
  tone: string;
  label: string;
  word: string;
  isSettling?: boolean;
} {
  const tools = `${server.tools} ${server.tools === 1 ? 'tool' : 'tools'}`;
  if (isSigningIn)
    return {
      tone: 'text-warn',
      label: 'Waiting for the browser to finish sign-in',
      word: 'signing in',
      isSettling: true,
    };
  if (server.is_killed)
    return {
      tone: 'text-dialog-hint',
      label: 'Killed — start it to reconnect',
      word: 'killed',
    };
  if (!server.enabled) return { tone: 'text-dialog-hint', label: 'Disabled', word: 'off' };
  if (server.is_connected) return { tone: 'text-ok', label: 'Connected', word: tools };
  if (server.status === 'unhealthy' || timedOut)
    return { tone: 'text-err', label: 'Unhealthy — could not connect', word: 'unhealthy' };
  if (server.url && !server.is_authorized)
    return {
      tone: 'text-warn',
      label: 'Not signed in — sign in to connect',
      word: 'sign in',
    };
  return {
    tone: 'text-warn',
    label: 'Connecting',
    word: 'connecting',
    isSettling: true,
  };
}

/** The whole non-secret spec of one server, under its own row. */
function McpServerDetails({ id, server }: { id: string; server: McpServer }) {
  const rows: [string, string][] = [];
  if (server.transport === 'stdio') {
    rows.push(['Command', server.command ?? '']);
    if (server.args?.length) rows.push(['Arguments', server.args.join(' ')]);
    if (server.cwd) rows.push(['Directory', server.cwd]);
  } else {
    rows.push(['Endpoint', server.url ?? '']);
    rows.push(['Sign-in', server.is_authorized ? 'Signed in' : 'Not signed in']);
  }
  rows.push(['Tools', String(server.tools)]);
  return (
    <div
      id={id}
      role="region"
      aria-label={`${server.name} details`}
      className="grid grid-cols-[auto_minmax(0,1fr)] gap-x-3 gap-y-1 border-t border-dialog-edge bg-panel-2 p-3"
    >
      {rows.map(([term, value]) => (
        <Fragment key={term}>
          <Text variant="meta">{term}</Text>
          <span className="min-w-0 wrap-anywhere text-dialog-foreground">
            <Text variant="meta" tone="inherit" title={value}>
              {(term === 'Command' || term === 'Directory' ? homeifyPath(value) : value)
                .split(/(\/)/)
                .map((part, index) => (
                  <Fragment key={index}>
                    {part}
                    {part === '/' && <wbr />}
                  </Fragment>
                ))}
            </Text>
          </span>
        </Fragment>
      ))}
    </div>
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
  const [isAdding, setIsAdding] = useState(false);
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
      action={
        <AddProviderButton
          auth={auth}
          isOpen={isAdding}
          onToggle={() => setIsAdding((open) => !open)}
        />
      }
    >
      {(fleetErr || fleetNote) && (
        <div className="space-y-2 p-3">
          {fleetErr && <Banner kind="err">{fleetErr.text}</Banner>}
          {fleetNote && <Banner kind="ok">{fleetNote.text}</Banner>}
        </div>
      )}

      {/* WHAT CAN STILL BE ADDED OPENS HERE, directly under the verb that asked
          for it and above the accounts it is about to join — not in a dialog
          standing on the dialog this panel already lives in. */}
      {isAdding && <AddProviderPicker auth={auth} onClose={() => setIsAdding(false)} />}

      {providers === null && (
        <p className="py-4 text-center">
          <Text variant="description">Checking provider sign-in…</Text>
        </p>
      )}

      {providers?.length === 0 && (
        <p className="py-4 text-center">
          <Text variant="description">No providers configured on this machine.</Text>
        </p>
      )}

      <ProviderRows auth={auth} />
    </SettingsPanel>
  );
}
