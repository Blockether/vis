import { useEffect, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { Subagent } from '../lib/types';
import {
  CircleAlertIcon,
  CircleCheckIcon,
  CircleDashedIcon,
  CircleDotIcon,
  CircleXIcon,
  MARK_NUDGE,
} from './icons';
import { Button, DialogFrame, Modal } from './ui';

// HOW MANY answers the subagent is waiting for. One badge for two open requests
// is what made answering the first of them look like nothing had happened.
function inputLabel(agent: Subagent): string {
  const open = agent.pending_input_count ?? 1;
  return open > 1 ? `Needs your input ×${open}` : 'Needs your input';
}

const STATUS: Record<Subagent['status'], string> = {
  queued: 'Queued',
  running: 'Running',
  completed: 'Completed',
  failed: 'Failed',
  cancelled: 'Cancelled',
  budget_limited: 'Budget reached',
};

export function AgentTeamView({
  agents, loading, error, pending, parentId, onRefresh, onOpen, onCancel,
}: {
  agents: readonly Subagent[];
  loading: boolean;
  error: string | null;
  pending: string | null;
  parentId?: string;
  onRefresh: () => void;
  onOpen: (sid: string) => void;
  onCancel: (sid: string) => void;
}) {
  const [confirm, setConfirm] = useState<string | null>(null);
  return (
    <div className="space-y-3 p-4 text-body text-white">
      <p className="text-ui text-dialog-hint">
        Subagents share this checkout. They report to their leader, not to other independent sessions.
      </p>
      <div className="flex flex-wrap gap-2">
        {parentId && <Button variant="quiet" onClick={() => onOpen(parentId)}>Open parent</Button>}
        <Button variant="quiet" disabled={loading} onClick={onRefresh}>Refresh team</Button>
      </div>
      {loading && <p role="status">Loading team…</p>}
      {error && <p role="alert" className="text-err-ink">{error}</p>}
      {!loading && !error && agents.length === 0 && (
        <p>No subagents yet. Ask the leader to delegate a bounded task.</p>
      )}
      <ul className="divide-y divide-dialog-edge">
        {agents.map((agent, index) => (
          <li key={agent.session_id} className="space-y-2 py-3">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <span className="flex items-baseline gap-2">
                <span className="text-ui font-bold">A{index + 1}</span>
                <span className="text-ui text-dialog-hint">
                  {agent.pending_input ? inputLabel(agent) : STATUS[agent.status]}
                </span>
              </span>
              <span className="text-ui text-dialog-hint">
                {agent.iterations_used} / {agent.iteration_budget} iterations
              </span>
            </div>
            <p className="whitespace-pre-wrap break-words">{agent.task}</p>
            <p className="break-words text-ui text-dialog-hint">
              {agent.provider && `${agent.provider} / `}{agent.model ?? 'Router default'}
              {agent.routing_locked && ' · Human model lock'}
            </p>
            {agent.usage?.cost_usd != null && (
              <p className="text-ui text-dialog-hint">${agent.usage.cost_usd.toFixed(4)}</p>
            )}
            <div className="flex flex-wrap gap-2">
              <Button variant="quiet" onClick={() => onOpen(agent.session_id)}>Inspect agent</Button>
              {['queued', 'running'].includes(agent.status) && (
                confirm === agent.session_id ? <>
                  <Button
                    variant="danger"
                    disabled={pending === agent.session_id}
                    onClick={() => { setConfirm(null); onCancel(agent.session_id); }}
                  >
                    Confirm stop
                  </Button>
                  <Button variant="quiet" onClick={() => setConfirm(null)}>Keep working</Button>
                </> : (
                  <Button
                    variant="quiet"
                    disabled={pending === agent.session_id}
                    onClick={() => setConfirm(agent.session_id)}
                  >
                    Stop agent
                  </Button>
                )
              )}
            </div>
            {confirm === agent.session_id && (
              <p className="text-ui text-dialog-hint">
                Stops this agent and its descendants, including queued work.
              </p>
            )}
          </li>
        ))}
      </ul>
    </div>
  );
}

/** Header entry and team inspector, with data supplied by the session controller. */
export function AgentTeamPanel({ open: controlledOpen, onOpenChange, ...props }: Parameters<typeof AgentTeamView>[0] & {
  open?: boolean;
  onOpenChange?: (open: boolean) => void;
}) {
  const [localOpen, setLocalOpen] = useState(false);
  const open = controlledOpen ?? localOpen;
  const setOpen = (next: boolean) => {
    setLocalOpen(next);
    onOpenChange?.(next);
  };
  const running = props.agents.filter((agent) => ['queued', 'running'].includes(agent.status)).length;
  const blocked = props.agents.filter((agent) => agent.pending_input).length;
  const label = `Agents: ${props.agents.length} total, ${running} active, ${blocked} need input`;
  return (
    <>
      <Button
        variant="quiet"
        density="panel"
        aria-label={label}
        aria-haspopup="dialog"
        onClick={() => setOpen(true)}
      >
        Agents {props.agents.length}
        <span className="hidden sm:inline">
          {blocked ? ` · ${blocked} need input` : running ? ` · ${running} active` : ''}
        </span>
      </Button>
      {open && (
        <Modal onDismiss={() => setOpen(false)}>
          <DialogFrame
            title="Agent team"
            subtitle={label}
            closeLabel="Close agent team"
            onClose={() => setOpen(false)}
          >
            <AgentTeamView
              {...props}
              onOpen={(child) => { setOpen(false); props.onOpen(child); }}
            />
          </DialogFrame>
        </Modal>
      )}
    </>
  );
}

// The strip says each agent's state as a SHAPE, with colour that agrees, like the
// live-view rail. Colour alone is not a state for every reader.
function stripMark(agent: Subagent): { Mark: typeof CircleDotIcon; ink: string; text: string } {
  if (agent.pending_input) {
    const open = agent.pending_input_count ?? 1;
    return { Mark: CircleAlertIcon, ink: 'text-warn', text: open > 1 ? `needs you ×${open}` : 'needs you' };
  }
  switch (agent.status) {
    case 'running':
      return { Mark: CircleDotIcon, ink: 'text-accent-ink', text: `${agent.iterations_used}/${agent.iteration_budget}` };
    case 'completed':
      return { Mark: CircleCheckIcon, ink: 'text-ok', text: 'done' };
    case 'failed':
      return { Mark: CircleXIcon, ink: 'text-err', text: 'failed' };
    case 'queued':
      return { Mark: CircleDashedIcon, ink: 'text-dialog-hint', text: 'queued' };
    case 'cancelled':
      return { Mark: CircleDashedIcon, ink: 'text-dialog-hint', text: 'stopped' };
    default:
      return { Mark: CircleAlertIcon, ink: 'text-dialog-hint', text: 'budget reached' };
  }
}

/**
 * The team at a glance, above the composer and outside Activity. It shows only
 * while an agent works or waits for a person; the header entry keeps the history.
 */
export function AgentTeamStrip({ agents, onShow }: {
  agents: readonly Subagent[];
  onShow: () => void;
}) {
  const live = agents.some((agent) => agent.pending_input || ['queued', 'running'].includes(agent.status));
  if (!live) return null;
  const running = agents.filter((agent) => ['queued', 'running'].includes(agent.status)).length;
  const blocked = agents.filter((agent) => agent.pending_input).length;
  return (
    <button
      type="button"
      aria-haspopup="dialog"
      aria-label={`Agent team: ${running} active, ${blocked} need input. Show team.`}
      onClick={onShow}
      className="mb-1.5 flex w-full min-w-0 items-center gap-3 overflow-hidden whitespace-nowrap rounded-none px-1 py-0.5 text-left text-ui text-white hover:underline focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent"
    >
      <span className="shrink-0 text-dialog-hint">Agents</span>
      {agents.map((agent, index) => {
        const { Mark, ink, text } = stripMark(agent);
        return (
          <span key={agent.session_id} title={agent.task} className="inline-flex shrink-0 items-center gap-1">
            <Mark className={`size-3 ${MARK_NUDGE} ${ink}`} />
            <span className="font-bold">A{index + 1}</span>
            <span className={agent.pending_input ? 'text-warn' : 'text-dialog-hint'}>{text}</span>
          </span>
        );
      })}
    </button>
  );
}

/** One poller for a session's team: the header entry and the strip read the same rows. */
export function useAgentTeam(client: GatewayClient, sid: string) {
  const [state, setState] = useState<{ sid: string; enabled: boolean; agents: Subagent[] }>({
    sid, enabled: false, agents: [],
  });
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [cancelError, setCancelError] = useState<string | null>(null);
  const [pending, setPending] = useState<string | null>(null);
  const [revision, setRevision] = useState(0);
  useEffect(() => {
    const controller = new AbortController();
    let fetching = false;
    const refresh = async () => {
      if (fetching || controller.signal.aborted || document.hidden) return;
      fetching = true;
      try {
        const setting = await client.setting('subagents', controller.signal, { scope: 'session', target_id: sid }).catch(() => null);
        if (controller.signal.aborted) return;
        if (setting?.enabled !== true) {
          setState({ sid, enabled: false, agents: [] });
          return;
        }
        const rows = await client.agents(sid, controller.signal);
        if (!Array.isArray(rows)) throw new Error('Gateway returned an invalid agent team');
        if (!controller.signal.aborted) {
          setState({ sid, enabled: true, agents: rows });
          setError(null);
        }
      } catch (cause) {
        if (!controller.signal.aborted) {
          setState((current) => (current.sid === sid ? { ...current, enabled: true } : { sid, enabled: true, agents: [] }));
          setError(cause instanceof Error ? cause.message : 'Could not load team. Refresh to try again.');
        }
      } finally {
        fetching = false;
        if (!controller.signal.aborted) setLoading(false);
      }
    };
    void refresh();
    const timer = window.setInterval(() => { void refresh(); }, 5000);
    document.addEventListener('visibilitychange', refresh);
    return () => {
      controller.abort();
      window.clearInterval(timer);
      document.removeEventListener('visibilitychange', refresh);
    };
  }, [client, sid, revision]);
  const cancel = async (child: string) => {
    setPending(child);
    setCancelError(null);
    try {
      await client.cancelAgent(sid, child);
      setRevision((value) => value + 1);
    } catch (cause) {
      setCancelError(cause instanceof Error ? cause.message : 'Could not stop agent. Try again.');
    } finally {
      setPending(null);
    }
  };
  const current = state.sid === sid ? state : { sid, enabled: false, agents: [] };
  return {
    enabled: current.enabled,
    agents: current.agents,
    loading,
    error: cancelError ?? error,
    pending,
    refresh: () => { setLoading(true); setRevision((value) => value + 1); },
    cancel: (child: string) => { void cancel(child); },
  };
}

export function AgentTeam({ client, sid, parentId, onOpen }: {
  client: GatewayClient;
  sid: string;
  parentId?: string;
  onOpen: (sid: string) => void;
}) {
  const team = useAgentTeam(client, sid);
  if (!team.enabled) return null;
  return (
    <AgentTeamPanel
      agents={team.agents}
      loading={team.loading}
      error={team.error}
      pending={team.pending}
      parentId={parentId}
      onRefresh={team.refresh}
      onCancel={team.cancel}
      onOpen={onOpen}
    />
  );
}
