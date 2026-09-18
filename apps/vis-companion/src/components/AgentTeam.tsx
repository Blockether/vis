import { useEffect, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { Subagent } from '../lib/types';
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
        {agents.map((agent) => (
          <li key={agent.session_id} className="space-y-2 py-3">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <span className="text-ui text-dialog-hint">
                {agent.pending_input ? inputLabel(agent) : STATUS[agent.status]}
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
            <p className="text-ui text-dialog-hint">
              Depth {agent.depth}
              {agent.usage?.cost_usd != null && ` · $${agent.usage.cost_usd.toFixed(4)}`}
            </p>
            {agent.depth > 1 && (
              <p className="break-words text-ui text-dialog-hint">
                Parent task: {agents.find((parent) => parent.session_id === agent.parent_id)?.task ?? agent.parent_id}
              </p>
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
export function AgentTeamPanel(props: Parameters<typeof AgentTeamView>[0]) {
  const [open, setOpen] = useState(false);
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

export function AgentTeam({ client, sid, parentId, onOpen }: {
  client: GatewayClient;
  sid: string;
  parentId?: string;
  onOpen: (sid: string) => void;
}) {
  const [enabled, setEnabled] = useState(false);
  const [agents, setAgents] = useState<Subagent[]>([]);
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
        const setting = await client.setting('subagents', controller.signal).catch(() => null);
        if (controller.signal.aborted) return;
        setEnabled(setting?.enabled === true);
        if (setting?.enabled !== true) return;
        const rows = await client.agents(sid, controller.signal);
        if (!Array.isArray(rows)) throw new Error('Gateway returned an invalid agent team');
        if (!controller.signal.aborted) {
          setAgents(rows);
          setError(null);
        }
      } catch (cause) {
        if (!controller.signal.aborted) {
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
  if (!enabled) return null;
  return (
    <AgentTeamPanel
      agents={agents}
      loading={loading}
      error={cancelError ?? error}
      pending={pending}
      parentId={parentId}
      onRefresh={() => { setLoading(true); setRevision((value) => value + 1); }}
      onCancel={(child) => { void cancel(child); }}
      onOpen={onOpen}
    />
  );
}
