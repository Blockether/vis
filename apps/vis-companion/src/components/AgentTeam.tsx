import { type CSSProperties, useCallback, useEffect, useState } from 'react';
import agentsSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/agents.json';
import type { GatewayClient } from '../lib/gateway';
import type { AgentTimeline, AgentTimelineItem, Subagent } from '../lib/types';
import { Button, DialogFrame, Modal } from './ui';

/** The roles of the agents contract, in its order. A role is a label: it sets no limit. */
export const ROLES: readonly { const: string; title: string }[] = agentsSchema.$defs.role.oneOf;

// The word of an agent without a role, in its row and in the team counts.
const NO_ROLE = 'agent';

// The ink of each contract role. A role that the Companion does not know reads like no role.
const ROLE_INK: ReadonlyMap<string, string> = new Map([
  ['research', 'text-role-research'],
  ['implement', 'text-role-implement'],
  ['verify', 'text-role-verify'],
  ['audit', 'text-role-audit'],
]);

/** The role word of an agent, with its ink. The views write the word in capitals. */
export function agentRole(agent: Subagent): { text: string; ink: string } {
  const role = agent.role?.trim() || NO_ROLE;
  return { text: role, ink: ROLE_INK.get(role) ?? 'text-dialog-hint' };
}

export const isLive = (agent: Subagent) => ['queued', 'running'].includes(agent.status);

/** The agents of one team in spawn order: by `created_at`, then in the order of the gateway. */
export function spawnOrder(agents: readonly Subagent[]): Subagent[] {
  return agents
    .map((agent, index) => ({ agent, index }))
    .sort((a, b) => (a.agent.created_at ?? 0) - (b.agent.created_at ?? 0) || a.index - b.index)
    .map(({ agent }) => agent);
}

/** The name of an agent for people: its title, else `#N`, its place in the spawn order of its team. */
export function agentLabel(agent: Subagent, agents: readonly Subagent[]): string {
  if (agent.title?.trim()) return agent.title.trim();
  const team = spawnOrder(agents.filter((other) => other.team_id === agent.team_id));
  return `#${team.findIndex((other) => other.session_id === agent.session_id) + 1}`;
}

/**
 * The status word of an agent, with the theme ink that agrees with it: `queued`, `running`,
 * `done` or `failed`. The views write the word in capitals. A failed verdict, a cancel and a
 * spent budget are all `failed`; the update of the row tells which. A question to its leader
 * is no status: the leader answers it, not a person.
 */
export function agentStatus(agent: Subagent): { text: string; ink: string } {
  switch (agent.status) {
    case 'queued':
      return { text: 'queued', ink: 'text-dialog-hint' };
    case 'running':
      return { text: 'running', ink: 'text-accent-ink' };
    case 'completed':
      if (agent.outcome === 'failed') return { text: 'failed', ink: 'text-err-ink' };
      return { text: 'done', ink: 'text-ok-ink' };
    default:
      return { text: 'failed', ink: 'text-err-ink' };
  }
}

// The end that `failed` hides, for an agent without a result.
const END_REASON: Partial<Record<Subagent['status'], string>> = {
  cancelled: 'cancelled',
  budget_limited: 'out of budget',
};

// The start of the current task: the spawn, or the last resume.
function startOf(agent: Subagent): number | undefined {
  const marks = [agent.created_at, agent.resumed_at].filter((mark): mark is number => typeof mark === 'number');
  return marks.length ? Math.max(...marks) : undefined;
}

/**
 * The time of the current task in ms: the time so far while the agent runs, the
 * duration when its task ended. A queued agent has no time yet.
 */
export function agentTime(agent: Subagent, now: number): number | undefined {
  const start = startOf(agent);
  if (start === undefined || agent.status === 'queued') return undefined;
  if (agent.status === 'running') return Math.max(0, now - start);
  return agent.finished_at == null ? undefined : Math.max(0, agent.finished_at - start);
}

/** `45s`, `3m 04s`, `1h 02m`. */
export function formatAgentTime(ms: number): string {
  const seconds = Math.floor(ms / 1000);
  if (seconds < 60) return `${seconds}s`;
  const minutes = Math.floor(seconds / 60);
  if (minutes < 60) return `${minutes}m ${String(seconds % 60).padStart(2, '0')}s`;
  return `${Math.floor(minutes / 60)}h ${String(minutes % 60).padStart(2, '0')}m`;
}

/**
 * The update of a row. While the agent works: its newest progress note, the note that its
 * leader also reads, else its latest Activity line, else the first line of its task. After
 * its task: its result, else the reason of its end.
 */
export function agentUpdate(agent: Subagent): string | undefined {
  if (!isLive(agent)) return agent.result ?? END_REASON[agent.status];
  return agent.note ?? agent.activity ?? (agent.task.trim().split('\n', 1)[0].trim() || undefined);
}

/** One count of a team line, such as `2 research` or `1 running`, with its ink. */
export interface TeamCount {
  text: string;
  count: number;
  ink: string;
}

// The order of the status counts: the work first, then the ends.
const STATUS_ORDER = ['running', 'queued', 'done', 'failed'];

function counted(words: readonly { text: string; ink: string }[], order: readonly string[]): TeamCount[] {
  const counts = new Map<string, TeamCount>();
  for (const word of words) counts.set(word.text, { ...word, count: (counts.get(word.text)?.count ?? 0) + 1 });
  const rank = (text: string) => (order.includes(text) ? order.indexOf(text) : order.length);
  return [...counts.values()].sort((a, b) => rank(a.text) - rank(b.text));
}

/** The agents counted by role, in the order of the contract, and by status word. */
export function teamCounts(agents: readonly Subagent[]): { roles: TeamCount[]; statuses: TeamCount[] } {
  return {
    roles: counted(agents.map(agentRole), [...ROLES.map((role) => role.const), NO_ROLE]),
    statuses: counted(agents.map(agentStatus), STATUS_ORDER),
  };
}

function Counts({ counts, className = '' }: { counts: readonly TeamCount[]; className?: string }) {
  return (
    <span className={`flex flex-wrap gap-x-3 ${className}`}>
      {counts.map(({ text, count, ink }) => (
        <span key={text} className={`whitespace-nowrap uppercase tabular-nums ${ink}`}>{`${count} ${text}`}</span>
      ))}
    </span>
  );
}

/** The counts of a team on one line: `TEAM  2 RESEARCH  1 VERIFY  1 RUNNING  2 DONE`. */
export function TeamLine({ agents }: { agents: readonly Subagent[] }) {
  const { roles, statuses } = teamCounts(agents);
  return (
    <span className="flex min-w-0 flex-1 flex-wrap items-baseline gap-x-4" data-team-line>
      <span className="font-bold uppercase text-white">team</span>
      <Counts counts={roles} />
      <Counts counts={statuses} className="sm:ml-auto" />
    </span>
  );
}

// The clock of the rows. It ticks once a second only while an agent runs.
function useNow(ticking: boolean): number {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    if (!ticking) return undefined;
    setNow(Date.now());
    const timer = window.setInterval(() => setNow(Date.now()), 1000);
    return () => window.clearInterval(timer);
  }, [ticking]);
  return now;
}

// The columns of the team table: ROLE, TITLE, UPDATE, STATUS and TIME, so the status stands
// right before the time. ROLE, TITLE and STATUS fit the longest word of the team, and TIME fits
// `59m 59s`, so a ticking time never moves a column. TITLE takes at most a third of the row; a
// longer title wraps. On a phone, UPDATE and TIME are the second line.
const COLUMNS = 'grid-cols-[var(--role)_minmax(0,1fr)_auto] sm:grid-cols-[var(--role)_min(var(--title),32%)_minmax(0,1fr)_var(--status)_7ch]';
const ROW = `w-full min-w-0 items-baseline gap-x-3 py-1 text-left ${COLUMNS}`;
const ROW_BUTTON = `group grid rounded-none focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent ${ROW}`;

// The widths of the columns that fit the team, in `ch` of the monospace font. A heading is the
// least width of its column; the bold title gets one more `ch`.
function columnWidths(rows: readonly Subagent[]): CSSProperties {
  const widest = (least: string, words: string[]) => Math.max(least.length, ...words.map((word) => word.length));
  return {
    '--role': `${widest('role', rows.map((agent) => agentRole(agent).text))}ch`,
    '--title': `${widest('title', rows.map((agent) => agentLabel(agent, rows))) + 1}ch`,
    '--status': `${widest('status', rows.map((agent) => agentStatus(agent).text))}ch`,
  } as CSSProperties;
}

// The table of one team, with the column headings on a wide screen.
function AgentTable({ rows, now, error, onSelect }: {
  rows: readonly Subagent[];
  now: number;
  error?: string | null;
  onSelect: (sid: string) => void;
}) {
  return (
    <div style={columnWidths(rows)}>
      <div aria-hidden className={`hidden border-b border-dialog-edge text-dialog-hint uppercase sm:grid ${ROW}`}>
        <span>role</span>
        <span>title</span>
        <span>update</span>
        <span>status</span>
        <span className="text-right">time</span>
      </div>
      <ul className="divide-y divide-dialog-edge/40">
        {rows.map((agent) => {
          const role = agentRole(agent);
          const status = agentStatus(agent);
          const update = agentUpdate(agent);
          const time = agentTime(agent, now);
          return (
            <li key={agent.session_id}>
              <button type="button" data-agent-row={agent.session_id} onClick={() => onSelect(agent.session_id)} className={ROW_BUTTON}>
                <span className={`order-1 uppercase sm:order-none ${role.ink}`}>{role.text}</span>
                <span className="order-2 font-bold [overflow-wrap:anywhere] group-hover:underline sm:order-none" title={agent.task}>
                  {agentLabel(agent, rows)}
                </span>
                <span
                  className={`order-4 col-span-2 line-clamp-2 text-dialog-hint [overflow-wrap:anywhere] sm:order-none sm:col-span-1 ${update ? '' : 'max-sm:hidden'}`}
                  title={update}
                >
                  {update ?? ''}
                </span>
                <span className={`order-3 whitespace-nowrap text-right uppercase sm:order-none sm:text-left ${status.ink}`}>{status.text}</span>
                <span className="order-5 col-start-3 whitespace-nowrap text-right text-dialog-hint tabular-nums sm:order-none sm:col-start-auto">
                  {time === undefined ? '' : formatAgentTime(time)}
                </span>
              </button>
            </li>
          );
        })}
        {error && <li role="alert" className="py-1 text-err-ink">{error}</li>}
      </ul>
    </div>
  );
}

/** The props of a team block. `timeline` reads the steps of one child. */
export interface TeamProps {
  agents: readonly Subagent[];
  pending: string | null;
  error?: string | null;
  onOpen: (sid: string) => void;
  onCancel: (sid: string) => void;
  timeline: (child: string) => Promise<AgentTimeline>;
}

/**
 * The subagents of one turn as a table in a frame: the team line with the counts by role and
 * by status, then one row for each agent in spawn order. A row opens the timeline of its
 * agent. The dock shows the table without the frame. A subagent never asks a person: its
 * leader answers it, so a row has no question and no control for one.
 */
export function TurnAgents({ agents, pending, error, onOpen, onCancel, timeline, placement = 'transcript' }: TeamProps & {
  /** The dock above the composer while the team works, or the turn of the transcript after. */
  placement?: 'dock' | 'transcript';
}) {
  const [selected, setSelected] = useState<string | null>(null);
  const now = useNow(agents.some((agent) => agent.status === 'running'));
  if (agents.length === 0) return null;
  const rows = spawnOrder(agents);
  const chosen = rows.find((agent) => agent.session_id === selected);
  const table = <AgentTable rows={rows} now={now} error={error} onSelect={setSelected} />;
  return (
    <>
      {placement === 'transcript' ? (
        <section
          aria-label="Subagents"
          className="my-3 border border-dialog-edge font-mono text-meta text-white sm:text-ui"
          data-transcript-part
        >
          <div className="flex border-b border-dialog-edge bg-panel px-3 py-1">
            <TeamLine agents={rows} />
          </div>
          <div className="px-3">{table}</div>
        </section>
      ) : table}
      {chosen && (
        <AgentTimelineSheet
          agent={chosen}
          label={agentLabel(chosen, rows)}
          now={now}
          pending={pending}
          load={timeline}
          onOpen={(child) => { setSelected(null); onOpen(child); }}
          onCancel={onCancel}
          onClose={() => setSelected(null)}
        />
      )}
    </>
  );
}

const STEP_WORDS: Record<AgentTimelineItem['kind'], string> = {
  started: 'Started',
  continued: 'Continued',
  note: 'Note',
  message: 'Message',
  finished: 'Finished',
};

const pad = (value: number) => String(value).padStart(2, '0');

/** The local clock time of an epoch ms, `14:03:07`. */
export function clockTime(at: number): string {
  const date = new Date(at);
  return `${pad(date.getHours())}:${pad(date.getMinutes())}:${pad(date.getSeconds())}`;
}

/**
 * The steps of one agent with their times, read from the timeline route, and the
 * way into its session. A working agent can also be stopped here.
 */
export function AgentTimelineSheet({ agent, label, now, pending, load, onOpen, onCancel, onClose }: {
  agent: Subagent;
  label: string;
  now: number;
  pending: string | null;
  load: (child: string) => Promise<AgentTimeline>;
  onOpen: (sid: string) => void;
  onCancel: (sid: string) => void;
  onClose: () => void;
}) {
  const [steps, setSteps] = useState<{ items: AgentTimelineItem[] } | { error: string } | null>(null);
  const [confirm, setConfirm] = useState(false);
  const child = agent.session_id;
  // Read again when the agent moves on, not on a clock.
  const progress = `${agent.status}:${agent.iterations_used}:${agent.finished_at ?? ''}:${agent.resumed_at ?? ''}`;
  useEffect(() => {
    let current = true;
    load(child).then(
      (timeline) => { if (current) setSteps({ items: Array.isArray(timeline?.items) ? timeline.items : [] }); },
      (cause: unknown) => {
        if (current) setSteps({ error: cause instanceof Error && cause.message ? cause.message : 'Could not read the timeline. Try again.' });
      },
    );
    return () => { current = false; };
  }, [load, child, progress]);
  const role = agentRole(agent);
  const status = agentStatus(agent);
  const time = agentTime(agent, now);
  return (
    <Modal size="fit" within="session" onDismiss={onClose}>
      <DialogFrame title={label} onClose={onClose}>
        <div className="space-y-3 px-4 py-3 font-mono text-meta text-white sm:text-ui" data-agent-timeline>
          <p className="flex flex-wrap gap-x-3">
            <span className={`uppercase ${role.ink}`}>{role.text}</span>
            <span className={`uppercase ${status.ink}`}>{status.text}</span>
            {time !== undefined && <span className="text-dialog-hint tabular-nums">{formatAgentTime(time)}</span>}
          </p>
          <p className="whitespace-pre-wrap break-words text-dialog-hint">{agent.task}</p>
          {steps === null ? (
            <p className="text-dialog-hint">Reading the timeline…</p>
          ) : 'error' in steps ? (
            <p role="alert" className="text-err-ink">{steps.error}</p>
          ) : steps.items.length === 0 ? (
            <p className="text-dialog-hint">No steps yet.</p>
          ) : (
            <ol aria-label="Timeline" className="space-y-1">
              {steps.items.map((item, index) => {
                const end = item.kind === 'finished' && item.status
                  ? agentStatus({ ...agent, status: item.status, outcome: item.outcome })
                  : null;
                const text = item.text || (item.kind === 'finished' && item.status ? END_REASON[item.status] : undefined);
                return (
                  <li key={index} className="grid grid-cols-[8ch_9ch_minmax(0,1fr)] gap-x-3">
                    <time dateTime={new Date(item.at).toISOString()} className="text-dialog-hint tabular-nums">{clockTime(item.at)}</time>
                    <span className="text-dialog-hint">{STEP_WORDS[item.kind] ?? item.kind}</span>
                    <span className="whitespace-pre-wrap break-words">
                      {end && <span className={`uppercase ${end.ink}`}>{end.text}</span>}
                      {end && text && ' · '}
                      {text}
                    </span>
                  </li>
                );
              })}
            </ol>
          )}
          <div className="flex flex-wrap items-center gap-2 pt-1">
            <Button density="inline" onClick={() => onOpen(child)}>Open session</Button>
            {isLive(agent) && (confirm ? <>
              <Button density="inline" variant="danger" disabled={pending === child} onClick={() => { setConfirm(false); onCancel(child); }}>
                Confirm stop
              </Button>
              <Button density="inline" variant="secondary" onClick={() => setConfirm(false)}>Keep working</Button>
            </> : (
              <Button density="inline" variant="secondary" disabled={pending === child} onClick={() => setConfirm(true)}>
                Stop agent
              </Button>
            ))}
          </div>
          {confirm && <p className="text-dialog-hint">Stops this agent and its descendants, including queued work.</p>}
        </div>
      </DialogFrame>
    </Modal>
  );
}

/** The session's subagents grouped by the turn that started them (`team_id`). */
export function agentsByTurn(agents: readonly Subagent[]): ReadonlyMap<string, Subagent[]> {
  const byTurn = new Map<string, Subagent[]>();
  for (const agent of agents) byTurn.set(agent.team_id, [...(byTurn.get(agent.team_id) ?? []), agent]);
  return byTurn;
}

/**
 * The teams of `sid` in the dock: the team of the running turn, and each team that
 * still has a queued or running agent after its turn ended. Spawn order.
 */
export function dockTeams(agents: readonly Subagent[], sid: string, runningTurnId?: string | null): Subagent[][] {
  const teams = new Map<string, Subagent[]>();
  for (const agent of agents) {
    if (agent.parent_id === sid) teams.set(agent.team_id, [...(teams.get(agent.team_id) ?? []), agent]);
  }
  return [...teams.entries()]
    .filter(([id, team]) => id === runningTurnId || team.some(isLive))
    .map(([, team]) => team);
}

/**
 * One reader for a session's subagents. It polls while a turn runs or an agent
 * works or waits, and reads again when `generation` changes.
 */
export function useAgentTeam(client: GatewayClient, sid: string, running: boolean, generation = 0) {
  const [state, setState] = useState<{ sid: string; agents: Subagent[] }>({ sid, agents: [] });
  const [pending, setPending] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [revision, setRevision] = useState(0);
  const current = state.sid === sid ? state.agents : [];
  const live = running || current.some(isLive);
  useEffect(() => {
    const controller = new AbortController();
    let fetching = false;
    const refresh = async () => {
      if (fetching || controller.signal.aborted || document.hidden) return;
      fetching = true;
      try {
        const rows = await client.agents(sid, controller.signal);
        if (!controller.signal.aborted && Array.isArray(rows)) setState({ sid, agents: rows });
      } catch {
        // The rows that the reader has stay; the next read tries again.
      } finally {
        fetching = false;
      }
    };
    void refresh();
    const timer = live ? window.setInterval(() => { void refresh(); }, 3000) : undefined;
    document.addEventListener('visibilitychange', refresh);
    return () => {
      controller.abort();
      if (timer !== undefined) window.clearInterval(timer);
      document.removeEventListener('visibilitychange', refresh);
    };
  }, [client, sid, revision, generation, live]);
  // One control call at a time for each child. It reads the team again after the call.
  // The result is the error text, or null when the call worked.
  const control = useCallback(async (child: string, call: () => Promise<unknown>, fallback: string) => {
    setPending(child);
    try {
      await call();
      return null;
    } catch (cause) {
      return cause instanceof Error && cause.message ? cause.message : fallback;
    } finally {
      setPending(null);
      setRevision((value) => value + 1);
    }
  }, []);
  const cancel = useCallback(async (child: string) => {
    setError(null);
    setError(await control(child, () => client.cancelAgent(sid, child), 'Could not stop agent. Try again.'));
  }, [client, sid, control]);
  const timeline = useCallback(
    (child: string) => client.agentTimeline(sid, child),
    [client, sid],
  );
  return { agents: current, pending, error, cancel, timeline };
}

/** What `useAgentTeam` gives its views. */
export type AgentTeam = ReturnType<typeof useAgentTeam>;
