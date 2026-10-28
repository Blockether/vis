import { useId, useState } from 'react';
import type { Subagent } from '../lib/types';
import { TeamLine, TurnAgents, dockTeams, type TeamProps } from './AgentTeam';
import { ChevronIcon, MARK_NUDGE } from './icons';

/**
 * The subagents of the running turn, above the composer, folded to one line: the time of
 * the team and its counts by status. The line opens the table of the team. A team stays here while
 * one of its agents still works after its turn ended; then it stands in that turn of the
 * transcript.
 */
export function AgentDock({ agents, sid, runningTurnId, ...team }: Omit<TeamProps, 'agents'> & {
  agents: readonly Subagent[];
  sid: string;
  runningTurnId?: string | null;
}) {
  const [open, setOpen] = useState(false);
  const body = useId();
  const teams = dockTeams(agents, sid, runningTurnId);
  if (teams.length === 0) return null;
  return (
    <section
      aria-label="Subagents of this turn"
      className="mb-1.5 border border-dialog-edge bg-panel font-mono text-meta text-white sm:text-ui"
      data-agent-dock
    >
      <button
        type="button"
        aria-expanded={open}
        aria-controls={body}
        onClick={() => setOpen((value) => !value)}
        className="flex w-full min-w-0 items-baseline gap-2 rounded-none px-3 py-1 text-left focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent"
      >
        <ChevronIcon open={open} className={`size-3 shrink-0 text-dialog-hint ${MARK_NUDGE}`} />
        <TeamLine agents={teams.flat()} />
      </button>
      <div id={body} hidden={!open} className="max-h-[40dvh] space-y-2 overflow-y-auto border-t border-dialog-edge px-3">
        {teams.map((members) => (
          <TurnAgents key={members[0].team_id} agents={members} placement="dock" {...team} />
        ))}
      </div>
    </section>
  );
}
