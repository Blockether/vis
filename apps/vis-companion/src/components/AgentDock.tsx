import type { Subagent } from '../lib/types';
import { TurnAgents, dockTeams, type TeamProps } from './AgentTeam';

/**
 * The subagents of the running turn, above the composer: one row each, statuses
 * only. A team stays here while one of its agents still works or waits after its
 * turn ended; then its rows stand in that turn of the transcript.
 */
export function AgentDock({ agents, sid, runningTurnId, ...team }: Omit<TeamProps, 'agents'> & {
  agents: readonly Subagent[];
  sid: string;
  runningTurnId?: string | null;
}) {
  const teams = dockTeams(agents, sid, runningTurnId);
  if (teams.length === 0) return null;
  return (
    <section
      aria-label="Subagents of this turn"
      className="mb-1.5 max-h-[40dvh] space-y-1 overflow-y-auto border border-dialog-edge bg-panel px-3 py-1"
      data-agent-dock
    >
      {teams.map((members) => (
        <TurnAgents key={members[0].team_id} agents={members} placement="dock" {...team} />
      ))}
    </section>
  );
}
