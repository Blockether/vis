import type { Subagent } from '../lib/types';
import { agentMark } from './AgentTeam';
import { ChevronIcon, MARK_NUDGE, RoleIcon } from './icons';

/**
 * The way back from a subagent session to the session that started it:
 * `‹ Leader › A2 task`. The team rows of a child session include the child itself,
 * so its A-number comes from its place in its team.
 */
export function AgentCrumb({ sid, parentId, agents, onOpen }: {
  sid: string;
  parentId: string;
  agents: readonly Subagent[];
  onOpen: (sid: string) => void;
}) {
  const self = agents.find((agent) => agent.session_id === sid);
  const team = self ? agents.filter((agent) => agent.team_id === self.team_id) : [];
  const mark = self && agentMark(self);
  return (
    <nav aria-label="Agent" className="flex min-w-0 items-center gap-1.5 border-b border-edge bg-panel px-3 py-1 font-mono text-meta text-dialog-hint" data-agent-crumb>
      <button
        type="button"
        onClick={() => onOpen(parentId)}
        className="flex shrink-0 items-center gap-1 rounded-none font-bold text-white hover:underline focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent"
      >
        <ChevronIcon back className={`size-3 ${MARK_NUDGE}`} />
        Leader
      </button>
      {self && mark && <>
        <ChevronIcon className={`size-3 shrink-0 ${MARK_NUDGE}`} />
        <mark.Mark className={`size-3 shrink-0 ${MARK_NUDGE} ${mark.ink}`} />
        {self.role && <RoleIcon role={self.role} className={`size-3 shrink-0 ${MARK_NUDGE}`} />}
        <span className="shrink-0 font-bold text-white">A{team.indexOf(self) + 1}</span>
        <span className="min-w-0 flex-1 truncate" title={self.task}>{self.task}</span>
      </>}
    </nav>
  );
}
