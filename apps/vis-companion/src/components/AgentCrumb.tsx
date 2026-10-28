import type { Subagent } from '../lib/types';
import { agentLabel, agentRole, agentStatus } from './AgentTeam';
import { ChevronIcon, MARK_NUDGE } from './icons';

/**
 * The way back from a subagent session to the session that started it:
 * `‹ Leader  RESEARCH  Map the routes  RUNNING`. The team rows of a child session
 * include the child itself, so an untitled child gets its `#N` from its team.
 */
export function AgentCrumb({ sid, parentId, agents, onOpen }: {
  sid: string;
  parentId: string;
  agents: readonly Subagent[];
  onOpen: (sid: string) => void;
}) {
  const self = agents.find((agent) => agent.session_id === sid);
  const role = self && agentRole(self);
  const status = self && agentStatus(self);
  return (
    <nav aria-label="Agent" className="flex min-w-0 items-center gap-3 border-b border-edge bg-panel px-3 py-1 font-mono text-meta text-dialog-hint" data-agent-crumb>
      <button
        type="button"
        onClick={() => onOpen(parentId)}
        className="flex shrink-0 items-center gap-1 rounded-none font-bold text-white hover:underline focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent"
      >
        <ChevronIcon back className={`size-3 ${MARK_NUDGE}`} />
        Leader
      </button>
      {self && role && status && <>
        <span className={`shrink-0 uppercase ${role.ink}`}>{role.text}</span>
        <span className="min-w-0 font-bold text-white [overflow-wrap:anywhere]" title={self.task}>{agentLabel(self, agents)}</span>
        <span className={`shrink-0 uppercase ${status.ink}`}>{status.text}</span>
      </>}
    </nav>
  );
}
