import type { Subagent } from '../lib/types';
import { agentLabel, agentStatus } from './AgentTeam';
import { ChevronIcon, MARK_NUDGE } from './icons';

/**
 * The way back from a subagent session to the session that started it:
 * `‹ Leader  research  Map the routes  running`. The team rows of a child session
 * include the child itself, so an untitled child gets its `#N` from its team.
 */
export function AgentCrumb({ sid, parentId, agents, onOpen }: {
  sid: string;
  parentId: string;
  agents: readonly Subagent[];
  onOpen: (sid: string) => void;
}) {
  const self = agents.find((agent) => agent.session_id === sid);
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
      {self && status && <>
        {self.role && <span className="shrink-0">{self.role}</span>}
        <span className="min-w-0 truncate font-bold text-white" title={self.task}>{agentLabel(self, agents)}</span>
        <span className={`shrink-0 ${status.ink}`}>{status.text}</span>
      </>}
    </nav>
  );
}
