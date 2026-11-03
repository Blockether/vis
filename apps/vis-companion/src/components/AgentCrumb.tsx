import type { Subagent } from '../lib/types';
import { LEADER_INK, LEADER_ROLE, agentLabel, agentRole } from './AgentTeam';

/** One step of an agent path: the role word with its ink, then the name. The leader has only its role word. */
export type AgentStep = Readonly<{ role?: Readonly<{ text: string; ink: string }>; label?: string }>;

/** The header path of a subagent session, `LEADER → IMPLEMENT Fix parser`, and the session it opens. */
export type AgentPath = Readonly<{ parentId: string; parent: AgentStep; self: AgentStep }>;

const LEADER: AgentStep = { role: { text: LEADER_ROLE, ink: LEADER_INK } };

const stepOf = (agent: Subagent, agents: readonly Subagent[]): AgentStep => ({
  role: agentRole(agent),
  label: agentLabel(agent, agents),
});

/**
 * The header path of subagent session `sid`, like the TUI breadcrumb. A parent that is a known
 * child shows its own role and name; the leader shows as `LEADER`. Before the team rows arrive,
 * the session title names the agent, so the way back is there at once. The team rows of a child
 * session include the child itself, so an untitled child gets its `#N` from its team.
 */
export function agentPath(
  sid: string,
  agent: Readonly<{ parent_id?: string; leader_id: string }>,
  agents: readonly Subagent[],
  title: string,
): AgentPath | undefined {
  const parentId = agent.parent_id;
  if (!parentId) return undefined;
  const self = agents.find((row) => row.session_id === sid);
  const parent = parentId === agent.leader_id ? undefined : agents.find((row) => row.session_id === parentId);
  return {
    parentId,
    parent: parent ? stepOf(parent, agents) : LEADER,
    self: self ? stepOf(self, agents) : { label: title },
  };
}
