import { useState } from 'react';
import type { Subagent } from '../lib/types';
import { agentMark, isAsking, isLive, TurnAgents } from './AgentTeam';
import { ChevronIcon, CircleAlertIcon, MARK_NUDGE, RoleIcon } from './icons';
import { DialogFrame, Modal } from './ui';

const CHIP = 'relative flex min-h-6 shrink-0 items-center gap-1 border px-1.5 rounded-none after:absolute after:inset-x-0 after:-inset-y-2 after:content-[""] hover:text-white focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent mouse:after:content-none';

// The teams of `sid` that still have an agent at work or waiting, in start order.
function activeTeams(agents: readonly Subagent[], sid: string): Subagent[][] {
  const teams = new Map<string, Subagent[]>();
  for (const agent of agents) {
    if (agent.parent_id === sid) teams.set(agent.team_id, [...(teams.get(agent.team_id) ?? []), agent]);
  }
  return [...teams.values()].filter((team) => team.some((agent) => isLive(agent) || isAsking(agent)));
}

/**
 * Chips above the composer while the agents of this session work or wait: one for
 * the open questions, then one for each agent. A tap opens the team sheet, a bottom
 * sheet on a phone and a box in the session pane on a desk.
 */
export function AgentDock({ agents, sid, pending, error, onOpen, onCancel }: {
  agents: readonly Subagent[];
  sid: string;
  pending: string | null;
  error?: string | null;
  onOpen: (sid: string) => void;
  onCancel: (sid: string) => void;
}) {
  const [open, setOpen] = useState(false);
  const teams = activeTeams(agents, sid);
  if (teams.length === 0) return null;
  const asking = teams.flat().filter(isAsking).length;
  return (
    <>
      <div
        role="group"
        aria-label="Agent team"
        className="mb-1.5 flex min-w-0 items-center gap-1.5 overflow-x-auto py-1 font-mono text-meta text-white"
        data-agent-dock
      >
        {asking > 0 && (
          <button type="button" onClick={() => setOpen(true)} className={`${CHIP} border-warn text-warn`}>
            <CircleAlertIcon className={`size-3 shrink-0 ${MARK_NUDGE}`} />
            {asking === 1 ? '1 question' : `${asking} questions`}
          </button>
        )}
        {teams.flatMap((team) => team.map((agent, index) => {
          const { Mark, ink, text } = agentMark(agent);
          return (
            <button
              key={agent.session_id}
              type="button"
              title={agent.task}
              onClick={() => setOpen(true)}
              className={`${CHIP} border-edge-strong ${isAsking(agent) ? 'text-warn' : 'text-dialog-hint'}`}
            >
              <Mark className={`size-3 shrink-0 ${MARK_NUDGE} ${ink}`} />
              {agent.role && <RoleIcon role={agent.role} className={`size-3 shrink-0 ${MARK_NUDGE}`} />}
              <span className="font-bold text-white">A{index + 1}</span>
              <span className="tabular-nums">{text}</span>
            </button>
          );
        }))}
        <button
          type="button"
          onClick={() => setOpen(true)}
          className="ml-auto flex shrink-0 items-center gap-1 rounded-none px-1 font-bold text-dialog-hint hover:text-white focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent"
        >
          Team
          <ChevronIcon className={`size-3 ${MARK_NUDGE}`} />
        </button>
      </div>
      {open && (
        <Modal size="fit" within="session" onDismiss={() => setOpen(false)}>
          <DialogFrame title="Team" onClose={() => setOpen(false)}>
            <div className="min-h-0 overflow-y-auto px-3">
              {teams.map((team) => (
                <TurnAgents
                  key={team[0].team_id}
                  agents={team}
                  pending={pending}
                  error={error}
                  onOpen={(child) => { setOpen(false); onOpen(child); }}
                  onCancel={onCancel}
                />
              ))}
            </div>
          </DialogFrame>
        </Modal>
      )}
    </>
  );
}
