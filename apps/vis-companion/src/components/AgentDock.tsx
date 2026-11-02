import { useId, useState } from 'react';
import type { Subagent } from '../lib/types';
import { TeamLine, TimelineToggle, TurnAgents, dockTeams, hasTimeline, useWide, type TeamProps } from './AgentTeam';
import { ChevronIcon, MARK_NUDGE } from './icons';

/**
 * The subagents of the running turn, above the composer, folded to one line: the time of
 * the team and its counts by status. The line opens the table of the team. On a wide screen,
 * Open timeline after the team word opens the table with the time bars of its agents. A team stays here while
 * one of its agents still works after its turn ended; then it stands in that turn of the
 * transcript.
 */
export function AgentDock({ agents, sid, runningTurnId, ...team }: Omit<TeamProps, 'agents'> & {
  agents: readonly Subagent[];
  sid: string;
  runningTurnId?: string | null;
}) {
  const [open, setOpen] = useState(false);
  const [isTimeline, setTimeline] = useState(false);
  const body = useId();
  const line = useId();
  const isWide = useWide();
  const teams = dockTeams(agents, sid, runningTurnId);
  if (teams.length === 0) return null;
  const toggle = isWide && teams.some(hasTimeline) ? (
    <TimelineToggle
      isOn={open && isTimeline}
      onToggle={() => {
        const isOn = !(open && isTimeline);
        setTimeline(isOn);
        if (isOn) setOpen(true);
      }}
    />
  ) : undefined;
  return (
    <section
      aria-label="Subagents of this turn"
      className="mb-1.5 border border-dialog-edge bg-panel font-mono text-meta text-white sm:text-ui"
      data-agent-dock
    >
      {/* The fold control lies under the whole line, so the timeline control can stand inside it. */}
      <div className="relative flex min-w-0 items-baseline gap-2 px-3 py-1 sm:py-1.5">
        <button
          type="button"
          aria-expanded={open}
          aria-controls={body}
          aria-labelledby={`${line}-team ${line}-time ${line}-counts`}
          onClick={() => setOpen((value) => !value)}
          className="absolute inset-0 rounded-none focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent"
        />
        <span className="pointer-events-none relative flex min-w-0 flex-1 items-baseline gap-2">
          <ChevronIcon open={open} className={`size-3 shrink-0 text-dialog-hint ${MARK_NUDGE}`} />
          <TeamLine agents={teams.flat()} action={toggle} labelId={line} />
        </span>
      </div>
      <div id={body} hidden={!open} className="max-h-[40dvh] space-y-2 overflow-y-auto border-t border-dialog-edge px-3">
        {teams.map((members) => (
          <TurnAgents key={members[0].team_id} agents={members} placement="dock" isTimeline={isTimeline} {...team} />
        ))}
      </div>
    </section>
  );
}
