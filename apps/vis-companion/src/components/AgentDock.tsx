import { useId, useState } from 'react';
import { useCommands } from '../lib/commands';
import type { Subagent, TeamClock } from '../lib/types';
import { TeamLine, TimelineToggle, TurnAgents, currentTurn, dockTeams, hasTimeline, longestClock, type TeamProps } from './AgentTeam';
import { ChevronIcon, MARK_NUDGE } from './icons';

/**
 * The subagents of the running turn, above the composer, folded to one line: the time of
 * the team, from its stored clock (`clocks`, by leader turn). With several docked teams, the line
 * shows the longest time. The line opens the table of the team.
 * Timeline at the end of the line opens the table with the time bars of its agents. A team stays here while
 * one of its agents still works after its turn ended; then it stands in that turn of the
 * transcript.
 */
export function AgentDock({ agents, sid, runningTurnId, leader, clocks, ...team }: Omit<TeamProps, 'agents' | 'clock'> & {
  agents: readonly Subagent[];
  sid: string;
  runningTurnId?: string | null;
  clocks?: ReadonlyMap<string, TeamClock>;
}) {
  const [open, setOpen] = useState(false);
  const [isTimeline, setTimeline] = useState(false);
  const body = useId();
  const line = useId();
  const teams = dockTeams(agents, sid, runningTurnId);
  // The timeline control and C-x b l: a folded dock opens on the timeline.
  const switchTimeline = () => {
    const isOn = !(open && isTimeline);
    setTimeline(isOn);
    if (isOn) setOpen(true);
  };
  // C-x b a folds the dock; C-x t t opens it.
  useCommands({
    'toggle-agent-dock': teams.length > 0 && (() => setOpen((value) => !value)),
    'toggle-agent-timeline': teams.length > 0 && switchTimeline,
    'agent-team': teams.length > 0 && (() => setOpen(true)),
  });
  if (teams.length === 0) return null;
  // One table for all docked teams, so each child has one row and one key. The leader of the
  // running turn heads it only when that turn started a team.
  const members = teams.flat();
  // Chosen at each paint: the dock paints again when a clock changes.
  const clock = longestClock(teams.map((team) => clocks?.get(currentTurn(team[0]))), Date.now());
  const head = teams.some((team) => currentTurn(team[0]) === runningTurnId) ? leader : undefined;
  const toggle = hasTimeline(members, head) ? (
    <TimelineToggle isOn={open && isTimeline} onToggle={switchTimeline} />
  ) : undefined;
  return (
    <section
      aria-label="Subagents of this turn"
      className="mb-1.5 border border-dialog-edge bg-panel font-mono text-meta text-white sm:text-ui"
      data-agent-dock
    >
      {/* The fold control lies under the whole line, so the timeline control can stand inside it. */}
      <div className="relative flex min-w-0 items-baseline gap-2 px-3 py-1">
        <button
          type="button"
          aria-expanded={open}
          aria-controls={body}
          aria-labelledby={`${line}-team ${line}-time`}
          onClick={() => setOpen((value) => !value)}
          className="absolute inset-0 rounded-none focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent"
        />
        <span className="pointer-events-none relative flex min-w-0 flex-1 items-baseline gap-2">
          <ChevronIcon open={open} className={`size-3 shrink-0 text-dialog-hint ${MARK_NUDGE}`} />
          <TeamLine agents={members} clock={clock} action={toggle} labelId={line} />
        </span>
      </div>
      <div id={body} hidden={!open} className="max-h-[40dvh] space-y-2 overflow-y-auto border-t border-dialog-edge px-3 sm:px-0">
        <TurnAgents agents={members} placement="dock" isTimeline={isTimeline} leader={head} {...team} />
      </div>
    </section>
  );
}
