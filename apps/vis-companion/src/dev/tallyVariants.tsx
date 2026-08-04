/**
 * WHERE DO THE FLEET'S TWO NUMBERS LIVE?
 *
 * The sessions list states "how many are running" and "how much is waiting" in
 * three places at once: the fleet header line, every scope chip, and every
 * machine section header. If a chip can carry both counts, the header line is
 * saying a second time what the strip already says one row below it.
 *
 * These proposals answer that, and each one has to survive the SOLO state:
 * with a single machine paired the strip does not exist at all, so a design
 * that moved the counts into the chips has moved them off the screen.
 */

import type { ReactNode } from 'react';
import { LiveTally, UnreadBadge } from '../components/ui';

interface TallyMachine {
  name: string;
  projects: number;
  live: number;
  unread: number;
  offline?: boolean;
}

/** Two answering machines and one that is not, with a two-digit unread so the
 *  chips are measured at their widest rather than at their prettiest. */
const FLEET: TallyMachine[] = [
  { name: 'macbook', projects: 2, live: 3, unread: 4 },
  { name: 'studio', projects: 3, live: 0, unread: 12 },
  { name: 'pi', projects: 1, live: 0, unread: 0, offline: true },
];

const SESSION_TOTAL = 773;

function machinesFor(state: string): TallyMachine[] {
  return state === 'solo' ? FLEET.slice(0, 1) : FLEET;
}

function fleetCounts(machines: TallyMachine[]) {
  return {
    live: machines.reduce((sum, machine) => sum + machine.live, 0),
    unread: machines.reduce((sum, machine) => sum + machine.unread, 0),
    projects: machines.reduce((sum, machine) => sum + machine.projects, 0),
  };
}

/** The counts a proposal paints inside one chip / one machine header. */
type Counts = (value: { live: number; unread: number }) => ReactNode;

function chipClass(isOn: boolean): string {
  return `inline-flex min-h-6 shrink-0 items-center gap-1.5 border px-2 font-mono text-meta transition-colors duration-150 ${
    isOn ? 'border-accent bg-hover font-bold text-white' : 'border-edge text-dialog-hint'
  }`;
}

/** Shipped today: green count in the host's own brackets, unread as the filled
 *  amber block the session row already uses. */
const mixedCounts: Counts = ({ live, unread }) => (
  <>
    {live > 0 && <LiveTally count={live} />}
    <UnreadBadge count={unread} />
  </>
);

/** Both counts as filled blocks — the literal "put live in a box too". */
const badgeCounts: Counts = ({ live, unread }) => (
  <>
    {live > 0 && (
      <span className="inline-flex items-center bg-ok px-1 font-mono text-chip font-bold text-ink">
        {live}
        <span className="sr-only"> live</span>
      </span>
    )}
    <UnreadBadge count={unread} />
  </>
);

/** Words, for a line that has room for them. */
function WordTallies({ live, unread }: { live: number; unread: number }) {
  return (
    <>
      <span className={`whitespace-nowrap ${live > 0 ? 'font-bold text-ok' : ''}`}>{live} live</span>
      {unread > 0 && <span className="whitespace-nowrap font-bold text-accent-ink">{unread} unread</span>}
    </>
  );
}

function NewSessionButton() {
  return (
    <div className="flex shrink-0 items-stretch">
      <span className="inline-flex min-h-6 items-center border border-accent bg-accent px-2 py-0.5 font-mono text-chip font-bold text-accent-foreground">
        New session
      </span>
      <span className="inline-flex min-h-6 items-center border border-l-0 border-accent bg-contrast px-2 py-0.5 font-mono text-chip text-contrast-foreground">
        ▾
      </span>
    </div>
  );
}

function HeaderBand({ machines, tallies }: { machines: TallyMachine[]; tallies: ReactNode }) {
  const { projects } = fleetCounts(machines);
  return (
    <div className="bg-panel-2 px-3 py-2.5 sm:px-4 sm:py-3">
      <div className="flex items-center justify-between gap-3">
        <div className="min-w-0">
          <p className="truncate font-mono text-body font-bold text-white">
            {machines.length > 1 ? 'Fleet' : 'Projects'}
          </p>
          <p className="mt-0.5 flex flex-wrap items-center gap-x-3 gap-y-0.5 font-mono text-meta text-dialog-hint">
            {machines.length > 1 && <span className="whitespace-nowrap">{machines.length} machines</span>}
            <span className="whitespace-nowrap">
              {projects} projects<span className="px-1 opacity-40">·</span>
              {SESSION_TOTAL} sessions
            </span>
            {tallies}
          </p>
        </div>
        <NewSessionButton />
      </div>
    </div>
  );
}

function Strip({ machines, counts }: { machines: TallyMachine[]; counts: Counts }) {
  if (machines.length < 2) return null;
  const fleet = fleetCounts(machines);
  return (
    <div className="flex items-center gap-1.5 overflow-x-auto border-t border-dialog-edge bg-panel px-3 py-2 sm:px-4">
      <span className={chipClass(true)}>
        All
        {counts(fleet)}
      </span>
      {machines.map((machine) => (
        <span key={machine.name} className={chipClass(false)}>
          {machine.name}
          {machine.offline ? <span className="opacity-70">offline</span> : counts(machine)}
        </span>
      ))}
    </div>
  );
}

function SearchRow() {
  return (
    <div className="flex min-h-10 items-center gap-2 border-y border-dialog-edge bg-panel px-3 sm:min-h-9 sm:px-4">
      <span className="shrink-0 font-mono text-ui text-accent-ink">›</span>
      <span className="font-mono text-meta text-dialog-hint">Filter title, project, session</span>
    </div>
  );
}

function MachineSection({
  machine,
  counts,
  withHeader,
}: {
  machine: TallyMachine;
  counts: Counts;
  withHeader: boolean;
}) {
  return (
    <section>
      {withHeader && (
        <header className="flex items-center justify-between gap-3 border-b border-dialog-edge bg-panel px-3 py-1.5 sm:px-4">
          <span className="flex min-w-0 items-center gap-2">
            <span className={`size-1.5 shrink-0 ${machine.offline ? 'bg-dialog-hint' : 'bg-ok'}`} aria-hidden="true" />
            <span className="truncate font-mono text-ui font-bold text-white">{machine.name}</span>
          </span>
          <span className="flex shrink-0 items-center gap-2 font-mono text-meta text-dialog-hint">
            <span>{machine.projects} projects</span>
            {counts(machine)}
          </span>
        </header>
      )}
      <div className="border-b border-dialog-edge px-3 py-2 sm:px-4">
        <p className="font-mono text-ui font-bold text-white">vis</p>
        <p className="font-mono text-meta text-dialog-hint">~/vis</p>
      </div>
      <div className="px-3 py-2 sm:px-4">
        <p className="font-mono text-ui text-white">Strict JSON Schema Compliance</p>
        <p className="font-mono text-meta text-dialog-hint">5ca90155 · 7 turns</p>
      </div>
    </section>
  );
}

function Proposal({
  state,
  headerTallies,
  chipCounts,
  sectionCounts,
}: {
  state: string;
  headerTallies: (fleet: { live: number; unread: number }, hasStrip: boolean) => ReactNode;
  chipCounts: Counts;
  sectionCounts: Counts;
}) {
  const machines = machinesFor(state);
  const fleet = fleetCounts(machines);
  const hasStrip = machines.length > 1;
  return (
    <section
      aria-label="Sessions"
      className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] sm:px-6 sm:pb-6 sm:pt-6"
    >
      <div className="flex h-full min-h-0 flex-col overflow-hidden border-b border-dialog-edge bg-panel sm:border">
        <HeaderBand machines={machines} tallies={headerTallies(fleet, hasStrip)} />
        <Strip machines={machines} counts={chipCounts} />
        <SearchRow />
        <div className="min-h-0 flex-1 overflow-y-auto border-t border-dialog-edge">
          {machines
            .filter((machine) => !machine.offline)
            .map((machine) => (
              <MachineSection key={machine.name} machine={machine} counts={sectionCounts} withHeader={hasStrip} />
            ))}
        </div>
      </div>
    </section>
  );
}

/**
 * A — SHIPPED. Every surface repeats itself: the header line says "3 live
 * 16 unread" in words, the strip says it again in a chip, and each machine
 * header says its own share a third time.
 */
export function TallyHeaderVariant({ state }: { state: string }) {
  return (
    <Proposal
      state={state}
      headerTallies={(fleet) => <WordTallies live={fleet.live} unread={fleet.unread} />}
      chipCounts={mixedCounts}
      sectionCounts={({ live, unread }) => <WordTallies live={live} unread={unread} />}
    />
  );
}

/**
 * B — TWO BADGES, NO WORDS. Live becomes a filled green block beside the amber
 * one and the header line drops both counts for good. Reads fast, but the two
 * blocks weigh the same, so "running" shouts as loudly as "someone is waiting
 * for you" — and solo, where there is no strip, the fleet stops counting at all.
 */
export function TallyBadgesVariant({ state }: { state: string }) {
  return (
    <Proposal
      state={state}
      headerTallies={() => null}
      chipCounts={badgeCounts}
      sectionCounts={badgeCounts}
    />
  );
}

/**
 * C — THE STRIP IS THE TALLY. The chip keeps the shipped pair (a light green
 * `[3]`, a solid amber badge) and becomes the ONLY place the numbers appear:
 * the header line and the machine headers go quiet while the strip is on
 * screen. With one machine paired there is no strip, so the header line takes
 * the counts back — the words appear exactly where nothing else can say them.
 */
export function TallyStripVariant({ state }: { state: string }) {
  return (
    <Proposal
      state={state}
      headerTallies={(fleet, hasStrip) =>
        hasStrip ? null : <WordTallies live={fleet.live} unread={fleet.unread} />
      }
      chipCounts={mixedCounts}
      sectionCounts={() => null}
    />
  );
}
