/**
 * WHERE DOES ONE MACHINE END AND THE NEXT ONE BEGIN?
 *
 * The shipped list stacks machine → project → session with ONE hairline between
 * every level, so the rule that closes `macbook` is the same rule that separates
 * two of its own projects: the eye reads a flat run of rows and the second
 * gateway looks like a third project of the first one.
 *
 * A machine is the biggest boundary on this screen — it is a different computer,
 * a different filesystem, a different `New session` target. These proposals give
 * that boundary its own weight, and each one has to survive:
 *
 *   - `solo`    one machine paired: the whole treatment must DISAPPEAR, down to
 *               the last pixel of gutter — the fleet costs a solo user nothing.
 *   - `offline` a machine that is not answering is still a machine: its block
 *               keeps its own boundary while saying it is degraded.
 *
 * The light palette is nearly flat (`--bg`, `--surface`, `--panel2` and
 * `--box-bg` are all `#faf3eb`), so a proposal that separates by FILL alone is
 * invisible on paper. Separation here is rules, space and ink.
 */

import type { ReactNode } from 'react';

import { MachineBanner, MachineGap, MachineMark, MachineRail } from '../components/ui';
import {
  MACHINE_COLORS,
  assignMachineColors,
  machineColor,
  type MachineColor,
} from '../lib/machine-colors';
import {
  MACHINES,
  byProject,
  manyMachines,
  projectRoot,
  sessionsOf,
  type FleetMachine,
  type FleetSession,
} from './fleet';

const STATUS_TONE: Record<FleetSession['status'], string> = {
  LIVE: 'text-ok',
  IDLE: 'text-dialog-hint',
  WAITING: 'text-accent-ink',
};

const STATUS_DOT: Record<FleetSession['status'], string> = {
  LIVE: 'bg-ok',
  IDLE: 'bg-dialog-hint',
  WAITING: 'bg-accent-ink',
};

function machinesFor(state: string): FleetMachine[] {
  if (state === 'solo') return MACHINES.slice(0, 1);
  if (state === 'offline') return MACHINES;
  if (state === 'many') return manyMachines(8);
  return MACHINES.filter((machine) => machine.state === 'online');
}

/** Every machine on screen, on its own hue — the map the rails are drawn from. */
function fleetColors(state: string): Map<string, MachineColor> {
  return assignMachineColors(machinesFor(state).map((machine) => machine.id));
}

function colorOf(colors: Map<string, MachineColor>, machine: FleetMachine): MachineColor {
  return machineColor(colors, machine.id);
}

/** One session, at the shipped row's weight and rhythm. */
function SessionRow({ session }: { session: FleetSession }) {
  return (
    <div className="flex min-h-14 items-start gap-2 px-3 py-2.5 [&+&]:border-t [&+&]:border-dialog-edge sm:min-h-12 sm:py-2 sm:pl-4 sm:pr-4">
      <span className="mt-0.5 shrink-0 font-mono text-body text-accent-ink opacity-40" aria-hidden="true">
        ›
      </span>
      <span className="min-w-0 flex-1">
        <span className="flex min-w-0 items-start justify-between gap-3">
          <span className="block min-w-0 truncate font-mono text-ui font-semibold text-white">
            {session.title}
          </span>
          <span className="flex shrink-0 items-center gap-1.5">
            {session.unread > 0 && (
              <span className="inline-flex items-center bg-accent px-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-accent-foreground">
                {session.unread > 1 ? `${session.unread} new` : 'new'}
              </span>
            )}
            <span
              className={`inline-flex items-center gap-1 font-mono text-chip font-bold tracking-[0.08em] ${STATUS_TONE[session.status]}`}
            >
              <span className={`size-1.5 ${STATUS_DOT[session.status]}`} />
              {session.status}
            </span>
          </span>
        </span>
        <span className="mt-1 flex items-center gap-x-2 font-mono text-chip text-dialog-hint">
          <span className="text-white/55">{session.id.slice(5)}</span>
          <span className="opacity-40" aria-hidden="true">
            ·
          </span>
          <span>{session.turns} turns</span>
          <span className="ml-auto shrink-0 pl-2">{session.ago}</span>
        </span>
      </span>
    </div>
  );
}

/** One project group, expanded, exactly as the shipped screen paints it. */
function ProjectSection({ project, sessions }: { project: string; sessions: FleetSession[] }) {
  const live = sessions.filter((session) => session.status === 'LIVE').length;
  return (
    <section className="border-t border-dialog-edge first:border-t-0">
      <header className="flex min-h-11 items-center justify-between gap-3 bg-panel-2 px-3 py-2 sm:px-4">
        <span className="flex min-w-0 items-center gap-2">
          <span className="shrink-0 font-mono text-ui text-dialog-hint" aria-hidden="true">
            ▾
          </span>
          <span className="min-w-0">
            <span className="block truncate font-mono text-ui font-bold text-white">{project}</span>
            <span className="mt-0.5 block truncate font-mono text-chip text-dialog-hint">
              {projectRoot(sessions)}
            </span>
          </span>
        </span>
        <span className="flex shrink-0 items-center gap-2 font-mono text-chip text-dialog-hint">
          <span>{sessions.length} sessions</span>
          {live > 0 && (
            <>
              <span className="opacity-40" aria-hidden="true">
                ·
              </span>
              <span className="inline-flex items-center gap-1 font-bold text-ok">
                <span className="size-1.5 bg-ok" />
                {live} live
              </span>
            </>
          )}
        </span>
      </header>
      <div className="border-t border-dialog-edge">
        {sessions.map((session) => (
          <SessionRow key={session.id} session={session} />
        ))}
      </div>
    </section>
  );
}

/** The projects of one machine, or its degraded line when it is not answering. */
/** The projects of one machine, or its degraded line when it is not answering. */
function MachineBody({ machine }: { machine: FleetMachine }) {
  if (machine.state !== 'online') {
    return (
      <p className="px-3 py-3 font-mono text-meta text-dialog-hint sm:px-4">
        This machine is not answering.
      </p>
    );
  }
  return (
    <>
      {byProject(sessionsOf(machine.sourceId ?? machine.id)).map(([project, sessions]) => (
        <ProjectSection key={project} project={project} sessions={sessions} />
      ))}
    </>
  );
}

/** Name, health dot and project count — the payload every header carries. */
/**
 * Name, mark and project count — the payload every header carries. With a `color`
 * the health dot becomes the machine's own hue: health is already a WORD here
 * (`offline`, plus a Retry and a line in the body), while the hue is the only
 * thing tying this banner to the rail below it and to the chip above it.
 */
function MachineFacts({ machine, color }: { machine: FleetMachine; color?: MachineColor }) {
  const projects = byProject(sessionsOf(machine.sourceId ?? machine.id)).length;
  return (
    <>
      <span className="flex min-w-0 items-center gap-2">
        {color ? (
          <MachineMark color={color} />
        ) : (
          <span
            className={`size-1.5 shrink-0 ${machine.state === 'online' ? 'bg-ok' : 'bg-dialog-hint'}`}
            aria-hidden="true"
          />
        )}
        <span className="truncate font-mono text-ui font-bold text-white">{machine.label}</span>
      </span>
      <span className="flex shrink-0 items-center gap-2 font-mono text-chip text-dialog-hint">
        {machine.state === 'online' ? (
          <span>
            {projects} {projects === 1 ? 'project' : 'projects'}
          </span>
        ) : (
          <>
            <span>offline</span>
            <span className="border border-edge px-1.5 py-0.5">Retry</span>
          </>
        )}
      </span>
    </>
  );
}

/** How ONE proposal wraps a single machine's block. `index` is its position. */
type MachineBlock = (args: { machine: FleetMachine; index: number; solo: boolean }) => ReactNode;

function Proposal({ state, block }: { state: string; block: MachineBlock }) {
  const machines = machinesFor(state);
  const solo = machines.length === 1;
  return (
    <section
      aria-label="Sessions"
      className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col sm:px-6 sm:pb-6 sm:pt-6"
    >
      <div className="flex h-full min-h-0 flex-col overflow-hidden border-b border-dialog-edge bg-panel sm:border">
        <div className="bg-panel-2 px-3 py-2.5 sm:px-4 sm:py-3">
          <div className="flex items-center justify-between gap-3">
            <div className="min-w-0">
              <p className="truncate font-mono text-body font-bold text-white">
                {solo ? 'Projects' : 'Fleet'}
              </p>
              <p className="mt-0.5 flex flex-wrap items-center gap-x-3 font-mono text-meta text-dialog-hint">
                {!solo && <span className="whitespace-nowrap">{machines.length} machines</span>}
                <span className="whitespace-nowrap">4 projects · 780 sessions</span>
              </p>
            </div>
            <div className="flex shrink-0 items-stretch">
              <span className="inline-flex min-h-6 items-center border border-accent bg-accent px-2 py-0.5 font-mono text-chip font-bold text-accent-foreground">
                New session
              </span>
              <span className="inline-flex min-h-6 items-center border border-l-accent-foreground/30 border-accent bg-accent px-2 py-0.5 font-mono text-chip font-bold text-accent-foreground">
                ▾
              </span>
            </div>
          </div>
        </div>
        {!solo && (
          <div className="flex items-center gap-1.5 overflow-x-auto border-t border-dialog-edge bg-panel px-3 py-2 sm:px-4">
            <span className="inline-flex min-h-6 shrink-0 items-center gap-1.5 border border-accent bg-hover px-2 font-mono text-meta font-bold text-white">
              All
            </span>
            {machines.map((machine) => (
              <span
                key={machine.id}
                className="inline-flex min-h-6 shrink-0 items-center gap-1.5 border border-edge px-2 font-mono text-meta text-dialog-hint"
              >
                {machine.label}
              </span>
            ))}
          </div>
        )}
        <div className="flex min-h-10 items-center gap-2 border-y border-dialog-edge bg-panel px-3 sm:min-h-9 sm:px-4">
          <span className="shrink-0 font-mono text-ui text-accent-ink">›</span>
          <span className="font-mono text-meta text-dialog-hint">Filter title, project, session</span>
        </div>
        <div className="min-h-0 flex-1 overflow-y-auto">
          {machines.map((machine, index) => (
            <div key={machine.id}>{block({ machine, index, solo })}</div>
          ))}
        </div>
      </div>
    </section>
  );
}

/**
 * 0 — SHIPPED. The machine header is one more hairline row: the same 1px rule
 * closes a project and a whole computer, so `tower` reads as a third project of
 * `studio-mbp`. This variant exists to be compared against, not chosen.
 */
export function MachineShippedVariant({ state }: { state: string }) {
  return (
    <Proposal
      state={state}
      block={({ machine, solo }) => (
        <section>
          {!solo && (
            <header className="flex items-center justify-between gap-3 border-b border-dialog-edge bg-panel px-3 py-1.5 sm:px-4">
              <MachineFacts machine={machine} />
            </header>
          )}
          <MachineBody machine={machine} />
        </section>
      )}
    />
  );
}

/**
 * A — GUTTER. Machines become blocks with air between them: a 12px band in the
 * page's own hover tone, closed top and bottom by the strong rule, so the eye
 * reads "this run of projects ended" before it reads any label. Costs vertical
 * space no phone can spare twice, and with one machine paired there is no band
 * at all.
 */
export function MachineGutterVariant({ state }: { state: string }) {
  return (
    <Proposal
      state={state}
      block={({ machine, index, solo }) => (
        <section>
          {!solo && index > 0 && (
            <div className="h-3 border-y border-edge-strong bg-hover" aria-hidden="true" />
          )}
          {!solo && (
            <header className="flex items-center justify-between gap-3 border-b border-dialog-edge bg-panel px-3 py-2 sm:px-4">
              <MachineFacts machine={machine} />
            </header>
          )}
          <MachineBody machine={machine} />
        </section>
      )}
    />
  );
}

/**
 * B — BANNER. The machine header stops being a row and becomes a slab: inked
 * background, uppercase name, and it STICKS to the top of the scroller, so the
 * answer to "which computer am I looking at" survives scrolling instead of
 * living only at the boundary. No extra vertical space is spent.
 */
export function MachineBannerVariant({ state }: { state: string }) {
  return (
    <Proposal
      state={state}
      block={({ machine, solo }) => (
        <section>
          {!solo && (
            <header className="sticky top-0 z-10 flex items-center justify-between gap-3 border-y border-edge-strong bg-hover px-3 py-1.5 font-bold uppercase tracking-[0.12em] sm:px-4">
              <MachineFacts machine={machine} />
            </header>
          )}
          <MachineBody machine={machine} />
        </section>
      )}
    />
  );
}

/**
 * C — RAIL. The machine owns a 2px rail down the left of everything it holds,
 * outdented header included, so the fleet reads as a tree without a band or a
 * slab: the line ENDS, and that is the boundary. Costs 12px of row width on a
 * phone, which is the price of the tree.
 */
/**
 * C — RAIL, IN THE MACHINE'S OWN COLOUR. Every paired gateway draws one hue out of
 * the sixteen in `src/lib/machine-colors.ts` and keeps it: a 2px rail down
 * everything it owns, banner included, and the same block in the banner and in the
 * scope chip. A boundary between two machines is then a COLOUR CHANGE — visible
 * before a word is read, and still visible scrolled to a random row deep inside a
 * machine, which a one-off band between blocks cannot be. Costs 12px of row width
 * on a phone, and nothing at all with one machine paired.
 */
export function MachineRailVariant({ state }: { state: string }) {
  const colors = fleetColors(state);
  return (
    <Proposal
      state={state}
      block={({ machine, solo }) =>
        solo ? (
          <section>
            <MachineBody machine={machine} />
          </section>
        ) : (
          <section>
            <MachineRail color={colorOf(colors, machine)}>
              <MachineBanner>
                <MachineFacts machine={machine} color={colorOf(colors, machine)} />
              </MachineBanner>
              <MachineBody machine={machine} />
            </MachineRail>
          </section>
        )
      }
    />
  );
}

/**
 * C2 — RAIL + BAND. The same coloured rail, with the page-coloured band kept
 * between blocks: the colour answers "whose rows are these", the air answers "this
 * machine ENDED". The question the shots have to settle is whether the band still
 * earns its 12px once the rails are different colours.
 */
export function MachineRailBandVariant({ state }: { state: string }) {
  const colors = fleetColors(state);
  return (
    <Proposal
      state={state}
      block={({ machine, index, solo }) =>
        solo ? (
          <section>
            <MachineBody machine={machine} />
          </section>
        ) : (
          <section>
            {index > 0 && <MachineGap />}
            <MachineRail color={colorOf(colors, machine)}>
              <MachineBanner>
                <MachineFacts machine={machine} color={colorOf(colors, machine)} />
              </MachineBanner>
              <MachineBody machine={machine} />
            </MachineRail>
          </section>
        )
      }
    />
  );
}

/**
 * The palette itself, so the sixteen hues can be judged as a set instead of two at
 * a time: one lightness, no green (green is LIVE), and every swatch has to hold on
 * BOTH the light page and the dark one — flip the theme and nothing may drop out.
 */
export function MachinePaletteVariant() {
  return (
    <section className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col sm:px-6 sm:py-6">
      <div className="flex min-h-0 flex-col overflow-hidden border-b border-dialog-edge bg-panel sm:border">
        <header className="border-b border-edge-strong bg-panel px-3 py-2 sm:px-4">
          <p className="font-mono text-ui font-bold uppercase tracking-[0.12em] text-white">
            Machine hues
          </p>
          <p className="mt-0.5 font-mono text-meta text-dialog-hint">
            16 identities · one lightness · no green
          </p>
        </header>
        <div className="grid min-h-0 flex-1 grid-cols-2 overflow-y-auto sm:grid-cols-4">
          {MACHINE_COLORS.map((color) => (
            <div
              key={color.name}
              className={`flex min-h-16 items-center gap-2 border-b border-r border-dialog-edge border-l-2 px-3 py-3 ${color.rail}`}
            >
              <MachineMark color={color} />
              <span className="truncate font-mono text-ui font-bold text-white">{color.name}</span>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}

/**
 * D — BLOCK. The two answers at once, because they answer different questions:
 * a page-coloured band closed by the strong rule says a machine ENDED, and the
 * name that follows it is set as a banner — uppercase, tracked, sticky — so it
 * cannot be misread as a third project of the machine above. The band is the
 * only cost, and it is charged once per EXTRA machine: the first block starts
 * flush and a solo fleet never pays it at all.
 */
export function MachineBlockVariant({ state }: { state: string }) {
  return (
    <Proposal
      state={state}
      block={({ machine, index, solo }) => (
        <section>
          {!solo && index > 0 && <MachineGap />}
          {!solo && (
            <MachineBanner>
              <MachineFacts machine={machine} />
            </MachineBanner>
          )}
          <MachineBody machine={machine} />
        </section>
      )}
    />
  );
}
