/**
 * "NEW SESSION" COSTS THREE ANSWERS. FIVE WAYS TO MAKE IT COST ONE.
 *
 * What ships today (`SessionsScreen` + `fleet.StartFlow`): tap New session,
 * answer WHICH MACHINE, answer WHICH WORKSPACE, and for a fork type a name in a
 * second dialog and press Create — three taps and a keyboard round trip before
 * a composer exists to say what the work actually is. Every one of those
 * answers is nearly always the same as last time.
 *
 * Each proposal attacks that waste from a different side: GUESS the answers and
 * let them be corrected (A, D), REMEMBER them (B), let the place the tap
 * happened answer them (C), or fold the whole order into a control already on
 * screen (E). Variant 0 photographs the cost so the others are judged against
 * it and not against a memory.
 *
 * Two states falsify these designs, so every proposal that can be shot in them
 * is: `solo` (one machine paired — the machine question must not exist at all,
 * not even as a pre-filled chip) and `offline` (the remembered destination is
 * on a gateway that is not answering).
 *
 * DEV-ONLY: reachable at `#/__design`; nothing shipped imports this file.
 */
import type { ReactNode } from 'react';
import { MACHINES, machineById, sessionsOf, type FleetMachine, type FleetSession } from './fleet';

/* ------------------------------------------------------------------ fixtures */

/** One ready-made destination: the whole order (machine, project, workspace). */
export interface StartTarget {
  id: string;
  project: string;
  root: string;
  machineId: string;
  /** What the session opens in. */
  workspace: 'the project itself' | 'a new draft' | 'a parked draft';
  /** Name of the parked draft, when that is what this destination resumes. */
  draft?: string;
  ago: string;
}

/**
 * The destinations a returning user actually has: the same repo on two machines,
 * a draft parked in it, and one target on the gateway that is down.
 */
export const RECENT_TARGETS: StartTarget[] = [
  {
    id: 'r-vis-project',
    project: 'vis',
    root: '~/vis',
    machineId: MACHINES[0].id,
    workspace: 'the project itself',
    ago: '7m ago',
  },
  {
    id: 'r-vis-draft',
    project: 'vis',
    root: '~/vis',
    machineId: MACHINES[0].id,
    workspace: 'a parked draft',
    draft: 'wire-rework',
    ago: '2h ago',
  },
  {
    id: 'r-infra',
    project: 'infrastructure',
    root: '~/infrastructure',
    machineId: MACHINES[1].id,
    workspace: 'the project itself',
    ago: '12m ago',
  },
  {
    id: 'r-vis-fresh-draft',
    project: 'vis',
    root: '~/vis',
    machineId: MACHINES[0].id,
    workspace: 'a new draft',
    ago: 'yesterday',
  },
  {
    id: 'r-fff',
    project: 'fff',
    root: '~/fff',
    machineId: MACHINES[2].id,
    workspace: 'the project itself',
    ago: '2d ago',
  },
];

/** The states each proposal is photographed in; the gallery registers these. */
export const START_STATES: Record<string, string[]> = {
  'start-shipped': ['default'],
  'start-ask': ['default', 'target', 'solo'],
  'start-recents': ['default', 'offline'],
  'start-row': ['default', 'draft'],
  'start-sheet': ['default', 'draft'],
  'start-command': ['default'],
};

/** The first message, already typed — the only answer that was ever the point. */
const PROMPT = 'make the transient band repaint the frame edge on every row it covers';

/* -------------------------------------------------------------------- atoms */

const BAND = 'px-3 py-2 font-mono text-chip uppercase tracking-[0.08em]';
/** The shipped question band: filled Blockether yellow closed by the warn rule. */
const ASK_BAND = 'border-b-2 border-warn-strong bg-accent font-bold text-accent-foreground';
const QUIET_BAND = 'border-b border-dialog-edge bg-panel-2 text-dialog-hint';
const FRAME =
  'border-t border-dialog-edge bg-panel sm:border sm:shadow-[8px_8px_0_var(--dialog-shadow)]';
const ROW = 'flex min-h-11 items-center gap-2 border-b border-dialog-edge px-3 py-2 text-left';
const PRIMARY =
  'shrink-0 border border-accent bg-accent px-2 py-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-accent-foreground';

function Caret() {
  return (
    <span
      aria-hidden="true"
      className="ml-0.5 inline-block h-4 w-2 animate-pulse bg-accent align-middle motion-reduce:animate-none"
    />
  );
}

function MachineName({ machine, className = '' }: { machine: FleetMachine; className?: string }) {
  return (
    <span className={`inline-flex min-w-0 items-center gap-1.5 ${className}`}>
      <span
        aria-hidden="true"
        className={`size-1.5 shrink-0 ${machine.state === 'online' ? 'bg-ok' : 'bg-muted'}`}
      />
      <span className="truncate">{machine.label}</span>
    </span>
  );
}

function Chip({
  children,
  selected = false,
  muted = false,
}: {
  children: ReactNode;
  selected?: boolean;
  muted?: boolean;
}) {
  const tone = selected
    ? 'border-accent bg-accent font-bold text-accent-foreground'
    : muted
      ? 'border-edge text-dialog-hint opacity-55'
      : 'border-edge text-white';
  return (
    <span className={`inline-flex min-h-7 items-center gap-1.5 border px-2 py-0.5 font-mono text-chip ${tone}`}>
      {children}
    </span>
  );
}

/** One tappable answer, at the shipped `StartOption`'s weight. */
function Option({
  title,
  hint,
  badge,
  tone = 'plain',
}: {
  title: ReactNode;
  hint?: ReactNode;
  badge?: string;
  tone?: 'plain' | 'muted' | 'accent';
}) {
  return (
    <div className={`${ROW} ${tone === 'muted' ? 'opacity-55' : ''}`}>
      <span className="min-w-0 flex-1">
        <span
          className={`block truncate font-mono text-ui font-bold ${tone === 'accent' ? 'text-accent-ink' : 'text-white'}`}
        >
          {title}
        </span>
        {hint && (
          <span className="mt-0.5 block truncate font-mono text-meta text-dialog-hint">{hint}</span>
        )}
      </span>
      {badge && (
        <span className="shrink-0 border border-edge px-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          {badge}
        </span>
      )}
    </div>
  );
}

/** Label + wrapping chips: a whole question answered on one 44px row. */
function PickRow({ label, children }: { label: string; children: ReactNode }) {
  return (
    <div className="flex items-start gap-2 border-b border-dialog-edge px-3 py-2">
      <span className="w-14 shrink-0 pt-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {label}
      </span>
      <span className="flex min-w-0 flex-1 flex-wrap gap-1.5">{children}</span>
    </div>
  );
}

function Stage({ children }: { children: ReactNode }) {
  return (
    <section className="mx-auto flex w-full max-w-[900px] flex-col p-0 sm:p-6">{children}</section>
  );
}

/* ------------------------------------------------------- the list underneath */

function SplitButton() {
  return (
    <span className="flex shrink-0 items-stretch">
      <span className="inline-flex min-h-7 items-center border border-accent bg-accent px-2 font-mono text-chip font-bold text-accent-foreground">
        New session
      </span>
      <span className="inline-flex min-h-7 items-center border border-l-accent-foreground/30 border-accent bg-accent px-1.5 font-mono text-chip font-bold text-accent-foreground">
        ▾
      </span>
    </span>
  );
}

function PlusButton() {
  return (
    <span
      aria-label="New session here"
      className="inline-flex size-8 shrink-0 items-center justify-center border border-edge font-mono text-ui font-bold text-accent-ink"
    >
      +
    </span>
  );
}

function ListRow({ session }: { session: FleetSession }) {
  return (
    <div className="flex items-start gap-2 border-b border-dialog-edge px-3 py-2">
      <span className="font-mono text-ui text-accent-ink opacity-40" aria-hidden="true">
        ›
      </span>
      <span className="min-w-0 flex-1">
        <span className="block truncate font-mono text-ui font-semibold text-white">
          {session.title}
        </span>
        <span className="mt-0.5 block truncate font-mono text-chip text-dialog-hint">
          {session.id.slice(-6)} · {session.turns} turns · {session.ago}
        </span>
      </span>
      {session.status === 'LIVE' && (
        <span className="shrink-0 font-mono text-chip font-bold tracking-[0.08em] text-ok">
          LIVE
        </span>
      )}
    </div>
  );
}

function FilterRow() {
  return (
    <div className="flex min-h-10 items-center gap-2 border-b border-dialog-edge bg-panel px-3">
      <span className="shrink-0 font-mono text-ui text-accent-ink">›</span>
      <span className="font-mono text-meta text-dialog-hint">Filter title, project, session</span>
    </div>
  );
}

/**
 * The sessions list of the report's screenshot, compressed to what a proposal
 * has to survive being drawn on top of.
 */
function SessionsList({
  plus = false,
  filter,
  popover,
}: {
  plus?: boolean;
  filter?: ReactNode;
  popover?: ReactNode;
}) {
  const macbook = MACHINES[0];
  const rows = sessionsOf(macbook.id);
  return (
    <div className="flex h-full flex-col bg-panel">
      <div className="flex items-center justify-between gap-3 border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
        <span className="min-w-0">
          <span className="block font-mono text-body font-bold text-white">Fleet</span>
          <span className="mt-0.5 block font-mono text-chip text-dialog-hint">
            2 machines · 4 projects · 813 sessions
          </span>
        </span>
        <SplitButton />
      </div>
      {filter ?? <FilterRow />}
      <div className="flex items-center justify-between gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-1.5">
        <MachineName
          machine={macbook}
          className="font-mono text-chip font-bold uppercase tracking-[0.08em] text-white"
        />
        <span className="flex items-center gap-2">
          <span className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
            1 project
          </span>
          {plus && <PlusButton />}
        </span>
      </div>
      <div className="relative">
        <div className="flex items-center gap-2 border-b border-dialog-edge px-3 py-2">
          <span className="min-w-0 flex-1">
            <span className="block font-mono text-ui font-bold text-white">vis</span>
            <span className="mt-0.5 block font-mono text-chip text-dialog-hint">
              ~/vis · 560 sessions
            </span>
          </span>
          {plus && <PlusButton />}
        </div>
        {popover}
      </div>
      {rows.map((session) => (
        <ListRow key={session.id} session={session} />
      ))}
    </div>
  );
}

/** A proposal painted where it opens: over the list it was started from. */
function Backdrop({ children, dim = true }: { children: ReactNode; dim?: boolean }) {
  return (
    <div className="relative min-h-[40rem] flex-1 overflow-hidden bg-ink">
      <div className="absolute inset-0">
        <SessionsList />
      </div>
      {dim && <div className="absolute inset-0 bg-black/40" />}
      {children}
    </div>
  );
}

/** Phone: a bottom sheet under the thumb. Desktop: a popover under the caret. */
function Sheet({ children }: { children: ReactNode }) {
  return (
    <div
      className={`absolute inset-x-0 bottom-0 max-h-[85%] overflow-y-auto sm:inset-x-auto sm:bottom-auto sm:right-4 sm:top-4 sm:w-96 ${FRAME}`}
    >
      {children}
    </div>
  );
}

/* ------------------------------------------------------------------ variant 0 */

function StepCard({ n, caption, children }: { n: string; caption: string; children: ReactNode }) {
  return (
    <div className="flex min-w-0 flex-1 flex-col">
      <p className="flex items-baseline gap-2 pb-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        <span className="bg-accent px-1 font-bold text-accent-foreground">{n}</span>
        {caption}
      </p>
      <div className="border border-dialog-edge bg-panel">{children}</div>
    </div>
  );
}

/** A row of the baseline menus: one line, because the point is the COUNT. */
function DenseOption({ title, hint }: { title: string; hint?: string }) {
  return (
    <div className="flex min-h-9 items-center justify-between gap-2 border-b border-dialog-edge px-2 py-1 last:border-b-0">
      <span className="truncate font-mono text-meta font-bold text-white">{title}</span>
      {hint && <span className="shrink-0 font-mono text-chip text-dialog-hint">{hint}</span>}
    </div>
  );
}

/**
 * 0 — WHAT IT COSTS TODAY. Three questions, in three separate surfaces, none of
 * which is the message; the composer only exists after all of them are answered.
 */
export function StartShippedVariant() {
  return (
    <Stage>
      <div className="p-3 sm:p-0">
        <p className="font-mono text-ui font-bold text-white">
          Shipped: three answers before the first word
        </p>
        <p className="mt-0.5 font-mono text-meta text-dialog-hint">
          Tap · tap · type a name · Create — and only now does a composer open to say what the work
          is.
        </p>
        <div className="mt-3 flex flex-col gap-3 sm:flex-row sm:gap-4">
          <StepCard n="tap 1" caption="which machine">
            <p className={`${ASK_BAND} ${BAND}`}>Create the session on</p>
            {MACHINES.filter((machine) => machine.state === 'online').map((machine) => (
              <DenseOption key={machine.id} title={machine.label} hint="781 sessions" />
            ))}
          </StepCard>
          <StepCard n="tap 2" caption="which workspace">
            <p className={`${ASK_BAND} ${BAND}`}>Start the session in · studio-mbp</p>
            <DenseOption title="The project itself" hint="default" />
            <DenseOption title="A new draft, with changes" />
            <DenseOption title="A new draft, without" />
            <p className={`${QUIET_BAND} ${BAND} border-t`}>Or a draft you parked</p>
            <DenseOption title="wire-rework" hint="in use" />
          </StepCard>
          <StepCard n="tap 3" caption="only for a draft: name it">
            <p className="border-b border-dialog-edge bg-dialog-title px-2 py-1.5 font-mono text-meta font-bold text-dialog-title-foreground">
              Name the draft
            </p>
            <div className="space-y-2 p-2">
              <p className="font-mono text-chip text-dialog-hint">
                A private copy of this project exactly as it is now.
              </p>
              <p className="border border-edge bg-input px-2 py-1 font-mono text-meta text-white">
                band-repaint
                <Caret />
              </p>
              <p className="flex justify-end gap-2">
                <span className="border border-edge px-2 py-0.5 font-mono text-chip text-dialog-hint">
                  Cancel
                </span>
                <span className={PRIMARY}>Create</span>
              </p>
            </div>
          </StepCard>
        </div>
        <p className="mt-3 border border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-meta text-dialog-hint">
          …then the session opens, empty, and you finally type{' '}
          <span className="text-white">“{PROMPT}”</span>.
        </p>
      </div>
    </Stage>
  );
}

/* ------------------------------------------------------------------ variant A */

function TargetPicker({ solo }: { solo: boolean }) {
  return (
    <div className="border-t border-dialog-edge bg-panel">
      {!solo && (
        <PickRow label="on">
          <Chip selected>
            <MachineName machine={MACHINES[0]} />
          </Chip>
          <Chip>
            <MachineName machine={MACHINES[1]} />
          </Chip>
          <Chip muted>
            <MachineName machine={MACHINES[2]} /> not answering
          </Chip>
        </PickRow>
      )}
      <PickRow label="in">
        <Chip selected>~/vis</Chip>
        <Chip>~/infrastructure</Chip>
        <Chip>~/tree-sitter-clojure</Chip>
      </PickRow>
      <PickRow label="as">
        <Chip selected>the project</Chip>
        <Chip>a new draft</Chip>
        <Chip>wire-rework</Chip>
      </PickRow>
    </div>
  );
}

/**
 * A — ASK BOX. "New session" opens the COMPOSER, never a question: the machine,
 * the project and the workspace are already answered from last time and stated
 * on one line you can tap. Zero taps in the common case; the override is one.
 */
export function StartAskVariant({ state }: { state: string }) {
  const solo = state === 'solo';
  return (
    <Stage>
      <Backdrop>
        <Sheet>
          <p className={`${ASK_BAND} ${BAND} flex items-center justify-between gap-2`}>
            <span>New session</span>
            <span className="font-normal tracking-normal">esc</span>
          </p>
          <p className="min-h-24 px-3 py-3 font-mono text-body text-white">
            {PROMPT}
            <Caret />
          </p>
          <div className="flex items-center gap-2 border-t border-dialog-edge bg-panel-2 px-3 py-2">
            <span className="min-w-0 flex-1 truncate font-mono text-meta text-dialog-hint">
              {solo ? 'in ' : 'on '}
              {!solo && <span className="font-bold text-white">studio-mbp</span>}
              {!solo && ' · '}
              <span className="font-bold text-white">~/vis</span> · the project itself
            </span>
            <span className="shrink-0 border border-edge px-1.5 py-0.5 font-mono text-chip uppercase tracking-[0.08em] text-accent-ink">
              change ▾
            </span>
          </div>
          {state === 'target' && <TargetPicker solo={solo} />}
          <div className="flex items-center justify-between gap-2 border-t border-dialog-edge px-3 py-2">
            <span className="font-mono text-chip text-dialog-hint">⏎ starts · ⇧⏎ newline</span>
            <span className={PRIMARY}>Start</span>
          </div>
        </Sheet>
      </Backdrop>
    </Stage>
  );
}

/* ------------------------------------------------------------------ variant B */

function targetTitle(target: StartTarget): string {
  return target.draft ? `${target.project} · ${target.draft}` : `${target.project} · ${target.workspace}`;
}

/**
 * B — RECENT DESTINATIONS. The menu stops asking and starts OFFERING: one row is
 * a whole order (machine + project + workspace), ranked by when you last used it.
 * One tap creates. The old chooser survives as the last row, for a new place.
 */
export function StartRecentsVariant({ state }: { state: string }) {
  // The gateway in MACHINES[2] is down in every state; `offline` adds the machine
  // that owns most of the recents, which is the state that decides this design.
  const dead = new Set(
    state === 'offline' ? [MACHINES[0].id, MACHINES[2].id] : [MACHINES[2].id],
  );
  return (
    <Stage>
      <Backdrop>
        <Sheet>
          <p className={`${ASK_BAND} ${BAND}`}>Start a session</p>
          {RECENT_TARGETS.map((target) => {
            const machine = machineById(target.machineId);
            const isDead = dead.has(target.machineId);
            return (
              <Option
                key={target.id}
                title={targetTitle(target)}
                hint={`${machine.label} · ${target.root} · ${target.ago}`}
                badge={isDead ? 'not answering' : undefined}
                tone={isDead ? 'muted' : 'plain'}
              />
            );
          })}
          <Option
            title="Somewhere else…"
            hint="Pick a machine, a project and a workspace"
            tone="accent"
          />
        </Sheet>
      </Backdrop>
    </Stage>
  );
}

/* ------------------------------------------------------------------ variant C */

/**
 * C — THE LIST IS THE PICKER. No chooser at all: every machine header and every
 * project row carries a +, so WHERE you tapped answers machine and project. The
 * workspace keeps its default; the small menu appears only if you hold the +.
 */
export function StartRowVariant({ state }: { state: string }) {
  const popover =
    state === 'draft' ? (
      <div className="absolute right-2 top-12 z-10 w-72 max-w-[calc(100%-1rem)] border border-dialog-edge bg-panel shadow-[8px_8px_0_var(--dialog-shadow)]">
        <p className={`${ASK_BAND} ${BAND}`}>New session in ~/vis</p>
        <Option title="The project itself" badge="tap" />
        <Option title="A new draft" hint="a private copy of this project" />
        <Option title="wire-rework" hint="parked draft · forked 2h ago" />
      </div>
    ) : null;
  return (
    <Stage>
      <div className="relative min-h-[40rem] flex-1 bg-ink">
        <SessionsList plus popover={popover} />
      </div>
    </Stage>
  );
}

/* ------------------------------------------------------------------ variant D */

/**
 * D — ONE SHEET, NOTHING SEQUENTIAL. The same three questions, but all of them
 * already answered and visible at once, next to the message box. Nothing is a
 * step: correct what is wrong, or ignore it all and press Start.
 */
export function StartSheetVariant({ state }: { state: string }) {
  const draft = state === 'draft';
  return (
    <Stage>
      <Backdrop>
        <div className="absolute inset-x-0 bottom-0 sm:inset-0 sm:flex sm:items-center sm:justify-center sm:p-6">
          <div
            className={`max-h-full w-full overflow-y-auto sm:max-w-md ${FRAME}`}
          >
            <p className="border-b border-dialog-edge bg-dialog-title px-3 py-2 font-mono text-ui font-bold text-dialog-title-foreground">
              New session
            </p>
            <PickRow label="on">
              <Chip selected>
                <MachineName machine={MACHINES[0]} />
              </Chip>
              <Chip>
                <MachineName machine={MACHINES[1]} />
              </Chip>
              <Chip muted>
                <MachineName machine={MACHINES[2]} /> not answering
              </Chip>
            </PickRow>
            <PickRow label="in">
              <Chip selected>~/vis</Chip>
              <Chip>~/infrastructure</Chip>
              <Chip>more…</Chip>
            </PickRow>
            <PickRow label="as">
              <Chip selected={!draft}>the project</Chip>
              <Chip selected={draft}>a new draft</Chip>
              <Chip>wire-rework</Chip>
            </PickRow>
            {draft && (
              <PickRow label="named">
                <span className="min-w-0 flex-1 border border-edge bg-input px-2 py-1 font-mono text-meta text-white">
                  band-repaint
                  <Caret />
                </span>
              </PickRow>
            )}
            <p className="min-h-24 px-3 py-3 font-mono text-body text-white">
              {PROMPT}
              {!draft && <Caret />}
            </p>
            <div className="flex items-center justify-between gap-2 border-t border-dialog-edge px-3 py-2">
              <span className="font-mono text-chip text-dialog-hint">
                Already answered · ⏎ starts
              </span>
              <span className={PRIMARY}>Start</span>
            </div>
          </div>
        </div>
      </Backdrop>
    </Stage>
  );
}

/* ------------------------------------------------------------------ variant E */

/**
 * E — THE FILTER BOX STARTS SESSIONS. The control already at the top of the list
 * takes a sentence instead of a needle: what you typed becomes the first message
 * of a new session on the last destination, and Enter is the whole interaction.
 */
export function StartCommandVariant() {
  const filter = (
    <>
      <div className="flex min-h-10 items-center gap-2 border-b border-dialog-edge bg-panel px-3">
        <span className="shrink-0 font-mono text-ui text-accent-ink">›</span>
        <span className="min-w-0 truncate font-mono text-meta text-white">
          {PROMPT}
          <Caret />
        </span>
      </div>
      <div className="border-b-2 border-warn-strong bg-accent px-3 py-2">
        <span className="block font-mono text-meta font-bold text-accent-foreground">
          ⏎ Start a session on studio-mbp · ~/vis
        </span>
        <span className="mt-0.5 block font-mono text-chip text-accent-foreground/80">
          What you typed becomes its first message · ⌥⏎ start it somewhere else
        </span>
      </div>
      <div className="border-b border-dialog-edge bg-panel-2 px-3 py-1.5 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        3 matches in 813 sessions
      </div>
    </>
  );
  return (
    <Stage>
      <div className="min-h-[40rem] flex-1 overflow-hidden bg-ink">
        <SessionsList filter={filter} />
      </div>
    </Stage>
  );
}
