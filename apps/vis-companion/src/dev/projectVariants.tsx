/**
 * THE CHOSEN FLOW — machine ⋯, Switch project, drafts.
 *
 * The comparison board (A–L) did its job and was answered: **1C · 2F · 3H+pencil ·
 * 4K**. What is photographed here is no longer a menu of options but the decided
 * product, one screenshot per step of it, assembled into a single sheet
 * (`#/__design?v=session-ux-board`) so the flow is judged as a flow. The reasons
 * live in `apps/vis-companion/SESSION-UX.md`; this file is what they look like.
 *
 *   C  every machine verb lives in the machine's own ⋯ — nothing new on the list
 *   F  `New session in a draft…` is a second verb there, only when drafts are on
 *   H  Switch project is a breadcrumb browser of the machine's filesystem, and a
 *      pencil turns that same header into a path you type yourself
 *   K  `Offer drafts` is one app switch, this device, every machine
 *
 * Written phone-first on purpose: every card on the board is a 390px column, and
 * a `sm:` rule inside one would answer to the BOARD's width, not the card's.
 *
 * DEV-ONLY: reachable at `#/__design`; nothing shipped imports this file.
 */
import type { ReactNode } from 'react';
import {
  MACHINES,
  byProject,
  projectRoot,
  sessionsOf,
  type FleetMachine,
  type FleetSession,
} from './fleet';

/** The states each proposal is photographed in; the gallery registers these. */
export const PROJECT_STATES: Record<string, string[]> = {
  'session-flow': ['menu', 'menu-off', 'browse', 'typed', 'new-folder', 'settings', 'solo'],
  'session-ux-board': ['default'],
};

/* -------------------------------------------------------------------- atoms */

const BAND = 'px-3 py-1.5 font-mono text-chip uppercase tracking-[0.08em]';
const LOUD_BAND = 'border-b-2 border-warn-strong bg-accent font-bold text-accent-foreground';
const QUIET_BAND = 'border-b border-dialog-edge bg-panel-2 text-dialog-hint';
const ROW = 'flex min-h-11 items-center gap-2 border-b border-dialog-edge px-3 py-2 text-left';
const PRIMARY =
  'shrink-0 border border-accent bg-accent px-2 py-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-accent-foreground';
const GHOST =
  'shrink-0 border border-edge px-2 py-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-white';

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

/** A square icon affordance at the 32px the shipped kebab uses. */
function IconButton({
  label,
  tone = 'plain',
  children,
}: {
  label: string;
  tone?: 'plain' | 'accent';
  children: ReactNode;
}) {
  const skin =
    tone === 'accent' ? 'border-accent bg-accent text-accent-foreground' : 'border-edge text-white';
  return (
    <span
      aria-label={label}
      className={`inline-flex size-8 shrink-0 items-center justify-center border font-mono text-ui font-bold ${skin}`}
    >
      {children}
    </span>
  );
}

function Toggle({ on }: { on: boolean }) {
  return (
    <span
      aria-hidden="true"
      className={`inline-flex h-6 w-11 shrink-0 items-center border px-0.5 ${
        on ? 'justify-end border-accent bg-accent' : 'justify-start border-edge bg-panel-2'
      }`}
    >
      <span className={`size-4 ${on ? 'bg-accent-foreground' : 'bg-muted'}`} />
    </span>
  );
}

/** One tappable answer: title, one line of consequence, an optional badge. */
function Item({
  title,
  hint,
  badge,
  tone = 'plain',
}: {
  title: ReactNode;
  hint?: ReactNode;
  badge?: ReactNode;
  tone?: 'plain' | 'muted' | 'accent';
}) {
  return (
    <div className={`${ROW} ${tone === 'muted' ? 'opacity-55' : ''}`}>
      <span className="min-w-0 flex-1">
        <span
          className={`block truncate font-mono text-ui font-bold ${
            tone === 'accent' ? 'text-accent-ink' : 'text-white'
          }`}
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
/* ------------------------------------------------------------- the list */

function Screen({ children }: { children: ReactNode }) {
  return (
    <div className="relative flex h-full min-h-[42rem] flex-col overflow-hidden bg-panel">
      {children}
    </div>
  );
}

function FleetBar({
  action,
  title = 'Fleet',
  subtitle = '3 machines · 5 projects · 813 sessions',
}: {
  action?: ReactNode;
  title?: string;
  subtitle?: string;
}) {
  return (
    <div className="flex items-center justify-between gap-3 border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
      <span className="min-w-0">
        <span className="block font-mono text-body font-bold text-white">{title}</span>
        <span className="mt-0.5 block font-mono text-chip text-dialog-hint">{subtitle}</span>
      </span>
      {action}
    </div>
  );
}

function FilterBar() {
  return (
    <div className="flex min-h-10 items-center gap-2 border-b border-dialog-edge bg-panel px-3">
      <span className="shrink-0 font-mono text-ui text-accent-ink">›</span>
      <span className="font-mono text-meta text-dialog-hint">Filter title, project, session</span>
    </div>
  );
}

function SessionRow({ session }: { session: FleetSession }) {
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

/**
 * The shipped list, with every row's actions supplied by the proposal. A machine
 * that is not answering shows why instead of its projects — a design is only
 * reviewable next to the machine it cannot start a session on.
 */
function FleetList({
  header,
  headerAction,
  machineAction,
  projectAction,
  underMachine,
  machines = MACHINES,
  withMachineHeader = true,
}: {
  header?: { title: string; subtitle: string };
  headerAction?: ReactNode;
  machineAction?: (machine: FleetMachine) => ReactNode;
  projectAction?: (project: string) => ReactNode;
  underMachine?: (machine: FleetMachine) => ReactNode;
  /** Solo is the falsifier: one machine paired must cost no machine chrome at all. */
  machines?: FleetMachine[];
  withMachineHeader?: boolean;
}) {
  return (
    <>
      <FleetBar action={headerAction} title={header?.title} subtitle={header?.subtitle} />
      <FilterBar />
      {machines.map((machine) => {
        const projects = byProject(sessionsOf(machine.id));
        const dead = machine.state !== 'online';
        return (
          <div key={machine.id} className="relative">
            {withMachineHeader && (
              <div
                className={`flex items-center justify-between gap-2 border-b border-dialog-edge bg-panel-2 pl-3 pr-2 ${
                  machineAction ? 'py-1' : 'py-1.5'
                }`}
              >
                <MachineName
                  machine={machine}
                  className={`font-mono text-chip font-bold uppercase tracking-[0.08em] ${
                    dead ? 'text-dialog-hint' : 'text-white'
                  }`}
                />
                <span className="flex items-center gap-2">
                  <span className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
                    {dead ? 'not answering' : `${projects.length} projects`}
                  </span>
                  {machineAction?.(machine)}
                </span>
              </div>
            )}
            {dead
              ? null
              : projects.map(([project, sessions]) => (
                  <div key={project}>
                    <div className="flex items-center gap-2 border-b border-dialog-edge pl-3 pr-2 py-2">
                      <span className="min-w-0 flex-1">
                        <span className="block truncate font-mono text-ui font-bold text-white">
                          {project}
                        </span>
                        <span className="mt-0.5 block truncate font-mono text-chip text-dialog-hint">
                          {projectRoot(sessions)} · {sessions.length * 8} sessions
                        </span>
                      </span>
                      {projectAction?.(project)}
                    </div>
                    {sessions.slice(0, 2).map((session) => (
                      <SessionRow key={session.id} session={session} />
                    ))}
                  </div>
                ))}
            {underMachine?.(machine)}
          </div>
        );
      })}
    </>
  );
}

/** A menu hung under the row that opened it — never over the whole screen. */
function Popover({ children, offset = 'top-full' }: { children: ReactNode; offset?: string }) {
  return (
    <div
      className={`absolute right-2 ${offset} z-10 w-72 border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)]`}
    >
      {children}
    </div>
  );
}

/** Phone: docked to the bottom edge, under the thumb, over a dimmed list. */
function BottomSheet({
  title,
  children,
  footer,
}: {
  title: string;
  children: ReactNode;
  footer?: ReactNode;
}) {
  return (
    <>
      <div className="absolute inset-0 z-10 bg-black/40" />
      <div className="absolute inset-x-0 bottom-0 z-20 flex max-h-[82%] flex-col border-t border-dialog-edge bg-panel">
        <div className={`flex items-center justify-between gap-2 ${BAND} ${LOUD_BAND}`}>
          <span className="truncate">{title}</span>
          <span className="shrink-0 opacity-70">esc</span>
        </div>
        <div className="min-h-0 flex-1 overflow-hidden">{children}</div>
        {footer}
      </div>
    </>
  );
}

/* --------------------------------------------------- C+F · the machine menu */

const KEBAB = '⋯';
const PENCIL = '✎';

/**
 * C. Every machine verb, in the machine's own overflow — so "which machine?" is
 * answered by the header that was tapped and never asked again. F adds the draft
 * as a SECOND VERB, and only while `Offer drafts` is on.
 */
function MachineMenu({
  label,
  withDraft = true,
  offset,
}: {
  label?: string;
  withDraft?: boolean;
  offset?: string;
}) {
  return (
    <Popover offset={offset}>
      {label && <p className={`${BAND} ${QUIET_BAND}`}>{label}</p>}
      <Item title="New session" hint="in vis · ~/vis · last used 7m ago" />
      {withDraft && (
        <Item
          title="New session in a draft…"
          hint="a private copy of vis, uncommitted work included"
        />
      )}
      <Item title="Switch project…" hint="browse this machine's files" tone="accent" />
      <Item title="Machine settings" hint="name, pairing, unpair" tone="muted" />
    </Popover>
  );
}

/* ------------------------------------------- H · the Switch project sheet */

interface Entry {
  name: string;
  meta: string;
  badge?: string;
}

const HOME_ENTRIES: Entry[] = [
  { name: 'vis', meta: '128 entries · main', badge: 'project' },
  { name: 'infrastructure', meta: '41 entries · main', badge: 'git' },
  {
    name: 'tree-sitter-clojure',
    meta: '22 entries · v0.0.13',
    badge: 'project',
  },
  { name: 'clj-imaging', meta: '19 entries · main', badge: 'git' },
  { name: 'spel', meta: '31 entries · main', badge: 'git' },
  { name: 'Downloads', meta: '412 entries' },
];

/**
 * The pencil is INK, not a control: a bare glyph on the header's own paper, at the
 * same 32px hit box as a kebab. A box around it would read as a second button
 * competing with the path it edits.
 */
function PencilButton({ label, active = false }: { label: string; active?: boolean }) {
  return (
    <span
      aria-label={label}
      className={`inline-flex size-8 shrink-0 items-center justify-center font-mono text-ui ${
        active ? 'font-bold text-accent-ink' : 'text-dialog-hint'
      }`}
    >
      {PENCIL}
    </span>
  );
}

/**
 * The path IS the control: an ancestor is a tap back up, and the pencil on its
 * right turns the very same header into a field you type into.
 */
function Crumbs({ trail, action }: { trail: string[]; action?: ReactNode }) {
  return (
    <div className="flex items-center gap-1 border-b border-dialog-edge bg-panel-2 px-3 py-2">
      <span className="shrink-0 font-mono text-meta text-dialog-hint">…</span>
      <span className="flex min-w-0 flex-1 items-center gap-1 overflow-hidden">
        {trail.map((crumb, index) => (
          <span key={crumb} className="flex min-w-0 items-center gap-1">
            <span className="shrink-0 font-mono text-meta text-dialog-hint">›</span>
            <span
              className={`truncate font-mono text-meta ${
                index === trail.length - 1 ? 'font-bold text-white' : 'text-accent-ink'
              }`}
            >
              {crumb}
            </span>
          </span>
        ))}
      </span>
      {action}
    </div>
  );
}

/** The pencil, taken: the crumbs are replaced by the path itself, still editable. */
function PathField() {
  return (
    <div className="flex items-center gap-2 border-b border-dialog-edge bg-panel px-3 py-2">
      <span className="min-w-0 flex-1 border border-accent bg-input px-2 py-1 font-mono text-ui text-white">
        ~/vis/apps/vis-c
        <span className="text-dialog-hint">ompanion</span>
        <Caret />
      </span>
      <PencilButton label="Back to browsing" active />
    </div>
  );
}

function FolderRow({ entry, dim = false }: { entry: Entry; dim?: boolean }) {
  return (
    <div className={`${ROW} ${dim ? 'opacity-40' : ''}`}>
      <span aria-hidden="true" className="shrink-0 font-mono text-ui text-accent-ink">
        ▸
      </span>
      <span className="min-w-0 flex-1">
        <span className="block truncate font-mono text-ui text-white">{entry.name}/</span>
        <span className="mt-0.5 block truncate font-mono text-meta text-dialog-hint">
          {entry.meta}
        </span>
      </span>
      {entry.badge && (
        <span
          className={`shrink-0 border px-1 font-mono text-chip uppercase tracking-[0.08em] ${
            entry.badge === 'project'
              ? 'border-accent text-accent-ink'
              : 'border-edge text-dialog-hint'
          }`}
        >
          {entry.badge}
        </span>
      )}
    </div>
  );
}

function SheetFooter({ path, primary }: { path: string; primary: string }) {
  return (
    <div className="border-t border-dialog-edge bg-panel-2 px-3 py-2">
      <p className="truncate font-mono text-meta text-dialog-hint">{path}</p>
      <div className="mt-1.5 flex items-center justify-between gap-2">
        <span className={GHOST}>New folder</span>
        <span className={PRIMARY}>{primary}</span>
      </div>
    </div>
  );
}

/**
 * H. One sheet, three moments: browsing the machine's filesystem, typing the path
 * instead, and making the folder that does not exist yet. It commits a FOLDER —
 * whether that folder is a repo is the gateway's answer, not a question asked here.
 */
function SwitchSheet({ mode }: { mode: 'browse' | 'typed' | 'new-folder' }) {
  const creating = mode === 'new-folder';
  const pencil = <PencilButton label="Type a path" />;
  return (
    <BottomSheet
      title="Switch project · studio-mbp"
      footer={
        <SheetFooter
          path={
            mode === 'typed'
              ? '/Users/fierycod/vis/apps/vis-companion'
              : creating
                ? '/Users/fierycod/code/band-repaint'
                : '/Users/fierycod'
          }
          primary={creating ? 'Create & switch' : 'Switch here'}
        />
      }
    >
      {mode === 'typed' ? (
        <>
          <PathField />
          <p className={`${BAND} ${QUIET_BAND}`}>2 matches</p>
          <Item title="~/vis/apps/vis-companion" hint="128 entries · main" badge="git" />
          <Item title="~/vis/apps/vis-companion/ios" hint="generated · not a repo" tone="muted" />
          <p className={`${BAND} ${QUIET_BAND}`}>already known here</p>
          <Item title="~/vis" hint="128 sessions" badge="project" />
          <Item title="~/tree-sitter-clojure" hint="8 sessions" badge="project" />
        </>
      ) : (
        <>
          <Crumbs trail={creating ? ['fierycod', 'code'] : ['Users', 'fierycod']} action={pencil} />
          {creating && (
            <div className="flex items-center gap-2 border-b border-dialog-edge bg-panel px-3 py-2">
              <span className="shrink-0 font-mono text-ui text-accent-ink">+</span>
              <span className="min-w-0 flex-1 border border-accent px-2 py-1 font-mono text-ui text-white">
                band-repaint
                <Caret />
              </span>
            </div>
          )}
          {(creating ? HOME_ENTRIES.slice(0, 4) : HOME_ENTRIES).map((entry) => (
            <FolderRow key={entry.name} entry={entry} dim={creating} />
          ))}
        </>
      )}
    </BottomSheet>
  );
}

/* ------------------------------------------------------- K · the switch itself */

function SettingsRow({ title, hint, on }: { title: string; hint: string; on?: boolean }) {
  return (
    <div className="flex items-start gap-3 border-b border-dialog-edge px-3 py-2.5">
      <span className="min-w-0 flex-1">
        <span className="block font-mono text-ui font-bold text-white">{title}</span>
        <span className="mt-0.5 block font-mono text-meta text-dialog-hint">{hint}</span>
      </span>
      {on === undefined ? (
        <span className="shrink-0 font-mono text-ui text-dialog-hint">›</span>
      ) : (
        <Toggle on={on} />
      )}
    </div>
  );
}

/* ------------------------------------------------------------ the whole flow */

const MACHINE_ACTION = (machine: FleetMachine) => (
  <IconButton
    label={`Actions for ${machine.label}`}
    tone={machine.id === MACHINES[0].id ? 'accent' : 'plain'}
  >
    {KEBAB}
  </IconButton>
);

const PROJECT_ACTION = (project: string) => (
  <IconButton label={`Actions for ${project}`}>{KEBAB}</IconButton>
);

/** The decided product, one state per step of it. */
export function SessionFlowVariant({ state }: { state: string }) {
  if (state === 'settings') {
    return (
      <Screen>
        <div className={`${BAND} ${QUIET_BAND}`}>settings</div>
        <div className={`${BAND} ${QUIET_BAND}`}>sessions</div>
        <SettingsRow
          title="Offer drafts"
          hint="Ask whether a session runs in the project or in a private copy of it."
          on
        />
        <SettingsRow
          title="Start in the last project"
          hint="A new session skips the picker when the machine has an obvious answer."
          on={false}
        />
        <div className={`${BAND} ${QUIET_BAND}`}>appearance</div>
        <SettingsRow title="Theme" hint="Follow the system" />
        <SettingsRow title="Notifications" hint="A turn finished while the app was closed" />
        <p className="px-3 py-2 font-mono text-meta text-dialog-hint">
          One answer for the whole fleet, on this device. Off, the second verb simply is not in the
          menu; a machine that cannot fork a folder still refuses — the switch decides the question,
          not the capability.
        </p>
      </Screen>
    );
  }
  if (state === 'solo') {
    return (
      <Screen>
        <div className="relative">
          <FleetList
            machines={MACHINES.slice(0, 1)}
            withMachineHeader={false}
            header={{ title: 'studio-mbp', subtitle: '3 projects · 214 sessions' }}
            headerAction={
              <IconButton label="Actions for studio-mbp" tone="accent">
                {KEBAB}
              </IconButton>
            }
            projectAction={PROJECT_ACTION}
          />
          <MachineMenu offset="top-[3.75rem]" />
        </div>
        <p className="mt-auto border-t border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-meta text-dialog-hint">
          One machine paired: no machine header, no chips, no machine question — the fleet bar IS
          the machine, and its ⋯ is the machine's ⋯.
        </p>
      </Screen>
    );
  }
  const menu = state === 'menu' || state === 'menu-off';
  const list = (
    <Screen>
      <FleetList
        machineAction={MACHINE_ACTION}
        projectAction={PROJECT_ACTION}
        underMachine={(machine) =>
          machine.id === MACHINES[0].id && menu ? (
            <MachineMenu label="studio-mbp" withDraft={state === 'menu'} />
          ) : null
        }
      />
      {state === 'menu-off' && (
        <p className="mt-auto border-t border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-meta text-dialog-hint">
          Settings › <span className="text-white">Offer drafts</span> off: the verb is not there,
          and nothing else in the app asks about copies.
        </p>
      )}
    </Screen>
  );
  if (menu) return list;
  return (
    <div className="relative h-full">
      {list}
      <SwitchSheet
        mode={state === 'typed' ? 'typed' : state === 'new-folder' ? 'new-folder' : 'browse'}
      />
    </div>
  );
}

/* ------------------------------------------------------------------- board */

export interface FlowStep {
  step: string;
  title: string;
  /** What this step costs and what it buys — the sentence the design is read on. */
  caption: string;
  variant: string;
  state: string;
}

export const FLOW_STEPS: FlowStep[] = [
  {
    step: '1',
    title: 'The machine ⋯ is the whole menu (C)',
    caption:
      'Nothing new on the list. The header you tapped is the machine, so no surface ever asks again; the project ⋯ still only deletes that project’s sessions.',
    variant: 'session-flow',
    state: 'menu',
  },
  {
    step: '2',
    title: 'Drafts off: the verb is gone (F)',
    caption:
      'The same menu with the switch off — three entries, no fork, nothing anywhere else in the app asking "the project or a copy?". That absence is what the toggle buys.',
    variant: 'session-flow',
    state: 'menu-off',
  },
  {
    step: '3',
    title: 'Switch project browses the machine (H)',
    caption:
      'A bottom sheet on the machine’s own filesystem: descend, climb the path, go above ~ to /. Known projects are badged; SWITCH HERE commits the folder under the thumb.',
    variant: 'session-flow',
    state: 'browse',
  },
  {
    step: '4',
    title: 'The pencil types the path (H + ✎)',
    caption:
      'The same header, edited: the crumbs become the path itself, matches narrow as you type, and the pencil stays lit as the way back to browsing. A keyboard beats six taps.',
    variant: 'session-flow',
    state: 'typed',
  },
  {
    step: '5',
    title: 'A folder is made in place',
    caption:
      'NEW FOLDER inserts an editable row in the list; ⏎ creates it and switches in one breath, so a project that does not exist yet never needs a second dialog.',
    variant: 'session-flow',
    state: 'new-folder',
  },
  {
    step: '6',
    title: 'Offer drafts is one app switch (K)',
    caption:
      'This device, every machine. It decides the QUESTION; a gateway that cannot fork a folder still refuses, which is a capability and not a preference.',
    variant: 'session-flow',
    state: 'settings',
  },
  {
    step: '7',
    title: 'Solo pays nothing (falsifier)',
    caption:
      'One machine paired: no machine header, no chips, no machine question. The fleet bar IS the machine and carries the same ⋯ — the feature costs a solo user one glyph.',
    variant: 'session-flow',
    state: 'solo',
  },
];

function renderStep(step: FlowStep) {
  return <SessionFlowVariant state={step.state} />;
}

function StepCard({ step }: { step: FlowStep }) {
  return (
    <figure className="flex w-[390px] flex-col border border-dialog-edge bg-panel-2">
      <figcaption className="flex min-h-[7rem] flex-col border-b border-dialog-edge px-3 py-2">
        <p className="flex items-baseline gap-2">
          <span className="shrink-0 bg-accent px-1.5 font-mono text-ui font-bold text-accent-foreground">
            {step.step}
          </span>
          <span className="min-w-0 font-mono text-ui font-bold text-white">{step.title}</span>
        </p>
        <p className="mt-1 font-mono text-meta text-dialog-hint">{step.caption}</p>
      </figcaption>
      <div className="h-[42rem] w-full overflow-hidden">{renderStep(step)}</div>
    </figure>
  );
}

/**
 * The answered board: one sheet of paper showing the flow that was chosen, in the
 * order a person walks it. A decision recorded only in prose is a decision nobody
 * can see was wrong.
 */
export function SessionUxBoardVariant() {
  return (
    <section className="mx-auto flex w-full max-w-[1680px] flex-col gap-6 p-6">
      <header>
        <h1 className="font-mono text-head font-bold text-white">
          Machine, project, draft — the chosen flow
        </h1>
        <p className="mt-1 font-mono text-meta text-dialog-hint">
          1C · 2F · 3H with a path pencil · 4K. Every machine verb lives in the machine’s ⋯, the
          draft is a second verb there, Switch project is a filesystem sheet you can also type into,
          and Offer drafts is one app switch. (`apps/vis-companion/SESSION-UX.md`)
        </p>
      </header>
      <div className="flex flex-wrap items-start gap-4">
        {FLOW_STEPS.map((step) => (
          <StepCard key={step.step} step={step} />
        ))}
      </div>
    </section>
  );
}
