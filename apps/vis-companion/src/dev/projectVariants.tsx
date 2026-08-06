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
  liveCount,
  projectRoot,
  sessionsOf,
  type FleetMachine,
  type FleetSession,
} from './fleet';
import { ChevronIcon, PencilIcon } from '../components/icons';
import { Button, MachineBanner, MachineGap, MachineMark, MachineRail } from '../components/ui';
import { assignMachineColors, machineColor } from '../lib/machine-colors';

/** The states each proposal is photographed in; the gallery registers these. */
export const PROJECT_STATES: Record<string, string[]> = {
  'session-flow': ['menu', 'menu-off', 'browse', 'typed', 'new-folder', 'settings', 'solo'],
  'session-ux-board': ['default'],
};

/* -------------------------------------------------------------------- atoms */

/* Every class string below is LIFTED from the screen it belongs to, never
   approximated: the list, its menus and its rows come from `SessionsScreen`, the
   panel header and the switch from `SettingsScreen`, the field from `ui.tsx`'s
   `Input`, and the machine chrome is the app's OWN `MachineBanner`, `MachineRail`
   and `MachineMark` rather than a look-alike. A proposal drawn in classes this
   app does not use is a proposal about a different app. */

/** `SessionsScreen`'s menu heading; amber only when it names the unskippable question. */
const BAND = 'px-3 py-2 font-mono text-chip uppercase tracking-[0.08em]';
const LOUD_BAND = 'border-b-2 border-warn-strong bg-accent font-bold text-accent-foreground';
const QUIET_BAND = 'border-b border-dialog-edge bg-panel-2 text-dialog-hint';
/** `StartOption`: one menu row — a 44px thumb target with a hairline under it. */
const ROW =
  'flex min-h-11 w-full items-start gap-2 border-b border-dialog-edge px-3 py-2 text-left';
/** The badge a row hangs on its right edge, exactly as `StartOption` draws it. */
const CHIP =
  'mt-0.5 shrink-0 border border-edge px-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint';
/** `ui.tsx`'s `Input`, in the focused state a caret implies. */
const FIELD =
  'min-h-7 min-w-0 flex-1 rounded-none border border-accent bg-input px-2.5 py-0.5 font-mono text-meta text-white ring-1 ring-accent/30';

function Caret() {
  return (
    <span
      aria-hidden="true"
      className="ml-0.5 inline-block h-4 w-2 animate-pulse bg-accent align-middle motion-reduce:animate-none"
    />
  );
}

// The mark is IDENTITY, not liveness: `MachineMark` wears the hue the shipped list
// gives this machine on its scope chip and its rail, and `offline` is the word that
// carries the state. A green/grey dot in the same slot means something else.
const MACHINE_COLORS = assignMachineColors(MACHINES.map((machine) => machine.id));
const machineHue = (machine: FleetMachine) => machineColor(MACHINE_COLORS, machine.id);

/** A session's status is one table in `SessionsScreen`: a tone and a dot, together. */
const STATUS_TONE: Record<FleetSession['status'], string> = {
  LIVE: 'text-ok',
  WAITING: 'text-warn-strong',
  IDLE: 'text-dialog-hint',
};
const STATUS_DOT: Record<FleetSession['status'], string> = {
  LIVE: 'animate-pulse bg-ok motion-reduce:animate-none',
  WAITING: 'bg-warn-strong',
  IDLE: 'border border-dialog-hint',
};

/**
 * The shipped kebab is frameless INK at a 44px tap target — `min-h-11 … px-3`,
 * `text-dialog-hint`, `hover:bg-hover` (`SessionsScreen`). A bordered box, and
 * worse an amber-filled one, is a control this app never draws.
 */
function KebabButton({ label, open = false }: { label: string; open?: boolean }) {
  return (
    <span
      aria-label={label}
      className={`flex min-h-11 shrink-0 items-center justify-center px-3 font-mono text-ui ${
        open ? 'bg-hover text-white' : 'text-dialog-hint'
      }`}
    >
      {KEBAB}
    </span>
  );
}

/** The app's own switch (`SettingsScreen`): a mono ON/OFF block, never a sliding knob. */
function Toggle({ on }: { on: boolean }) {
  return (
    <span
      aria-hidden="true"
      className={`mt-0.5 inline-flex h-8 w-[3.25rem] shrink-0 items-center justify-center border border-transparent font-mono text-chip font-black tracking-[0.08em] ${
        on ? 'bg-accent text-accent-foreground' : 'bg-panel-2 text-dialog-hint'
      }`}
    >
      {on ? 'ON' : 'OFF'}
    </span>
  );
}

/**
 * One tappable answer: title, one line of consequence, an optional badge. This is
 * `StartOption` verbatim — and every row of a menu is one, so nothing in a menu is
 * tinted, dimmed or emphasised into looking like a different KIND of answer.
 */
function Item({ title, hint, badge }: { title: ReactNode; hint?: ReactNode; badge?: ReactNode }) {
  return (
    <div className={ROW}>
      <span className="min-w-0 flex-1">
        <span className="block truncate font-mono text-ui font-bold text-white">{title}</span>
        {hint && <span className="mt-0.5 block font-mono text-meta text-dialog-hint">{hint}</span>}
      </span>
      {badge && <span className={CHIP}>{badge}</span>}
    </div>
  );
}

/** `SettingsPanel`'s header: the accent is a 2px tick beside the name, not a filled band. */
function SettingsBand({ title }: { title: string }) {
  return (
    <div className="flex min-h-8 items-center gap-3 border-b border-dialog-edge bg-panel-2 px-3 py-1.5">
      <h3 className="min-w-0 truncate border-l-2 border-accent pl-2 font-mono text-meta font-black uppercase tracking-[0.12em] text-white">
        {title}
      </h3>
    </div>
  );
}
/* ------------------------------------------------------------- the list */

/** The panel the sessions list lives in, flush under the app header as on a phone. */
function Screen({ children }: { children: ReactNode }) {
  return (
    <div className="relative flex h-full min-h-[42rem] flex-col overflow-hidden border-b border-dialog-edge bg-panel">
      {children}
    </div>
  );
}

/**
 * The counts line is the shipped one: every fact is a WHOLE nowrap unit and the
 * groups are separated by space, so a wrap can never strand a `·` at the end of a
 * line. Scoped to one machine the machine count is gone — the title says it.
 */
function FleetBar({
  action,
  title = 'Fleet',
  machines = 3,
  projects = 5,
  sessions = 813,
}: {
  action?: ReactNode;
  title?: string;
  machines?: number;
  projects?: number;
  sessions?: number;
}) {
  return (
    <div className="bg-panel-2 px-3 py-2.5">
      <div className="flex items-center justify-between gap-3">
        <div className="min-w-0">
          <p className="truncate font-mono text-body font-bold text-white">{title}</p>
          <p className="mt-0.5 flex flex-wrap items-center gap-x-3 gap-y-0.5 font-mono text-meta text-dialog-hint">
            {machines > 1 && <span className="whitespace-nowrap">{machines} machines</span>}
            <span className="whitespace-nowrap">
              {projects} projects
              <span className="px-1 opacity-40">·</span>
              {sessions} sessions
            </span>
          </p>
        </div>
        {action}
      </div>
    </div>
  );
}

function FilterBar() {
  return (
    <div className="flex min-h-10 items-center border-y border-dialog-edge bg-panel px-3">
      <span className="shrink-0 font-mono text-ui text-accent-ink">›</span>
      <span className="min-w-0 flex-1 px-2 py-2 font-mono text-ui text-dialog-hint">
        Filter title, project, session
      </span>
    </div>
  );
}

/**
 * A session row, at the shipped geometry: a `w-8` disclosure column that opens the
 * usage rollup, a `min-h-14` body, the status as a dot AND a word, and the facts
 * line with the time pushed to the right edge. Rows are separated by their own
 * `[&+&]` rule, so the last row of a project does not draw one.
 */
function SessionRow({ session }: { session: FleetSession }) {
  return (
    <div className="[&+&]:border-t [&+&]:border-dialog-edge">
      <div className="flex items-stretch">
        <span
          aria-hidden="true"
          className="flex w-8 shrink-0 items-start justify-center pt-2.5 font-mono text-body text-accent-ink opacity-40"
        >
          ›
        </span>
        <span className="flex min-h-14 min-w-0 flex-1 items-start py-2.5 pl-2 pr-3 text-left">
          <span className="min-w-0 flex-1">
            <span className="flex min-w-0 items-start justify-between gap-3">
              <span className="block min-w-0 truncate font-mono text-ui font-semibold text-white">
                {session.title}
              </span>
              <span
                className={`inline-flex shrink-0 items-center gap-1 font-mono text-chip font-bold tracking-[0.08em] ${STATUS_TONE[session.status]}`}
              >
                <span className={`size-1.5 ${STATUS_DOT[session.status]}`} />
                {session.status}
              </span>
            </span>
            <span className="mt-1 flex flex-wrap items-center gap-x-2 gap-y-1 font-mono text-chip text-dialog-hint">
              <span className="text-white/55">{session.id.slice(-6)}</span>
              <span className="opacity-40" aria-hidden="true">
                ·
              </span>
              <span>{session.turns} turns</span>
              <span className="ml-auto shrink-0 pl-2">{session.ago}</span>
            </span>
          </span>
        </span>
      </div>
    </div>
  );
}

/**
 * The shipped list, with every row's actions supplied by the proposal: the app's
 * own `MachineGap` between machines, its `MachineRail` down everything one machine
 * owns, its `MachineBanner` on top and its project header inside. A machine that is
 * not answering says `offline` and offers a Retry — a design is only reviewable
 * next to the machine it cannot start a session on.
 */
function FleetList({
  header,
  headerAction,
  machineAction,
  projectAction,
  machines = MACHINES,
}: {
  header?: {
    title: string;
    machines?: number;
    projects: number;
    sessions: number;
  };
  headerAction?: ReactNode;
  machineAction?: (machine: FleetMachine) => ReactNode;
  projectAction?: (project: string) => ReactNode;
  machines?: FleetMachine[];
}) {
  return (
    <>
      <FleetBar
        action={headerAction}
        {...header}
        machines={header?.machines ?? machines.length}
      />
      <FilterBar />
      <div className="border-t border-dialog-edge">
        {machines.map((machine, index) => {
          const projects = byProject(sessionsOf(machine.id));
          const dead = machine.state !== 'online';
          return (
            <section key={machine.id}>
              {index > 0 && <MachineGap />}
              <MachineRail color={machineHue(machine)}>
                  <MachineBanner>
                    <span className="flex min-w-0 items-center gap-2">
                      <MachineMark color={machineHue(machine)} />
                      <span className="truncate font-mono text-ui font-bold text-white">
                        {machine.label}
                      </span>
                    </span>
                    <span className="flex shrink-0 items-center gap-2 font-mono text-chip text-dialog-hint">
                      {dead ? (
                        <>
                          <span>offline</span>
                          <span className="border border-edge px-1.5 py-0.5">Retry</span>
                        </>
                      ) : (
                        <span>
                          {projects.length} {projects.length === 1 ? 'project' : 'projects'}
                        </span>
                      )}
                      {machineAction?.(machine)}
                    </span>
                  </MachineBanner>
                {dead ? (
                  <p className="px-3 py-3 font-mono text-meta text-dialog-hint">
                    This machine is not answering.
                  </p>
                ) : (
                  projects.map(([project, sessions]) => {
                    const live = liveCount(sessions);
                    return (
                      <section
                        key={project}
                        className="border-t border-dialog-edge first:border-t-0"
                      >
                        <header className="flex items-stretch bg-panel-2">
                          <span className="flex min-h-11 min-w-0 flex-1 items-center justify-between gap-3 px-3 py-2 text-left">
                            <span className="flex min-w-0 items-center gap-2">
                              <ChevronIcon open className="size-3.5 text-dialog-hint" />
                              <span className="min-w-0">
                                <span className="block truncate font-mono text-ui font-bold text-white">
                                  {project}
                                </span>
                                <span className="mt-0.5 block truncate font-mono text-chip text-dialog-hint">
                                  {projectRoot(sessions)}
                                </span>
                              </span>
                            </span>
                            <span className="flex shrink-0 items-center gap-2 font-mono text-chip text-dialog-hint">
                              <span>{sessions.length * 8} sessions</span>
                              {live > 0 && (
                                <>
                                  <span className="opacity-40" aria-hidden="true">
                                    ·
                                  </span>
                                  <span className="inline-flex items-center gap-1 font-bold text-ok">
                                    <span className="size-1.5 animate-pulse bg-ok motion-reduce:animate-none" />
                                    {live} live
                                  </span>
                                </>
                              )}
                            </span>
                          </span>
                          {projectAction?.(project)}
                        </header>
                        <div className="border-t border-dialog-edge">
                          {sessions.slice(0, 2).map((session) => (
                            <SessionRow key={session.id} session={session} />
                          ))}
                        </div>
                      </section>
                    );
                  })
                )}
              </MachineRail>
            </section>
          );
        })}
      </div>
    </>
  );
}

/**
 * A menu in this app is a SHEET on a phone. Both shipped `role="menu"` portals in
 * `SessionsScreen` dock to the bottom edge over a scrim — `inset-x-0 bottom-0`,
 * `border-t-2 border-accent` — and only become a popover pinned under the caret
 * from `sm:` up. Every card here is a 390px column, so the docked form is the
 * true one; a floating popover on a phone is a screen the app has never shown.
 */
function Sheet({
  title,
  tone = 'loud',
  children,
  footer,
}: {
  title: string;
  /** One amber per surface: a sheet that carries a primary button spends it there. */
  tone?: 'loud' | 'quiet';
  children: ReactNode;
  footer?: ReactNode;
}) {
  return (
    <>
      <div className="absolute inset-0 z-10 bg-black/40" />
      <div className="absolute inset-x-0 bottom-0 z-20 flex max-h-[82%] flex-col border-t-2 border-accent bg-panel">
        <div className={`${BAND} ${tone === 'loud' ? LOUD_BAND : QUIET_BAND} truncate`}>
          {title}
        </div>
        <div className="min-h-0 flex-1 overflow-hidden">{children}</div>
        {footer}
      </div>
    </>
  );
}

/* --------------------------------------------------- C+F · the machine menu */

const KEBAB = '⋯';

/**
 * C. Every machine verb, in the machine's own overflow — so "which machine?" is
 * answered by the header that was tapped and never asked again. F adds the draft
 * as a SECOND VERB, and only while `Offer drafts` is on. It is the shipped menu
 * chrome: docked, over a scrim, its one amber spent on the band that names the
 * machine you tapped.
 */
function MachineMenu({ label, withDraft = true }: { label: string; withDraft?: boolean }) {
  return (
    <Sheet title={label}>
      <Item title="New session" hint="in vis · ~/vis · last used 7m ago" />
      {withDraft && (
        <Item
          title="New session in a draft…"
          hint="a private copy of vis, uncommitted work included"
        />
      )}
      <Item title="Switch project…" hint="browse this machine's files" />
      <Item title="Machine settings" hint="name, pairing, unpair" />
    </Sheet>
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
 * The pencil is the app's own `PencilIcon` — a stroked 16px glyph, like every
 * other icon here — drawn as INK on the header's own paper at a 44px tap target.
 * A `✎` dingbat is whatever the fallback face happens to ship, and a box around
 * it would read as a second button competing with the path it edits.
 */
function PencilButton({ label, active = false }: { label: string; active?: boolean }) {
  return (
    <span
      aria-label={label}
      className={`inline-flex size-11 shrink-0 items-center justify-center ${
        active ? 'text-accent-ink' : 'text-dialog-hint'
      }`}
    >
      <PencilIcon className="size-4" />
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

/** The pencil, taken: the crumbs are replaced by the path itself, in the app's own `Input` skin. */
function PathField() {
  return (
    <div className="flex items-center gap-2 border-b border-dialog-edge bg-panel px-3 py-1">
      <span className={FIELD}>
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
      <ChevronIcon className="mt-0.5 size-3.5 shrink-0 text-dialog-hint" />
      <span className="min-w-0 flex-1">
        <span className="block truncate font-mono text-ui text-white">{entry.name}/</span>
        <span className="mt-0.5 block truncate font-mono text-meta text-dialog-hint">
          {entry.meta}
        </span>
      </span>
      {/* One badge look in the whole app (`StartOption`'s): the WORD says whether this
          is a repo or a folder Vis already knows, exactly as `offline` does upstairs.
          A second, accent-bordered chip would spend the amber twice on one surface. */}
      {entry.badge && <span className={CHIP}>{entry.badge}</span>}
    </div>
  );
}

function SheetFooter({ path, primary }: { path: string; primary: string }) {
  return (
    <div className="border-t border-dialog-edge bg-panel-2 px-3 py-2">
      <p className="truncate font-mono text-meta text-dialog-hint">{path}</p>
      {/* `quiet` beside `solid` on purpose: two bordered boxes side by side read as
          rivals, which is the whole reason the shipped Button has that variant. */}
      <div className="mt-1.5 flex items-center justify-between gap-2">
        <Button variant="quiet">New folder</Button>
        <Button>{primary}</Button>
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
    <Sheet
      tone="quiet"
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
          <Item title="~/vis/apps/vis-companion/ios" hint="generated · not a repo" />
          <p className={`${BAND} ${QUIET_BAND}`}>already known here</p>
          <Item title="~/vis" hint="128 sessions" badge="project" />
          <Item title="~/tree-sitter-clojure" hint="8 sessions" badge="project" />
        </>
      ) : (
        <>
          <Crumbs trail={creating ? ['fierycod', 'code'] : ['Users', 'fierycod']} action={pencil} />
          {creating && (
            <div className="flex items-center gap-2 border-b border-dialog-edge bg-panel px-3 py-1.5">
              <span className="shrink-0 font-mono text-ui text-accent-ink">+</span>
              <span className={FIELD}>
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
    </Sheet>
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
  <KebabButton label={`Actions for ${machine.label}`} open={machine.id === MACHINES[0].id} />
);

const PROJECT_ACTION = (project: string) => <KebabButton label={`Actions for ${project}`} />;

/** The decided product, one state per step of it. */
export function SessionFlowVariant({ state }: { state: string }) {
  if (state === 'settings') {
    return (
      <Screen>
        <SettingsBand title="Sessions" />
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
        <SettingsBand title="Appearance" />
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
        <FleetList
          machines={MACHINES.slice(0, 1)}
          header={{ title: 'Projects', projects: 3, sessions: 214 }}
          machineAction={MACHINE_ACTION}
          projectAction={PROJECT_ACTION}
        />
        <MachineMenu label="studio-mbp" />
      </Screen>
    );
  }
  const menu = state === 'menu' || state === 'menu-off';
  const list = (
    <Screen>
      <FleetList machineAction={MACHINE_ACTION} projectAction={PROJECT_ACTION} />
      {menu && <MachineMenu label="studio-mbp" withDraft={state === 'menu'} />}
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
      'A bottom sheet on the machine’s own filesystem: descend, climb the path, go above ~ to /. Known projects are badged; Switch here commits the folder under the thumb.',
    variant: 'session-flow',
    state: 'browse',
  },
  {
    step: '4',
    title: 'The pencil types the path (H + pencil)',
    caption:
      'The same header, edited: the crumbs become the path itself, matches narrow as you type, and the pencil stays lit as the way back to browsing. A keyboard beats six taps.',
    variant: 'session-flow',
    state: 'typed',
  },
  {
    step: '5',
    title: 'A folder is made in place',
    caption:
      'New folder inserts an editable row in the list; ⏎ creates it and switches in one breath, so a project that does not exist yet never needs a second dialog.',
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
    title: 'Solo keeps the machine panel',
    caption:
      'One machine paired: the machine panel remains visible with its identity, project totals, and actions; only the fleet chooser is absent.',
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
