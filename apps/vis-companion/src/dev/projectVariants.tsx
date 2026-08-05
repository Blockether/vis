/**
 * WHOSE ACTION IS IT? — the machine's own row, drafts, and Switch project.
 *
 * The settled part of this question lives in `apps/vis-companion/SESSION-UX.md`:
 * a machine owns projects, a project owns sessions, and an action belongs to the
 * row that owns the noun it acts on. What is NOT settled is photographed here,
 * one letter per proposal, and assembled into a single comparison board
 * (`#/__design?v=session-ux-board`) so the options are judged side by side
 * instead of one memory at a time.
 *
 *   A–D  where "New session" lives, now that "which machine?" should not be a step
 *   E–G  how a draft is offered, once `Offer drafts` decides whether it is offered
 *   H–J  the Switch-project sheet: a real browser of the machine's filesystem
 *   K–L  where the `Offer drafts` switch itself belongs
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
  'place-new-session': ['shipped', 'machine-button', 'machine-dots', 'row-plus'],
  'draft-offer': ['off', 'verb', 'chip'],
  'switch-project': ['breadcrumb', 'path', 'new-folder'],
  'drafts-toggle': ['app', 'machine'],
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

function Chip({ children, selected = false }: { children: ReactNode; selected?: boolean }) {
  return (
    <span
      className={`inline-flex min-h-7 items-center gap-1.5 border px-2 py-0.5 font-mono text-chip ${
        selected
          ? 'border-accent bg-accent font-bold text-accent-foreground'
          : 'border-edge text-white'
      }`}
    >
      {children}
    </span>
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

function FleetBar({ action }: { action?: ReactNode }) {
  return (
    <div className="flex items-center justify-between gap-3 border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
      <span className="min-w-0">
        <span className="block font-mono text-body font-bold text-white">Fleet</span>
        <span className="mt-0.5 block font-mono text-chip text-dialog-hint">
          3 machines · 5 projects · 813 sessions
        </span>
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
  headerAction,
  machineAction,
  projectAction,
  underMachine,
}: {
  headerAction?: ReactNode;
  machineAction?: (machine: FleetMachine) => ReactNode;
  projectAction?: (project: string) => ReactNode;
  underMachine?: (machine: FleetMachine) => ReactNode;
}) {
  return (
    <>
      <FleetBar action={headerAction} />
      <FilterBar />
      {MACHINES.map((machine) => {
        const projects = byProject(sessionsOf(machine.id));
        const dead = machine.state !== 'online';
        return (
          <div key={machine.id} className="relative">
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
function Popover({ children }: { children: ReactNode }) {
  return (
    <div className="absolute right-2 top-full z-10 w-72 border border-dialog-edge bg-panel shadow-[6px_6px_0_var(--dialog-shadow)]">
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

/* ------------------------------------------------- A–D · where it lives */

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

const KEBAB = '⋯';

/**
 * A–D. The machine question is the first tap of every session started today; each
 * proposal deletes it from a different place.
 */
export function PlaceNewSessionVariant({ state }: { state: string }) {
  if (state === 'shipped') {
    return (
      <Screen>
        <FleetList headerAction={<SplitButton />} />
        <BottomSheet title="Create the session on">
          <Item title="studio-mbp" hint="~/vis · ~/tree-sitter-clojure" badge="8ms" />
          <Item title="tower" hint="~/vis · ~/infrastructure" badge="12ms" />
          <Item title="vps-eu" hint="not answering" tone="muted" />
        </BottomSheet>
      </Screen>
    );
  }
  if (state === 'machine-button') {
    return (
      <Screen>
        <FleetList
          machineAction={(machine) =>
            machine.state === 'online' ? (
              <span className="flex items-center gap-1.5">
                <span className={PRIMARY}>New session</span>
                <IconButton label={`Actions for ${machine.label}`}>{KEBAB}</IconButton>
              </span>
            ) : (
              <span className="flex items-center gap-1.5 opacity-40">
                <span className={GHOST}>New session</span>
                <IconButton label={`Actions for ${machine.label}`}>{KEBAB}</IconButton>
              </span>
            )
          }
        />
      </Screen>
    );
  }
  if (state === 'machine-dots') {
    return (
      <Screen>
        <FleetList
          machineAction={(machine) => (
            <IconButton
              label={`Actions for ${machine.label}`}
              tone={machine.id === MACHINES[0].id ? 'accent' : 'plain'}
            >
              {KEBAB}
            </IconButton>
          )}
          underMachine={(machine) =>
            machine.id === MACHINES[0].id ? (
              <Popover>
                <p className={`${BAND} ${QUIET_BAND}`}>studio-mbp</p>
                <Item title="New session" hint="choose the project" />
                <Item title="New session in a draft…" hint="a private copy of the project" />
                <Item title="Switch project…" hint="browse this machine's files" />
                <Item title="Machine settings" hint="name, pairing, drafts" tone="muted" />
              </Popover>
            ) : null
          }
        />
      </Screen>
    );
  }
  return (
    <Screen>
      <FleetList
        machineAction={(machine) => (
          <span className="flex items-center gap-1.5">
            {machine.state === 'online' && (
              <IconButton label={`New session on ${machine.label}`} tone="accent">
                +
              </IconButton>
            )}
            <IconButton label={`Actions for ${machine.label}`}>{KEBAB}</IconButton>
          </span>
        )}
        projectAction={(project) => (
          <span className="flex items-center gap-1.5">
            <IconButton label={`New session in ${project}`}>+</IconButton>
            <IconButton label={`Actions for ${project}`}>{KEBAB}</IconButton>
          </span>
        )}
      />
    </Screen>
  );
}

/* ---------------------------------------------------- E–G · the draft */

/** The destination sheet a machine-level start opens: which project, and how. */
function DestinationSheet({ withDraft }: { withDraft: boolean }) {
  return (
    <BottomSheet
      title="New session on studio-mbp"
      footer={
        withDraft ? (
          <div className="flex items-start gap-2 border-t border-dialog-edge bg-panel-2 px-3 py-2">
            <span className="w-8 shrink-0 pt-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
              as
            </span>
            <span className="flex min-w-0 flex-1 flex-wrap gap-1.5">
              <Chip selected>the project</Chip>
              <Chip>a new draft</Chip>
              <Chip>wire-rework</Chip>
            </span>
          </div>
        ) : null
      }
    >
      <Item title="vis" hint="~/vis · 16 sessions" badge="7m ago" />
      <Item title="tree-sitter-clojure" hint="~/tree-sitter-clojure · 8 sessions" badge="3h ago" />
      <Item title="Switch project…" hint="browse this machine's files" tone="accent" />
    </BottomSheet>
  );
}

/**
 * E–G. `Offer drafts` decides whether the fork is a question at all; these three
 * are what "on" can look like, and what "off" buys.
 */
export function DraftOfferVariant({ state }: { state: string }) {
  if (state === 'chip') {
    return (
      <Screen>
        <FleetList
          machineAction={(machine) => (
            <IconButton
              label={`New session on ${machine.label}`}
              tone={machine.id === MACHINES[0].id ? 'accent' : 'plain'}
            >
              +
            </IconButton>
          )}
        />
        <DestinationSheet withDraft />
      </Screen>
    );
  }
  const off = state === 'off';
  return (
    <Screen>
      <FleetList
        machineAction={(machine) => (
          <IconButton
            label={`Actions for ${machine.label}`}
            tone={machine.id === MACHINES[0].id ? 'accent' : 'plain'}
          >
            {KEBAB}
          </IconButton>
        )}
        underMachine={(machine) =>
          machine.id === MACHINES[0].id ? (
            <Popover>
              <p className={`${BAND} ${QUIET_BAND}`}>studio-mbp</p>
              <Item title="New session" hint={off ? 'in vis · ~/vis' : 'choose the project'} />
              {!off && (
                <Item
                  title="New session in a draft…"
                  hint="a private copy, uncommitted work included"
                />
              )}
              <Item title="Switch project…" hint="browse this machine's files" />
            </Popover>
          ) : null
        }
      />
      {off && (
        <div className="mt-auto border-t border-dialog-edge bg-panel-2 px-3 py-2">
          <p className="font-mono text-meta text-dialog-hint">
            Settings › <span className="text-white">Offer drafts</span> off: nothing asks about
            copies.
          </p>
        </div>
      )}
    </Screen>
  );
}

/* ------------------------------------------- H–J · the Switch project sheet */

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

function Crumbs({ trail }: { trail: string[] }) {
  return (
    <div className="flex items-center gap-1 overflow-hidden border-b border-dialog-edge bg-panel-2 px-3 py-2">
      <span className="shrink-0 font-mono text-meta text-dialog-hint">…</span>
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
 * H–J. The sheet behind `Switch project`. It commits a FOLDER; whether that folder
 * is a repo is the gateway's answer, not a question asked here.
 */
export function SwitchProjectVariant({ state }: { state: string }) {
  const list = (
    <Screen>
      <FleetList
        machineAction={(machine) => (
          <IconButton
            label={`Actions for ${machine.label}`}
            tone={machine.id === MACHINES[0].id ? 'accent' : 'plain'}
          >
            {KEBAB}
          </IconButton>
        )}
      />
    </Screen>
  );
  if (state === 'path') {
    return (
      <div className="relative h-full">
        {list}
        <BottomSheet
          title="Switch project · studio-mbp"
          footer={
            <SheetFooter path="/Users/fierycod/vis/apps/vis-companion" primary="Switch here" />
          }
        >
          <div className="flex items-center gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2">
            <span className="shrink-0 font-mono text-ui text-accent-ink">/</span>
            <span className="min-w-0 flex-1 truncate font-mono text-ui text-white">
              ~/vis/apps/vis-c
              <span className="text-dialog-hint">ompanion</span>
              <Caret />
            </span>
            <span className="shrink-0 border border-edge px-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
              tab
            </span>
          </div>
          <p className={`${BAND} ${QUIET_BAND}`}>2 matches</p>
          <Item title="~/vis/apps/vis-companion" hint="128 entries · main" badge="git" />
          <Item title="~/vis/apps/vis-companion/ios" hint="generated · not a repo" tone="muted" />
          <p className={`${BAND} ${QUIET_BAND}`}>already known here</p>
          <Item title="~/vis" hint="128 sessions" badge="project" />
          <Item title="~/tree-sitter-clojure" hint="8 sessions" badge="project" />
        </BottomSheet>
      </div>
    );
  }
  const creating = state === 'new-folder';
  return (
    <div className="relative h-full">
      {list}
      <BottomSheet
        title="Switch project · studio-mbp"
        footer={
          <SheetFooter
            path={creating ? '/Users/fierycod/code/band-repaint' : '/Users/fierycod'}
            primary={creating ? 'Create & switch' : 'Switch here'}
          />
        }
      >
        <Crumbs trail={creating ? ['fierycod', 'code'] : ['Users', 'fierycod']} />
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
      </BottomSheet>
    </div>
  );
}

/* --------------------------------------------------- K–L · the switch itself */

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

/** K–L. One switch for the account, or one switch per machine. */
export function DraftsToggleVariant({ state }: { state: string }) {
  if (state === 'machine') {
    return (
      <Screen>
        <div className={`${BAND} ${QUIET_BAND}`}>machines</div>
        <div className="flex items-center justify-between gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2">
          <MachineName machine={MACHINES[0]} className="font-mono text-ui font-bold text-white" />
          <span className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
            8ms · paired
          </span>
        </div>
        <SettingsRow
          title="Offer drafts"
          hint="Sessions on studio-mbp may run in a private copy of the project."
          on
        />
        <SettingsRow title="Machine name" hint="studio-mbp" />
        <SettingsRow title="Unpair" hint="Sessions stay on the machine." />
        <div className="flex items-center justify-between gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2">
          <MachineName machine={MACHINES[1]} className="font-mono text-ui font-bold text-white" />
          <span className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
            drafts off
          </span>
        </div>
        <div className="flex items-center justify-between gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2">
          <MachineName
            machine={MACHINES[2]}
            className="font-mono text-ui font-bold text-dialog-hint"
          />
          <span className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
            not answering
          </span>
        </div>
        <p className="px-3 py-2 font-mono text-meta text-dialog-hint">
          Three machines, three answers — and the one that is down cannot be asked.
        </p>
      </Screen>
    );
  }
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
        One answer for the whole fleet, on this device. A machine that cannot fork a folder still
        refuses — the switch decides the question, not the capability.
      </p>
    </Screen>
  );
}

/* ------------------------------------------------------------------- board */

export interface BoardOption {
  letter: string;
  title: string;
  /** What it costs and what it costs you — the sentence the choice is made on. */
  caption: string;
  variant: string;
  state: string;
}

export interface BoardSection {
  question: string;
  note: string;
  options: BoardOption[];
}

export const BOARD_SECTIONS: BoardSection[] = [
  {
    question: '1 · Where does "New session" live?',
    note: 'Solo (one machine paired) hides every machine header, so in B–D the button returns to the fleet bar — the fleet IS the machine.',
    options: [
      {
        letter: 'A',
        title: 'Shipped: one button, then "which machine?"',
        caption:
          'Today. Every session, on every machine, pays the same first tap — including the machine that is not answering.',
        variant: 'place-new-session',
        state: 'shipped',
      },
      {
        letter: 'B',
        title: 'A labelled button on each machine',
        caption:
          'Where you tap IS the machine. Costs one button per header; a dead machine wears a disabled one instead of lying.',
        variant: 'place-new-session',
        state: 'machine-button',
      },
      {
        letter: 'C',
        title: 'Everything inside the machine ⋯',
        caption:
          'Nothing new on screen and one home for machine actions — but starting is never fewer than two taps.',
        variant: 'place-new-session',
        state: 'machine-dots',
      },
      {
        letter: 'D',
        title: '+ on the machine and on the project',
        caption:
          'Machine + asks which project; project + starts there at once. Fastest, four glyphs per screen, and hidden on a machine collapsed shut.',
        variant: 'place-new-session',
        state: 'row-plus',
      },
    ],
  },
  {
    question: '2 · How is a draft offered?',
    note: '`Offer drafts` off is the baseline: nothing below appears at all.',
    options: [
      {
        letter: 'E',
        title: 'Off: never asked',
        caption:
          'What the switch buys. One verb, no fork, no second dialog — and the whole menu is three lines.',
        variant: 'draft-offer',
        state: 'off',
      },
      {
        letter: 'F',
        title: 'A second verb in the menu',
        caption:
          'The fork is a choice of VERB, made before anything opens. Two entries that differ by four words, forever.',
        variant: 'draft-offer',
        state: 'verb',
      },
      {
        letter: 'G',
        title: 'A chip row in the destination sheet',
        caption:
          'One surface answers project and workspace; the parked draft is offered by name. Disappears whole when drafts are off.',
        variant: 'draft-offer',
        state: 'chip',
      },
    ],
  },
  {
    question: '3 · What does Switch project look like?',
    note: "A bottom sheet on the machine you tapped, browsing that machine's filesystem — down, back up the path, above ~ to /.",
    options: [
      {
        letter: 'H',
        title: 'Breadcrumb browser',
        caption:
          'The path is the control: tap an ancestor to climb, a folder to descend, and the footer commits where you stand.',
        variant: 'switch-project',
        state: 'breadcrumb',
      },
      {
        letter: 'I',
        title: 'Path field with completion',
        caption:
          'Type the destination; the list is a completion plus what this machine already knows. Instant on a keyboard, fiddly under a thumb.',
        variant: 'switch-project',
        state: 'path',
      },
      {
        letter: 'J',
        title: 'Creating a folder, inline',
        caption:
          'H, mid-creation: the new folder is a row in the list it will join, not a second dialog. Create & switch is one press.',
        variant: 'switch-project',
        state: 'new-folder',
      },
    ],
  },
  {
    question: '4 · Where does the "Offer drafts" switch live?',
    note: "The capability is always the gateway's (no repo, no draft). Only the QUESTION is being placed.",
    options: [
      {
        letter: 'K',
        title: 'App settings',
        caption:
          'One answer for the fleet, on this device. Found where every other preference is; a phone and a laptop can disagree.',
        variant: 'drafts-toggle',
        state: 'app',
      },
      {
        letter: 'L',
        title: 'Per machine',
        caption:
          'Travels with the gateway and matches how the work differs per box — at the price of a screen per machine and a fleet that disagrees with itself.',
        variant: 'drafts-toggle',
        state: 'machine',
      },
    ],
  },
];

function renderOption(option: BoardOption) {
  if (option.variant === 'place-new-session')
    return <PlaceNewSessionVariant state={option.state} />;
  if (option.variant === 'draft-offer') return <DraftOfferVariant state={option.state} />;
  if (option.variant === 'switch-project') return <SwitchProjectVariant state={option.state} />;
  return <DraftsToggleVariant state={option.state} />;
}

function BoardCard({ option }: { option: BoardOption }) {
  return (
    <figure className="flex w-[390px] flex-col border border-dialog-edge bg-panel-2">
      <figcaption className="flex min-h-[6.5rem] flex-col border-b border-dialog-edge px-3 py-2">
        <p className="flex items-baseline gap-2">
          <span className="shrink-0 bg-accent px-1.5 font-mono text-ui font-bold text-accent-foreground">
            {option.letter}
          </span>
          <span className="min-w-0 font-mono text-ui font-bold text-white">{option.title}</span>
        </p>
        <p className="mt-1 font-mono text-meta text-dialog-hint">{option.caption}</p>
      </figcaption>
      <div className="h-[42rem] w-full overflow-hidden">{renderOption(option)}</div>
    </figure>
  );
}

/**
 * Every open question on one sheet of paper. The board is the deliverable: a
 * proposal compared against a memory of another proposal is not compared at all.
 */
export function SessionUxBoardVariant() {
  return (
    <section className="mx-auto flex w-full max-w-[1680px] flex-col gap-6 p-6">
      <header>
        <h1 className="font-mono text-head font-bold text-white">
          Machine, project, draft — the open questions
        </h1>
        <p className="mt-1 font-mono text-meta text-dialog-hint">
          Settled already: the project ⋯ is unchanged, the machine gets its own ⋯, Switch project is
          its name, and it opens a bottom sheet over the machine's own filesystem.
          (`apps/vis-companion/SESSION-UX.md`)
        </p>
      </header>
      {BOARD_SECTIONS.map((section) => (
        <section key={section.question} className="flex flex-col gap-3">
          <div className="border-l-4 border-accent bg-panel-2 px-3 py-2">
            <p className="font-mono text-subhead font-bold text-white">{section.question}</p>
            <p className="mt-0.5 font-mono text-meta text-dialog-hint">{section.note}</p>
          </div>
          <div className="flex flex-wrap items-start gap-4">
            {section.options.map((option) => (
              <BoardCard key={option.letter} option={option} />
            ))}
          </div>
        </section>
      ))}
    </section>
  );
}
