/**
 * Multi-gateway sessions-list PROPOSALS, painted with the shipped Tailwind
 * vocabulary so a screenshot is an honest preview and not a mockup in another
 * tool. Each variant answers the same question — "which machine is this work
 * on?" — with a different amount of chrome.
 *
 * DEV-ONLY: reachable at `#/__design` while `vite` runs; never imported by the
 * app itself. Promoting a variant means moving its markup into
 * `screens/SessionsScreen.tsx` and feeding it real sessions.
 */
import type { ReactNode } from 'react';
import {
  MACHINES,
  SESSIONS,
  byProject,
  liveCount,
  machineById,
  projectRoot,
  sessionsOf,
  unreadCount,
  type FleetMachine,
  type FleetSession,
} from './fleet';

const STATUS_TONE: Record<FleetSession['status'], string> = {
  LIVE: 'text-ok',
  WAITING: 'text-warn-strong',
  IDLE: 'text-dialog-hint',
};

const STATUS_DOT: Record<FleetSession['status'], string> = {
  LIVE: 'bg-ok',
  WAITING: 'bg-warn-strong',
  IDLE: 'bg-muted',
};

const MACHINE_DOT: Record<FleetMachine['state'], string> = {
  online: 'bg-ok',
  offline: 'bg-muted',
  auth: 'bg-warn-strong',
};

/** The one atom every proposal shares: a machine's name behind its health dot. */
function MachineDot({ machine }: { machine: FleetMachine }) {
  return (
    <span
      aria-hidden="true"
      className={`size-1.5 shrink-0 ${MACHINE_DOT[machine.state]} ${
        machine.state === 'online' ? 'animate-pulse motion-reduce:animate-none' : ''
      }`}
    />
  );
}

function StatusChip({ session }: { session: FleetSession }) {
  return (
    <span
      className={`inline-flex shrink-0 items-center gap-1 font-mono text-chip font-bold tracking-[0.08em] ${STATUS_TONE[session.status]}`}
    >
      <span
        className={`size-1.5 ${STATUS_DOT[session.status]} ${
          session.status === 'LIVE' ? 'animate-pulse motion-reduce:animate-none' : ''
        }`}
      />
      {session.status}
    </span>
  );
}

/**
 * One session row. `machine` is passed only where the design puts provenance on
 * the ROW; the machine-first variant omits it because its section already said so.
 */
function SessionRow({ session, machine }: { session: FleetSession; machine?: FleetMachine }) {
  return (
    <div className="flex items-stretch [&+&]:border-t [&+&]:border-dialog-edge">
      <span className="flex w-8 shrink-0 items-start justify-center pt-2.5 font-mono text-body text-accent-ink opacity-40 sm:w-9 sm:pt-2">
        {'\u203a'}
      </span>
      <span className="flex min-h-14 min-w-0 flex-1 flex-col py-2.5 pl-2 pr-3 sm:min-h-12 sm:py-2 sm:pr-4">
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
            <StatusChip session={session} />
          </span>
        </span>
        <span className="mt-1 flex flex-wrap items-center gap-x-2 gap-y-1 font-mono text-chip text-dialog-hint">
          {machine && (
            <>
              <span className="inline-flex items-center gap-1 text-white/70">
                <MachineDot machine={machine} />
                {machine.label}
              </span>
              <span className="opacity-40" aria-hidden="true">·</span>
            </>
          )}
          <span className="text-white/55">{session.id.slice(-6)}</span>
          <span className="opacity-40" aria-hidden="true">·</span>
          <span>{session.turns} turns</span>
          <span className="ml-auto shrink-0 pl-2">{session.ago}</span>
        </span>
      </span>
    </div>
  );
}

function ProjectSection({
  project,
  sessions,
  withMachine,
}: {
  project: string;
  sessions: FleetSession[];
  withMachine: boolean;
}) {
  const live = liveCount(sessions);
  return (
    <section className="border-t border-dialog-edge first:border-t-0">
      <header className="bg-panel-2">
        <div className="flex min-h-11 w-full items-center justify-between gap-3 px-3 py-2 sm:px-4">
          <span className="flex min-w-0 items-center gap-2">
            <span className="shrink-0 font-mono text-ui text-dialog-hint" aria-hidden="true">▾</span>
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
                <span className="opacity-40" aria-hidden="true">·</span>
                <span className="inline-flex items-center gap-1 font-bold text-ok">
                  <span className="size-1.5 animate-pulse bg-ok motion-reduce:animate-none" />
                  {live} live
                </span>
              </>
            )}
          </span>
        </div>
      </header>
      <div className="border-t border-dialog-edge">
        {sessions.map((session) => (
          <SessionRow
            key={session.id}
            session={session}
            machine={withMachine ? machineById(session.machineId) : undefined}
          />
        ))}
      </div>
    </section>
  );
}

/** The split "New session ▾" control the list already ships. */
function NewSessionButton({ suffix }: { suffix?: string }) {
  return (
    <div className="flex shrink-0 items-stretch">
      <span className="inline-flex min-h-6 items-center border border-accent bg-accent px-2 py-0.5 font-mono text-chip font-bold text-accent-foreground">
        New session{suffix ? ` on ${suffix}` : ''}
      </span>
      <span className="inline-flex min-h-6 items-center border border-l-accent-foreground/30 border-accent bg-accent px-2 py-0.5 font-mono text-chip font-bold text-accent-foreground">
        ▾
      </span>
    </div>
  );
}

function PanelFrame({ children }: { children: ReactNode }) {
  return (
    <section
      aria-label="Sessions"
      className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] sm:px-6 sm:pb-6 sm:pt-6"
    >
      <div className="flex h-full min-h-0 flex-col overflow-hidden border-b border-dialog-edge bg-panel sm:border">
        {children}
      </div>
    </section>
  );
}

function SearchRow() {
  return (
    <div className="flex min-h-10 items-center gap-2 border-y border-dialog-edge bg-panel px-3 sm:min-h-9 sm:px-4">
      <span className="shrink-0 font-mono text-ui text-accent-ink">›</span>
      <span className="font-mono text-meta text-dialog-hint">Filter sessions</span>
    </div>
  );
}

/* ------------------------------------------------------------------ variant A */

/**
 * A — PROVENANCE CHIP. The list stays single-machine; the header states which
 * machine it is and switches in one tap instead of four through Machines.
 * Cheapest honest fix; still a filing cabinet with a label on the drawer.
 */
export function ChipHeaderVariant({ state }: { state: string }) {
  const current = MACHINES[0];
  const sessions = sessionsOf(current.id);
  const menuOpen = state === 'menu';
  return (
    <PanelFrame>
      <div className="relative bg-panel-2 px-3 py-2.5 sm:px-4 sm:py-3">
        <div className="flex items-center justify-between gap-3">
          <div className="min-w-0">
            <p className="font-mono text-body font-bold text-white">Projects</p>
            <p className="mt-0.5 flex flex-wrap items-center gap-x-2 gap-y-0.5 font-mono text-meta text-dialog-hint">
              <span
                className={`inline-flex items-center gap-1.5 border px-1.5 py-0.5 font-bold text-white ${
                  menuOpen ? 'border-accent bg-hover' : 'border-edge'
                }`}
              >
                <MachineDot machine={current} />
                {current.label}
                <span aria-hidden="true" className="text-dialog-hint">▾</span>
              </span>
              <span className="whitespace-nowrap">
                {byProject(sessions).length} projects
                <span className="px-1 opacity-40">·</span>
                {sessions.length} sessions
              </span>
              <span className="whitespace-nowrap font-bold text-ok">● {liveCount(sessions)} live</span>
            </p>
          </div>
          <NewSessionButton />
        </div>
        {menuOpen && (
          <div className="absolute left-3 top-full z-20 w-64 border border-dialog-edge bg-panel-2 shadow-lg sm:left-4">
            <p className="border-b border-dialog-edge px-3 py-2 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
              Machines
            </p>
            {MACHINES.map((machine) => {
              const owned = sessionsOf(machine.id);
              const unread = unreadCount(owned);
              return (
                <div
                  key={machine.id}
                  className={`flex min-h-11 items-center justify-between gap-3 px-3 py-2 ${
                    machine.id === current.id ? 'bg-hover' : ''
                  }`}
                >
                  <span className="flex min-w-0 items-center gap-2">
                    <MachineDot machine={machine} />
                    <span className="truncate font-mono text-ui text-white">{machine.label}</span>
                  </span>
                  <span className="flex shrink-0 items-center gap-2 font-mono text-chip text-dialog-hint">
                    {machine.state === 'offline' ? (
                      <span>offline</span>
                    ) : (
                      <>
                        {liveCount(owned) > 0 && (
                          <span className="font-bold text-ok">{liveCount(owned)} live</span>
                        )}
                        {unread > 0 && <span className="font-bold text-accent-ink">{unread} unread</span>}
                      </>
                    )}
                    {machine.id === current.id && <span className="text-white">✓</span>}
                  </span>
                </div>
              );
            })}
          </div>
        )}
      </div>
      <SearchRow />
      <div className="min-h-0 flex-1 overflow-hidden">
        {byProject(sessions).map(([project, rows]) => (
          <ProjectSection key={project} project={project} sessions={rows} withMachine={false} />
        ))}
      </div>
    </PanelFrame>
  );
}

/* ------------------------------------------------------------------ variant B */

/**
 * B — ONE FLEET LIST. Every paired machine's sessions merge into one
 * project-grouped list; the machine is a chip on the row and the strip is a
 * FILTER, not a mode. With one machine paired the strip and chips do not render
 * at all, so the feature costs nothing until you own a second machine.
 */
export function FleetStripVariant({ state }: { state: string }) {
  // `solo` is the honest test of the idea: with one machine paired the strip,
  // the row chips and the machine count must all vanish.
  const solo = state === 'solo';
  const fleet = solo ? MACHINES.slice(0, 1) : MACHINES;
  const filtered = state === 'filtered';
  const active = filtered ? fleet[1] ?? null : null;
  const paired = SESSIONS.filter((s) => fleet.some((machine) => machine.id === s.machineId));
  const shown = active
    ? sessionsOf(active.id)
    : paired.filter((s) => machineById(s.machineId).state !== 'offline');
  // Counts follow the FILTER: a header that keeps quoting fleet totals while the
  // list below shows one machine is the lie that makes people distrust the chip.
  const scope = active ? sessionsOf(active.id) : paired;
  const offline = fleet.filter((machine) => machine.state === 'offline');
  return (
    <PanelFrame>
      <div className="bg-panel-2 px-3 py-2.5 sm:px-4 sm:py-3">
        <div className="flex items-center justify-between gap-3">
          <div className="min-w-0">
            <p className="font-mono text-body font-bold text-white">Projects</p>
            <p className="mt-0.5 flex flex-wrap items-center gap-x-3 gap-y-0.5 font-mono text-meta text-dialog-hint">
              {fleet.length > 1 && (
                <span className="whitespace-nowrap">{fleet.length} machines</span>
              )}
              <span className="whitespace-nowrap">
                {byProject(shown).length} projects
                <span className="px-1 opacity-40">·</span>
                {scope.length} sessions
              </span>
              <span className="whitespace-nowrap font-bold text-ok">● {liveCount(scope)} live</span>
              <span className="whitespace-nowrap font-bold text-accent-ink">
                {unreadCount(scope)} unread
              </span>
            </p>
          </div>
          <NewSessionButton />
        </div>
      </div>
      {/* The fleet strip. One machine paired -> this whole row is absent. */}
      {fleet.length > 1 && (
        <div className="flex items-center gap-1.5 overflow-x-auto border-t border-dialog-edge bg-panel px-3 py-1.5 sm:px-4">
          <span
            className={`inline-flex min-h-7 shrink-0 items-center gap-1.5 border px-2 font-mono text-chip ${
              active ? 'border-edge text-dialog-hint' : 'border-accent bg-hover font-bold text-white'
            }`}
          >
            All
            <span className="font-bold text-ok">{liveCount(paired)}●</span>
          </span>
          {fleet.map((machine) => {
            const owned = sessionsOf(machine.id);
            const isActive = active?.id === machine.id;
            return (
              <span
                key={machine.id}
                className={`inline-flex min-h-7 shrink-0 items-center gap-1.5 border px-2 font-mono text-chip ${
                  isActive ? 'border-accent bg-hover font-bold text-white' : 'border-edge text-dialog-hint'
                }`}
              >
                <MachineDot machine={machine} />
                {machine.label}
                {machine.state === 'offline' ? (
                  <span className="opacity-70">○</span>
                ) : (
                  liveCount(owned) > 0 && <span className="font-bold text-ok">{liveCount(owned)}●</span>
                )}
                {unreadCount(owned) > 0 && (
                  <span className="font-bold text-accent-ink">{unreadCount(owned)}</span>
                )}
              </span>
            );
          })}
        </div>
      )}
      <SearchRow />
      <div className="min-h-0 flex-1 overflow-hidden">
        {byProject(shown).map(([project, rows]) => (
          <ProjectSection
            key={project}
            project={project}
            sessions={rows}
            withMachine={!active && fleet.length > 1}
          />
        ))}
        {/* Degradation is a LINE, not a screen: an unreachable machine keeps its
            cached rows one tap away instead of throwing the tab back to Machines. */}
        {!active &&
          offline.map((machine) => (
            <div
              key={machine.id}
              className="flex items-center justify-between gap-3 border-t border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-chip text-dialog-hint sm:px-4"
            >
              <span className="flex min-w-0 items-center gap-2">
                <MachineDot machine={machine} />
                <span className="truncate">
                  {machine.label} offline · {sessionsOf(machine.id).length} sessions from cache
                </span>
              </span>
              <span className="shrink-0 border border-edge px-1.5 py-0.5 uppercase tracking-[0.08em] text-white">
                Retry
              </span>
            </div>
          ))}
      </div>
    </PanelFrame>
  );
}

/* ------------------------------------------------------------------ variant C */

/**
 * C — MACHINE FIRST. Machine › Project › Session, machines collapsed with
 * rollups. Legible fleet, but it charges every single-machine user one extra
 * level of depth and re-elevates the machine to a PLACE.
 */
export function MachineFirstVariant() {
  return (
    <PanelFrame>
      <div className="bg-panel-2 px-3 py-2.5 sm:px-4 sm:py-3">
        <div className="flex items-center justify-between gap-3">
          <div className="min-w-0">
            <p className="font-mono text-body font-bold text-white">Machines</p>
            <p className="mt-0.5 flex flex-wrap items-center gap-x-3 gap-y-0.5 font-mono text-meta text-dialog-hint">
              <span className="whitespace-nowrap">
                {MACHINES.length} machines
                <span className="px-1 opacity-40">·</span>
                {SESSIONS.length} sessions
              </span>
              <span className="whitespace-nowrap font-bold text-ok">● {liveCount(SESSIONS)} live</span>
            </p>
          </div>
          <NewSessionButton suffix="studio-mbp" />
        </div>
      </div>
      <SearchRow />
      <div className="min-h-0 flex-1 overflow-hidden">
        {MACHINES.map((machine, index) => {
          const owned = sessionsOf(machine.id);
          const expanded = index === 0;
          return (
            <section key={machine.id} className="border-t border-dialog-edge first:border-t-0">
              <div className="flex min-h-12 items-center justify-between gap-3 bg-panel-2 px-3 py-2 sm:px-4">
                <span className="flex min-w-0 items-center gap-2">
                  <span className="shrink-0 font-mono text-ui text-dialog-hint" aria-hidden="true">
                    {expanded ? '▾' : '▸'}
                  </span>
                  <MachineDot machine={machine} />
                  <span className="min-w-0">
                    <span className="block truncate font-mono text-ui font-bold text-white">
                      {machine.label}
                    </span>
                    <span className="mt-0.5 block truncate font-mono text-chip text-dialog-hint">
                      {machine.state === 'offline'
                        ? 'offline · cached'
                        : `${byProject(owned).length} projects · ${machine.latencyMs}ms`}
                    </span>
                  </span>
                </span>
                <span className="flex shrink-0 items-center gap-2 font-mono text-chip text-dialog-hint">
                  {liveCount(owned) > 0 && (
                    <span className="inline-flex items-center gap-1 font-bold text-ok">
                      <span className="size-1.5 animate-pulse bg-ok motion-reduce:animate-none" />
                      {liveCount(owned)} live
                    </span>
                  )}
                  {unreadCount(owned) > 0 && (
                    <span className="font-bold text-accent-ink">{unreadCount(owned)} unread</span>
                  )}
                </span>
              </div>
              {expanded && (
                <div className="border-t border-dialog-edge pl-3 sm:pl-4">
                  {byProject(owned).map(([project, rows]) => (
                    <ProjectSection key={project} project={project} sessions={rows} withMachine={false} />
                  ))}
                </div>
              )}
            </section>
          );
        })}
      </div>
    </PanelFrame>
  );
}
