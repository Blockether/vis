/**
 * WHAT DRAWS "DELETE EVERY SESSION IN THIS PROJECT"?
 *
 * The project-group header ships a bare ✕ next to its caret. ✕ means CLOSE
 * everywhere else in this app — collapsing a section, dismissing a dialog — so the
 * one place it means DESTROY is the one place it is most likely to be misread. It is
 * also the ONLY place "delete" is drawn as ✕: the row swipe already uses a trash can.
 *
 * These proposals answer the same question — "how do you offer a 40-session delete that
 * cannot be read as close and carries its own blast radius?" — with a different amount
 * of honesty. Each is painted in the shipped Tailwind vocabulary and the REAL dialog
 * chrome (`DialogFrame` / `Banner` / `Button`), so a screenshot is an honest preview.
 *
 * The fixture is one project that owns FORTY sessions, one of them LIVE: a bare ✕ that
 * deletes 1 and a bare ✕ that deletes 40 are drawn identically, and that is the bug.
 *
 * DEV-ONLY: reachable at `#/__design`; nothing shipped imports this file.
 */

import type { ReactNode } from 'react';
import { createPortal } from 'react-dom';
import { Banner, Button, DialogFrame } from '../components/ui';
import { liveCount, unreadCount, type FleetSession } from './fleet';

/** The exact trash glyph the row swipe ships, inlined so the dev file stands alone. */
function TrashIcon() {
  return (
    <svg
      viewBox="0 0 16 16"
      aria-hidden="true"
      className="size-4 fill-none stroke-current stroke-[1.4]"
    >
      <path d="M2.8 4.3h10.4M6.3 4.3V2.6h3.4v1.7M4.2 4.3l.7 9h6.2l.7-9" strokeLinejoin="round" />
      <path d="M6.6 6.6v4.4M9.4 6.6v4.4" />
    </svg>
  );
}

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

function SessionRow({ session }: { session: FleetSession }) {
  return (
    <div className="flex items-stretch [&+&]:border-t [&+&]:border-dialog-edge">
      <span className="flex w-8 shrink-0 items-start justify-center pt-2.5 font-mono text-body text-accent-ink opacity-40 sm:w-9 sm:pt-2">
        {'\u203a'}
      </span>
      <span className="flex min-h-14 min-w-0 flex-1 flex-col py-2.5 pl-2 pr-3 mouse:min-h-12 mouse:py-2 sm:pr-4">
        <span className="flex min-w-0 items-start justify-between gap-3">
          <span className="block min-w-0 truncate font-mono text-ui font-semibold text-white">
            {session.title}
          </span>
          <span className={`inline-flex shrink-0 items-center gap-1.5 font-mono text-chip font-bold tracking-[0.08em] ${STATUS_TONE[session.status]}`}>
            <span className={`size-1.5 ${STATUS_DOT[session.status]} ${session.status === 'LIVE' ? 'animate-pulse motion-reduce:animate-none' : ''}`} />
            {session.status}
          </span>
        </span>
        <span className="mt-1 flex flex-wrap items-center gap-x-2 gap-y-1 font-mono text-chip text-dialog-hint">
          <span className="text-white/55">{session.id.slice(-6)}</span>
          <span className="opacity-40" aria-hidden="true">·</span>
          <span>{session.turns} turns</span>
          <span className="ml-auto shrink-0 pl-2">{session.ago}</span>
        </span>
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
    <div className="flex min-h-10 items-center gap-2 border-y border-dialog-edge bg-panel px-3 mouse:min-h-9 sm:px-4">
      <span className="shrink-0 font-mono text-ui text-accent-ink">›</span>
      <span className="font-mono text-meta text-dialog-hint">Filter sessions</span>
    </div>
  );
}

/* --------------------------------------------------------------- the fixture */

const BULK_PROJECT = 'vis';
const BULK_ROOT = '~/vis';
const BULK_MACHINE = 'studio-mbp';
const TITLES = [
  'multi-gateway sessions',
  'push intent resolves by gateway id',
  'tui transient band chrome',
  'gateway wire total encoding',
  'fleet scope chip strip',
  'native image xmx tuning',
  'ruff pin bump',
  'feature toggle hydration',
  'confirm dialog copy',
  'swipe delete affordance',
  'screenshot capture journal',
  'session rename flow',
  'companion version mirror',
  'safe area insets',
  'tailwind v4 tokens',
  'react compiler lint',
  'design gallery index',
  'wire snake_case round trip',
  'grammar rev bump',
  'nightly benchmark sweep',
];
const AGOS = ['2m', '5m', '12m', '18m', '47m', '1h', '2h', '3h', '5h', '8h', '1d', '2d', '4d', '1w'];
const STATUSES: FleetSession['status'][] = ['IDLE', 'WAITING', 'LIVE'];

/** Forty sessions in one project, the first one LIVE — the blast radius the ✕ hides. */
function bulkSessions(count: number): FleetSession[] {
  return Array.from({ length: count }, (_, i) => ({
    id: `sess-bulk-${i.toString(16).padStart(4, '0')}`,
    title: TITLES[i % TITLES.length],
    project: BULK_PROJECT,
    root: BULK_ROOT,
    machineId: 'a1b2c3d4e5f60718',
    status: i === 0 ? 'LIVE' : STATUSES[i % STATUSES.length],
    turns: 3 + ((i * 7) % 60),
    unread: i % 6 === 0 ? (i % 3) + 1 : 0,
    ago: AGOS[i % AGOS.length],
  }));
}

const SESSIONS = bulkSessions(40);
const VISIBLE = 6;

/* --------------------------------------------------------- the four controls */

type Treatment = 'shipped' | 'trash-header' | 'footer' | 'kebab';

/**
 * The ✕ sits in the header for shipped / trash-header / kebab; the footer treatment
 * removes it entirely and pages the danger into the open group as a labeled action.
 */
function HeaderDelete({ treatment, n, menuOpen }: { treatment: Treatment; n: number; menuOpen: boolean }) {
  if (treatment === 'footer') return null;
  if (treatment === 'trash-header') {
    return (
      <button
        type="button"
        aria-label={`Delete ${BULK_PROJECT} with all ${n} of its sessions`}
        className="flex min-h-11 shrink-0 items-center gap-1.5 px-3 font-mono text-ui text-dialog-hint transition-colors duration-150 hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none motion-reduce:transition-none sm:px-4"
      >
        <TrashIcon />
        <span className="hidden sm:inline">Delete all</span>
      </button>
    );
  }
  if (treatment === 'kebab') {
    return (
      <div className="relative shrink-0">
        <button
          type="button"
          aria-label="Project actions"
          className="flex min-h-11 items-center justify-center px-3 font-mono text-ui text-dialog-hint transition-colors duration-150 hover:bg-hover hover:text-white focus-visible:bg-hover focus-visible:text-white focus-visible:outline-none motion-reduce:transition-none sm:px-4"
        >
          <span aria-hidden="true">⋯</span>
        </button>
        {menuOpen && (
          <div className="absolute right-0 top-full z-20 w-60 border border-dialog-edge bg-panel-2 shadow-lg">
            <button
              type="button"
              className="flex w-full items-center gap-2 px-3 py-2.5 text-left font-mono text-ui text-err transition-colors hover:bg-err/15 hover:text-white focus-visible:bg-err/15 focus-visible:outline-none"
            >
              <TrashIcon />
              Purge all {n} sessions
            </button>
          </div>
        )}
      </div>
    );
  }
  // shipped: the bare ✕, identical whether n is 1 or 40.
  return (
    <button
      type="button"
      aria-label={`Delete ${BULK_PROJECT} with all ${n} of its sessions`}
      className="flex min-h-11 shrink-0 items-center justify-center px-3 font-mono text-ui text-dialog-hint transition-colors duration-150 hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none motion-reduce:transition-none sm:px-4"
    >
      <span aria-hidden="true">✕</span>
    </button>
  );
}

/* ------------------------------------------------------------------ the panel */

function DeleteList({
  treatment,
  open,
  menuOpen,
  confirm,
}: {
  treatment: Treatment;
  open: boolean;
  menuOpen: boolean;
  confirm: boolean;
}) {
  const live = liveCount(SESSIONS);
  const rows = open ? SESSIONS.slice(0, VISIBLE) : SESSIONS.filter((s) => s.status === 'LIVE').slice(0, VISIBLE);
  const remaining = open ? SESSIONS.length - VISIBLE : 0;
  return (
    <PanelFrame>
      <div className="bg-panel-2 px-3 py-2.5 sm:px-4 sm:py-3">
        <div className="flex items-center justify-between gap-3">
          <div className="min-w-0">
            <p className="font-mono text-body font-bold text-white">Projects</p>
            <p className="mt-0.5 flex flex-wrap items-center gap-x-3 gap-y-0.5 font-mono text-meta text-dialog-hint">
              <span className="whitespace-nowrap">
                {SESSIONS.length} sessions
                <span className="px-1 opacity-40">·</span>
                {live} live
              </span>
              <span className="whitespace-nowrap font-bold text-accent-ink">{unreadCount(SESSIONS)} unread</span>
            </p>
          </div>
        </div>
      </div>
      <SearchRow />
      <div className="min-h-0 flex-1 overflow-hidden">
        <section className="border-t border-dialog-edge first:border-t-0" aria-label={`${BULK_PROJECT} sessions`}>
          <header className="relative flex items-stretch bg-panel-2">
            <button
              type="button"
              aria-expanded={open}
              className="flex min-h-11 min-w-0 flex-1 items-center justify-between gap-3 px-3 py-2 text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none sm:px-4"
            >
              <span className="flex min-w-0 items-center gap-2">
                <span className="shrink-0 font-mono text-ui text-dialog-hint" aria-hidden="true">
                  {open ? '▾' : '▸'}
                </span>
                <span className="min-w-0">
                  <span className="block truncate font-mono text-ui font-bold text-white">{BULK_PROJECT}</span>
                  <span className="mt-0.5 block truncate font-mono text-chip text-dialog-hint" title={BULK_ROOT}>
                    {BULK_ROOT}
                  </span>
                </span>
              </span>
              <span className="flex shrink-0 items-center gap-2 font-mono text-chip text-dialog-hint">
                <span>{SESSIONS.length} sessions</span>
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
            </button>
            <HeaderDelete treatment={treatment} n={SESSIONS.length} menuOpen={menuOpen} />
          </header>
          {open && rows.length > 0 && (
            <div className="border-t border-dialog-edge">
              {rows.map((session) => (
                <SessionRow key={session.id} session={session} />
              ))}
              {remaining > 0 && (
                <button
                  type="button"
                  className="flex w-full items-center justify-center gap-2 border-t border-dialog-edge px-3 py-2.5 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint transition-colors duration-150 hover:bg-hover hover:text-white focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none sm:px-4"
                >
                  Show {remaining} more
                </button>
              )}
              {/* The footer treatment: the only place "delete" lives, and it carries
                  its own blast radius, so it cannot be read as "close" and cannot hide 40. */}
              {treatment === 'footer' && (
                <button
                  type="button"
                  className="flex w-full items-center justify-center gap-2 border-t-2 border-dialog-edge px-3 py-3 font-mono text-ui font-bold uppercase tracking-[0.08em] text-err transition-colors duration-150 hover:bg-err/15 hover:text-white focus-visible:bg-err/15 focus-visible:text-white focus-visible:outline-none motion-reduce:transition-none sm:px-4"
                >
                  <TrashIcon />
                  Delete all {SESSIONS.length} sessions
                </button>
              )}
            </div>
          )}
        </section>
      </div>
      {confirm && createPortal(<ConfirmDialog count={SESSIONS.length} live={live} />, document.body)}
    </PanelFrame>
  );
}

/** The shipped confirmation modal, verbatim chrome: the count arrives only AFTER the tap. */
function ConfirmDialog({ count, live }: { count: number; live: number }) {
  return (
    <div
      className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 p-[max(1rem,env(safe-area-inset-top))] pb-[max(1rem,env(safe-area-inset-bottom))] pl-[max(1rem,env(safe-area-inset-left))] pr-[max(1rem,env(safe-area-inset-right))]"
      role="presentation"
    >
      <div className="w-full max-w-md" role="presentation">
        <DialogFrame title="Delete project" onClose={() => {}}>
          <div className="space-y-3 p-4">
            <p className="truncate font-mono text-meta text-dialog-hint">
              {BULK_PROJECT} · {BULK_MACHINE}
            </p>
            <p className="font-mono text-body text-white">
              Delete this project and all {count} sessions in it, with every transcript, from {BULK_MACHINE}? This cannot be undone.
            </p>
            {live > 0 && (
              <Banner kind="warn">
                {live === 1
                  ? 'One of them is running right now and will be stopped.'
                  : `${live} of them are running right now and will be stopped.`}
              </Banner>
            )}
            <div className="flex justify-end gap-2">
              <Button variant="ghost">Cancel</Button>
              <Button variant="danger">Delete</Button>
            </div>
          </div>
        </DialogFrame>
      </div>
    </div>
  );
}

/* ---------------------------------------------------------------- the variants */

/** 0 — SHIPPED: a bare ✕ that reads as "close" and hides its blast radius. */
export function DeleteShippedVariant({ state }: { state: string }) {
  return <DeleteList treatment="shipped" open={state !== 'confirm'} menuOpen={false} confirm={state === 'confirm'} />;
}

/** A — TRASH + LABEL in the header: correct semantics, consistent with the row swipe. */
export function DeleteTrashHeaderVariant({ state }: { state: string }) {
  return (
    <DeleteList
      treatment="trash-header"
      open={state !== 'confirm'}
      menuOpen={false}
      confirm={state === 'confirm'}
    />
  );
}

/** B — LABELED DANGER FOOTER: the control carries its count and lives inside the open
 *       group, so it is neither always-present nor confusable with "close".
 *       `collapsed` is the falsifier: the safer design costs discoverability — with the
 *       group shut, there is no delete affordance at all. */
export function DeleteFooterVariant({ state }: { state: string }) {
  return <DeleteList treatment="footer" open={state !== 'collapsed'} menuOpen={false} confirm={false} />;
}

/** C — KEBAB OVERFLOW: the destructive action is no longer a primary header glyph. */
export function DeleteKebabVariant({ state }: { state: string }) {
  return <DeleteList treatment="kebab" open menuOpen={state === 'open'} confirm={false} />;
}
