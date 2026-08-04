/**
 * WHERE DOES THE BLOCKETHER YELLOW GO ON THE "CREATE THE SESSION ON" MENU?
 *
 * The start menu is the only place in the app that asks a question with
 * consequences — which computer, and which workspace, the agent is about to
 * start writing in — and it is painted entirely in `bg-panel-2` + `text-dialog-
 * hint`: a grey band over grey rows. A dialog gets the app's amber: a yellow
 * slab thrown BEHIND it, down and to the right (`--dialog-shadow`, 8px 8px).
 * This menu inherits that shadow but hangs UNDER the caret it came from, so the
 * only colour it owns sits below the fold of a phone sheet, where nobody looks.
 *
 * These proposals move the amber to the TOP EDGE — the first thing read, and on
 * a phone bottom sheet the only edge that is on screen at all. Each one has to
 * survive:
 *
 *   - `create`  the machine picker of the screenshot: one band, three rows.
 *   - `start`   the draft menu: TWO bands ("Start the session in" and "Or a
 *               draft you parked"). A treatment that shouts once is a barcode
 *               when it is charged twice — this is the falsifier.
 *   - `offline` no machine is answering: the band is the only content, so the
 *               colour must not promise something that cannot be tapped.
 *
 * The light palette is nearly flat (`--bg`, `--surface`, `--panel2` are all
 * `#faf3eb`), which is exactly why the amber has to be a FILL or a RULE, not a
 * tint; and `--primary` (#ffc420) is a fill that is unreadable as text, so a
 * yellow band carries `text-accent-foreground` ink, never amber-on-paper.
 *
 * DEV-ONLY: reachable at `#/__design`; nothing shipped imports this file.
 */

import type { ReactNode } from 'react';

/** How a proposal spends the amber. The rest of the menu is identical markup. */
interface MenuChrome {
  /** Frame border + wherever this proposal throws the yellow slab. */
  frame: string;
  /** A bar painted INSIDE the frame's top edge, above the first band. */
  cap: string | null;
  /** The band that asks the question. */
  head: string;
}

const SHIPPED_FRAME =
  'border-t border-dialog-edge sm:border sm:shadow-[8px_8px_0_var(--dialog-shadow)]';
const QUIET_HEAD = 'border-b border-dialog-edge bg-panel-2 text-dialog-hint';

const CHROME: Record<string, MenuChrome> = {
  // 0 — what ships today: the amber is the drop shadow, bottom-right, behind.
  shipped: { frame: SHIPPED_FRAME, cap: null, head: QUIET_HEAD },
  // A — a 4px amber rule capping the frame; the band only changes ink.
  cap: {
    frame: SHIPPED_FRAME,
    cap: 'h-1 bg-accent',
    head: 'border-b border-dialog-edge bg-panel-2 font-bold text-accent-ink',
  },
  // B — the band IS the colour: a filled Blockether-yellow title bar, closed by
  // the amber rule the data table uses for its head, and charged exactly once.
  band: {
    frame: SHIPPED_FRAME,
    cap: null,
    head: 'border-b-2 border-warn-strong bg-accent font-bold text-accent-foreground',
  },
  // C — dialog chrome, flipped: the dark title bar the app's dialogs wear, with
  // the yellow slab thrown UP instead of down, so the colour is above the menu.
  lift: {
    frame: 'border-t border-dialog-edge sm:border sm:shadow-[8px_-8px_0_var(--dialog-shadow)]',
    cap: null,
    head: 'border-b border-dialog-edge bg-dialog-title font-bold text-dialog-title-foreground',
  },
  // D — B, minus the yellow underneath: the slab goes NEUTRAL, so the menu
  // carries Blockether yellow exactly once, at the top, and nowhere else.
  crown: {
    frame: 'border-t-2 border-accent sm:border sm:border-dialog-edge sm:shadow-[8px_8px_0_var(--line2)]',
    cap: null,
    head: 'border-b-2 border-warn-strong bg-accent font-bold text-accent-foreground',
  },
};

interface MenuMachine {
  label: string;
  host: string;
  sessions: number;
  live: number;
}

/** Neutral placeholder gateways — a public repo never names a real deployment. */
const MENU_MACHINES: MenuMachine[] = [
  { label: 'studio-mbp', host: '10.0.0.5:7890', sessions: 781, live: 5 },
  { label: 'tower', host: '127.0.0.1:7890', sessions: 46, live: 1 },
  { label: 'visgw', host: 'gateway.example.com', sessions: 3, live: 0 },
];

const BAND = 'px-3 py-2 font-mono text-chip uppercase tracking-[0.08em]';

/** One row of the menu, at the shipped `StartOption`'s weight and rhythm. */
function Option({ title, hint, badge }: { title: string; hint: string; badge?: string }) {
  return (
    <div className="flex min-h-11 w-full items-start gap-2 border-b border-dialog-edge px-3 py-2 text-left">
      <span className="min-w-0 flex-1">
        <span className="block truncate font-mono text-ui font-bold text-white">{title}</span>
        <span className="mt-0.5 block font-mono text-meta text-dialog-hint">{hint}</span>
      </span>
      {badge && (
        <span className="mt-0.5 shrink-0 border border-edge px-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          {badge}
        </span>
      )}
    </div>
  );
}

/** The list the menu is opened on top of, so the amber is judged in context. */
function Backdrop() {
  return (
    <div className="absolute inset-0 flex flex-col overflow-hidden">
      <div className="flex items-center justify-between gap-3 border-b border-dialog-edge bg-panel-2 px-3 py-2.5 sm:px-4">
        <span className="font-mono text-body font-bold text-white">Projects</span>
        <span className="inline-flex items-center">
          <span className="border border-accent bg-accent px-2 py-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-accent-foreground">
            New session
          </span>
          <span className="border border-l-0 border-accent bg-panel px-1.5 py-1 font-mono text-chip text-white">
            ▾
          </span>
        </span>
      </div>
      {['vis', 'spel', 'clj-imaging'].map((project) => (
        <div key={project} className="border-b border-dialog-edge px-3 py-2.5 sm:px-4">
          <span className="block font-mono text-ui font-bold text-white">{project}</span>
          <span className="mt-0.5 block font-mono text-chip text-dialog-hint">
            ~/{project} · 12 sessions
          </span>
        </div>
      ))}
    </div>
  );
}

function MenuBody({ state }: { state: string }) {
  if (state === 'start') {
    return (
      <>
        <Option
          title="The project itself"
          hint="Edits land straight in the repo — no isolated copy."
          badge="Default"
        />
        <Option
          title="A new draft, with my uncommitted changes"
          hint="A private copy of this project exactly as it is now."
        />
        <Option
          title="A new draft, without my uncommitted changes"
          hint="A private copy of this project as of your last commit."
        />
        {/* The SECOND band. A proposal that survives here is charged twice in
            one menu; the amber must still read as one question, not a stripe. */}
        <p className={`${QUIET_HEAD} ${BAND} border-t`}>Or a draft you parked</p>
        <Option title="wire-rework" hint="forked 2h ago" badge="in use" />
        <Option title="tui-transient" hint="forked yesterday" />
      </>
    );
  }
  if (state === 'offline') {
    return (
      <p className="px-3 py-3 font-mono text-meta text-dialog-hint">
        No paired machine is answering right now.
      </p>
    );
  }
  return (
    <>
      {MENU_MACHINES.map((machine) => (
        <Option
          key={machine.label}
          title={machine.label}
          hint={`${machine.sessions} sessions · ${machine.host}`}
          badge={machine.live > 0 ? `${machine.live} live` : undefined}
        />
      ))}
    </>
  );
}

/**
 * The menu exactly where it opens: a thumb-reachable bottom sheet on a phone,
 * a popover pinned under the `New session` caret from `sm` up.
 */
function MenuFrame({ chrome, state }: { chrome: MenuChrome; state: string }) {
  const question = state === 'start' ? 'Start the session in · studio-mbp' : 'Create the session on';
  return (
    <div className="relative min-h-[36rem] flex-1 overflow-hidden bg-ink">
      <Backdrop />
      <div className="absolute inset-0 bg-black/40 sm:bg-transparent" />
      <div
        role="menu"
        aria-label={question}
        className={`absolute inset-x-0 bottom-0 max-h-[70vh] overflow-y-auto bg-panel sm:inset-x-auto sm:bottom-auto sm:left-auto sm:right-4 sm:top-14 sm:w-80 ${chrome.frame}`}
      >
        {chrome.cap && <div className={chrome.cap} aria-hidden="true" />}
        <p className={`${chrome.head} ${BAND}`}>{question}</p>
        <MenuBody state={state} />
      </div>
    </div>
  );
}

function Panel({ children }: { children: ReactNode }) {
  return (
    <section className="mx-auto flex w-full max-w-[900px] flex-col p-0 sm:p-6">{children}</section>
  );
}

export function MenuShippedVariant({ state }: { state: string }) {
  return (
    <Panel>
      <MenuFrame chrome={CHROME.shipped} state={state} />
    </Panel>
  );
}

export function MenuCapVariant({ state }: { state: string }) {
  return (
    <Panel>
      <MenuFrame chrome={CHROME.cap} state={state} />
    </Panel>
  );
}

export function MenuBandVariant({ state }: { state: string }) {
  return (
    <Panel>
      <MenuFrame chrome={CHROME.band} state={state} />
    </Panel>
  );
}

export function MenuLiftVariant({ state }: { state: string }) {
  return (
    <Panel>
      <MenuFrame chrome={CHROME.lift} state={state} />
    </Panel>
  );
}

export function MenuCrownVariant({ state }: { state: string }) {
  return (
    <Panel>
      <MenuFrame chrome={CHROME.crown} state={state} />
    </Panel>
  );
}
