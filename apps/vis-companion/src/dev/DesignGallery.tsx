/**
 * The design gallery: proposed screens painted in the REAL app shell, at a real
 * viewport, with the real theme — the GUI counterpart of the TUI capture journal.
 *
 * Route (dev server only): `#/__design` lists the proposals;
 * `#/__design?v=<variant>&state=<state>` renders exactly one, alone, so a
 * screenshot of the viewport IS the proposal. `scripts/design-shots.mjs` drives
 * this route with spel and writes one PNG per variant × state × viewport.
 *
 * Nothing here is imported by the app: `main.tsx` reaches it through a
 * `import.meta.env.DEV` branch, so a store build never contains it.
 */
import { useEffect, useLayoutEffect } from 'react';
import { Header, Shell, TabBar } from '../App';
import { BUNDLED_DARK, BUNDLED_LIGHT } from '../lib/palettes';
import { applyTheme } from '../lib/theme';
import { ChipHeaderVariant, FleetStripVariant, MachineFirstVariant } from './variants';
import { DataTableVariant } from './tableVariants';
import { TallyBadgesVariant, TallyHeaderVariant, TallyStripVariant } from './tallyVariants';
import { HUMAN_INPUT_STATES, HumanInputSheetVariant } from './humanInputVariants';
import {
  ARTIFACT_STATES,
  ArtifactsDockVariant,
  ArtifactsSheetVariant,
  ArtifactsTurnsVariant,
} from './artifactVariants';
import {
  MenuBandVariant,
  MenuCapVariant,
  MenuCrownVariant,
  MenuLiftVariant,
  MenuShippedVariant,
} from './menuVariants';
import {
  DeleteFooterVariant,
  DeleteKebabVariant,
  DeleteShippedVariant,
  DeleteTrashHeaderVariant,
} from './deleteVariants';
import {
  START_STATES,
  StartAskVariant,
  StartCommandVariant,
  StartRecentsVariant,
  StartRowVariant,
  StartSheetVariant,
  StartShippedVariant,
} from './startVariants';
import { PROJECT_STATES, SessionFlowVariant, SessionUxBoardVariant } from './projectVariants';


export interface DesignVariant {
  id: string;
  title: string;
  /** One line of intent — what this design claims, in the reviewer's language. */
  blurb: string;
  /** Named states worth their own screenshot (menu open, machine offline, ...). */
  states: string[];
  /**
   * `WIDTHxHEIGHT` when this proposal is only itself at one size — a comparison
   * board photographed at 390px is a column of clipped thumbnails. The page owns
   * the matrix, so it owns the viewport too.
   */
  viewport?: string;
  render: (state: string) => React.JSX.Element;
}

export const DESIGN_VARIANTS: DesignVariant[] = [
  {
    id: 'start-shipped',
    title: 'Start 0 · Shipped (three questions)',
    blurb:
      'Which machine, which workspace, what to call the draft — three surfaces and a keyboard before the one answer that matters, the first message, can be typed at all.',
    states: START_STATES['start-shipped'],
    render: () => <StartShippedVariant />,
  },
  {
    id: 'start-ask',
    title: 'Start A · Ask box (type first)',
    blurb:
      'New session opens the COMPOSER, not a question: the destination is one grey line you can tap, pre-answered from last time, and Send is what creates the session.',
    states: START_STATES['start-ask'],
    render: (state) => <StartAskVariant state={state} />,
  },
  {
    id: 'start-recents',
    title: 'Start B · Recent destinations',
    blurb:
      'The menu stops asking and starts offering: every row is a whole answer (machine + project + workspace) ranked by recency, so a repeat start is ONE tap.',
    states: START_STATES['start-recents'],
    render: (state) => <StartRecentsVariant state={state} />,
  },
  {
    id: 'start-row',
    title: 'Start C · The list is the picker',
    blurb:
      'No chooser at all: a + on every machine and project header, so WHERE you tap answers machine and project, and the workspace fork is a two-row press-menu.',
    states: START_STATES['start-row'],
    render: (state) => <StartRowVariant state={state} />,
  },
  {
    id: 'start-sheet',
    title: 'Start D · One sheet, nothing sequential',
    blurb:
      'The same three questions, all visible and all pre-answered, over the first-message box: never a wizard, and the draft name is an inline field instead of a second dialog.',
    states: START_STATES['start-sheet'],
    render: (state) => <StartSheetVariant state={state} />,
  },
  {
    id: 'start-command',
    title: 'Start E · The filter box starts sessions',
    blurb:
      'The control already at the top of the list becomes the command bar: type, and the first row is “Start · vis on studio-mbp”. Enter, and the prompt is already sent.',
    states: START_STATES['start-command'],
    render: () => <StartCommandVariant />,
  },
  {
    id: 'a-chip',
    title: 'A · Provenance chip',
    blurb:
      'One machine at a time, but the header finally says WHICH, and switching is one tap instead of four.',
    states: ['default', 'menu'],
    render: (state) => <ChipHeaderVariant state={state} />,
  },
  {
    id: 'b-fleet',
    title: 'B · One fleet list',
    blurb:
      'Every machine merges into one project-grouped list; the machine is a chip on the row and a filter, never a mode.',
    states: ['default', 'filtered', 'solo'],
    render: (state) => <FleetStripVariant state={state} />,
  },
  {
    id: 'c-machine-first',
    title: 'C · Machine first',
    blurb:
      'Machine › Project › Session. The fleet is legible at a glance, at the cost of one more level.',
    states: ['default'],
    render: () => <MachineFirstVariant />,
  },
  {
    id: 'table',
    title: 'Table · vis-table viewer',
    blurb:
      'An attached CSV is DATA: content-width columns, a # gutter that owns selection, an amber head you can find, blue numbers, NULL that is not an empty string, and a cell inspector.',
    states: ['default', 'sorted', 'rows', 'cell', 'blob', 'wide', 'tall', 'solo'],
    render: (state) => <DataTableVariant state={state} />,
  },
  {
    id: 'tally-header',
    title: 'Tally A · Header says it',
    blurb:
      'Live and unread are stated three times: the fleet line, the chip strip, and every machine header.',
    states: ['default', 'solo'],
    render: (state) => <TallyHeaderVariant state={state} />,
  },
  {
    id: 'tally-badges',
    title: 'Tally B · Two badges everywhere',
    blurb:
      'The shipped pair of filled blocks, repeated in every machine header, with the fleet line always silent.',
    states: ['default', 'solo'],
    render: (state) => <TallyBadgesVariant state={state} />,
  },
  {
    id: 'tally-strip',
    title: 'Tally C · The strip is the tally (shipped)',
    blurb:
      'Chips own both counts as filled blocks while the strip exists; with one machine paired the header line takes them back.',
    states: ['default', 'solo'],
    render: (state) => <TallyStripVariant state={state} />,
  },
  {
    id: 'menu-shipped',
    title: 'Menu 0 · Shipped (grey)',
    blurb:
      'The question a session cannot be created without answering is typed in hint grey on the same paper as the rows under it.',
    states: ['create', 'start', 'offline'],
    render: (state) => <MenuShippedVariant state={state} />,
  },
  {
    id: 'menu-cap',
    title: 'Menu A · Amber cap',
    blurb:
      'A 3px Blockether rule seals the TOP edge and the question takes amber ink: the menu gets a lid, the rows keep the quiet.',
    states: ['create', 'start', 'offline'],
    render: (state) => <MenuCapVariant state={state} />,
  },
  {
    id: 'menu-band',
    title: 'Menu B · Amber title band',
    blurb:
      'The question becomes a filled Blockether band — the dialog title bar in yellow — and every later band stays grey, so the menu has exactly one head.',
    states: ['create', 'start', 'offline'],
    render: (state) => <MenuBandVariant state={state} />,
  },
  {
    id: 'menu-lift',
    title: 'Menu C · Dialog chrome, amber lifted',
    blurb:
      'The dialog title bar itself, with the amber slab thrown UPWARD instead of down: the menu hangs from its own yellow.',
    states: ['create', 'start', 'offline'],
    render: (state) => <MenuLiftVariant state={state} />,
  },
  {
    id: 'menu-crown',
    title: 'Menu D · Crown (band, nothing underneath)',
    blurb:
      'B with the drop slab gone neutral: Blockether yellow is spent exactly once, on the top edge and the question, and never below the menu.',
    states: ['create', 'start', 'offline'],
    render: (state) => <MenuCrownVariant state={state} />,
  },
  {
    id: 'delete-shipped',
    title: 'Delete 0 · Shipped (bare ✕)',
    blurb:
      'The only place in the app where "delete" is drawn as a ✕: it reads as "close", matches nothing else, and a control that erases 40 looks identical to one that erases 1.',
    states: ['default', 'confirm'],
    render: (state) => <DeleteShippedVariant state={state} />,
  },
  {
    id: 'delete-trash',
    title: 'Delete A · Trash + label',
    blurb:
      'The same TrashIcon the row swipe-delete uses, with "Delete all N" spelled out. Correct semantics, and the count arrives before you commit.',
    states: ['default', 'confirm'],
    render: (state) => <DeleteTrashHeaderVariant state={state} />,
  },
  {
    id: 'delete-footer',
    title: 'Delete B · Danger footer',
    blurb:
      'No control in the persistent header at all. Open the group and the blast radius — "Delete all N sessions" — is a full-width action that can never be read as "close".',
    states: ['collapsed', 'open'],
    render: (state) => <DeleteFooterVariant state={state} />,
  },
  {
    id: 'delete-kebab',
    title: 'Delete C · Overflow (⋯)',
    blurb:
      'The destructive action leaves the header entirely and lives behind a kebab menu, so a thumb cannot reach it by accident.',
    states: ['default', 'open'],
    render: (state) => <DeleteKebabVariant state={state} />,
  },
  {
    id: 'human-input',
    title: 'Human input · The run stopped to ask you',
    blurb:
      'A parked run is a bottom sheet on a phone and a card on a desk; the question scrolls, the two buttons that end the pause never do, and a pause with no way out never offers a Cancel.',
    states: HUMAN_INPUT_STATES,
    render: (state) => <HumanInputSheetVariant state={state} />,
  },
  {
    id: 'artifacts-sheet',
    title: 'Artifacts A · Grid sheet',
    blurb:
      'Share leaves the header and ARTIFACTS takes the slot: one chip with a count, one sheet over the transcript, every figure/PDF/clip the model produced as a tile filtered by kind.',
    states: ARTIFACT_STATES['artifacts-sheet'],
    render: (state) => <ArtifactsSheetVariant state={state} />,
  },
  {
    id: 'artifacts-turns',
    title: 'Artifacts B · Indexed by turn',
    blurb:
      'The same sheet, ordered the way the session happened: one band per turn with the tool that made it, so an artifact is never separated from the question it answered.',
    states: ARTIFACT_STATES['artifacts-turns'],
    render: (state) => <ArtifactsTurnsVariant state={state} />,
  },
  {
    id: 'artifacts-dock',
    title: 'Artifacts C · Dock beside the transcript',
    blurb:
      'No overlay: the chip pins a filmstrip under the header on a phone and a column beside the transcript on a desk, so the reading never stops to look at what was produced.',
    states: ARTIFACT_STATES['artifacts-dock'],
    render: (state) => <ArtifactsDockVariant state={state} />,
  },
  {
    id: 'session-ux-board',
    title: 'Board · Machine, project, draft — the chosen flow',
    blurb:
      'The deliverable, answered: 1C the machine ⋯ owns every verb, 2F the draft is a second one, 3H Switch project browses (or types) the filesystem, 4K one app switch — plus the solo falsifier.',
    states: PROJECT_STATES['session-ux-board'],
    viewport: '1728x1840',
    render: () => <SessionUxBoardVariant />,
  },
  {
    id: 'session-flow',
    title: 'Machine · The chosen flow, step by step',
    blurb:
      'Every machine verb in the machine’s own ⋯, the draft as a second verb there, Switch project as a filesystem sheet you can also type a path into, and one app switch behind all of it.',
    states: PROJECT_STATES['session-flow'],
    render: (state) => <SessionFlowVariant state={state} />,
  },
];

const noop = () => {};

/** Parses `#/__design?v=…&state=…&theme=light|dark`. */
export function designRoute(hash: string): {
  variant: string | null;
  state: string;
  theme: 'light' | 'dark';
} {
  const query = hash.slice(hash.indexOf('?') + 1);
  const params = new URLSearchParams(hash.includes('?') ? query : '');
  return {
    variant: params.get('v'),
    state: params.get('state') ?? 'default',
    theme: params.get('theme') === 'dark' ? 'dark' : 'light',
  };
}

/**
 * A proposal is only reviewed when it has been seen in BOTH shipped palettes:
 * the same amber that reads as an accent on paper is a flare on ink. Layout
 * effect, so the palette is on the root before the ready flag lets the camera
 * fire.
 */
function usePalette(theme: 'light' | 'dark') {
  useLayoutEffect(() => {
    const palette = theme === 'dark' ? BUNDLED_DARK : BUNDLED_LIGHT;
    applyTheme({ ...palette, css_vars: palette.css_vars ?? {} });
  }, [theme]);
}

/**
 * A screenshot taken before the webfont lands renders the whole list in a
 * fallback face and every measurement in it is a lie. The flag flips only after
 * fonts are ready AND the browser has painted, so `spel wait --fn` is exact.
 */
function useShotReady() {
  useEffect(() => {
    let cancelled = false;
    const flags = window as unknown as {
      __designReady?: boolean;
      __designShots?: { id: string; state: string; viewport?: string }[];
    };
    // The gallery, not the capture script, owns what exists to photograph.
    flags.__designShots = DESIGN_VARIANTS.flatMap((entry) =>
      entry.states.map((state) => ({ id: entry.id, state, viewport: entry.viewport })),
    );
    const ready = () => {
      if (cancelled) return;
      requestAnimationFrame(() =>
        requestAnimationFrame(() => {
          if (cancelled) return;
          // A dialog does not arrive at full opacity: `DialogFrame` fades and
          // lifts over 200ms out of its `@starting-style`. Two frames in, the
          // camera catches a translucent frame over its own backdrop, and every
          // contrast judgement made on that photograph is wrong. Wait for the
          // finite animations to land; a spinner never finishes, so skip it.
          void Promise.allSettled(
            document
              .getAnimations()
              .filter((anim) => anim.effect?.getComputedTiming().iterations !== Infinity)
              .map((anim) => anim.finished),
          ).then(() => {
            if (!cancelled) flags.__designReady = true;
          });
        }),
      );
    };
    void document.fonts.ready.then(ready);
    return () => {
      cancelled = true;
      flags.__designReady = false;
    };
  }, []);
}

export function DesignGallery() {
  const { variant, state, theme } = designRoute(window.location.hash);
  usePalette(theme);
  useShotReady();

  const chosen = DESIGN_VARIANTS.find((entry) => entry.id === variant);
  return (
    <Shell>
      <Header tab="sessions" hasConn onTab={noop} onAppSettings={noop} />
      <main className="min-h-0 flex-1 overflow-x-hidden overflow-y-auto overscroll-contain">
        {chosen ? chosen.render(state) : <VariantIndex />}
      </main>
      <TabBar tab="sessions" onTab={noop} />
    </Shell>
  );
}

function VariantIndex() {
  return (
    <section aria-label="Design proposals" className="mx-auto w-full max-w-[1400px] p-3 sm:p-6">
      <p className="font-mono text-body font-bold text-white">Design proposals</p>
      <p className="mt-0.5 font-mono text-meta text-dialog-hint">
        Multi-gateway sessions list. Pick one; `npm run design:shots` captures them all.
      </p>
      <div className="mt-3 border border-dialog-edge bg-panel">
        {DESIGN_VARIANTS.flatMap((entry) =>
          entry.states.map((state) => (
            <a
              key={`${entry.id}-${state}`}
              href={`#/__design?v=${entry.id}&state=${state}`}
              className="flex min-h-11 items-center justify-between gap-3 border-b border-dialog-edge px-3 py-2 last:border-b-0 hover:bg-hover"
            >
              <span className="min-w-0">
                <span className="block truncate font-mono text-ui font-bold text-white">
                  {entry.title}
                  {state === 'default' ? '' : ` — ${state}`}
                </span>
                <span className="mt-0.5 block font-mono text-chip text-dialog-hint">
                  {entry.blurb}
                </span>
              </span>
              <span className="shrink-0 font-mono text-chip text-accent-ink">open ›</span>
            </a>
          )),
        )}
      </div>
    </section>
  );
}
