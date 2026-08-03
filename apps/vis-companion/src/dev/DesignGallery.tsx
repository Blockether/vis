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

export interface DesignVariant {
  id: string;
  title: string;
  /** One line of intent — what this design claims, in the reviewer's language. */
  blurb: string;
  /** Named states worth their own screenshot (menu open, machine offline, ...). */
  states: string[];
  render: (state: string) => React.JSX.Element;
}

export const DESIGN_VARIANTS: DesignVariant[] = [
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
    blurb: 'Machine › Project › Session. The fleet is legible at a glance, at the cost of one more level.',
    states: ['default'],
    render: () => <MachineFirstVariant />,
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
      __designShots?: { id: string; state: string }[];
    };
    // The gallery, not the capture script, owns what exists to photograph.
    flags.__designShots = DESIGN_VARIANTS.flatMap((entry) =>
      entry.states.map((state) => ({ id: entry.id, state })),
    );
    const ready = () => {
      if (cancelled) return;
      requestAnimationFrame(() =>
        requestAnimationFrame(() => {
          if (!cancelled) flags.__designReady = true;
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
                <span className="mt-0.5 block font-mono text-chip text-dialog-hint">{entry.blurb}</span>
              </span>
              <span className="shrink-0 font-mono text-chip text-accent-ink">open ›</span>
            </a>
          )),
        )}
      </div>
    </section>
  );
}
