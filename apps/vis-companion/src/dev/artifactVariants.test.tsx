import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import gallerySource from './DesignGallery.tsx?raw';
import {
  ARTIFACT_STATES,
  ARTIFACTS,
  ArtifactsDockVariant,
  ArtifactsSheetVariant,
  ArtifactsTurnsVariant,
  artifactsFor,
} from './artifactVariants';

/**
 * The gallery is production code, and these three proposals only mean something
 * if the fixture can falsify them: a session that produced NOTHING (the control
 * has to disappear), and one whose whole output is documents and logs (nothing
 * to thumbnail). Two "different" states that render the same markup produce
 * byte-identical PNGs and a reviewer who thinks he compared something.
 */
describe('artifacts design fixtures', () => {
  it('registers every photographed proposal in the gallery', () => {
    // Importing the gallery module drags the whole app in, so the registry is
    // read as text: an unregistered variant never reaches a screenshot.
    for (const [id, states] of Object.entries(ARTIFACT_STATES)) {
      expect(gallerySource, id).toContain(`id: '${id}'`);
      expect(gallerySource, id).toContain(`states: ARTIFACT_STATES['${id}']`);
      expect(new Set(states).size, id).toBe(states.length);
    }
  });

  it('photographs the state where the feature must cost nothing', () => {
    for (const [id, states] of Object.entries(ARTIFACT_STATES)) {
      expect(states, id).toContain('empty');
      expect(states, id).toContain('docs');
    }
    expect(artifactsFor('empty')).toEqual([]);
  });

  it('gives the docs state nothing to thumbnail', () => {
    const docs = artifactsFor('docs');
    expect(docs.length).toBeGreaterThan(1);
    expect(
      docs.some((entry) => entry.kind === 'image' || entry.kind === 'video'),
    ).toBe(false);
    expect(docs.some((entry) => entry.kind === 'doc')).toBe(true);
    expect(docs.some((entry) => entry.kind === 'file')).toBe(true);
  });

  it('measures the default state at its widest', () => {
    const all = artifactsFor('default');
    // Two digits, a clip, a paged PDF and more than one turn: the layout is
    // photographed where it is hardest, not where it is prettiest.
    expect(all.length).toBeGreaterThan(9);
    expect(all.some((entry) => entry.kind === 'video')).toBe(true);
    expect(
      all.some((entry) => entry.kind === 'doc' && (entry.pages ?? 0) > 1),
    ).toBe(true);
    expect(new Set(all.map((entry) => entry.turn)).size).toBeGreaterThan(2);
  });

  it('keeps every artifact citable', () => {
    // The whole point of the index is provenance: what made it, when, how big.
    // A tile that cannot say that is a thumbnail, not an artifact.
    for (const entry of ARTIFACTS) {
      expect(entry.name, entry.name).toMatch(/\.[a-z0-9]+$/);
      expect(entry.tool, entry.name).toBeTruthy();
      expect(entry.turn, entry.name).toBeGreaterThan(0);
      expect(entry.size, entry.name).toMatch(/^[\d.]+ (kB|MB)$/);
    }
    // Newest first, so the sheet opens on what just appeared.
    const turns = ARTIFACTS.map((entry) => entry.turn);
    expect(turns).toEqual([...turns].sort((a, b) => b - a));
  });
});

/**
 * Driving all three proposals in a real browser found every artifacts control
 * painted as a decorated `<span>`: the chip, the ✕, the filter strip, the tiles
 * and the transcript jumps were unclickable, untabbable and silent to a screen
 * reader, and the accessibility snapshot of a full gallery listed three
 * controls — all of them app chrome. A proposal that cannot be operated is a
 * picture of a feature, so the markup itself is pinned here.
 */
const VARIANTS = {
  'artifacts-sheet': ArtifactsSheetVariant,
  'artifacts-turns': ArtifactsTurnsVariant,
  'artifacts-dock': ArtifactsDockVariant,
} as const;

const markup = (id: keyof typeof VARIANTS, state: string) => {
  const Variant = VARIANTS[id];
  return renderToStaticMarkup(<Variant state={state} />);
};

const count = (html: string, needle: string) => html.split(needle).length - 1;

describe('artifacts proposals are operable', () => {
  const ids = Object.keys(VARIANTS) as (keyof typeof VARIANTS)[];

  it('opens every artifact as a button that announces its provenance', () => {
    for (const id of ids) {
      const html = markup(id, 'default');
      for (const entry of artifactsFor('default')) {
        // The visible tile is a thumbnail with no text in it; this label is the
        // only thing a screen reader can read out.
        const label = `Open ${entry.name}, ${entry.media}, ${entry.size}, produced in turn ${entry.turn} by ${entry.tool}`;
        expect(html, `${id} / ${entry.name}`).toContain(
          `<button type="button" aria-label="${label}`,
        );
      }
    }
  });

  it('gives the repurposed slot a word, a count and a state', () => {
    for (const id of ids) {
      const html = markup(id, 'default');
      // On a phone the chip is only `▣ 12`, so the noun lives in the label.
      expect(html, id).toContain(
        'aria-label="12 artifacts produced by the model"',
      );
      expect(html, id).toContain('aria-expanded="true"');
      const controls = /aria-controls="([^"]+)"/.exec(html)?.[1] ?? '';
      expect(controls, id).not.toBe('');
      // Whatever the chip claims to own has to exist on the page.
      for (const target of controls.split(' ')) {
        expect(html, `${id} / ${target}`).toContain(`id="${target}"`);
      }
    }
  });

  it('names the way out of a surface that covers the transcript', () => {
    for (const id of ['artifacts-sheet', 'artifacts-turns'] as const) {
      expect(markup(id, 'default'), id).toContain(
        'aria-label="Close artifacts"',
      );
    }
    // C never covers anything, so it has nothing to dismiss.
    expect(markup('artifacts-dock', 'default')).not.toContain(
      'aria-label="Close artifacts"',
    );
  });

  it('says which filter is on, and disables the empty ones', () => {
    const html = markup('artifacts-sheet', 'default');
    expect(html).toContain('aria-label="Filter artifacts by kind"');
    expect(html).toContain('aria-label="All, 12 artifacts" class');
    expect(html).toContain('aria-pressed="true"');
    expect(html).toContain('aria-pressed="false"');
    // A session of documents offers Pictures, greyed, rather than silently
    // dropping a chip and reshaping the strip.
    const docs = markup('artifacts-sheet', 'docs');
    expect(docs).toContain('aria-label="Pictures, 0 artifacts"');
    expect(
      /disabled=""[^>]*aria-pressed="false" aria-label="Pictures/.test(docs),
    ).toBe(true);
  });

  it('answers "which run made this" once per turn', () => {
    const html = markup('artifacts-turns', 'default');
    const turns = new Set(artifactsFor('default').map((entry) => entry.turn));
    for (const turn of turns) {
      expect(html, `turn ${turn}`).toContain(
        `aria-label="Show turn ${turn} in the transcript"`,
      );
    }
    expect(count(html, 'in the transcript"')).toBe(turns.size);
    // Photographed at 1280px: turn 7 had no remembered question and the band
    // quoted nothing at all — two quote marks around empty space.
    expect(html).not.toContain('“”');
  });

  it('costs a session that produced nothing not one control', () => {
    for (const id of ids) {
      const html = markup(id, 'empty');
      expect(html, id).not.toContain('artifacts produced by the model');
      expect(html, id).not.toContain('aria-label="Open ');
      expect(html, id).not.toContain('role="region"');
      expect(html, id).not.toContain('aria-controls');
    }
  });
});

/**
 * Both of these were measured in an emulated iPhone/iPad, not reasoned about:
 * headless Chromium reports `pointer: fine` at every width, so the tablet only
 * tells the truth under real device emulation — and there the chip that had
 * inherited Share's `h-6` was a 40x24 target beside a 48px settings button.
 */
describe('artifacts density follows the pointer', () => {
  it('gives the chip a 44px box that only a cursor shrinks', () => {
    const chip =
      /<button[^>]*aria-label="12 artifacts produced by the model"[^>]*>/.exec(
        markup('artifacts-sheet', 'default'),
      )?.[0];
    expect(chip).toBeTruthy();
    expect(chip).toContain('min-h-11');
    // `▣ 12` alone is 40px wide on a phone, so the width is squared up too.
    expect(chip).toContain('min-w-11');
    expect(chip).toContain('mouse:min-h-6');
    expect(chip).toContain('mouse:min-w-0');
    // A width query may not shrink a hit box; only the pointer may.
    expect(chip).not.toMatch(/sm:min-h|sm:min-w|sm:h-/);
  });

  it('holds every tile inside its own cell', () => {
    // A <button> sizes to fit its content: without `w-full` the dock's 112px
    // filmstrip cells let the truncated name bleed 5-10px over the next tile.
    for (const tile of markup('artifacts-dock', 'default').match(
      /<button[^>]*aria-label="Open [^>]*>/g,
    ) ?? []) {
      // Both shapes of artifact row — the filmstrip tile and the column row —
      // fill their cell rather than their text.
      expect(tile).toContain('w-full');
    }
  });
});
