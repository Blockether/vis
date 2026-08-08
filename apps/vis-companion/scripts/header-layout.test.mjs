import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// The app bar reads: brand at the far left, one long quiet stretch, one control at
// the right edge. It used to carry a Sessions/Machines nav beside that control —
// a whole navigation for a verb (pairing) used twice a year, duplicated below the
// breakpoint by a tab bar. Both are gone: the fleet strip in the list pairs, and
// the single cog here means PREFERENCES, this device's own settings.

const app = readFileSync(
  join(dirname(fileURLToPath(import.meta.url)), '..', 'src', 'App.tsx'),
  'utf8',
);

/** The opening tag of the element carrying `aria-label="<label>"`. */
export function openingTag(source, label) {
  const at = source.indexOf(`aria-label="${label}"`);
  if (at < 0) throw new Error(`no element labelled ${label}`);
  const start = source.lastIndexOf('<', at);
  const end = source.indexOf('>', at);
  return source.slice(start, end + 1);
}

/** The literal class list of an opening tag. */
export function classes(tag) {
  const raw = /className="([^"]*)"/.exec(tag);
  return raw ? raw[1].split(/\s+/).filter(Boolean) : [];
}

describe('app bar', () => {
  const cog = classes(openingTag(app, 'Open preferences'));

  it('carries no navigation of its own', () => {
    expect(app).not.toContain('Primary navigation');
    expect(app).not.toContain('export function TabBar');
  });

  // Regression, user report ("search should be cross machine and it should be on top
  // in the header"): the field was the list's third row of chrome, directly under the
  // chip naming one machine. It is the bar's own middle now — nothing scopes it — and
  // it takes the free space, so the cog still holds the right edge.
  it('gives the bar\'s middle to a fleet-wide search', () => {
    const search = app.indexOf('label="Search sessions on every machine"');
    const preferences = app.indexOf('aria-label="Open preferences"');
    expect(search).toBeGreaterThan(0);
    expect(search).toBeLessThan(preferences);
    // The field is the app's own `SearchField` now, so the call site may only
    // POSITION it — the face (Button's box, paper at rest) belongs to the component.
    expect(app).toContain('<SearchField');
    expect(app).toContain('<SearchField');
    expect(app).toContain('className="mx-2 min-w-0 flex-1 sm:mx-3 sm:max-w-[32rem]"');
    // Pairing is a twice-a-year verb; it stopped renting the bar.
    expect(app).not.toContain('aria-label="Pair a machine"');
  });

  // Regression, user report ("the input is not looking sexy for iPhones"): the field
  // shared one 48px row with the wordmark and a word-button, so on a 390px phone the
  // screen's own verb was a hairline slab with barely 110px of usable width. Giving it
  // its OWN band below `sm:` was worse — a full-width framed slab under the bar — and
  // the report said so. Search belongs ON the bar at every width (every convention for
  // it puts it in the top bar, one line high); the width comes from the field taking
  // the bar's free space, and it is capped where the bar is wide so a focused field is
  // never a 1000px frame.
  it('keeps search on the bar’s one row at every width', () => {
    const bar = app.slice(app.indexOf('<header'), app.indexOf('aria-label="Vis"'));
    expect(bar).not.toContain('flex-wrap');
    expect(bar).not.toContain('pb-2');
    const search = app.slice(app.indexOf('<SearchField'), app.indexOf('ONE SCREEN, ONE COG'));
    expect(search).toContain('flex-1');
    expect(search).toContain('min-w-0');
    expect(search).not.toContain('w-full');
    expect(search).toMatch(/sm:max-w-\[/);
  });

  // Regression, user report ("make them nice buttons like the fucking New Session"):
  // the bar's two verbs were hand-rolled `<button className=…>` slabs of bare text.
  // They are the app's own `Button` now, so the CLUSTER holds the free space and the
  // component owns every metric — a call site may only position.
  it('hands the free space to the trailing cluster, not to a control', () => {
    expect(app).toContain('<div className="ml-auto flex h-12 items-center gap-2">');
    // Nothing else competes for that space now, so there is no breakpoint at which
    // the cluster gives it up.
    expect(cog).not.toContain('sm:ml-0');
  });

  // Reported earlier: the trailing controls sat at the wrong margin, a lone cog glyph
  // centred in a 48px cell while every `⋯` below it sat at 14px. A button is not
  // centred in a cell and does not bleed past the paper: it wears the bar's own gutter
  // and re-spells none of its own face at the call site.
  it('leaves the cog’s face to the component', () => {
    expect(cog).not.toContain('place-items-center');
    expect(cog).not.toContain('-mr-3');
    expect(cog.every((c) => ['shrink-0', 'whitespace-nowrap'].includes(c))).toBe(true);
  });
});
