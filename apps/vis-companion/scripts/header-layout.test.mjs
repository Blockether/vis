import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// The app bar reads: brand at the far left, one long quiet stretch, two marks at the
// right edge — the glass that opens the search page and the cog that means PREFERENCES,
// this device's own settings. It used to carry a Sessions/Machines nav beside them (a
// whole navigation for a verb used twice a year, duplicated below the breakpoint by a
// tab bar) and then an always-open search box holding its whole middle. Both are gone.

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

/** The call-site opening tag of the control carrying `label="<label>"`. */
export function labelledTag(source, label) {
  const at = source.indexOf(`label="${label}"`);
  if (at < 0) throw new Error(`no control labelled ${label}`);
  const start = source.lastIndexOf('<', at);
  const end = source.indexOf('>', at);
  return source.slice(start, end + 1);
}

describe('app bar', () => {
  const cog = classes(labelledTag(app, 'Open preferences'));

  it('carries no navigation of its own', () => {
    expect(app).not.toContain('Primary navigation');
    expect(app).not.toContain('export function TabBar');
  });

  // Regression, user report ("just search icon that triggers full page search"): the
  // open field held the bar's whole middle at every width — the widest object on a
  // 390px phone, permanently, for a question that is asked in bursts. It is a MARK
  // now, beside the cog, and pressing it turns the screen into the search.
  it('spends the bar on two marks and no box', () => {
    const resting = app.slice(
      app.indexOf('aria-label="Vis"'),
      app.indexOf('</header>'),
    );
    expect(resting).not.toContain('<SearchField');
    expect(labelledTag(app, 'Search all machines')).toContain('<IconButton');
    expect(labelledTag(app, 'Open preferences')).toContain('<IconButton');
    expect(app).toContain('<div className="ml-auto flex h-12 items-center gap-2">');
    // Pairing is a twice-a-year verb; it stopped renting the bar.
    expect(app).not.toContain('aria-label="Pair a machine"');
  });

  // The search is a PAGE: the bar becomes a way back plus the field, the list under it
  // is the answer, and nothing else rides the bar while it is open. A fleet-wide query
  // is the screen, not a filter parked in a corner of it.
  it('turns the bar into the search page and hands it a way back', () => {
    const page = app.slice(
      app.indexOf('{isSearching ? ('),
      app.indexOf('aria-label="Vis"'),
    );
    expect(page).toContain('<BackButton label="Close search"');
    expect(page).toContain('label="Search sessions on every machine"');
    expect(page).toContain('placeholder="Search all machines…"');
    // The field is the app's own `SearchField`, so the call site may only POSITION it —
    // the face (box, paper at rest, the trailing ✕) belongs to the component.
    const field = classes(page.slice(page.indexOf('<SearchField')));
    expect(field.every((c) => ['ml-3', 'min-w-0', 'flex-1'].includes(c))).toBe(true);
    expect(field).toContain('flex-1');
  });

  // Opening a page that a human still has to tap into asks for the tap twice, and a
  // page that took the whole bar has to be leavable without aiming at a control.
  it('puts the caret in the page and takes Escape back out', () => {
    expect(app).toContain('searchRef.current?.focus()');
    expect(app).toContain("event.key === 'Escape'");
    expect(app).toContain("event.key !== '/'");
    // `/` never fires while someone is already typing somewhere else.
    expect(app).toContain("at.tagName === 'INPUT'");
  });

  // Leaving the page clears the query, so the list a human comes back to is the one
  // they left rather than a silently filtered copy of it.
  it('clears the query when the page closes', () => {
    const close = app.slice(
      app.indexOf('onCloseSearch={'),
      app.indexOf('onAppSettings={'),
    );
    expect(close).toContain('setSearching(false)');
    expect(close).toContain('setQuery("")');
  });

  // Reported earlier: the trailing controls sat at the wrong margin, a lone cog glyph
  // centred in a 48px cell while every `⋯` below it sat at 14px. A button is not
  // centred in a cell and does not bleed past the paper: it wears the bar's own gutter
  // and re-spells none of its own face at the call site.
  it('leaves both marks’ faces to the component', () => {
    expect(cog).toEqual([]);
    expect(classes(labelledTag(app, 'Search all machines'))).toEqual([]);
  });
});
