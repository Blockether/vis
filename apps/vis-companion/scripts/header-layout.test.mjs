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

  // Regression, user report ("PAIR SHOULD BE ON THE FUCKING HEADER"): pairing was a
  // chip at the end of the list's fleet strip, so it moved and changed shape with the
  // fleet. It is app chrome: it belongs beside the app's own cog, always in one place.
  it('pairs from the bar, immediately before the cog', () => {
    const pair = app.indexOf('aria-label="Pair a machine"');
    const preferences = app.indexOf('aria-label="Open preferences"');
    expect(pair).toBeGreaterThan(0);
    expect(pair).toBeLessThan(preferences);
    expect(classes(openingTag(app, 'Pair a machine'))).toContain('ml-auto');
  });

  it('hands the free space to the trailing cluster', () => {
    expect(classes(openingTag(app, 'Pair a machine'))).toContain('ml-auto');
    // Nothing else competes for that space now, so there is no breakpoint at which
    // the cog gives it up.
    expect(cog).not.toContain('sm:ml-0');
  });

  // Reported: the trailing controls sit at the wrong margin. On a phone the app bar
  // and the sessions list are one column down the right edge, but the list wears a
  // 2px frame (`border-r-2`) the bar does not, so a cog CENTRED in its 48px cell put
  // its ink 16px from the screen while every `⋯` and chevron below it sat at 14px.
  it('pads the cog to the list’s own content edge instead of centring it', () => {
    expect(cog).not.toContain('place-items-center');
    expect(cog).toContain('justify-items-end');
    // The list gutter (`pl-3`/`sm:pl-4`) plus the panel's own 2px frame.
    expect(cog).toContain('pr-3.5');
    expect(cog).toContain('sm:pr-4.5');
  });
});
