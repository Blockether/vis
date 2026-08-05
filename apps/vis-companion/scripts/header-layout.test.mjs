import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// The desktop header reads: brand at the far left, then a long quiet stretch, then
// the two controls you actually reach for — the Sessions/Machines tabs and the cog —
// grouped together at the right edge. `mx-auto` centred the tabs in a 1400px header,
// so they floated in the middle of the bar, tied to nothing. Only ONE element may
// claim the free space: the nav takes it on desktop (`sm:ml-auto`, cog follows it),
// and the cog takes it below the breakpoint where the nav is not rendered at all.

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

describe('desktop header', () => {
  const nav = classes(openingTag(app, 'Primary navigation'));

  it('pushes the Sessions and Machines tabs to the right, beside the cog', () => {
    expect(nav).not.toContain('mx-auto');
    expect(nav.some((token) => /^(sm:|mouse:)?justify-center$/.test(token))).toBe(false);
    expect(nav).toContain('sm:ml-auto');
  });

  it('still hides that nav below the desktop breakpoint', () => {
    expect(nav).toContain('hidden');
    expect(nav).toContain('sm:flex');
  });

  it('hands the free space to the nav on desktop and to the cog without it', () => {
    const cog = classes(openingTag(app, 'Open application settings'));
    // Below `sm:` the nav is `hidden`, so its auto margin collapses and the cog is the
    // only thing left to hold the right edge; at `sm:` the nav has already taken the
    // space and the cog must sit flush against it instead of splitting it in two.
    expect(cog).toContain('ml-auto');
    expect(cog).toContain('sm:ml-0');
  });
});
