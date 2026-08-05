import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// The desktop header reads left to right: brand, then where you can GO, then the
// cog pinned to the far edge. `mx-auto` on the primary nav centred the Sessions
// and Machines tabs in a 1400px header, so on a desktop window they floated in
// the middle of the bar, detached from the wordmark they belong to. Navigation
// starts where the row starts; only the settings cog is allowed to claim the
// remaining space with `ml-auto`.

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

  it('keeps the Sessions and Machines tabs at the left, beside the wordmark', () => {
    expect(nav).not.toContain('mx-auto');
    expect(nav).not.toContain('ml-auto');
    expect(nav.some((token) => /^(sm:|mouse:)?justify-center$/.test(token))).toBe(false);
    expect(nav.some((token) => /^(sm:|mouse:)?ml-\d/.test(token))).toBe(true);
  });

  it('still hides that nav below the desktop breakpoint', () => {
    expect(nav).toContain('hidden');
    expect(nav).toContain('sm:flex');
  });

  it('leaves the settings cog as the only control that claims the free space', () => {
    expect(classes(openingTag(app, 'Open application settings'))).toContain('ml-auto');
  });
});
