// Regression for #381: the web app drew its shell like a desktop window. Window styling
// belongs to the desktop app (`data-host`, stamped by `lib/host.ts`), never to a browser tab.
import { readFileSync } from 'node:fs';
import { describe, expect, it } from 'vitest';

// Vitest stubs CSS imports, so read the shipped stylesheet itself.
const css = readFileSync(new URL('../index.css', import.meta.url), 'utf8');

describe('the host styles', () => {
  it('scope window styling to the desktop app', () => {
    expect(css).toMatch(/@custom-variant desktop \(&:where\(\[data-host='desktop'\]/);
    expect(css).toMatch(/@custom-variant web \(&:where\(\[data-host='web'\]/);
  });

  it('keep the web shell square and edge to edge', () => {
    const rule = css.match(/:root\[data-host='web'\][^{]*\{([^}]*)\}/);
    expect(rule?.[0]).toContain('[data-viewport-shell]');
    expect(rule?.[1]).toMatch(/border-radius: 0;/);
    expect(rule?.[1]).toMatch(/border: 0;/);
    expect(rule?.[1]).toMatch(/margin: 0;/);
  });
});
