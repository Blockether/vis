import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// Regression: the THINKING band's ink said `italic` and the screen painted it
// perfectly upright, while the TUI slants the same reasoning. Nothing in the
// components was wrong — `font-synthesis: none` forbids the browser from
// faking a slant, and only the UPRIGHT axis of each variable family was ever
// imported, so there was no italic face to switch to and every `italic` in the
// app was a no-op. Fontsource's bare package entry is that upright axis alone.

const css = readFileSync(
  join(dirname(fileURLToPath(import.meta.url)), '..', 'src', 'index.css'),
  'utf8',
);

/** Every family the stylesheet pulls in, and whether its italic axis came too. */
const families = ['inter', 'jetbrains-mono'];

describe('web fonts', () => {
  it('synthesises nothing, so every slant must be a real face', () => {
    expect(css).toContain('font-synthesis: none');
  });

  it('imports the italic axis of every family it sets text in', () => {
    for (const family of families) {
      expect(css).toContain(`@import '@fontsource-variable/${family}';`);
      expect(css).toContain(
        `@import '@fontsource-variable/${family}/wght-italic.css';`,
      );
    }
  });
});
