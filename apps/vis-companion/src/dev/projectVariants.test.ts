import { describe, expect, it } from 'vitest';
import gallerySource from './DesignGallery.tsx?raw';
import { BOARD_SECTIONS, PROJECT_STATES } from './projectVariants';

/**
 * The board is the deliverable, and a board is only a comparison when every
 * letter on it renders a state that exists: a card pointing at a state nobody
 * wrote falls through to the default and two "different" options become the same
 * photograph — which is exactly how three identical PNGs shipped once before.
 */
describe('machine-action design board', () => {
  it('registers every photographed proposal in the gallery', () => {
    // Importing the gallery module drags the whole app in, so the registry is
    // read as text: an unregistered variant never reaches a screenshot.
    for (const id of Object.keys(PROJECT_STATES)) {
      expect(gallerySource, id).toContain(`id: '${id}'`);
      expect(gallerySource, id).toContain(`states: PROJECT_STATES['${id}']`);
    }
    for (const [id, states] of Object.entries(PROJECT_STATES)) {
      expect(new Set(states).size, id).toBe(states.length);
    }
  });

  it('shoots the board at its own viewport, because 390px would clip it', () => {
    // The page owns the matrix, so it owns the size: `design-shots.mjs` reads
    // this back and skips the phone/desktop sweep for that one proposal.
    expect(gallerySource).toMatch(/id: 'session-ux-board',[\s\S]*?viewport: '\d+x\d+',/);
  });

  it('gives every board card a state its variant actually has', () => {
    for (const section of BOARD_SECTIONS) {
      for (const option of section.options) {
        expect(PROJECT_STATES[option.variant], option.letter).toBeDefined();
        expect(PROJECT_STATES[option.variant], `${option.letter} ${option.state}`).toContain(
          option.state,
        );
      }
    }
  });

  it('letters the options A.. once each, in reading order', () => {
    const letters = BOARD_SECTIONS.flatMap((section) => section.options.map((o) => o.letter));
    expect(letters.length).toBe(new Set(letters).size);
    // A reviewer answers with a letter; a gap or a repeat makes that answer ambiguous.
    expect(letters).toEqual(
      letters.map((_, index) => String.fromCharCode('A'.charCodeAt(0) + index)),
    );
  });

  it('photographs every state of every proposal somewhere', () => {
    // A state nothing on the board shows is a proposal nobody will look at.
    const shown = new Set(
      BOARD_SECTIONS.flatMap((section) =>
        section.options.map((option) => `${option.variant}:${option.state}`),
      ),
    );
    for (const [id, states] of Object.entries(PROJECT_STATES)) {
      if (id === 'session-ux-board') continue;
      for (const state of states) expect(shown, `${id}:${state}`).toContain(`${id}:${state}`);
    }
  });
});
