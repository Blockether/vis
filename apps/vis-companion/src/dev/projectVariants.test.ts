import { describe, expect, it } from 'vitest';
import gallerySource from './DesignGallery.tsx?raw';
import { FLOW_STEPS, PROJECT_STATES } from './projectVariants';

/**
 * The board is the deliverable, and a flow is only a flow when every card renders
 * the state it names: a card pointing at a state nobody wrote falls through to the
 * default and two "different" steps become the same photograph — which is exactly
 * how three identical PNGs shipped once before.
 */
describe('session flow design board', () => {
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

  it('gives every step a state its variant actually has', () => {
    for (const step of FLOW_STEPS) {
      expect(PROJECT_STATES[step.variant], step.step).toBeDefined();
      expect(PROJECT_STATES[step.variant], `${step.step} ${step.state}`).toContain(step.state);
    }
  });

  it('numbers the steps 1.. once each, in walking order', () => {
    const steps = FLOW_STEPS.map((step) => step.step);
    expect(steps.length).toBe(new Set(steps).size);
    // The flow is read as an order; a gap or a repeat makes a reference ambiguous.
    expect(steps).toEqual(steps.map((_, index) => String(index + 1)));
  });

  it('photographs every state of the flow somewhere on the board', () => {
    // A state nothing on the board shows is a screen nobody will look at.
    const shown = new Set(FLOW_STEPS.map((step) => `${step.variant}:${step.state}`));
    for (const [id, states] of Object.entries(PROJECT_STATES)) {
      if (id === 'session-ux-board') continue;
      for (const state of states) expect(shown, `${id}:${state}`).toContain(`${id}:${state}`);
    }
  });
});
