import { describe, expect, it } from 'vitest';
import gallerySource from './DesignGallery.tsx?raw';
import { RECENT_TARGETS, START_STATES } from './startVariants';
import { MACHINES } from './fleet';

/**
 * The gallery is production code, and a proposal about SPEED is only reviewable
 * against a fleet that can falsify it: a destination parked on the machine that
 * is down, the same project on two machines, and one state per photograph. Three
 * "different" states that render the same markup produce byte-identical PNGs and
 * a reviewer who thinks he compared something.
 */
describe('start-flow design fixtures', () => {
  it('registers every photographed proposal in the gallery', () => {
    // Importing the gallery module drags the whole app in, so the registry is
    // read as text: an unregistered variant never reaches a screenshot.
    const gallery = gallerySource;
    for (const id of Object.keys(START_STATES)) {
      expect(gallery, id).toContain(`id: '${id}'`);
      expect(gallery, id).toContain(`states: START_STATES['${id}']`);
    }
    for (const [id, states] of Object.entries(START_STATES)) {
      expect(new Set(states).size, id).toBe(states.length);
    }
  });

  it('offers a repeat start on more than one machine', () => {
    // A one-tap destination list is only honest when the fleet it ranks is a
    // fleet: a list that never crosses machines proves nothing about the fleet.
    const machines = new Set(RECENT_TARGETS.map((target) => target.machineId));
    expect(machines.size).toBeGreaterThan(1);
  });

  it('parks one destination on the machine that is not answering', () => {
    // The falsifier for B: an unreachable machine must not be offered as a tap.
    const dead = MACHINES.filter((machine) => machine.state !== 'online').map((m) => m.id);
    expect(dead.length).toBeGreaterThan(0);
    expect(RECENT_TARGETS.some((target) => dead.includes(target.machineId))).toBe(true);
  });

  it('covers every workspace answer the shipped menu asks for', () => {
    const kinds = new Set(RECENT_TARGETS.map((target) => target.workspace));
    expect(kinds).toEqual(new Set(['the project itself', 'a new draft', 'a parked draft']));
    // A resumed draft is named; the other kinds have nothing to name yet.
    for (const target of RECENT_TARGETS) {
      expect(Boolean(target.draft), target.id).toBe(target.workspace === 'a parked draft');
    }
  });
});
