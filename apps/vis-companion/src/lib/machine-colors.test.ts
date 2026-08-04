import { describe, expect, it } from 'vitest';

import {
  MACHINE_COLORS,
  assignMachineColors,
  machineColor,
  preferredColorIndex,
} from './machine-colors';

const keys = (count: number): string[] =>
  Array.from({ length: count }, (_, index) => `gateway-${index}`);

describe('MACHINE_COLORS', () => {
  it('is a palette wide enough that a real fleet never runs out', () => {
    expect(MACHINE_COLORS.length).toBeGreaterThanOrEqual(10);
    expect(MACHINE_COLORS.length).toBeLessThanOrEqual(20);
  });

  it('is 16 DIFFERENT hues — no two machines may wear the same rail', () => {
    expect(new Set(MACHINE_COLORS.map((color) => color.name)).size).toBe(MACHINE_COLORS.length);
    expect(new Set(MACHINE_COLORS.map((color) => color.rail)).size).toBe(MACHINE_COLORS.length);
    expect(new Set(MACHINE_COLORS.map((color) => color.dot)).size).toBe(MACHINE_COLORS.length);
  });

  // Tailwind compiles class names it finds in source; a template string would
  // silently ship a rail with no colour at all.
  it('spells its utilities out, rail as a border and block as a fill', () => {
    for (const color of MACHINE_COLORS) {
      expect(color.rail).toBe(`border-machine-${color.name}`);
      expect(color.dot).toBe(`bg-machine-${color.name}`);
    }
  });
});

describe('assignMachineColors', () => {
  it('gives every machine of a fleet its own colour', () => {
    const assigned = assignMachineColors(keys(MACHINE_COLORS.length));

    expect(assigned.size).toBe(MACHINE_COLORS.length);
    expect(new Set([...assigned.values()].map((color) => color.name)).size).toBe(
      MACHINE_COLORS.length,
    );
  });

  // The colour is the machine's identity: pairing another gateway, or the list
  // arriving in a different order, must not repaint the machine you know.
  it('keeps a machine on its own hue when the fleet around it changes', () => {
    const alone = assignMachineColors(['aa11']).get('aa11');
    const crowded = assignMachineColors(['zz99', 'aa11', 'bb22']).get('aa11');

    expect(alone).toBeDefined();
    expect(crowded).toBe(alone);
  });

  it('resolves a hash collision by taking the next free hue, not by sharing', () => {
    const collide = keys(200).filter(
      (key) => preferredColorIndex(key) === preferredColorIndex('gateway-0'),
    );
    expect(collide.length).toBeGreaterThan(1);

    const assigned = assignMachineColors(collide.slice(0, 2));
    const [first, second] = [...assigned.values()];
    expect(first).not.toBe(second);
  });

  it('still answers for a fleet bigger than the palette', () => {
    const assigned = assignMachineColors(keys(MACHINE_COLORS.length + 5));

    expect(assigned.size).toBe(MACHINE_COLORS.length + 5);
    for (const color of assigned.values()) expect(MACHINE_COLORS).toContain(color);
  });
});

describe('machineColor', () => {
  it('answers with the hue the fleet assigned', () => {
    const colors = assignMachineColors(['aa11', 'bb22']);

    expect(machineColor(colors, 'aa11')).toBe(colors.get('aa11'));
  });

  // A machine can arrive between the colour map and the render; a rail with no
  // colour would be a crash or an invisible boundary, so the lookup is total.
  it('answers for a machine the map has never seen', () => {
    const color = machineColor(new Map(), 'brand-new');

    expect(color).toBe(MACHINE_COLORS[preferredColorIndex('brand-new')]);
  });
});
