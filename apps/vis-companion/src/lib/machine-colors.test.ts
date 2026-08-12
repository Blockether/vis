import { describe, expect, it } from 'vitest';

import {
  MACHINE_COLORS,
  type MachineColor,
  assignMachineColors,
  machineColor,
  preferredColorIndex,
} from './machine-colors';

const keys = (count: number): string[] =>
  Array.from({ length: count }, (_, index) => `gateway-${index}`);

/** Where a colour sits in the ramp, so a test can measure the gap between two. */
const slot = (color: MachineColor | undefined): number => MACHINE_COLORS.indexOf(color!);

/** The gap two machines show, measured the short way round the wheel. */
const gap = (one: number, other: number): number => {
  const direct = Math.abs(one - other) % MACHINE_COLORS.length;
  return Math.min(direct, MACHINE_COLORS.length - direct);
};

/** The closest any two machines of an assignment come to each other. */
const closestPair = (assigned: Map<string, MachineColor>): number => {
  const slots = [...assigned.values()].map(slot);
  let closest = MACHINE_COLORS.length;
  for (let one = 0; one < slots.length; one += 1)
    for (let other = one + 1; other < slots.length; other += 1)
      closest = Math.min(closest, gap(slots[one]!, slots[other]!));
  return closest;
};

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

  // The colour is the machine's identity, and the first machine of the fleet
  // anchors it: gateways paired later spread around it, never the other way.
  it('keeps the first machine on the hue it hashed to', () => {
    const alone = assignMachineColors(['aa11']).get('aa11');
    const leading = assignMachineColors(['aa11', 'zz99', 'bb22']).get('aa11');

    expect(alone).toBe(MACHINE_COLORS[preferredColorIndex('aa11')]);
    expect(leading).toBe(alone);
  });

  // Two gateways whose URLs hashed to neighbouring slots — coral #d95445 and
  // orange #d26004 — each painted a red rail and a red block, and a fleet of two
  // looked like one machine listed twice.
  it('never lets two machines wear neighbouring hues', () => {
    expect(preferredColorIndex('http://10.0.0.3:7890')).toBe(11);
    expect(preferredColorIndex('http://10.0.0.8:7890')).toBe(12);

    const adjacent = assignMachineColors(['http://10.0.0.3:7890', 'http://10.0.0.8:7890']);

    expect(closestPair(adjacent)).toBe(MACHINE_COLORS.length / 2);
  });

  // A fleet spends the whole wheel: halves for two machines, thirds for three.
  it('spaces a fleet across the palette, not merely off each other', () => {
    for (let size = 2; size <= MACHINE_COLORS.length; size += 1) {
      const assigned = assignMachineColors(keys(size));

      expect(assigned.size).toBe(size);
      expect(closestPair(assigned)).toBeGreaterThanOrEqual(
        Math.floor(MACHINE_COLORS.length / size),
      );
    }
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
