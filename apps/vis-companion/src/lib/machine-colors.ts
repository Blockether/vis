/**
 * Stable, contrast-safe machine colours connect scope controls to owned rows. The
 * palette reserves green for live status and distributes active machines around the
 * hue wheel; names and outage treatment remain redundant non-colour cues.
 */

export interface MachineColor {
  /** Stable palette name — what tests and the design gallery call this hue. */
  readonly name: string;
  /** The 2px rail down the machine's block. */
  readonly rail: string;
  /** The same hue as a solid block, for the banner and the scope chip. */
  readonly dot: string;
}

/**
 * Class names are written out in full because Tailwind scans SOURCE TEXT: a
 * `border-machine-${name}` template would compile to no CSS at all.
 */
export const MACHINE_COLORS: readonly MachineColor[] = [
  { name: 'teal', rail: 'border-machine-teal', dot: 'bg-machine-teal' },
  { name: 'aqua', rail: 'border-machine-aqua', dot: 'bg-machine-aqua' },
  { name: 'cyan', rail: 'border-machine-cyan', dot: 'bg-machine-cyan' },
  { name: 'azure', rail: 'border-machine-azure', dot: 'bg-machine-azure' },
  { name: 'blue', rail: 'border-machine-blue', dot: 'bg-machine-blue' },
  { name: 'indigo', rail: 'border-machine-indigo', dot: 'bg-machine-indigo' },
  { name: 'violet', rail: 'border-machine-violet', dot: 'bg-machine-violet' },
  { name: 'purple', rail: 'border-machine-purple', dot: 'bg-machine-purple' },
  { name: 'orchid', rail: 'border-machine-orchid', dot: 'bg-machine-orchid' },
  { name: 'magenta', rail: 'border-machine-magenta', dot: 'bg-machine-magenta' },
  { name: 'rose', rail: 'border-machine-rose', dot: 'bg-machine-rose' },
  { name: 'coral', rail: 'border-machine-coral', dot: 'bg-machine-coral' },
  { name: 'orange', rail: 'border-machine-orange', dot: 'bg-machine-orange' },
  { name: 'bronze', rail: 'border-machine-bronze', dot: 'bg-machine-bronze' },
  { name: 'brass', rail: 'border-machine-brass', dot: 'bg-machine-brass' },
  { name: 'olive', rail: 'border-machine-olive', dot: 'bg-machine-olive' },
];

/** FNV-1a: tiny, dependency-free, and identical in every runtime the app ships in. */
function hashKey(key: string): number {
  let hash = 0x811c9dc5;
  for (let index = 0; index < key.length; index += 1) {
    hash ^= key.charCodeAt(index);
    hash = Math.imul(hash, 0x01000193) >>> 0;
  }
  return hash;
}

/**
 * The hue a machine WANTS: derived from its key alone, so the same gateway keeps
 * its colour across reloads, across reorderings, and on every device.
 */
export function preferredColorIndex(key: string): number {
  return hashKey(key) % MACHINE_COLORS.length;
}

/** Distance between two palette slots, the short way round the wheel. */
function paletteDistance(one: number, other: number): number {
  const direct = Math.abs(one - other) % MACHINE_COLORS.length;
  return Math.min(direct, MACHINE_COLORS.length - direct);
}

/**
 * The slots a fleet of this size stands on: evenly spread around the whole wheel
 * from the anchor. Merely "not the same slot" is not a boundary a phone can read
 * — neighbouring entries of the ramp are about 20° of hue apart, so a fleet of
 * two that hashed next to each other wore two reds. Spending the wheel instead
 * puts two machines opposite, three on thirds, and never leaves a pair closer
 * than `floor(16 / fleet)` steps.
 */
function fleetSlots(anchor: number, fleetSize: number): number[] {
  const size = Math.min(fleetSize, MACHINE_COLORS.length);
  return Array.from({ length: size }, (_, step) => {
    return (anchor + Math.round((step * MACHINE_COLORS.length) / size)) % MACHINE_COLORS.length;
  });
}

/**
 * Give every machine in one fleet a colour you can TELL APART, not merely a
 * different one. Machine keys are sorted first, making both the anchor and every
 * assignment independent of pairing or API order. The canonical first key keeps
 * the hue it hashed to; every other machine takes the free spread slot nearest
 * its own hue. A fleet wider than the palette falls back to each machine's
 * preferred hue, which is the only case where a hue repeats.
 */
export function assignMachineColors(keys: readonly string[]): Map<string, MachineColor> {
  const colors = new Map<string, MachineColor>();
  const fleet = [...new Set(keys)].sort();
  if (fleet.length === 0) return colors;
  const free = new Set(fleetSlots(preferredColorIndex(fleet[0]), fleet.length));
  for (const key of fleet) {
    const preferred = preferredColorIndex(key);
    let chosen = preferred;
    let nearest = MACHINE_COLORS.length;
    for (const slot of free) {
      const distance = paletteDistance(preferred, slot);
      if (distance < nearest) {
        nearest = distance;
        chosen = slot;
      }
    }
    free.delete(chosen);
    colors.set(key, MACHINE_COLORS[chosen]);
  }
  return colors;
}

/**
 * The colour of ONE machine, as a total function: a fleet map plus a key always
 * answers with a hue, so callers never branch on `undefined` and a machine that
 * arrived after the map was built still paints the hue it prefers.
 */
export function machineColor(colors: Map<string, MachineColor>, key: string): MachineColor {
  return colors.get(key) ?? MACHINE_COLORS[preferredColorIndex(key)];
}
