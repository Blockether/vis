/**
 * A machine is a COLOUR, not only a name.
 *
 * The sessions list is a fleet: machine → project → session. A name has to be
 * read, and on a phone it scrolls away; a colour is recognised before anything
 * is read, which is exactly what a boundary between two computers needs. Every
 * paired machine takes one hue from this palette and wears it in three places —
 * the rail down the left of everything it owns, the block in its banner, and the
 * block in its scope chip — so the chip you tapped and the rows you got back are
 * the same colour.
 *
 * The palette is 16 hues at ONE lightness (oklch L 0.62), which is what makes it
 * theme-proof: every entry clears 3:1 against the light page (#faf3eb) and 4.7:1
 * against the dark one (#0f1117), so a single hex per machine paints correctly on
 * paper AND on ink — no per-theme fork, no `light-dark()`, nothing for a gateway
 * palette to override. The green window (hue 130°–168°) is deliberately EMPTY:
 * green means LIVE on this screen, and a machine that happened to hash into green
 * would have been reporting a status it does not have.
 *
 * The palette is a RAMP around the wheel, so two entries beside each other —
 * coral #d95445 and orange #d26004 — read as one red. A fleet therefore never
 * takes neighbours: `assignMachineColors` spends the whole wheel on the machines
 * you actually have, so two paired gateways sit opposite each other and three
 * sit on thirds.
 *
 * Colour is redundancy, never the only cue: the machine's name is always next to
 * its block, and a machine that is not answering keeps its hue drained to an
 * outline — with the name, and the transport's own reason, in the tile's label and
 * title, where a 6px block cannot speak.
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
 * different one. The FIRST machine keeps the hue it hashed to and anchors the
 * fleet; every other machine takes the free spread slot nearest its own hue, so
 * the assignment is deterministic, independent of how the list is ordered after
 * the anchor, and as close to each machine's preference as spacing allows. A
 * fleet wider than the palette falls back to the hue each machine prefers, which
 * is the only case where a hue repeats.
 */
export function assignMachineColors(keys: readonly string[]): Map<string, MachineColor> {
  const colors = new Map<string, MachineColor>();
  const fleet = [...new Set(keys)];
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
