/**
 * The eight hues a session group can wear, and the class that fills each one.
 *
 * The palette is CLOSED and the gateway keeps it
 * (`com.blockether.vis.contract.gateway/session-group-colors`): a group carries a
 * TOKEN rather than a colour, so the TUI inks a terminal cell and this app fills a
 * theme colour from the same value. Anything else — a newer gateway, a hand-written
 * PATCH that got through — reads as `slate`, so a band always has a swatch instead
 * of a hole where one belongs.
 *
 * Class names are written out in full because Tailwind scans SOURCE TEXT: a
 * `bg-group-${color}` template would compile to no CSS at all.
 */

/** Every token a group may carry, in the order a picker offers them. */
export const GROUP_COLORS = [
  'slate',
  'blue',
  'green',
  'amber',
  'red',
  'violet',
  'cyan',
  'pink',
] as const;

export type SessionGroupColor = (typeof GROUP_COLORS)[number];

/** What a group with nothing said about its colour wears — the gateway's default too. */
export const DEFAULT_GROUP_COLOR: SessionGroupColor = 'slate';

const SWATCHES: Record<SessionGroupColor, string> = {
  slate: 'bg-group-slate',
  blue: 'bg-group-blue',
  green: 'bg-group-green',
  amber: 'bg-group-amber',
  red: 'bg-group-red',
  violet: 'bg-group-violet',
  cyan: 'bg-group-cyan',
  pink: 'bg-group-pink',
};

/** Is this one of the palette's own tokens? */
export function isGroupColor(value: unknown): value is SessionGroupColor {
  return typeof value === 'string' && (GROUP_COLORS as readonly string[]).includes(value);
}

/** The token a wire value stands for, as a total function. */
export function groupColor(value: string | null | undefined): SessionGroupColor {
  return isGroupColor(value) ? value : DEFAULT_GROUP_COLOR;
}

/** The class that fills a group's swatch: its dot, and the rail beside its band. */
export function groupSwatch(value: string | null | undefined): string {
  return SWATCHES[groupColor(value)];
}
