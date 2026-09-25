/**
 * The hues a session group can wear, and the class that fills each one.
 *
 * The palette is CLOSED and the backend owns all of it. The gateway contract
 * (`com.blockether.vis.contract.gateway/session-group-colors`) names the tokens,
 * `theme.clj` (`session-group-swatches`) gives each one its hue, and
 * `clojure -X:companion-themes` ships both as `group-colors.generated.ts` and the
 * `--color-group-*` block of `themes.generated.css`. A group carries a TOKEN rather
 * than a colour, so the TUI inks a terminal cell and this app fills a swatch from
 * the same value. Anything else — a newer gateway, a hand-written PATCH that got
 * through — reads as the default, so a band always has a swatch instead of a hole
 * where one belongs.
 */

import {
  DEFAULT_GROUP_COLOR,
  GROUP_COLORS,
  GROUP_SWATCHES,
  type SessionGroupColor,
} from './group-colors.generated';

export { DEFAULT_GROUP_COLOR, GROUP_COLORS, type SessionGroupColor };

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
  return GROUP_SWATCHES[groupColor(value)];
}
