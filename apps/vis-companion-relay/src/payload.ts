/**
 * How big a notification is allowed to be, decided HERE rather than by the
 * provider.
 *
 * Apple answers a payload over its limit with `PayloadTooLarge` and Google with
 * `INVALID_ARGUMENT`. Both are final verdicts — no retry improves them — so a
 * relay that learns the limit from the provider has spent a round trip to lose
 * an alert. It costs one `TextEncoder` pass to know beforehand.
 *
 * The two halves of a notification do not deserve the same answer. A title and
 * body are a PREVIEW that a lock screen truncates anyway, so a preview that
 * does not fit is trimmed into one that does. `data` is machine-readable and
 * may be ciphertext for the Notification Service Extension: half of it decrypts
 * to nothing, so it is never touched, and a `data` map that cannot fit is
 * refused whole instead of silently corrupted.
 */

import type { Notification } from "./types";

const encoder = new TextEncoder();

export function byteLength(value: string): number {
  return encoder.encode(value).length;
}

/**
 * The first `chars` characters, plus an ellipsis so the cut is visible. Never
 * ends on a lone high surrogate: the split half of an emoji encodes as U+FFFD,
 * which is both three wasted bytes and a visible box on the lock screen.
 */
function keep(value: string, chars: number): string {
  if (chars >= value.length) return value;
  if (chars <= 0) return "";
  let cut = value.slice(0, chars);
  if (/[\uD800-\uDBFF]$/.test(cut)) cut = cut.slice(0, -1);
  return `${cut.trimEnd()}\u2026`;
}

type Render = (notification: Notification) => string;

/**
 * The most of one text field that still renders inside `limit`, by bisection:
 * a 16 KiB body costs about fourteen renders instead of one per character, and
 * the answer accounts for JSON escaping because it measures the real payload.
 */
function shrink(
  notification: Notification,
  field: "body" | "title",
  render: Render,
  limit: number,
): Notification {
  const full = notification[field];
  let low = 0;
  let high = full.length;
  while (low < high) {
    const mid = Math.ceil((low + high) / 2);
    if (byteLength(render({ ...notification, [field]: keep(full, mid) })) <= limit) low = mid;
    else high = mid - 1;
  }
  return { ...notification, [field]: keep(full, low) };
}

/**
 * The same notification when it already fits, a trimmed one when its text is
 * what made it too big, and `null` when even a wordless notification does not
 * fit — at which point `data` is what does not fit, and the caller must hear so.
 */
export function fitNotification(
  notification: Notification,
  render: Render,
  limit: number,
): Notification | null {
  if (byteLength(render(notification)) <= limit) return notification;
  let fitted = notification;
  for (const field of ["body", "title"] as const) {
    fitted = shrink(fitted, field, render, limit);
    if (byteLength(render(fitted)) <= limit) return fitted;
  }
  return null;
}
