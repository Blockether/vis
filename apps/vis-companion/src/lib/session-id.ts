/**
 * The clipboard form of a Vis session id.
 *
 * A bare UUID is anonymous: pasted into a chat, an issue or another agent's
 * prompt it could be any identifier at all. Every copy affordance — the TUI
 * header chip and this app's session-id `CopyChip` — stamps the marker in
 * front, so whoever reads it next recognises a Vis session and can hand it
 * straight to `read_session` / `get_session`, which strip the marker again.
 *
 * Mirrors `session-id-marker-prefix` in
 * `src/com/blockether/vis/internal/header.clj`; the two must stay identical.
 */
export const SESSION_ID_MARKER_PREFIX = 'vis_session_id#';

/** `vis_session_id#<uuid>`, or '' when there is no id worth copying. */
export function markSessionId(id: string | null | undefined): string {
  const trimmed = id?.trim();
  return trimmed ? `${SESSION_ID_MARKER_PREFIX}${trimmed}` : '';
}
