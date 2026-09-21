import type { Session, TranscriptTurn } from './types';

/**
 * The gateway owns "new". It counts the answers addressed to the human, remembers
 * how many of them this owner has read, and paints `is_unread` / `unread_answers`
 * on every session row it serves. Council coordination and subagent results never
 * advance that count.
 *
 * So nothing is stored here. This module reads what the row already says, and
 * measures how far the reader has got on the screen in front of them — which
 * `GatewayClient.markSessionRead` reports back. Every surface of the machine shows
 * the same badge, and reading a session on the phone clears it in the TUI too.
 */

/** Settled human answers, already filtered by the gateway. */
export function answeredTurnCount(session: Session | null | undefined): number {
  const count = Number(session?.answer_count ?? 0);
  return Number.isFinite(count) && count > 0 ? count : 0;
}

/**
 * Answers the reader can currently see, including a settled running-turn bubble whose
 * transcript row has not reached the gateway response yet.
 */
export function visibleAnsweredTurnCount(
  session: Session | null | undefined,
  turns: readonly TranscriptTurn[],
  runningTurnStatus: string | null | undefined,
  runningRequestKind: string | null | undefined = session?.running_request_kind,
): number {
  const settledTranscriptTurns = turns.filter(
    (turn) => turn.request_kind !== 'council' && !['running', 'pending', 'cancelled', 'interrupted'].includes(turn.status ?? ''),
  ).length;
  const settledRunningTurn = runningRequestKind !== 'council' && runningTurnStatus != null
    && !['running', 'pending', 'cancelled', 'interrupted'].includes(runningTurnStatus) ? 1 : 0;
  return Math.max(answeredTurnCount(session), settledTranscriptTurns + settledRunningTurn);
}

/**
 * How many unread answers a session is holding (1+ when unread), as the gateway
 * counted them. A session that is currently RUNNING never counts: a turn still in
 * flight has no answer yet, and flagging it would make the badge mean "busy"
 * instead of "unread".
 */
export function unreadTurnCount(session: Session): number {
  if (session.live || session.is_unread !== true) return 0;
  const unread = Number(session.unread_answers ?? 0);
  return Number.isFinite(unread) && unread > 0 ? unread : 0;
}
