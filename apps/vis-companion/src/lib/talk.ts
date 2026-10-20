import { mergeAddresses, normalizeAddress } from './endpoints';
import type { GatewayConn } from './types';

/**
 * The Action Button link `vis://talk?machine=<url>&session=<new|id>&at=<nonce>`.
 *
 * The `Talk to Vis` App Intent opens it. `at` makes each press a new URL, so the
 * deep-link dedupe never drops a press. Each parsed link gets its own `nonce`.
 */
export interface TalkLink {
  machine?: string;
  /** A session id, or `new` for a new session. */
  session: string;
  nonce: string;
}

export function parseTalkLink(raw: string): TalkLink | null {
  const trimmed = (raw || '').trim();
  if (!/^vis:\/\/\/?talk(?:[/?#]|$)/i.test(trimmed)) return null;
  const query = new URLSearchParams(trimmed.split('?')[1]?.split('#')[0] ?? '');
  const machine = query.get('machine')?.trim() || undefined;
  const session = query.get('session')?.trim() || 'new';
  const nonce = query.get('at')?.trim() || String(Date.now());
  return { ...(machine ? { machine } : {}), session, nonce };
}

/** The machine of the link: the one with this address or id, else the fallback. */
export function talkConnection(
  link: TalkLink,
  conns: readonly GatewayConn[],
  fallback: GatewayConn | null,
): GatewayConn | null {
  if (link.machine) {
    const wanted = normalizeAddress(link.machine);
    const found = conns.find(
      (conn) =>
        conn.id === link.machine ||
        mergeAddresses([conn.url], conn.alts).some((url) => normalizeAddress(url) === wanted),
    );
    if (found) return found;
  }
  return fallback ?? conns[0] ?? null;
}
