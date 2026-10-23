import type { Session } from '../../lib/types';

/** One sidebar row and how deep it stands under the session that started it. */
export interface NestedSession {
  session: Session;
  depth: number;
}

/**
 * Put each subagent session right after the session that started it (`agent.parent_id`),
 * one step deeper. A child whose parent is not in `rows` (another page, a folded group)
 * keeps its own place at depth 0. Parents keep their order, and so do the children of one parent.
 */
export function nestAgentSessions(rows: readonly Session[]): NestedSession[] {
  const ids = new Set(rows.map((session) => session.id));
  const parentOf = (session: Session) => {
    const parent = session.agent?.role === 'subagent' ? session.agent.parent_id : undefined;
    return parent && parent !== session.id && ids.has(parent) ? parent : undefined;
  };
  const children = new Map<string, Session[]>();
  for (const session of rows) {
    const parent = parentOf(session);
    if (parent) children.set(parent, [...(children.get(parent) ?? []), session]);
  }
  const out: NestedSession[] = [];
  const placed = new Set<string>();
  const place = (session: Session, depth: number) => {
    if (placed.has(session.id)) return;
    placed.add(session.id);
    out.push({ session, depth });
    for (const child of children.get(session.id) ?? []) place(child, depth + 1);
  };
  for (const session of rows) if (!parentOf(session)) place(session, 0);
  // A cycle has no root; its rows still show, flat.
  for (const session of rows) place(session, 0);
  return out;
}
