import { describe, expect, it } from 'vitest';
import type { Session } from '../../lib/types';
import { nestAgentSessions } from './agent-nesting';

const base = { live: false, current_turn_id: null, server_time_ms: 0, turn_count: 0 };
const leader = (id: string): Session => ({ ...base, id, agent: { role: 'leader', leader_id: id } });
const child = (id: string, parent: string): Session => ({
  ...base,
  id,
  agent: { role: 'subagent', parent_id: parent, leader_id: parent },
});

const shape = (rows: Session[]) => nestAgentSessions(rows).map(({ session, depth }) => `${depth}:${session.id}`);

describe('nestAgentSessions', () => {
  it('puts children under their parent in their own order', () => {
    expect(shape([child('a2', 'lead'), leader('other'), child('a1', 'lead'), leader('lead')])).toEqual([
      '0:other',
      '0:lead',
      '1:a2',
      '1:a1',
    ]);
  });

  it('nests a grandchild one step deeper', () => {
    expect(shape([leader('lead'), child('a1', 'lead'), child('b1', 'a1')])).toEqual(['0:lead', '1:a1', '2:b1']);
  });

  it('keeps a child flat when its parent is not in the list', () => {
    expect(shape([child('a1', 'elsewhere'), leader('lead')])).toEqual(['0:a1', '0:lead']);
  });

  it('shows every row once, also for a cycle', () => {
    expect(shape([child('x', 'y'), child('y', 'x')])).toHaveLength(2);
  });
});
