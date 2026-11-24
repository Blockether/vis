import { expect, it } from 'vitest';
import { boardPlace, groupBoard, moveBoardIssue, orderBoard, type BoardIssue } from './board';
import { applyLivePatch, liveViewFromWire } from './live-view';

const now = Date.UTC(2026, 5, 1);
const columns = [{ id: 'open', title: 'Open' }, { id: 'done', title: 'Done', is_done: true }];
const issues: BoardIssue[] = [
  { id: 'a', title: 'Read the guide', column: 'open', rank: 1, key: 'APP-8', labels: ['Docs'], priority: 'low', assignee: 'sam', epic: 'guide', is_flagged: true, updated_at: now - 2 },
  { id: 'b', title: 'Test the shell', column: 'open', rank: 0, kind: 'bug', priority: 'high', updated_at: now - 1 },
  { id: 'c', title: 'Check the list', column: 'open', rank: 1, priority: 'low', assignee: 'sam' },
  { id: 'd', title: 'Proofread the guide', column: 'done', rank: 1, labels: ['Docs'], is_flagged: true, updated_at: now },
];
const ids = (items: BoardIssue[]) => items.map((issue) => issue.id);

it('preserves array order for tied ranks and inserts at the full column index', () => {
  expect(ids(orderBoard(issues.slice(0, 3)))).toEqual(['b', 'a', 'c']);
  const place = boardPlace(issues, 'd', 'open', { id: 'c', after: false });
  expect(place).toEqual({ column: 'open', index: 2 });
  const moved = moveBoardIssue(issues, 'd', place);
  expect(ids(orderBoard(moved.filter((issue) => issue.column === 'open')))).toEqual(['b', 'a', 'd', 'c']);
  expect(ids(orderBoard(moveBoardIssue(moved, 'b', boardPlace(moved, 'b', 'open', { id: 'c', after: true }))))).toEqual(['a', 'd', 'c', 'b']);
});

it('groups the shown issues by person, epic or priority', () => {
  const people = [{ id: 'sam', name: 'Sam Lee' }];
  const epics = [{ id: 'guide', title: 'Guide' }];
  const group = (key: 'assignee' | 'epic' | 'priority') => groupBoard(issues, key, people, epics).map(({ title, count }) => [title, count]);
  expect(group('assignee')).toEqual([['Sam Lee', 2], ['Unassigned', 2]]);
  expect(group('epic')).toEqual([['Guide', 1], ['No epic', 3]]);
  expect(group('priority')).toEqual([['High', 1], ['Medium', 1], ['Low', 2]]);
  expect(ids(groupBoard(issues, 'assignee', people, epics)[0].columns.get('open') ?? [])).toEqual(['a', 'c']);
});

it('replaces issues by id and clears a selection when the issue goes away', () => {
  const view = liveViewFromWire({ id: 'v', title: 'Test', seq: 0, nodes: [{ id: 'b', type: 'board', columns, issues, is_selectable: true, selected_ids: ['a'] }] });
  if (!view) throw new Error('Expected a view');
  const changed = applyLivePatch(view, { patch: { seq: 1, ops: [
    { op: 'append', node_id: 'b', issues: [{ ...issues[0], title: 'Read the new guide' }] },
    { op: 'remove', node_id: 'b', item_ids: ['b'] },
    { op: 'set', node_id: 'b', columns: [{ ...columns[0], title: 'Next' }, columns[1]], group_by: 'epic' },
  ] } });
  const node = changed.nodes[0];
  if (node?.type !== 'board') throw new Error('Expected a board');
  expect(view.nodes[0]).toMatchObject({ group_by: 'none' });
  expect(node).toMatchObject({ group_by: 'epic' });
  expect(node.columns[0].title).toBe('Next');
  expect(node.issues.map(({ id, title }) => [id, title])).toEqual([['a', 'Read the new guide'], ['c', 'Check the list'], ['d', 'Proofread the guide']]);
  expect(node.selected_ids).toEqual(['a']);
  const replaced = applyLivePatch(changed, { patch: { seq: 2, ops: [{ op: 'replace', node_id: 'b', issues: [issues[2]] }] } }).nodes[0];
  expect(replaced).toMatchObject({ issues: [{ id: 'c' }], selected_ids: [] });
});
