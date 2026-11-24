import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { BOARD_KINDS, BOARD_PRIORITIES, moveBoardIssue, type BoardAction, type BoardIssue } from '../lib/board';
import { applyLivePatch, liveViewFromWire } from '../lib/live-view';
import { LiveViewPanel } from './LiveView';

const columns = [
  { id: 'next', title: 'To do' }, { id: 'doing', title: 'In progress', tone: 'running', limit: 4 },
  { id: 'review', title: 'In review', tone: 'warn', limit: 3 }, { id: 'done', title: 'Done', tone: 'ok', is_done: true },
];
const people = [
  { id: 'ava', name: 'Ava Chen', color: '#845ef7' }, { id: 'seb', name: 'Seb Reyes', color: '#a0522d' },
  { id: 'mei', name: 'Mei Patel', color: '#2c9a75' }, { id: 'lee', name: 'Lee Stone', color: '#447ccc' },
];
const epics = [
  { id: 'start', title: 'First use', color: '#845ef7' }, { id: 'shell', title: 'New shell', color: '#447ccc' },
  { id: 'access', title: 'Access for all', color: '#2c9a75' },
];
const titles = [
  'Plan the next release', 'Write the first-use guide', 'Make the search field easier to find', 'Set up the test project',
  'Show a clear error when a file does not open', 'Add a way to find recent work', 'Check touch targets on a small screen',
  'Show the next step after sign-in', 'Check keyboard order in dialogs', 'Test the new navigation', 'Save the last open view',
  'Make large lists easier to read', 'Write the release notes', 'Fix a jump in the sparkline', 'Add a short tour for new users',
  'Check links in shared notes', 'Update the list of supported devices', 'Show which filters are active', 'Set a default workspace',
  'Fix a blank name in the profile', 'Check the light theme', 'Add tests for date fields', 'Reduce the time to open a panel',
  'Make the activity list easier to scan', 'Update the help links',
];
const now = Date.now();
const issues: BoardIssue[] = titles.map((title, index) => ({
  id: `issue-${index + 1}`, key: `APP-${121 + index}`, title, rank: index,
  column: columns[index < 7 ? 0 : index < 13 ? 1 : index < 18 ? 2 : 3].id,
  kind: BOARD_KINDS[index % BOARD_KINDS.length], priority: BOARD_PRIORITIES[index % BOARD_PRIORITIES.length],
  assignee: people[index % 5]?.id, epic: epics[index % 4]?.id, points: index % 5,
  labels: [['design'], ['docs'], ['tests', 'phone'], ['app'], []][index % 5],
  text: '## Goal\n\nMake the next step clear. Check the result on a phone and with a keyboard.',
  due: index % 4 === 1 ? new Date(now + (index % 8 === 1 ? -3 : 7) * 86400000).toISOString().slice(0, 10) : undefined,
  is_flagged: index === 4 || index === 8, subtasks_done: index % 4, subtasks_total: index % 3 === 0 ? 4 : undefined,
  updated_at: now - index * 86400000,
}));
const view = liveViewFromWire({ id: 'work', title: 'Board', seq: 0, nodes: [{
  id: 'board', label: 'Board', type: 'board', columns, people, epics, issues, is_editable: true, is_selectable: true,
}] });
if (!view) throw new Error('Expected a board');
const first = view;

function ShowcaseBoard() {
  const [view, setView] = useState(first);
  const patch = (op: Record<string, unknown>) => setView((held) => applyLivePatch(held, { patch: { seq: held.seq + 1, ops: [op] } }));
  const action = ({ node_id, ...action }: BoardAction) => {
    setView((held) => {
      const node = held.nodes.find((one) => one.id === node_id);
      if (node?.type !== 'board') return held;
      let op: Record<string, unknown>;
      if (action.action === 'move') {
        op = { op: 'replace', issues: moveBoardIssue(node.issues, action.item_id, action) };
      } else if (action.is_removed) {
        op = { op: 'remove', item_ids: [action.item_id] };
      } else {
        const issue = node.issues.find((one) => one.id === action.item_id);
        const values = Object.fromEntries(Object.entries({ ...issue, ...action.values }).filter(([, value]) => value !== null));
        op = { op: 'append', issues: [{ id: `new-${held.seq}`, key: `APP-${200 + held.seq}`, rank: 1000, ...values, updated_at: Date.now() }] };
      }
      return applyLivePatch(held, { patch: { seq: held.seq + 1, ops: [{ ...op, node_id }] } });
    });
    return true;
  };
  return <LiveViewPanel view={view} inDialog onBoardAction={action}
    onSelect={(node_id, selected_ids) => patch({ op: 'set', node_id, selected_ids })} />;
}

const meta = { title: 'Session/Live view/Board', component: ShowcaseBoard, parameters: { layout: 'fullscreen' } } satisfies Meta<typeof ShowcaseBoard>;
export default meta;
export const Showcase: StoryObj<typeof meta> = {};
