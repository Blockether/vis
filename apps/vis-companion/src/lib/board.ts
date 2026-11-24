import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import type { LiveTone } from './live-view';

const defs = viewSchema.$defs;
export type BoardKind = 'task' | 'story' | 'bug' | 'epic' | 'subtask';
export type BoardPriority = 'highest' | 'high' | 'medium' | 'low' | 'lowest';
export const BOARD_KINDS = defs.board_issue_kind.enum as readonly BoardKind[];
export const BOARD_PRIORITIES = defs.board_priority.enum as readonly BoardPriority[];
export const DEFAULT_KIND = defs.board_issue_kind.default as BoardKind;
export const DEFAULT_PRIORITY = defs.board_priority.default as BoardPriority;
export const ISSUE_LIMITS = defs.board_issue.properties;

export interface BoardColumn { id: string; title: string; tone?: LiveTone; limit?: number; is_done?: boolean }
export interface BoardPerson { id: string; name: string; color?: string }
export interface BoardEpic { id: string; title: string; color?: string }
export interface BoardIssue {
  id: string;
  key?: string;
  title: string;
  column: string;
  rank?: number;
  kind?: BoardKind;
  priority?: BoardPriority;
  assignee?: string;
  epic?: string;
  points?: number;
  labels?: string[];
  text?: string;
  due?: string;
  is_flagged?: boolean;
  subtasks_done?: number;
  subtasks_total?: number;
  updated_at?: number;
}

export type BoardIssueChange = Partial<Pick<BoardIssue,
  'title' | 'column' | 'kind' | 'priority' | 'labels' | 'text' | 'is_flagged'
>> & { assignee?: string | null; epic?: string | null; points?: number | null; due?: string | null };
export type BoardAction =
  | ({ action: 'move'; item_id: string; node_id: string } & BoardPlace)
  | { action: 'edit'; node_id: string; item_id?: string; values: BoardIssueChange; is_removed?: boolean };

export type BoardGroup = 'none' | 'assignee' | 'epic' | 'priority';
export interface BoardFilters {
  query: string;
  assignees: (string | null)[];
  flagged: boolean;
  recent: boolean;
  hideDone: boolean;
  epic: string | null;
  label: string;
  priority: BoardPriority | '';
  kind: BoardKind | '';
}

export const EMPTY_FILTERS: BoardFilters = {
  query: '', assignees: [], flagged: false, recent: false, hideDone: false,
  epic: null, label: '', priority: '', kind: '',
};

export const WEEK_MS = 7 * 24 * 60 * 60 * 1000;

export function boardFilterCount(filters: BoardFilters): number {
  return filters.assignees.length + Number(filters.flagged) + Number(filters.recent) +
    Number(filters.hideDone) + Number(filters.epic !== null) + Number(Boolean(filters.label)) +
    Number(Boolean(filters.priority)) + Number(Boolean(filters.kind));
}

export function filterBoard(issues: readonly BoardIssue[], columns: readonly BoardColumn[], filters: BoardFilters, now: number) {
  const query = filters.query.trim().toLocaleLowerCase();
  const done = new Set(columns.filter((column) => column.is_done).map((column) => column.id));
  return issues.filter((issue) =>
    (!query || [issue.title, issue.key, ...issue.labels ?? []].some((word) => word?.toLocaleLowerCase().includes(query))) &&
    (!filters.assignees.length || filters.assignees.includes(issue.assignee ?? null)) &&
    (!filters.flagged || issue.is_flagged) &&
    (!filters.recent || (issue.updated_at !== undefined && issue.updated_at >= now - WEEK_MS && issue.updated_at <= now)) &&
    (!filters.hideDone || !done.has(issue.column)) &&
    (filters.epic === null || issue.epic === filters.epic) &&
    (!filters.label || issue.labels?.includes(filters.label)) &&
    (!filters.priority || (issue.priority ?? DEFAULT_PRIORITY) === filters.priority) &&
    (!filters.kind || (issue.kind ?? DEFAULT_KIND) === filters.kind),
  );
}

export function orderBoard(issues: readonly BoardIssue[]): BoardIssue[] {
  return [...issues].sort((a, b) => (a.rank ?? 0) - (b.rank ?? 0));
}

export interface BoardPlace { column: string; index: number }

/** A drop beside a visible issue uses its place in the whole column. */
export function boardPlace(
  issues: readonly BoardIssue[], itemId: string, column: string,
  near?: { id: string; after: boolean },
): BoardPlace {
  const other = orderBoard(issues.filter((issue) => issue.column === column && issue.id !== itemId));
  const at = near ? other.findIndex((issue) => issue.id === near.id) : -1;
  return { column, index: at < 0 ? other.length : at + Number(near?.after) };
}

export function moveBoardIssue(issues: BoardIssue[], id: string, { column, index }: BoardPlace): BoardIssue[] {
  const issue = issues.find((one) => one.id === id);
  if (!issue) return issues;
  const old = orderBoard(issues.filter((one) => one.column === column));
  const next = old.filter((one) => one.id !== id);
  const at = Math.min(Math.max(0, index), next.length);
  if (issue.column === column && old[at]?.id === id) return issues;
  next.splice(at, 0, issue);
  const ranks = new Map(next.map((one, rank) => [one.id, rank]));
  return issues.map((one) => {
    const rank = ranks.get(one.id);
    return rank === undefined || (one.column === column && one.rank === rank) ? one : { ...one, rank, column };
  });
}

export function boardLaneKey(issue: BoardIssue, group: BoardGroup): string | null {
  if (group === 'priority') return issue.priority ?? DEFAULT_PRIORITY;
  return group === 'none' ? null : issue[group] ?? null;
}

export interface BoardLane { id: string | null; title: string; count: number; columns: Map<string, BoardIssue[]> }

export function groupBoard(issues: readonly BoardIssue[], group: BoardGroup, people: readonly BoardPerson[], epics: readonly BoardEpic[]): BoardLane[] {
  const lanes = new Map<string | null, BoardLane>();
  for (const issue of orderBoard(issues)) {
    const id = boardLaneKey(issue, group);
    let lane = lanes.get(id);
    if (!lane) {
      const title = group === 'assignee' ? (id === null ? 'Unassigned' : people.find((person) => person.id === id)?.name ?? id)
        : group === 'epic' ? (id === null ? 'No epic' : epics.find((epic) => epic.id === id)?.title ?? id)
        : id === null ? '' : boardWord(id);
      lane = { id, title, count: 0, columns: new Map() };
      lanes.set(id, lane);
    }
    lane.count += 1;
    const column = lane.columns.get(issue.column);
    if (column) column.push(issue);
    else lane.columns.set(issue.column, [issue]);
  }
  if (group === 'none') return [lanes.get(null) ?? { id: null, title: '', count: 0, columns: new Map() }];
  return [...lanes.values()].sort((a, b) => {
    if (a.id === null) return 1;
    if (b.id === null) return -1;
    return group === 'priority'
      ? BOARD_PRIORITIES.indexOf(a.id as BoardPriority) - BOARD_PRIORITIES.indexOf(b.id as BoardPriority)
      : a.title.localeCompare(b.title);
  });
}

export function boardWord(word: string) {
  return word.charAt(0).toUpperCase() + word.slice(1);
}
