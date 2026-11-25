import { useEffect, useId, useLayoutEffect, useRef, useState, type CSSProperties, type PointerEvent as ReactPointerEvent } from 'react';
import { ArrowDown, ArrowDownToLine, ArrowUp, ArrowUpToLine, Bug, CalendarDays, Check, CheckSquare, ChevronDown, ChevronLeft, ChevronRight, ChevronsDown, ChevronsUp, Equal, Flag, GripVertical, Layers, Plus, UserRound, Zap } from 'lucide-react';

import {
  BOARD_KINDS, BOARD_PRIORITIES, DEFAULT_KIND, DEFAULT_PRIORITY, ISSUE_LIMITS,
  boardPlace, boardWord, groupBoard, moveBoardIssue, orderBoard,
  type BoardAction, type BoardColumn, type BoardEpic, type BoardIssue, type BoardIssueChange, type BoardKind, type BoardPerson, type BoardPlace, type BoardPriority,
} from '../lib/board';
import type { LiveBoardNode } from '../lib/live-view';
import { Markdown } from './ChatContent';
import { Banner, Button, Checkbox, CommitInput, DialogFrame, Field, IconButton, Input, Modal, Select, SurfaceButton, TextArea } from './ui';

const KIND_MARK = {
  task: [CheckSquare, 'text-link'], story: [Layers, 'text-ok-ink'], bug: [Bug, 'text-err-ink'],
  epic: [Zap, 'text-machine-violet'], subtask: [Layers, 'text-machine-blue'],
} as const;
const PRIORITY_MARK = {
  highest: [ChevronsUp, 'text-err-ink'], high: [ArrowUp, 'text-err-ink'], medium: [Equal, 'text-warn'],
  low: [ArrowDown, 'text-link'], lowest: [ChevronsDown, 'text-dialog-hint'],
} as const;
const KIND_OPTIONS = BOARD_KINDS.map((value) => ({ value, label: boardWord(value) }));
const PRIORITY_OPTIONS = BOARD_PRIORITIES.map((value) => ({ value, label: boardWord(value) }));
const TONE_MARK = { idle: 'bg-dialog-hint', running: 'bg-link', ok: 'bg-ok', warn: 'bg-warn', error: 'bg-err' };

function KindMark({ kind = DEFAULT_KIND }: { kind?: BoardKind }) {
  const [Icon, color] = KIND_MARK[kind];
  return <span role="img" aria-label={boardWord(kind)} title={boardWord(kind)} className={color}><Icon aria-hidden="true" size={16} /></span>;
}

function PriorityMark({ priority = DEFAULT_PRIORITY }: { priority?: BoardPriority }) {
  const [Icon, color] = PRIORITY_MARK[priority];
  const label = `${boardWord(priority)} priority`;
  return <span role="img" aria-label={label} title={label} className={color}><Icon aria-hidden="true" size={16} /></span>;
}

function Avatar({ person, name = person?.name ?? 'Unassigned' }: { person?: BoardPerson; name?: string }) {
  const initials = name.trim().split(/\s+/).slice(0, 2).map((word) => Array.from(word)[0]).join('').toUpperCase();
  return <span className={`inline-grid size-6 place-items-center font-mono text-[0.625rem]/none font-semibold ${markClass}`} title={name} role="img" aria-label={name} style={colorStyle(person?.color)}>
    {name === 'Unassigned' ? <UserRound size={14} aria-hidden="true" /> : initials}
  </span>;
}

const markClass = 'border border-[color-mix(in_oklab,var(--board-mark,var(--color-accent))_50%,var(--color-edge))] bg-[color-mix(in_oklab,var(--board-mark,var(--color-accent))_18%,var(--color-panel))] text-[color:color-mix(in_oklab,var(--board-mark,var(--color-accent))_35%,var(--color-dialog-foreground))]';

function colorStyle(color?: string): CSSProperties | undefined {
  return color ? ({ '--board-mark': color } as CSSProperties) : undefined;
}

function localDay(now: number) {
  const date = new Date(now);
  const month = String(date.getMonth() + 1).padStart(2, '0');
  return `${date.getFullYear()}-${month}-${String(date.getDate()).padStart(2, '0')}`;
}

function IssueCard({ issue, person, epic, today, open, move, canDrag, isDragged }: {
  issue: BoardIssue; person?: BoardPerson; epic?: BoardEpic; today: string; open?: () => void;
  move?: () => void; canDrag: boolean; isDragged: boolean;
}) {
  const metadata = useId();
  const total = issue.subtasks_total ?? 0;
  const done = Math.min(total, issue.subtasks_done ?? 0);
  return (
    <article className={`relative min-w-0 border border-edge bg-panel motion-safe:transition-colors hover:border-edge-strong focus-within:border-edge-strong ${issue.is_flagged ? 'border-s-2 border-s-warn bg-warn-surface' : ''} ${isDragged ? 'opacity-45' : ''}`} data-board-issue={issue.id}
      data-mouse-drag={canDrag || undefined}>
      {open && <SurfaceButton className="z-10"
        aria-label={[issue.key, issue.title].filter(Boolean).join(' ')} aria-describedby={metadata} onClick={open} />}
      <div id={metadata} className="pointer-events-none flex flex-col gap-2 p-3">
        <div className={`flex min-w-0 items-center gap-2 text-meta text-dialog-hint ${move ? 'pr-10' : ''}`}>
          <KindMark kind={issue.kind} />
          {issue.key && <span className="truncate font-mono">{issue.key}</span>}
          {issue.is_flagged && <span role="img" aria-label="Flagged" title="Flagged" className="text-err-ink"><Flag size={14} fill="currentColor" aria-hidden="true" /></span>}
        </div>
        <p className="line-clamp-2 break-words text-body font-medium leading-snug text-dialog-foreground">{issue.title}</p>
        {(issue.epic || Boolean(issue.labels?.length)) && <div className="flex flex-wrap gap-1 text-meta leading-snug">
          {issue.epic && <span className={`max-w-full truncate px-1.5 py-0.5 ${markClass}`} style={colorStyle(epic?.color)}>{epic?.title ?? issue.epic}</span>}
          {issue.labels?.map((label) => <span key={label} className="max-w-full truncate border border-edge bg-panel-2 px-1.5 py-0.5 text-dialog-hint">{label}</span>)}
        </div>}
        <div className="flex min-h-6 items-center gap-2 text-meta text-dialog-hint">
          <PriorityMark priority={issue.priority} />
          {issue.points !== undefined && <span aria-label={`${issue.points} points`} className="border border-edge bg-panel-2 px-1.5 font-mono tabular-nums">{issue.points}</span>}
          {issue.due && <time dateTime={issue.due} className={`flex min-w-0 items-center gap-1 font-mono ${issue.due < today ? 'text-err-ink' : ''}`}>
            <CalendarDays size={13} aria-hidden="true" />{issue.due}
          </time>}
          <div className="ml-auto"><Avatar person={person} name={person?.name ?? issue.assignee ?? 'Unassigned'} /></div>
        </div>
        {total > 0 && <div className="flex items-center gap-2 text-meta text-dialog-hint">
          <progress className="h-1 min-w-0 flex-1 appearance-none bg-edge [&::-webkit-progress-bar]:bg-edge [&::-webkit-progress-value]:bg-ok [&::-moz-progress-bar]:bg-ok" aria-label="Subtasks" value={done} max={total} />
          <span className="font-mono tabular-nums">{done}/{total}</span>
        </div>}
      </div>
      {move && <div className="absolute right-1.5 top-1.5 z-20">
        <IconButton variant="quiet" className="touch-none" label={`Move ${issue.key ?? issue.title}`} data-board-grip onClick={move}>
          <GripVertical size={16} aria-hidden="true" />
        </IconButton>
      </div>}
    </article>
  );
}

function MovePicker({ issue, issues, columns, onMove, close }: {
  issue: BoardIssue; issues: BoardIssue[]; columns: BoardColumn[]; onMove: (place: BoardPlace) => void; close?: () => void;
}) {
  const inColumn = orderBoard(issues.filter((one) => one.column === issue.column));
  const index = inColumn.findIndex((one) => one.id === issue.id);
  const buttons = [
    { label: 'Move to top', Icon: ArrowUpToLine, index: 0, disabled: index <= 0 },
    { label: 'Move up', Icon: ArrowUp, index: index - 1, disabled: index <= 0 },
    { label: 'Move down', Icon: ArrowDown, index: index + 1, disabled: index < 0 || index >= inColumn.length - 1 },
    { label: 'Move to bottom', Icon: ArrowDownToLine, index: inColumn.length - 1, disabled: index < 0 || index >= inColumn.length - 1 },
  ];
  const choose = (place: BoardPlace) => {
    onMove(place);
    close?.();
  };
  return <div className="flex flex-col gap-3">
    <Select aria-label="Move to column" placeholder="Move to column" value=""
      options={columns.filter((column) => column.id !== issue.column).map((column) => ({ value: column.id, label: column.title }))}
      onValueChange={(column) => choose(boardPlace(issues, issue.id, column))} />
    <div className="flex flex-wrap gap-2" role="group" aria-label="Order in column">
      {buttons.map(({ label, Icon, index, disabled }) => <Button key={label} type="button" variant="secondary" className="inline-flex items-center gap-1.5" disabled={disabled}
        onClick={() => choose({ column: issue.column, index })}><Icon size={14} aria-hidden="true" />{label}</Button>)}
    </div>
  </div>;
}

function Description({ text, edit }: { text: string; edit?: (text: string) => void }) {
  const [draft, setDraft] = useState(text);
  const [isEditing, setIsEditing] = useState(false);
  return <section className="grid min-w-0 gap-3" aria-label="Description">
    <div className="flex items-center justify-between gap-2">
      <h4 className="text-body font-semibold">Description</h4>
      {edit && !isEditing && <Button type="button" variant="secondary" onClick={() => { setDraft(text); setIsEditing(true); }}>Edit description</Button>}
    </div>
    {isEditing && edit ? <>
      <TextArea aria-label="Description" value={draft} rows={8} maxLength={ISSUE_LIMITS.text.maxLength}
        onChange={(event) => setDraft(event.target.value)} />
      <div className="flex flex-wrap gap-2">
        <Button type="button" onClick={() => { if (draft !== text) edit(draft); setIsEditing(false); }}>Save description</Button>
        <Button type="button" variant="quiet" onClick={() => setIsEditing(false)}>Cancel</Button>
      </div>
      {draft && <div className="border-t border-edge pt-3"><p className="mb-2 text-meta text-dialog-hint">Preview</p><Markdown>{draft}</Markdown></div>}
    </> : text ? <Markdown>{text}</Markdown> : <p className="text-ui text-dialog-hint">No description</p>}
  </section>;
}

function parseLabels(text: string) {
  return [...new Set(text.split(',').map((word) => word.trim()).filter(Boolean)
    .map((word) => word.slice(0, ISSUE_LIMITS.labels.items.maxLength)))].slice(0, ISSUE_LIMITS.labels.maxItems);
}

function IssueFields({ values, columns, people, epics, set }: {
  values: BoardIssueChange; columns: BoardColumn[]; people: BoardPerson[]; epics: BoardEpic[];
  set: (change: BoardIssueChange) => void;
}) {
  const points = String(values.points ?? '');
  const labels = values.labels?.join(', ') ?? '';
  return <>
    <Field label="Status"><Select aria-label="Status" value={values.column ?? ''}
      options={columns.map((one) => ({ value: one.id, label: one.title }))} onValueChange={(column) => set({ column })} /></Field>
    <Field label="Assignee"><Select aria-label="Assignee" value={values.assignee ?? ''}
      options={[{ value: '', label: 'Unassigned' }, ...people.map((one) => ({ value: one.id, label: one.name }))]}
      onValueChange={(assignee) => set({ assignee: assignee || null })} /></Field>
    <Field label="Priority"><Select aria-label="Priority" value={values.priority ?? DEFAULT_PRIORITY}
      options={PRIORITY_OPTIONS} onValueChange={(priority) => set({ priority: priority as BoardPriority })} /></Field>
    <Field label="Kind"><Select aria-label="Kind" value={values.kind ?? DEFAULT_KIND}
      options={KIND_OPTIONS} onValueChange={(kind) => set({ kind: kind as BoardKind })} /></Field>
    <Field label="Epic"><Select aria-label="Epic" value={values.epic ?? ''}
      options={[{ value: '', label: 'No epic' }, ...epics.map((one) => ({ value: one.id, label: one.title }))]}
      onValueChange={(epic) => set({ epic: epic || null })} /></Field>
    <Field label="Points"><CommitInput value={points} type="number" min={ISSUE_LIMITS.points.minimum} max={ISSUE_LIMITS.points.maximum} step="any" onCommit={(text) => set({ points: text === '' ? null : Number(text) })} /></Field>
    <Field label="Labels"><CommitInput value={labels} onCommit={(text) => set({ labels: parseLabels(text) })} /></Field>
    <Field label="Due date"><CommitInput value={values.due ?? ''} type="date" onCommit={(due) => set({ due: due || null })} /></Field>
    <Checkbox type="button" className="col-span-full" isOn={values.is_flagged === true} onClick={() => set({ is_flagged: !values.is_flagged })}>Flagged</Checkbox>
  </>;
}

function IssueDetail({ issue, node, issues, close, edit, remove, move, error }: {
  issue: BoardIssue; node: LiveBoardNode; issues: BoardIssue[]; close: () => void; error?: string;
  edit?: (change: BoardIssueChange) => Promise<boolean>; remove?: () => Promise<boolean>; move?: (place: BoardPlace) => void;
}) {
  const [confirmation, setConfirmation] = useState(false);
  const [removing, setRemoving] = useState(false);
  const [pending, setPending] = useState<{ issue: BoardIssue; values: BoardIssueChange }>({ issue, values: {} });
  const values = { ...issue, ...(pending.issue === issue ? pending.values : {}) };
  const change = (values: BoardIssueChange) => {
    if (!edit) return;
    setPending((held) => ({ issue, values: { ...(held.issue === issue ? held.values : {}), ...values } }));
    void edit(values).then((ok) => {
      if (ok) return;
      setPending((held) => {
        if (held.issue !== issue) return held;
        const next = Object.fromEntries(Object.entries(held.values).filter(([key, value]) => values[key as keyof BoardIssueChange] !== value));
        return { issue, values: next };
      });
    });
  };
  const column = node.columns.find((one) => one.id === issue.column);
  const assignee = node.people.find((one) => one.id === issue.assignee);
  const epic = node.epics.find((one) => one.id === issue.epic);
  const details = [
    ['Status', column?.title ?? issue.column], ['Assignee', assignee?.name ?? issue.assignee ?? 'Unassigned'],
    ['Priority', boardWord(issue.priority ?? DEFAULT_PRIORITY)], ['Kind', boardWord(issue.kind ?? DEFAULT_KIND)],
    ['Epic', epic?.title ?? issue.epic ?? 'No epic'], ['Points', issue.points ?? 'None'],
    ['Labels', issue.labels?.join(', ') || 'None'], ['Due date', issue.due ?? 'None'], ['Flagged', issue.is_flagged ? 'Yes' : 'No'],
  ];
  return <Modal size="split" onDismiss={close}>
    <DialogFrame title={issue.key ?? 'Issue details'} onClose={close} manageFocus>
      <div className="grid gap-6 p-4 text-dialog-foreground sm:grid-cols-[minmax(0,1fr)_minmax(12rem,36%)] sm:p-6">
        {error && <div className="col-span-full"><Banner kind="err" role="alert">{error}</Banner></div>}
        <div className="grid min-w-0 content-start gap-5">
          {edit ? <Field label="Title"><CommitInput required maxLength={ISSUE_LIMITS.title.maxLength}
            value={values.title} onCommit={(title) => change({ title })} /></Field> : <h3 className="text-title font-semibold">{values.title}</h3>}
          <Description text={values.text ?? ''} edit={edit ? (text) => change({ text }) : undefined} />
        </div>
        <section className="min-w-0 border-t border-edge pt-4 sm:border-l sm:border-t-0 sm:pl-4 sm:pt-0" aria-label="Issue fields">
          <h4 className="mb-3 text-body font-semibold">Details</h4>
          {edit ? <div className="grid gap-3"><IssueFields values={values} columns={node.columns} people={node.people} epics={node.epics} set={change} /></div>
            : <dl className="grid grid-cols-[auto_minmax(0,1fr)] gap-x-3 gap-y-2 text-ui">
              {details.map(([label, value]) => <div className="contents" key={label}><dt className="text-dialog-hint">{label}</dt><dd className="break-words">{value}</dd></div>)}
            </dl>}
        </section>
        {move && <section className="grid gap-3 border-t border-edge pt-4 sm:col-span-full" aria-label="Move issue">
          <h4 className="text-body font-semibold">Move to</h4>
          <MovePicker issue={issue} issues={issues} columns={node.columns} onMove={move} />
        </section>}
        {remove && <div className="flex flex-wrap items-center gap-2 border-t border-edge pt-4 sm:col-span-full">
          {confirmation ? <>
            <p className="w-full text-ui" role="status">Delete this issue? You cannot undo this change.</p>
            <Button type="button" variant="danger" disabled={removing} onClick={() => {
              setRemoving(true);
              void remove().then((ok) => { setRemoving(false); if (ok) close(); });
            }}>Confirm delete</Button>
            <Button type="button" variant="quiet" disabled={removing} onClick={() => setConfirmation(false)}>Cancel</Button>
          </> : <Button type="button" variant="danger" onClick={() => setConfirmation(true)}>Delete issue</Button>}
        </div>}
      </div>
    </DialogFrame>
  </Modal>;
}

function InlineCreate({ column, create }: { column: BoardColumn; create: (values: BoardIssueChange) => Promise<boolean> }) {
  const [open, setOpen] = useState(false);
  const [title, setTitle] = useState('');
  const [saving, setSaving] = useState(false);
  const input = useRef<HTMLInputElement>(null);
  const opener = useRef<HTMLButtonElement>(null);
  const restoreFocus = useRef(false);
  const close = (shouldRestoreFocus: boolean) => {
    restoreFocus.current = shouldRestoreFocus;
    setTitle('');
    setOpen(false);
  };
  useLayoutEffect(() => {
    if (open) input.current?.focus();
    else if (restoreFocus.current) {
      restoreFocus.current = false;
      opener.current?.focus({ preventScroll: true });
    }
  }, [open]);
  return open ? <form className="flex min-w-0 items-start gap-1" onSubmit={(event) => {
    event.preventDefault();
    if (!title.trim() || saving) return;
    setSaving(true);
    const form = event.currentTarget;
    void create({ title: title.trim(), column: column.id }).then((ok) => {
      setSaving(false);
      if (ok) close(form.contains(form.ownerDocument.activeElement));
    });
  }}>
    <Input ref={input} aria-label={`Title in ${column.title}`} sizeMode="form" className="flex-1"
      value={title} maxLength={ISSUE_LIMITS.title.maxLength} placeholder="Issue title" onChange={(event) => setTitle(event.target.value)}
      onKeyDown={(event) => {
        if (event.key === 'Escape' && !event.nativeEvent.isComposing) {
          event.preventDefault(); event.stopPropagation();
          if (!saving) close(true);
        }
      }} />
    <Button type="submit" variant="primary" className="self-center" disabled={saving || !title.trim()}>Add</Button>
  </form> : <Button ref={opener} type="button" variant="quiet" className="flex w-full items-center gap-1.5 text-left" aria-label={`Create in ${column.title}`} onClick={() => setOpen(true)}>
    <Plus size={16} aria-hidden="true" /> Create
  </Button>;
}


interface BoardDrop { column: string; lane: string; near?: { id: string; after: boolean } }
interface BoardDrag { id: string; drop: BoardDrop | null }

function scrollAtEdge(element: HTMLElement, x: number, axis: 'x' | 'y') {
  const rect = element.getBoundingClientRect();
  const low = axis === 'x' ? rect.left : rect.top;
  const high = axis === 'x' ? rect.right : rect.bottom;
  const edge = Math.min(44, (high - low) / 4);
  if (x < low || x > high || !edge) return;
  const over = x < low + edge ? x - low - edge : x > high - edge ? x - high + edge : 0;
  const step = Math.round((over / edge) * 16);
  if (step) {
    if (axis === 'x') element.scrollLeft += step;
    else element.scrollTop += step;
  }
}

function dropAt(board: HTMLElement, id: string, lane: string, x: number, y: number): BoardDrop | null {
  const hit = document.elementFromPoint(x, y);
  const column = hit?.closest<HTMLElement>('[data-board-column][data-board-lane]');
  if (!column || !board.contains(column)) return null;
  const { boardColumn, boardLane } = column.dataset;
  if (!boardColumn || boardLane !== lane) return null;
  let last: string | undefined;
  for (const card of column.querySelectorAll<HTMLElement>('[data-board-issue]')) {
    const key = card.dataset.boardIssue;
    if (!key || key === id) continue;
    const box = card.getBoundingClientRect();
    if (y < (box.top + box.bottom) / 2) return { column: boardColumn, lane: boardLane, near: { id: key, after: false } };
    last = key;
  }
  return { column: boardColumn, lane: boardLane, near: last ? { id: last, after: true } : undefined };
}

function sameDrop(a: BoardDrop | null, b: BoardDrop | null) {
  return a?.column === b?.column && a?.lane === b?.lane && a?.near?.id === b?.near?.id && a?.near?.after === b?.near?.after;
}

function useBoardDrag(seq: number, move: (id: string, drop: BoardDrop) => void) {
  const [drag, setDrag] = useState<BoardDrag | null>(null);
  const cleanup = useRef<(() => void) | null>(null);
  const suppress = useRef(false);
  const current = useRef(seq);
  useLayoutEffect(() => { current.current = seq; }, [seq]);

  useEffect(() => () => cleanup.current?.(), []);
  function start(event: ReactPointerEvent<HTMLElement>) {
    suppress.current = false;
    if (cleanup.current || event.button !== 0 || !event.isPrimary || !(event.target instanceof Element)) return;
    const board = event.currentTarget.querySelector<HTMLElement>('[data-board-scroll]');
    const card = event.target.closest<HTMLElement>('[data-board-issue][data-mouse-drag]');
    const id = card?.dataset.boardIssue;
    const lane = card?.closest<HTMLElement>('[data-board-lane]')?.dataset.boardLane;
    const touch = event.pointerType !== 'mouse';
    if (!board || !id || !lane || (touch && !event.target.closest('[data-board-grip]'))) return;

    const { pointerId, clientX: fromX, clientY: fromY } = event;
    let x = fromX;
    let y = fromY;
    let active = false;
    let drop: BoardDrop | null = null;
    let frame = 0;
    let timer = 0;

    const tick = () => {
      if (current.current !== seq) { cancel(); return; }
      scrollAtEdge(board, x, 'x');
      const column = document.elementFromPoint(x, y)?.closest<HTMLElement>('[data-board-column]');
      const scroll = column?.querySelector<HTMLElement>('[data-board-column-scroll]');
      if (scroll && board.contains(scroll)) scrollAtEdge(scroll, y, 'y');
      const next = dropAt(board, id, lane, x, y);
      if (!sameDrop(next, drop)) {
        drop = next;
        setDrag({ id, drop });
      }
      frame = requestAnimationFrame(tick);
    };
    const activate = () => {
      active = true;
      suppress.current = true;
      setDrag({ id, drop });
      frame = requestAnimationFrame(tick);
    };
    const finish = () => {
      window.removeEventListener('pointermove', pointerMove);
      window.removeEventListener('pointerup', pointerUp);
      window.removeEventListener('pointercancel', pointerCancel);
      window.removeEventListener('keydown', key, true);
      window.removeEventListener('blur', cancel);
      clearTimeout(timer);
      cancelAnimationFrame(frame);
      cleanup.current = null;
    };
    const cancel = () => {
      finish();
      if (active) setDrag(null);
    };
    const pointerMove = (next: PointerEvent) => {
      if (next.pointerId !== pointerId) return;
      x = next.clientX;
      y = next.clientY;
      if (!active && Math.hypot(x - fromX, y - fromY) >= (touch ? 10 : 5)) {
        if (touch) {
          suppress.current = true;
          cancel();
        } else activate();
      }
      if (active && next.cancelable) next.preventDefault();
    };
    const pointerUp = (next: PointerEvent) => {
      if (next.pointerId !== pointerId) return;
      cancel();
      if (!active || current.current !== seq) return;
      const target = dropAt(board, id, lane, next.clientX, next.clientY);
      if (target) move(id, target);
    };
    const pointerCancel = (next: PointerEvent) => { if (next.pointerId === pointerId) cancel(); };
    const key = (next: KeyboardEvent) => {
      if (next.key !== 'Escape' || !active) return;
      next.preventDefault();
      next.stopPropagation();
      cancel();
    };
    window.addEventListener('pointermove', pointerMove, { passive: false });
    window.addEventListener('pointerup', pointerUp);
    window.addEventListener('pointercancel', pointerCancel);
    window.addEventListener('keydown', key, true);
    window.addEventListener('blur', cancel);
    cleanup.current = finish;
    if (touch) timer = window.setTimeout(activate, 250);
  }
  const stop = () => { cleanup.current?.(); setDrag(null); };
  const blockClick = () => {
    const block = suppress.current;
    suppress.current = false;
    return block;
  };
  const blockRelease = (event: ReactPointerEvent<HTMLElement>) => {
    if (suppress.current) event.preventDefault();
  };
  return { drag, start, stop, blockClick, blockRelease };
}

type BoardOverlay = { type: 'move'; id: string } | null;

export function LiveBoard({ node, seq, onSelect, onAction }: {
  node: LiveBoardNode; seq: number; onSelect?: (nodeId: string, itemIds: string[]) => void;
  onAction?: (action: BoardAction) => Promise<boolean> | boolean;
}) {
  const [closedColumns, setClosedColumns] = useState<string[]>([]);
  const [closedLanes, setClosedLanes] = useState<string[]>([]);
  const [optimistic, setOptimistic] = useState<{ seq: number; issues: BoardIssue[]; action: BoardAction } | null>(null);
  const [failure, setFailure] = useState('');
  const [overlay, setOverlay] = useState<BoardOverlay>(null);
  const [selection, setSelection] = useState<{ seq: number; id?: string }>();
  const canEdit = node.is_editable && Boolean(onAction);
  const issues = optimistic?.seq === seq ? optimistic.issues : node.issues;
  const issueIds = new Set(issues.map((issue) => issue.id));
  const today = localDay(Date.now());
  // The owner chooses which issues show and how they group; the board only lays them out.
  const group = node.group_by;
  const lanes = groupBoard(issues, group, node.people, node.epics);
  const peopleById = new Map(node.people.map((person) => [person.id, person]));
  const epicsById = new Map(node.epics.map((epic) => [epic.id, epic]));
  const selectedId = selection?.seq === seq ? selection.id : node.selected_ids[0];
  const selected = node.is_selectable ? issues.find((issue) => issue.id === selectedId) : undefined;
  const toMove = canEdit && overlay?.type === 'move' ? issues.find((issue) => issue.id === overlay.id) : undefined;
  const closeOverlay = () => setOverlay(null);
  const choose = (id?: string) => {
    setFailure('');
    setSelection({ seq, id });
    if (node.is_selectable) onSelect?.(node.id, id ? [id] : []);
  };
  const send = async (action: BoardAction) => {
    setFailure('');
    try {
      if (await onAction?.(action)) return true;
      setFailure('The board did not accept the change.');
    } catch {
      setFailure('The change could not be sent. Check the connection and try again.');
    }
    return false;
  };
  const move = (id: string, place: BoardPlace) => {
    if (!canEdit || !issueIds.has(id) || !node.columns.some((column) => column.id === place.column)) return;
    const changed = moveBoardIssue(issues, id, place);
    if (changed === issues) return;
    const action: BoardAction = { action: 'move', node_id: node.id, item_id: id, ...place };
    setOptimistic({ seq, issues: changed, action });
    void send(action).then((ok) => {
      if (!ok) setOptimistic((held) => held?.action === action ? null : held);
    });
  };
  const edit = (values: BoardIssueChange, id?: string, isRemoved?: true) => send({
    action: 'edit', node_id: node.id, values,
    ...(id ? { item_id: id } : {}), ...(isRemoved ? { is_removed: true } : {}),
  });
  const { drag, start, stop, blockClick, blockRelease } = useBoardDrag(seq, (id, drop) => move(id, boardPlace(issues, id, drop.column, drop.near)));
  const toggle = (held: string[], id: string) => held.includes(id) ? held.filter((one) => one !== id) : [...held, id];
  const totals = new Map<string, number>();
  for (const issue of issues) totals.set(issue.column, (totals.get(issue.column) ?? 0) + 1);

  return <section className="board min-w-0 [--board-column-width:max(15rem,85vw)] sm:[--board-column-width:17rem] data-[dragging]:select-none" aria-label={node.label ? `${node.label} issues` : 'Board issues'} data-dragging={drag ? true : undefined}
    onPointerDown={start} onPointerUpCapture={blockRelease} onClickCapture={(event) => {
      if (event.detail > 0 && blockClick()) { event.preventDefault(); event.stopPropagation(); }
    }}>
    {failure && !selected && <Banner kind="err" role="alert">{failure}</Banner>}
    {node.columns.length === 0 ? <p className="py-6 text-ui text-dialog-hint">No columns yet</p> :
      <div className={`overflow-x-auto overscroll-x-contain py-2.5 focus-visible:outline-2 focus-visible:-outline-offset-2 focus-visible:outline-accent max-sm:max-h-[72dvh] max-sm:overflow-y-auto ${drag ? '' : 'max-sm:snap-x max-sm:snap-proximity'}`} data-board-scroll tabIndex={0} role="region" aria-label="Board columns">
        <div className="grid min-w-full gap-2.5" style={{ gridTemplateColumns: node.columns.map((column) => closedColumns.includes(column.id) ? '2.75rem' : 'minmax(var(--board-column-width), 1fr)').join(' ') }}>
          {node.columns.map((column) => {
            const closed = closedColumns.includes(column.id);
            const total = totals.get(column.id) ?? 0;
            const over = column.limit !== undefined && total > column.limit;
            const Collapse = closed ? ChevronRight : ChevronLeft;
            return <div className={`z-[2] flex min-h-11 min-w-0 items-center gap-1.5 border max-sm:sticky max-sm:top-0 max-sm:snap-start ${closed ? 'justify-center' : 'justify-between pl-2.5 pr-1.5'} ${over ? 'border-edge border-t-err bg-err/10' : 'border-edge bg-panel-2'}`} key={column.id}>
              {!closed && <><div className="flex min-w-0 items-center gap-2">
                <span className={`size-2 shrink-0 ${TONE_MARK[column.tone ?? (column.is_done ? 'ok' : 'idle')]}`} aria-hidden="true" />
                <h3 className="min-w-0 truncate text-ui font-semibold uppercase tracking-wide" title={column.title}>{column.title}</h3>
                {column.is_done && <span role="img" aria-label="Done column" className="text-ok-ink"><Check size={15} aria-hidden="true" /></span>}
                <span className="shrink-0 border border-edge px-1 font-mono text-meta tabular-nums text-dialog-hint" title={`${total} ${total === 1 ? 'issue' : 'issues'}`}>{total}</span>
              </div>
              {column.limit !== undefined && <span className={`shrink-0 whitespace-nowrap font-mono text-meta ${over ? 'font-bold text-err-ink' : 'text-dialog-hint'}`}
                title={over ? 'Work in progress is over the limit' : 'Work in progress'} aria-label={`${total} of ${column.limit} work in progress${over ? ', over limit' : ''}`}>{total} / {column.limit}</span>}</>}
              <IconButton variant="quiet" aria-expanded={!closed}
                label={`${closed ? 'Expand' : 'Collapse'} ${column.title}`} onClick={() => { stop(); setClosedColumns(toggle(closedColumns, column.id)); }}>
                <Collapse size={16} aria-hidden="true" />
              </IconButton>
            </div>;
          })}
          {lanes.length === 0 && <p className="col-span-full border-b border-edge bg-panel-2 p-3 text-ui text-dialog-hint">No issues</p>}
          {lanes.map((lane) => {
            const laneKey = JSON.stringify([group, lane.id]);
            const laneClosed = group !== 'none' && closedLanes.includes(laneKey);
            return <div className="contents" key={laneKey}>
              {group !== 'none' && <div className="col-span-full border-b border-edge bg-panel-2">
                <div className="sticky left-0 inline-flex"><Button type="button" variant="quiet" className="inline-flex items-center gap-1.5" aria-expanded={!laneClosed}
                  aria-label={`${lane.title}, ${lane.count} ${lane.count === 1 ? 'issue' : 'issues'}`}
                  onClick={() => { stop(); setClosedLanes(toggle(closedLanes, laneKey)); }}>
                  {laneClosed ? <ChevronRight size={16} aria-hidden="true" /> : <ChevronDown size={16} aria-hidden="true" />}
                  <span className="max-w-48 truncate">{lane.title}</span><span className="px-1 font-mono text-meta text-dialog-hint">{lane.count}</span>
                </Button></div>
              </div>}
              {!laneClosed && node.columns.map((column) => {
                if (closedColumns.includes(column.id)) return <div key={column.id} className="min-w-0 border border-edge bg-panel-2" />;
                const cards = lane.columns.get(column.id) ?? [];
                const drop = drag?.drop?.column === column.id && drag.drop.lane === laneKey ? drag.drop : null;
                return <div key={column.id} className={`flex min-h-32 min-w-0 flex-col gap-1.5 border bg-panel-2 p-1.5 ${drop ? 'border-accent' : 'border-edge'}`} data-board-column={column.id} data-board-lane={laneKey}>
                  <div className="min-h-11 max-h-[min(55dvh,34rem)] space-y-2.5 overflow-auto overscroll-y-contain p-1.5 [scrollbar-width:thin]" data-board-column-scroll aria-label={`${column.title} issues`}>
                    {cards.map((issue) => <div className="relative data-[drop]:after:pointer-events-none data-[drop]:after:absolute data-[drop]:after:inset-x-0 data-[drop]:after:z-[3] data-[drop]:after:h-[3px] data-[drop]:after:bg-accent data-[drop=before]:after:-top-1.5 data-[drop=after]:after:-bottom-1.5" key={issue.id}
                      data-drop={drop?.near?.id === issue.id ? (drop.near.after ? 'after' : 'before') : undefined}>
                      <IssueCard issue={issue} person={issue.assignee ? peopleById.get(issue.assignee) : undefined}
                        epic={issue.epic ? epicsById.get(issue.epic) : undefined} today={today}
                        open={node.is_selectable ? () => choose(issue.id) : undefined}
                        move={canEdit ? () => setOverlay({ type: 'move', id: issue.id }) : undefined}
                        canDrag={canEdit} isDragged={drag?.id === issue.id} />
                    </div>)}
                    {cards.length === 0 && <p className="border border-dashed border-edge p-3 text-ui text-dialog-hint">{drop ? 'Drop here' : 'No issues'}</p>}
                  </div>
                  {canEdit && group === 'none' && <InlineCreate column={column} create={edit} />}
                </div>;
              })}
            </div>;
          })}
          {canEdit && group !== 'none' && node.columns.map((column) => <div key={column.id} className="min-w-0">
            {!closedColumns.includes(column.id) && <InlineCreate column={column} create={edit} />}
          </div>)}
        </div>
      </div>}
    {toMove && <Modal size="fit" onDismiss={closeOverlay}>
      <DialogFrame title={`Move ${toMove.key ?? 'issue'}`} onClose={closeOverlay} manageFocus>
        <div className="grid gap-3 p-4 sm:p-6">
          <p className="text-body text-dialog-foreground">{toMove.title}</p>
          <MovePicker issue={toMove} issues={issues} columns={node.columns} onMove={(place) => move(toMove.id, place)} close={closeOverlay} />
        </div>
      </DialogFrame>
    </Modal>}
    {selected && !overlay && <IssueDetail key={selected.id} node={node} issue={selected} issues={issues} close={() => choose()} error={failure}
      edit={canEdit ? (values) => edit(values, selected.id) : undefined}
      remove={canEdit ? () => edit({}, selected.id, true) : undefined}
      move={canEdit ? (place) => move(selected.id, place) : undefined} />}
  </section>;
}
