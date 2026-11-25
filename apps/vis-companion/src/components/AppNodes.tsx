/**
 * The review nodes of an app attachment: a `diff` with threads on its lines, and
 * a `thread` of its own. Both read the same host state and save through the same
 * `draft` action. Without an app the threads are read only.
 */

import { Fragment, useEffect, useMemo, useRef, useState, type CSSProperties, type PointerEvent as ReactPointerEvent, type ReactNode } from 'react';

import {
  APP_ACTION_WORDS,
  isRetryable,
  type AppActionResult,
  type AppActionStatus,
} from '../lib/app-actions';
import {
  anchorKey,
  anchorLines,
  anchorRange,
  lineOrders,
  startOf,
  inSpan,
  fileChange,
  filterDiffFiles,
  type DiffSearchMatch,
  fileStat,
  hiddenLinesBefore,
  diffModel,
  fileTree,
  splitRows,
  type AppDiffFile,
  type AppDiffHunk,
  type AppDiffLine,
  type DiffSpan,
  type SplitRow,
} from '../lib/app-diff';
import { useMediaMatch } from '../lib/fit-rows';
import {
  LIVE_MESSAGE_CHARS,
  isBlankDraft,
  type LiveDiffNode,
  type LiveDiffPreview,
  type LiveMessage,
  type LiveMenuNode,
  type LiveInputNode,
  type LiveNode,
  type LiveShortcut,
  type LiveThread,
  type LiveThreadNode,
} from '../lib/live-view';
import type { ViewAnchor } from '../lib/view';
import {
  ChevronIcon,
  DiffChangeIcon,
  COMMAND_ICONS,
  RefreshIcon,
  ReviewFileIcon,
  RenameArrowIcon,
  WhitespaceIcon,
  TestFilesIcon,
  PlusIcon,
  CollapseAllIcon,
  ExpandAllIcon,
  FileTreeIcon,
  SideBySideIcon,
  UnifiedDiffIcon,
  WrapIcon,
} from './icons';
import { DiffSearch, useDiffSearch } from './AppDiffSearch';
import { ImageNodeView } from './AppImage';
import { MediaNodeView } from './AppMedia';
import { ariaShortcut } from '../lib/app-shortcuts';
import { Button, CopyChip, DropdownMenu, type DropdownItem } from './ui';
import { useReviewPref, useReviewState } from '../lib/review-memory';
import { CODE_DEFAULT_CLASS, highlightSegments, languageOfPath, type SyntaxSegment } from '../lib/syntax';

/** What an app gives its review nodes. Absent, the nodes are read only. */
export interface AppNodeActions {
  /**
   * Save the draft of one thread, or start a thread on `anchor` or on the canvas
   * item `itemId`. The next message of the session adds each saved draft. A null
   * body removes the draft.
   */
  draft: (
    nodeId: string,
    target: { threadId?: string; anchor?: ViewAnchor; itemId?: string; body: string | null },
    baseSeq?: number,
  ) => Promise<AppActionResult>;
  /** The version of the draft shown by the app. */
  baseSeq?: number;
  /** The app is closed or its owner is not connected: show, do not act. */
  isReadOnly: boolean;
}

const TEXTAREA =
  'block w-full resize-y border border-field-edge bg-input px-2.5 py-1 font-mono text-ui text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted';

const STATUS_INK: Record<AppActionStatus, string> = {
  idle: 'text-dialog-hint',
  saving: 'text-dialog-hint',
  saved: 'text-ok',
  offline: 'text-warn',
  conflict: 'text-warn',
  stale: 'text-warn',
  closed: 'text-dialog-hint',
  invalid: 'text-err',
  unbound: 'text-warn',
  disconnected: 'text-warn',
  failed: 'text-err',
};

interface ActionStatusProps {
  status: AppActionStatus;
  isOnBand?: boolean;
}

/**
 * The state word of a Save or a Send. Nothing shows while it is idle. On a dialog
 * band the word takes the band's ink: a warning amber on the amber band is unreadable.
 */
export function ActionStatus({ status, isOnBand = false }: ActionStatusProps) {
  if (status === 'idle') return null;
  return (
    <span
      role="status"
      className={`font-mono text-ui ${isOnBand ? 'font-bold text-inherit' : STATUS_INK[status]}`}
    >
      {APP_ACTION_WORDS[status]}
    </span>
  );
}

function authorWord(message: LiveMessage): string {
  if (message.author.name) return message.author.name;
  return { human: 'Human', agent: 'Agent', extension: 'Extension' }[message.author.kind];
}

interface MessagesProps {
  messages: readonly LiveMessage[];
}

function Messages({ messages }: MessagesProps) {
  if (messages.length === 0) return null;
  return (
    <ol className="space-y-2">
      {messages.map((message) => (
        <li key={message.id} className="min-w-0" data-message-id={message.id}>
          <span className="block font-mono text-ui font-bold text-dialog-foreground">
            {authorWord(message)}
          </span>
          <p className="whitespace-pre-wrap font-mono text-body text-white [overflow-wrap:anywhere]">
            {message.body}
          </p>
        </li>
      ))}
    </ol>
  );
}

interface DraftEditorProps {
  label: string;
  saved: string;
  memoryId: string;
  submitLabel?: string;
  placeholder?: string;
  /** Blank text saves an empty draft: a finding added without a note. */
  isBlankAllowed?: boolean;
  /** The longest text. A thread on a board holds less than a thread of a diff. */
  maxLength?: number;
  showFailureReason?: boolean;
  /** Wait for the read after a conflict, then let the person save against that version. */
  canRetryConflict?: boolean;
  onSave: (body: string) => Promise<AppActionResult>;
  onClose: () => void;
}

/**
 * The editor of one draft. Save keeps the draft on the host, and the next message
 * of the session adds it. Cancel closes the editor and keeps the saved draft. A
 * blank Save on a new line creates nothing.
 */
export function DraftEditor({
  label,
  saved,
  memoryId,
  submitLabel = 'Save',
  placeholder = 'Add a comment…',
  isBlankAllowed = false,
  maxLength = LIVE_MESSAGE_CHARS,
  showFailureReason = false,
  canRetryConflict,
  onSave,
  onClose,
}: DraftEditorProps) {
  const [text, setText] = useReviewState(`${memoryId}:text`, saved);
  const close = () => {
    setText(saved);
    onClose();
  };
  const [outcome, setOutcome] = useState<Pick<AppActionResult, 'status' | 'message'>>({ status: 'idle' });
  const { status } = outcome;
  const failureReason = showFailureReason && status === 'failed' ? outcome.message : undefined;
  const isBusy = status === 'saving';
  const isConflict = canRetryConflict !== undefined && status === 'conflict';
  const isWaiting = isConflict && !canRetryConflict;
  const saving = useRef(false);
  const isBlank = isBlankDraft(text);
  const isTooLong = text.length > maxLength && Array.from(text).length > maxLength;
  const save = async () => {
    if (saving.current || isTooLong || isWaiting) return;
    if (isBlank && saved === '' && !isBlankAllowed) {
      onClose();
      return;
    }
    saving.current = true;
    setOutcome({ status: 'saving' });
    try {
      const result = await onSave(isBlank ? '' : text);
      setOutcome(result);
      if (result.status === 'saved') close();
    } catch {
      setOutcome({ status: 'failed' });
    } finally {
      saving.current = false;
    }
  };
  return (
    <form
      className="diff-draft space-y-2"
      onSubmit={(event) => {
        event.preventDefault();
        void save();
      }}
      onKeyDown={(event) => {
        if (event.key === 'Escape') {
          event.preventDefault();
          event.stopPropagation();
          close();
        }
      }}
    >
      <textarea
        autoFocus
        rows={3}
        placeholder={placeholder}
        className={TEXTAREA}
        value={text}
        aria-invalid={isTooLong || Boolean(failureReason) || undefined}
        aria-label={label}
        aria-keyshortcuts="Meta+Enter Control+Enter Escape"
        disabled={isBusy}
        onChange={(event) => {
          setText(event.target.value);
          if (!isBusy && !isConflict) setOutcome({ status: 'idle' });
        }}
        onKeyDown={(event) => {
          if (
            !event.nativeEvent.isComposing && event.keyCode !== 229 &&
            event.key === 'Enter' &&
            (event.metaKey || event.ctrlKey) &&
            (!isBlank || isBlankAllowed) &&
            !isTooLong &&
            !saving.current
          ) {
            event.preventDefault();
            void save();
          }
        }}
      />
      {isTooLong && (
        <p role="status" className="text-err-ink">Use at most {maxLength} characters.</p>
      )}
      {failureReason && (
        <p role="alert" className="break-words font-mono text-ui text-err-ink">{failureReason}</p>
      )}
      <div className="diff-draft-actions flex flex-wrap items-center gap-2">
        <span className="diff-submit-hint">⌘/Ctrl Enter to save</span>
        <Button type="button" variant="secondary" onClick={close}>
          Cancel
        </Button>
        <Button
          type="submit"
          variant="primary"
          disabled={isBusy || isWaiting || isTooLong || (submitLabel === 'Comment' && isBlank)}
        >
          {isConflict ? 'Retry save' : isRetryable(status) ? 'Retry' : submitLabel}
        </Button>
        {!failureReason && <ActionStatus status={status} />}
      </div>
    </form>
  );
}

interface ThreadBlockProps {
  thread: LiveThread;
  nodeId: string;
  actions?: AppNodeActions;
  title?: string;
}

/**
 * One thread. A thread without messages is a comment of the human, and the next
 * message of the session adds its draft. A thread with messages is a finding: its
 * messages stay as text, and "Add to message" adds them, with an optional note.
 */
function ThreadBlock({ thread, nodeId, actions, title }: ThreadBlockProps) {
  const [isEditing, setEditing] = useReviewState(`${nodeId}:${thread.id}:editing`, false);
  const [status, setStatus] = useState<AppActionStatus>('idle');
  const isFinding = thread.messages.length > 0;
  const isAdded = thread.draft !== undefined;
  const draft = thread.draft ?? '';
  const canEdit = Boolean(actions) && !actions?.isReadOnly;
  const remove = async () => {
    if (!actions) return;
    setStatus('saving');
    const result = await actions.draft(nodeId, { threadId: thread.id, body: null });
    setStatus(result.status === 'saved' ? 'idle' : result.status);
  };
  return (
    <section
      className="min-w-0 space-y-2 border-l-2 border-dialog-edge bg-panel-2 px-3 py-2"
      aria-label={title ?? 'Thread'}
      data-thread-id={thread.id}
    >
      <Messages messages={thread.messages} />
      {isAdded && !isEditing && (
        <div className="min-w-0">
          <span className="block font-mono text-ui text-dialog-hint">
            {isFinding ? 'In the next message' : 'Comment · in the next message'}
          </span>
          {draft !== '' && (
            <p className="whitespace-pre-wrap font-mono text-body text-dialog-foreground [overflow-wrap:anywhere]">
              {draft}
            </p>
          )}
        </div>
      )}
      {isEditing && actions ? (
        <DraftEditor
          memoryId={`${nodeId}:${thread.id}:${draft}`}
          label={isFinding ? 'Note' : 'Edit comment'}
          saved={draft}
          placeholder={isFinding ? 'Add a note (optional)…' : undefined}
          submitLabel={isFinding && !isAdded ? 'Add to message' : 'Save'}
          isBlankAllowed={isFinding}
          onSave={(body) => actions.draft(nodeId, { threadId: thread.id, body })}
          onClose={() => setEditing(false)}
        />
      ) : (
        canEdit && (
          <div className="flex flex-wrap items-center gap-2">
            <Button variant="secondary" density="compact" onClick={() => setEditing(true)}>
              {!isAdded ? 'Add to message' : isFinding ? 'Edit note' : 'Edit'}
            </Button>
            {isAdded && (
              <Button
                variant="secondary"
                density="compact"
                disabled={status === 'saving'}
                onClick={() => void remove()}
              >
                Remove
              </Button>
            )}
            <ActionStatus status={status} />
          </div>
        )
      )}
    </section>
  );
}

interface ThreadNodeViewProps {
  node: LiveThreadNode;
  actions?: AppNodeActions;
}

/** A thread node: one discussion. */
export function ThreadNodeView({ node, actions }: ThreadNodeViewProps) {
  const thread: LiveThread = {
    id: node.id,
    messages: node.messages,
    ...(node.draft === undefined ? {} : { draft: node.draft }),
  };
  return (
    <div className="min-w-0">
      <ThreadBlock thread={thread} nodeId={node.id} actions={actions} title={node.label} />
    </div>
  );
}

/**
 * A gutter reserves three character slots, and a smaller one for single digits. After
 * the number it keeps two characters free, where the Add comment control stands.
 */
function numberWidth(digits: number): number {
  return digits === 1 ? 41.11 : Math.max(48.94, (digits + 3.25) * 7.83);
}

/** One pointer drag over the diff lines. A `comment` drag opens an editor on its lines when it ends. */
interface LineDrag {
  kind: 'select' | 'comment';
  pointerId: number;
  pivot: ViewAnchor;
  current: ViewAnchor;
}

/**
 * The line under the pointer, in the file of the drag. Like `@pierre/diffs`, the point
 * decides, so the code of a row and the gutter of a row both count. A split row has two
 * lines: the column under the pointer gives the side.
 */
function lineAt(event: PointerEvent, drag: LineDrag): ViewAnchor | null {
  const hit =
    document.elementFromPoint?.(event.clientX, event.clientY) ??
    (event.target instanceof Element ? event.target : null);
  const cells = [...(hit?.closest('tr')?.querySelectorAll<HTMLElement>('[data-line-number]') ?? [])].filter(
    (cell) => cell.dataset.linePath === drag.pivot.path,
  );
  const cell = [...cells].reverse().find((one) => one.getBoundingClientRect().left <= event.clientX) ?? cells[0];
  if (!cell) return null;
  const side = cell.dataset.lineSide === 'old' ? 'old' : 'new';
  return { path: drag.pivot.path, side, line: Number(cell.dataset.lineNumber) };
}

interface GutterProps {
  line: AppDiffLine;
  digits: number;
  canComment: boolean;
  path: string;
  /** `isExtended` is a Shift click: it selects the lines from the last picked line. */
  onActivate: (line: AppDiffLine, isExtended: boolean) => void;
  /** A pointer press on the number or on the control starts a drag over the lines. */
  onPress: (line: AppDiffLine, kind: LineDrag['kind'], event: ReactPointerEvent) => void;
  side?: 'old' | 'new';
  /** The line is the last line of the selection. It holds the Add comment control. */
  selected: boolean;
  /** An editor is open in this diff. Like T3 Code, the hover control then stays away. */
  isDrafting: boolean;
  onComment: (line: AppDiffLine) => void;
}

/** The number stays visible while the file scrolls sideways. */
function Gutter({
  line,
  digits,
  canComment,
  path,
  onActivate,
  onPress,
  side = 'new',
  selected,
  isDrafting,
  onComment,
}: GutterProps) {
  return (
    <td
      data-kind={line.kind}
      className="diff-gutter"
      style={{ width: numberWidth(digits) }}
      data-line-path={line.anchor.path}
      data-line-side={line.anchor.side}
      data-line-number={line.anchor.line}
    >
      {canComment ? (
        <button
          type="button"
          className="diff-line-button app-focus-ring"
          aria-label={`Comment on ${path} ${line.anchor.side === 'old' ? 'old ' : ''}line ${line.anchor.line}`}
          onClick={(event) => onActivate(line, event.shiftKey)}
          onPointerDown={(event) => {
            if (event.button === 0 && !event.shiftKey && event.pointerType !== 'touch') onPress(line, 'select', event);
          }}
        >
          {side === 'old' ? line.old_line : line.anchor.line}
        </button>
      ) : side === 'old' ? (
        line.old_line
      ) : (
        line.anchor.line
      )}
      {/* A pointer gets the control on the line under it. The keyboard and a touch
          screen select the line first, through its number, and then get the control. */}
      {canComment && (selected || !isDrafting) && (
        <button
          type="button"
          className="diff-add-comment app-focus-ring"
          {...(selected ? { 'aria-label': 'Add comment' } : { 'aria-hidden': true, tabIndex: -1, 'data-hover-only': '' })}
          onClick={() => onComment(line)}
          onPointerDown={(event) => {
            if (event.button === 0) onPress(line, 'comment', event);
          }}
        >
          <PlusIcon className="size-3" />
        </button>
      )}
    </td>
  );
}

interface SearchTextProps {
  text: string;
  offset: number;
  match?: DiffSearchMatch;
}

/** The current search occurrence does not erase a changed word's tint. */
function SearchText({ text, offset, match }: SearchTextProps) {
  if (!match || match.end <= offset || match.start >= offset + text.length) return text;
  const start = Math.max(0, match.start - offset);
  const end = Math.min(text.length, match.end - offset);
  return (
    <>
      {text.slice(0, start)}
      <mark data-search-hit>{text.slice(start, end)}</mark>
      {text.slice(end)}
    </>
  );
}

/**
 * The pieces of one changed word in their code colours. Word and colour edges both
 * cut the text, so a coloured token that a change splits keeps its colour on both
 * sides of the change.
 */
function paintedRuns(
  words: readonly DiffSpan[],
  colours: readonly SyntaxSegment[],
): { text: string; start: number; isChanged: boolean; className: string }[][] {
  const edges: { end: number; className: string }[] = [];
  let at = 0;
  for (const colour of colours) {
    at += colour.text.length;
    edges.push({ end: at, className: colour.className });
  }
  let start = 0;
  let edge = 0;
  return words.map((word) => {
    const end = start + word.text.length;
    const runs: { text: string; start: number; isChanged: boolean; className: string }[] = [];
    let from = start;
    while (from < end) {
      while (edge < edges.length && edges[edge]!.end <= from) edge += 1;
      const colour = edges[edge];
      const to = Math.min(end, colour?.end ?? end);
      runs.push({
        text: word.text.slice(from - start, to - start),
        start: from,
        isChanged: word.isChanged,
        className: colour?.className ?? CODE_DEFAULT_CLASS,
      });
      from = to;
    }
    start = end;
    return runs;
  });
}

interface LineCodeProps {
  line: AppDiffLine;
  words?: readonly DiffSpan[];
  match?: DiffSearchMatch;
  /** The Prism language of the file; '' draws the code in one colour. */
  language: string;
}

/** Changed words and code colours retain the same line box as the surrounding code. */
function LineCode({ line, words, match, language }: LineCodeProps) {
  const parts = words ?? [{ text: line.text, isChanged: false }];
  const colours = language ? highlightSegments(line.text, language) : [];
  return (
    <td data-kind={line.kind} className="diff-code">
      {line.kind !== 'context' && (
        <span className="sr-only">{line.kind === 'add' ? 'Added: ' : 'Removed: '}</span>
      )}
      {paintedRuns(parts, colours).map((runs, at) => {
        const pieces = runs.map((run) => {
          const text = <SearchText text={run.text} offset={run.start} match={match} />;
          return run.className === CODE_DEFAULT_CLASS ? (
            <Fragment key={run.start}>{text}</Fragment>
          ) : (
            <span key={run.start} className={run.className}>
              {text}
            </span>
          );
        });
        return parts[at]!.isChanged ? (
          <span key={at} className="diff-word">
            {pieces}
          </span>
        ) : (
          <Fragment key={at}>{pieces}</Fragment>
        );
      })}
    </td>
  );
}

interface HunkProps {
  hunk: AppDiffHunk;
  file: AppDiffFile;
  at: number;
  digits: number;
  words: ReadonlyMap<string, DiffSpan[]>;
  split: boolean;
  nodeId: string;
  byAnchor: ReadonlyMap<string, LiveThread[]>;
  opened: ViewAnchor | null;
  selected: ViewAnchor | null;
  searchAnchor: string | undefined;
  searchMatch: DiffSearchMatch | undefined;
  onComment: (line: AppDiffLine) => void;
  actions?: AppNodeActions;
  onActivate: (line: AppDiffLine, isExtended: boolean) => void;
  onPress: (line: AppDiffLine, kind: LineDrag['kind'], event: ReactPointerEvent) => void;
  /** The position of each line in its file, for the rows of a range. */
  orders: ReadonlyMap<string, number>;
  onClose: () => void;
}

interface LineThreadsProps {
  lines: AppDiffLine[];
  model: HunkProps;
}

/** A thread occupies the file width, rather than the code's scroll width. */
function LineThreads({ lines, model }: LineThreadsProps) {
  const threads = lines.flatMap((line) => model.byAnchor.get(anchorKey(line.anchor)) ?? []);
  const opened = lines.some((line) => model.opened && anchorKey(line.anchor) === anchorKey(model.opened))
    ? model.opened
    : null;
  if (!threads.length && !opened) return null;
  return (
    <tr>
      <td colSpan={model.split ? 4 : 2} className="p-0">
        <div className="diff-threads space-y-2 px-2 py-2">
          {threads.map((thread) => (
            <ThreadBlock
              key={thread.id}
              thread={thread}
              nodeId={model.nodeId}
              actions={model.actions}
              title={`Thread on ${model.file.path} ${thread.anchor ? anchorLines(thread.anchor) : 'line'}`}
            />
          ))}
          {opened && model.actions && (
            <DraftEditor
              memoryId={`${model.nodeId}:${anchorKey(startOf(opened))}:${anchorKey(opened)}`}
              label={`Comment on ${model.file.path} ${anchorLines(opened)}`}
              saved=""
              submitLabel="Comment"
              onSave={(body) =>
                model.actions!.draft(model.nodeId, {
                  anchor: opened,
                  body,
                })
              }
              onClose={model.onClose}
            />
          )}
        </div>
      </td>
    </tr>
  );
}

/** Both modes use one table per hunk inside the file's shared scroller. */
function HunkView(model: HunkProps) {
  const rows: SplitRow[] = model.split
    ? splitRows(model.hunk.lines)
    : model.hunk.lines.map((line) => ({ new: line }));
  const cell = (line: AppDiffLine | undefined, side: 'old' | 'new') =>
    line ? (
      <>
        <Gutter
          line={line}
          digits={model.digits}
          path={model.file.path}
          canComment={Boolean(model.actions) && !model.actions?.isReadOnly}
          onActivate={model.onActivate}
          onPress={model.onPress}
          side={side}
          selected={Boolean(model.selected && anchorKey(line.anchor) === anchorKey(model.selected))}
          isDrafting={model.opened !== null}
          onComment={model.onComment}
        />
        <LineCode
          line={line}
          words={model.words.get(anchorKey(line.anchor))}
          language={languageOfPath(model.file.path)}
          match={
            model.searchMatch && anchorKey(line.anchor) === model.searchAnchor ? model.searchMatch : undefined
          }
        />
      </>
    ) : (
      <>
        <td className="diff-gutter diff-empty" style={{ width: numberWidth(model.digits) }} />
        <td className="diff-empty" />
      </>
    );
  return (
    <div className="diff-hunk-block min-w-0">
      <HunkSeparator hunks={model.file.hunks} at={model.at} />
      <table className="diff-table" data-diff-lines>
        <tbody>
          {rows.map((row, i) => {
            const old =
              row.old?.kind === 'context'
                ? {
                    ...row.old,
                    anchor: {
                      ...row.old.anchor,
                      side: 'old' as const,
                      line: row.old.old_line!,
                    },
                  }
                : row.old;
            const contextOld =
              row.new?.kind === 'context'
                ? {
                    ...row.new,
                    anchor: {
                      ...row.new.anchor,
                      side: 'old' as const,
                      line: row.new.old_line!,
                    },
                  }
                : undefined;
            const lines = [model.split ? old : contextOld, row.new].filter((line): line is AppDiffLine =>
              Boolean(line),
            );
            return (
              <DiffRow
                key={i}
                old={old}
                next={row.new}
                split={model.split}
                lines={lines}
                model={model}
                cell={cell}
              />
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

interface DiffRowProps {
  old?: AppDiffLine;
  next?: AppDiffLine;
  split: boolean;
  lines: AppDiffLine[];
  model: HunkProps;
  cell: (line: AppDiffLine | undefined, side: 'old' | 'new') => ReactNode;
}

function DiffRow({ old, next, split, lines, model, cell }: DiffRowProps) {
  const isSelected = lines.some((line) => inSpan(model.selected, line.anchor, model.orders));
  const selection = model.selected?.start_line === undefined ? 'single' : 'range';
  return (
    <>
      <tr
        data-anchor={next ? anchorKey(next.anchor) : undefined}
        data-search-current={lines.some((line) => anchorKey(line.anchor) === model.searchAnchor) || undefined}
        data-line-kind={!split ? next?.kind : undefined}
        data-selected-line={isSelected ? selection : undefined}
      >
        {split && cell(old, 'old')}
        {cell(next, 'new')}
      </tr>
      <LineThreads lines={lines} model={model} />
    </>
  );
}

const CHANGE_MARK: Record<ReturnType<typeof fileChange>, { letter: string; word: string; ink: string }> = {
  added: { letter: 'A', word: 'Added', ink: 'text-ok-ink' },
  deleted: { letter: 'D', word: 'Deleted', ink: 'text-err-ink' },
  renamed: { letter: 'R', word: 'Renamed', ink: 'text-link-fg' },
  modified: { letter: 'M', word: 'Modified', ink: 'text-link-fg' },
};

interface DiffStatProps {
  additions: number;
  deletions: number;
  isFile?: boolean;
}

/**
 * The +N and -N of a file or a diff: removals first. A file hides a zero side, like
 * T3 Code. A binary file has no lines to count, so it shows no counts.
 */
function DiffStat({ additions, deletions, isFile = false }: DiffStatProps) {
  const both = !isFile || additions === deletions || (additions > 0 && deletions > 0);
  return (
    <span className="diff-stat flex shrink-0 items-center gap-1 font-mono tabular-nums">
      {(both || deletions > 0) && <span className="text-err-ink">-{deletions}</span>}
      {(both || additions > 0) && <span className="text-ok-ink">+{additions}</span>}
    </span>
  );
}

interface FileHeaderProps {
  file: AppDiffFile;
  canFold: boolean;
  isOpen: boolean;
  onToggle: () => void;
}

/**
 * The band of one file: chevron, change mark, path, copy and the line counts. It
 * stays at the top while its lines scroll by, and a press folds the file.
 */
function FileHeader({ file, canFold, isOpen, onToggle }: FileHeaderProps) {
  const kind = fileChange(file);
  const change = CHANGE_MARK[kind];
  const renamed = file.old_path && file.new_path && file.old_path !== file.new_path;
  const label = (
    <>
      <DiffChangeIcon kind={kind} className={`size-4 diff-change-${kind}`} />
      <span className="sr-only">{change.word}</span>
      <span className="diff-file-path">
        {renamed && (
          <>
            <span className="diff-old-path">{file.old_path}</span>
            <RenameArrowIcon className="size-4" />
          </>
        )}
        <span>{file.path}</span>
      </span>
    </>
  );
  if (!canFold) {
    return (
      <header className="diff-file-header">
        <div className="diff-fold">
          <span className="size-4 shrink-0" aria-hidden />
          {label}
        </div>
        <CopyChip
          value={file.path}
          label={`Copy path ${file.path}`}
          title="Copy file path"
          density="compact"
          className="diff-copy"
        />
        <span className="flex-1" />
        {!file.binary && <DiffStat {...fileStat(file)} isFile />}
      </header>
    );
  }
  return (
    <header className={`diff-file-header ${isOpen ? 'is-open' : ''}`}>
      <button
        type="button"
        className="diff-fold app-focus-ring"
        aria-expanded={isOpen}
        aria-label={`${isOpen ? 'Fold' : 'Unfold'} ${file.path}`}
        onClick={onToggle}
      >
        <ChevronIcon open={isOpen} className="size-4 text-dialog-hint" />
        {label}
      </button>
      <CopyChip
        value={file.path}
        label={`Copy path ${file.path}`}
        title="Copy file path"
        density="compact"
        className="diff-copy"
      />
      <span className="flex-1" />
      {!file.binary && <DiffStat {...fileStat(file)} isFile />}
    </header>
  );
}

interface HunkSeparatorProps {
  hunks: readonly AppDiffHunk[];
  at: number;
}

/** The quiet bar between two hunks: how many unchanged lines the diff leaves out. */
function HunkSeparator({ hunks, at }: HunkSeparatorProps) {
  const hidden = hiddenLinesBefore(hunks, at);
  if (hidden === 0) return null;
  const header = hunks[at].header;
  const unit = hidden === 1 ? 'line' : 'lines';
  return (
    <ContextBand
      reach={at === 0 ? 'up' : 'both'}
      title={`${header}; source context is not included in this patch`}
      label={hidden === null ? header : `${hidden} unmodified ${unit}`}
    />
  );
}

interface ContextBandProps {
  label: string;
  title: string;
  reach: 'up' | 'down' | 'both';
}

/**
 * A patch cannot reveal lines that its producer did not include. The mark points to
 * where the left-out lines are, as in T3 Code: up above the first hunk, both ways
 * between two hunks, down after the last.
 */
function ContextBand({ label, title, reach }: ContextBandProps) {
  return (
    <div className="diff-context" title={title}>
      <span className="diff-context-marker" aria-hidden>
        {reach === 'both' ? (
          <ExpandAllIcon className="size-3.5" />
        ) : (
          <ChevronIcon open={reach === 'down'} className={reach === 'up' ? 'size-3.5 [&>path]:-rotate-90' : 'size-3.5'} />
        )}
      </span>
      <span className="diff-hunk">{label}</span>
    </div>
  );
}

/** The lines that a diff mounts at once. The files after them mount only near the viewport. */
const DIFF_EAGER_LINES = 600;
/** A phone shows about 45 rows and builds rows slower, so it mounts fewer lines at once. */
const DIFF_EAGER_LINES_PHONE = 160;
/** A tablet or a desk. A narrower screen is a phone. */
const WIDE_SCREEN = '(min-width: 48rem)';
/** How far outside its scroller a file mounts, so that a scroll meets rows and not a gap. */
const DIFF_NEAR_MARGIN = 1200;
const DIFF_ROW_HEIGHT = 20;
const DIFF_HUNK_HEIGHT = 28;
const DIFF_FILE_HEADER_HEIGHT = 40;

/** The height of a file whose rows are not mounted, before the browser measured it. */
function estimatedFileHeight(file: AppDiffFile): number {
  const lines = file.hunks.reduce((count, hunk) => count + hunk.lines.length, 0);
  return DIFF_FILE_HEADER_HEIGHT + lines * DIFF_ROW_HEIGHT + file.hunks.length * DIFF_HUNK_HEIGHT;
}

/** The files after the eager lines of a diff. A short diff has none. */
function lazyFilePaths(files: readonly AppDiffFile[], eagerLines: number): ReadonlySet<string> {
  const paths = new Set<string>();
  let lines = 0;
  for (const file of files) {
    if (lines >= eagerLines) paths.add(file.path);
    lines += file.hunks.reduce((count, hunk) => count + hunk.lines.length, 0);
  }
  return paths;
}

function scrollerOf(el: Element | undefined): Element | null {
  for (let parent = el?.parentElement ?? null; parent; parent = parent.parentElement) {
    const overflow = getComputedStyle(parent).overflowY;
    if (overflow === 'auto' || overflow === 'scroll') return parent;
  }
  return null;
}

interface NearFiles {
  near: ReadonlySet<string>;
  /** The measured height of a file that left, so that the scroll length stays. */
  heights: ReadonlyMap<string, number>;
}

/**
 * A long diff mounts the rows of a file only near the viewport. All rows at once
 * block the page: 19,000 lines are 139,000 elements.
 * An engine that cannot observe mounts every file.
 */
function useNearFiles(files: readonly AppDiffFile[], sections: { current: Map<string, HTMLElement> }) {
  const eagerLines = useMediaMatch(WIDE_SCREEN) ? DIFF_EAGER_LINES : DIFF_EAGER_LINES_PHONE;
  const lazy = useMemo(() => lazyFilePaths(files, eagerLines), [files, eagerLines]);
  const [seen, setSeen] = useState<NearFiles>(() => ({ near: new Set(), heights: new Map() }));
  const canObserve = typeof IntersectionObserver !== 'undefined';
  useEffect(() => {
    if (!canObserve || lazy.size === 0) return;
    const paths = new Map<Element, string>();
    for (const [path, el] of sections.current) if (lazy.has(path)) paths.set(el, path);
    const observer = new IntersectionObserver(
      (entries) =>
        setSeen((held) => {
          const near = new Set(held.near);
          const heights = new Map(held.heights);
          let isChanged = false;
          for (const entry of entries) {
            const path = paths.get(entry.target);
            if (path === undefined || entry.isIntersecting === near.has(path)) continue;
            isChanged = true;
            if (entry.isIntersecting) near.add(path);
            else {
              near.delete(path);
              heights.set(path, entry.boundingClientRect.height);
            }
          }
          return isChanged ? { near, heights } : held;
        }),
      { root: scrollerOf(paths.keys().next().value), rootMargin: `${DIFF_NEAR_MARGIN}px 0px` },
    );
    for (const el of paths.keys()) observer.observe(el);
    return () => observer.disconnect();
  }, [canObserve, lazy, sections]);
  return {
    isMounted: (path: string) => !canObserve || !lazy.has(path) || seen.near.has(path),
    heights: seen.heights,
  };
}

interface DiffNodeViewProps {
  node: LiveDiffNode;
  /** The name of the diff. It is the first word of the toolbar, so it stays in view with it. */
  heading?: ReactNode;
  actions?: AppNodeActions;
  /** The click and keys of the toolbar buttons. Absent, they are read only. */
  commands?: DiffCommandActions;
}

/** Unified and split diffs share anchored comments, file controls and one scroller per file. */
export function DiffNodeView({ node, heading, actions, commands }: DiffNodeViewProps) {
  const {
    files: sourceFiles,
    words,
    digits,
    columns,
    total,
    elsewhere,
  } = useMemo(() => diffModel(node.patch, node.threads), [node.patch, node.threads]);

  const [opened, setOpened] = useReviewState<ViewAnchor | null>(`${node.id}:opened`, null);
  const [selected, setSelected] = useReviewState<ViewAnchor | null>(`${node.id}:selected`, null);
  const orders = useMemo(() => lineOrders(sourceFiles), [sourceFiles]);

  // The line where a Shift click starts its range.
  const pivot = useRef<ViewAnchor | null>(null);
  const drag = useRef<LineDrag | null>(null);
  // The click that follows a press is part of the drag, not a second action.
  const isClickSkipped = useRef(false);
  const [isDragging, setDragging] = useState(false);

  useEffect(() => {
    if (!isDragging) return;
    const move = (event: PointerEvent) => {
      const held = drag.current;
      if (!held || event.pointerId !== held.pointerId) return;
      event.preventDefault();
      const at = lineAt(event, held);
      if (!at || anchorKey(at) === anchorKey(held.current)) return;
      held.current = at;
      setSelected(anchorRange(held.pivot, at, orders));
    };
    const end = (event: PointerEvent) => {
      const held = drag.current;
      if (!held || event.pointerId !== held.pointerId) return;
      drag.current = null;
      setDragging(false);
      if (event.type === 'pointercancel') return;
      const range = anchorRange(held.pivot, lineAt(event, held) ?? held.current, orders);
      setSelected(range);
      if (held.kind === 'comment') setOpened(range);
      isClickSkipped.current = true;
      setTimeout(() => {
        isClickSkipped.current = false;
      });
    };
    document.addEventListener('pointermove', move);
    document.addEventListener('pointerup', end);
    document.addEventListener('pointercancel', end);
    return () => {
      document.removeEventListener('pointermove', move);
      document.removeEventListener('pointerup', end);
      document.removeEventListener('pointercancel', end);
    };
  }, [isDragging, setSelected, setOpened, orders]);

  const press = (line: AppDiffLine, kind: LineDrag['kind'], event: ReactPointerEvent) => {
    event.preventDefault();
    // The control on a line of the selection drags the end of the selection, as in T3 Code.
    const held = kind === 'comment' && selected && inSpan(selected, line.anchor, orders) ? selected : null;
    const start = held ? startOf(held) : line.anchor;
    const current = held ? { path: held.path, side: held.side, line: held.line } : line.anchor;
    drag.current = { kind, pointerId: event.pointerId, pivot: start, current };
    pivot.current = start;
    setSelected(anchorRange(start, current, orders));
    setOpened(null);
    setDragging(true);
  };

  const [folded, setFolded] = useReviewState<ReadonlySet<string>>(`${node.id}:folded`, () => new Set());
  const [split, setSplit] = useReviewPref(`${node.id}:split`, false);
  const [wrap, setWrap] = useReviewPref(`${node.id}:wrap`, false);
  const [tree, setTree] = useReviewPref(`${node.id}:tree`, false);
  const [pickedFile, setPickedFile] = useReviewState<string | null>(`${node.id}:pickedFile`, null);
  const [treeFolded, setTreeFolded] = useReviewState<ReadonlySet<string>>(`${node.id}:treeFolded`, () => new Set());
  const [hideTests, setHideTests] = useReviewPref(`${node.id}:hideTests`, false);
  const [hideWhitespace, setHideWhitespace] = useReviewPref(`${node.id}:hideWhitespace`, false);

  const files = useMemo(
    () => filterDiffFiles(sourceFiles, hideTests, hideWhitespace),
    [sourceFiles, hideTests, hideWhitespace],
  );

  const refs = useRef(new Map<string, HTMLElement>());
  const container = useRef<HTMLDivElement>(null);

  const search = useDiffSearch(files, container, (line) => {
    if (line)
      setFolded((held) => {
        const next = new Set(held);
        next.delete(line.anchor.path);
        return next;
      });
  });

  const { searchAnchor, currentResult } = search;
  const nearFiles = useNearFiles(files, refs);

  // The open editor and the current match stay mounted wherever the scroll is.
  const hasRows = (path: string) =>
    nearFiles.isMounted(path) || opened?.path === path || currentResult?.line.anchor.path === path;

  const byAnchor = useMemo(() => {
    const map = new Map<string, LiveThread[]>();
    for (const thread of node.threads) {
      if (!thread.anchor) continue;
      const key = anchorKey(thread.anchor);
      map.set(key, [...(map.get(key) ?? []), thread]);
    }
    return map;
  }, [node.threads]);

  const toggle = (path: string) =>
    setFolded((held) => {
      const next = new Set(held);
      if (next.has(path)) next.delete(path);
      else next.add(path);
      return next;
    });

  const previews = useMemo(() => {
    const byPath = new Map<string, LiveDiffPreview['node'][]>();
    for (const { path, node: preview } of node.previews ?? []) {
      const held = byPath.get(path);
      if (held) held.push(preview);
      else byPath.set(path, [preview]);
    }
    return byPath;
  }, [node.previews]);
  const canFold = (file: AppDiffFile) => file.hunks.length > 0 || previews.has(file.path);
  const foldable = files.filter(canFold);
  const allFolded = foldable.length > 0 && foldable.every((file) => folded.has(file.path));

  return (
    <div
      ref={container}
      tabIndex={0}
      className="app-diff min-w-0 space-y-3 app-focus-ring"
      data-wrap={wrap}
      data-split={split}
      data-dragging={isDragging || undefined}
      onKeyDown={(event) => {
        if (event.nativeEvent.isComposing || event.keyCode === 229 || event.altKey) return;
        if (event.key.toLowerCase() === 'f' && !event.shiftKey && (event.metaKey || event.ctrlKey)) {
          event.preventDefault();
          event.stopPropagation();
          search.show();
        } else if (search.findOpen && event.key.toLowerCase() === 'g' && (event.metaKey || event.ctrlKey)) {
          event.preventDefault();
          event.stopPropagation();
          search.moveMatch(event.shiftKey ? -1 : 1);
        } else if (search.findOpen && event.key === 'Escape') {
          event.preventDefault();
          event.stopPropagation();
          search.closeFind();
        }
      }}
    >
      <DiffSearch search={search} />
      <DiffToolbar
        onFind={search.show}
        split={split}
        wrap={wrap}
        tree={tree}
        allFolded={allFolded}
        count={files.length}
        total={total}
        base={node.old_revision}
        heading={heading}
        nodes={node.children ?? []}
        commands={commands}
        hideTests={hideTests}
        hideWhitespace={hideWhitespace}
        onTests={() => setHideTests(!hideTests)}
        onWhitespace={() => setHideWhitespace(!hideWhitespace)}
        onSplit={setSplit}
        onWrap={() => setWrap(!wrap)}
        onTree={() => setTree(!tree)}
        onFold={() => setFolded(allFolded ? new Set() : new Set(foldable.map((file) => file.path)))}
      />
      <div className={`min-w-0 ${tree ? 'diff-with-tree' : ''}`}>
        {tree && (
          <nav className="diff-file-tree" aria-label="Changed files">
            <div className="diff-tree-header">
              Files <span>{files.length}</span>
            </div>
            {fileTree(files, treeFolded).map((row) =>
              row.kind === 'folder' ? (
                <button
                  type="button"
                  aria-label={`Folder ${row.key}`}
                  aria-expanded={!treeFolded.has(row.key)}
                  onClick={() =>
                    setTreeFolded((held) => {
                      const next = new Set(held);
                      if (next.has(row.key)) next.delete(row.key);
                      else next.add(row.key);
                      return next;
                    })
                  }
                  key={row.key}
                  className="diff-tree-folder app-focus-ring"
                  style={{ paddingLeft: 8 + row.depth * 12 }}
                >
                  <ChevronIcon open={!treeFolded.has(row.key)} className="size-3" />
                  {row.name}
                </button>
              ) : (
                <button
                  key={row.key}
                  type="button"
                  aria-label={`Open ${row.file.path}`}
                  aria-current={pickedFile === row.file.path ? true : undefined}
                  className={`diff-tree-file diff-tree-${fileChange(row.file)} app-focus-ring`}
                  style={{ paddingLeft: 8 + row.depth * 12 }}
                  onClick={() => {
                    setPickedFile(row.file.path);
                    setFolded((held) => {
                      const next = new Set(held);
                      next.delete(row.file.path);
                      return next;
                    });
                    refs.current.get(row.file.path)?.scrollIntoView({ block: 'nearest' });
                  }}
                >
                  <ReviewFileIcon path={row.file.path} className="size-3.5" />
                  <span className="min-w-0 flex-1 truncate" title={row.file.path}>
                    {row.name}
                  </span>
                  <span className="diff-tree-status">{CHANGE_MARK[fileChange(row.file)].letter}</span>
                </button>
              ),
            )}
          </nav>
        )}
        <div className="diff-files min-w-0">
          {!files.length && <p className="diff-no-changes">No net changes in this selection.</p>}
          {files.map((file) => (
            <section
              key={file.path}
              ref={(el) => {
                if (el) refs.current.set(file.path, el);
                else refs.current.delete(file.path);
              }}
              className="diff-file min-w-0"
              style={
                {
                  '--diff-number-width': `${numberWidth(digits.get(file.path) ?? 1)}px`,
                  '--diff-code-columns': columns.get(file.path) ?? 0,
                  // The height that an unpainted file stands in for until it is painted once.
                  '--diff-file-height': `${folded.has(file.path) ? DIFF_FILE_HEADER_HEIGHT : estimatedFileHeight(file)}px`,
                  minHeight:
                    folded.has(file.path) || hasRows(file.path)
                      ? undefined
                      : (nearFiles.heights.get(file.path) ?? estimatedFileHeight(file)),
                } as CSSProperties
              }
              aria-label={file.path}
            >
              <FileHeader file={file} canFold={canFold(file)} isOpen={!folded.has(file.path)} onToggle={() => toggle(file.path)} />
              {!folded.has(file.path) && previews.get(file.path)?.map((preview, at) => (
                <div key={`${preview.id}:${at}`} className="p-2">
                  {preview.type === 'image' ? <ImageNodeView node={preview} /> : <MediaNodeView node={preview} />}
                </div>
              ))}
              {!folded.has(file.path) && file.hunks.length > 0 && hasRows(file.path) && (
                <div className="diff-file-scroll" data-diff-scroll>
                  {file.hunks.map((hunk, at) => (
                    <HunkView
                      key={`${hunk.header}:${at}`}
                      hunk={hunk}
                      file={file}
                      at={at}
                      digits={digits.get(file.path) ?? 1}
                      words={words}
                      split={split}
                      nodeId={node.id}
                      byAnchor={byAnchor}
                      opened={opened}
                      actions={actions}
                      onActivate={(line, isExtended) => {
                        if (isClickSkipped.current) return;
                        const from = pivot.current ?? (selected && startOf(selected));
                        if (isExtended && from) {
                          setSelected(anchorRange(from, line.anchor, orders));
                        } else {
                          pivot.current = line.anchor;
                          setSelected(line.anchor);
                        }
                        setOpened(null);
                      }}
                      onPress={press}
                      orders={orders}
                      selected={selected}
                      searchAnchor={searchAnchor}
                      searchMatch={currentResult}
                      onComment={(line) => {
                        if (isClickSkipped.current) return;
                        // The control on a line of the selection comments on all its lines.
                        const anchor = selected && inSpan(selected, line.anchor, orders) ? selected : line.anchor;
                        if (anchor === line.anchor) pivot.current = line.anchor;
                        setSelected(anchor);
                        setOpened(anchor);
                      }}
                      onClose={() => {
                        // A closed editor ends the comment, so its lines are no longer selected.
                        setOpened(null);
                        setSelected(null);
                        pivot.current = null;
                      }}
                    />
                  ))}
                  {fileChange(file) === 'modified' && (
                    <ContextBand
                      reach="down"
                      label="More unchanged context may be available"
                      title="Source context is not included in this patch"
                    />
                  )}
                </div>
              )}
            </section>
          ))}
        </div>
      </div>
      {elsewhere.length > 0 && (
        <div className="space-y-2">
          {elsewhere.map((thread) => (
            <ThreadBlock key={thread.id} thread={thread} nodeId={node.id} actions={actions} />
          ))}
        </div>
      )}
    </div>
  );
}


/** What the toolbar of a diff needs from its view: the click and the key of each button. */
export interface DiffCommandActions {
  onActivate?: (id: string) => Promise<void> | void;
  keys?: ReadonlyMap<string, LiveShortcut>;
  isSettled: boolean;
  renderToggle?: (node: LiveInputNode) => ReactNode;
}

function menuItems(nodes: readonly LiveNode[], keys?: ReadonlyMap<string, LiveShortcut>): DropdownItem[] {
  return nodes.flatMap<DropdownItem>((node) => {
    if (node.type === 'divider') return [{ id: node.id, label: '', isSeparator: true }];
    if (node.type !== 'menu' && node.type !== 'button') return [];
    const Icon = node.icon && COMMAND_ICONS[node.icon];
    const item = { id: node.id, label: node.label, icon: Icon && <Icon className="size-3.5 shrink-0" /> };
    if (node.type === 'menu') return [{ ...item, children: menuItems(node.children, keys) }];
    const shortcut = keys?.get(node.id);
    return [{ ...item, disabled: node.is_disabled, checked: node.is_selected, danger: node.variant === 'destructive', shortcut: shortcut && ariaShortcut(shortcut.key) }];
  });
}

export function AppCommandMenu({ node, isSettled, onActivate, keys }: DiffCommandActions & { node: LiveMenuNode }) {
  const [pending, setPending] = useState(false);
  const Icon = node.icon && COMMAND_ICONS[node.icon];
  return (
    <DropdownMenu
      nodeId={node.id}
      label={node.label}
      icon={Icon && <Icon className="size-4" />}
      items={menuItems(node.children, keys)}
      disabled={isSettled || pending || !onActivate}
      onChoose={async (id) => {
        if (!onActivate || pending) return;
        setPending(true);
        try {
          await onActivate(id);
        } finally {
          setPending(false);
        }
      }}
    />
  );
}

function DiffCommand({ node, commands }: { node: LiveNode; commands?: DiffCommandActions }) {
  const [pending, setPending] = useState(false);
  if (node.type === 'toggle') return <span data-view-control className="min-w-0">{commands?.renderToggle?.(node)}</span>;
  if (node.type === 'menu') return <span data-view-control className="min-w-0"><AppCommandMenu node={node} isSettled={commands?.isSettled ?? true} keys={commands?.keys} onActivate={commands?.onActivate} /></span>;
  if (node.type !== 'button') return null;
  const Icon = node.icon && COMMAND_ICONS[node.icon];
  const shortcut = commands?.keys?.get(node.id);
  return (
    <button
      type="button"
      className={Icon ? 'app-focus-ring' : 'diff-command app-focus-ring'}
      data-node-id={node.id}
      data-variant={node.variant}
      aria-label={Icon ? node.label : undefined}
      aria-pressed={node.is_selected}
      title={shortcut ? `${node.label} (${shortcut.key})` : node.label}
      aria-keyshortcuts={shortcut ? ariaShortcut(shortcut.key) : undefined}
      aria-busy={pending}
      disabled={commands?.isSettled || !commands?.onActivate || pending || node.is_disabled}
      onClick={async () => {
        if (!commands?.onActivate || pending) return;
        setPending(true);
        try {
          await commands.onActivate(node.id);
        } finally {
          setPending(false);
        }
      }}
    >
      {Icon === RefreshIcon ? <RefreshIcon isBusy={pending} className="size-3.5" /> : Icon ? (
        <Icon className="size-3.5" />
      ) : <span className="min-w-0 truncate">{node.label}</span>}
    </button>
  );
}

interface DiffToolbarProps {
  /** The app's own commands (`view.json` `diff` `children`). */
  nodes: readonly LiveNode[];
  commands?: DiffCommandActions;
  onFind: () => void;
  split: boolean;
  wrap: boolean;
  tree: boolean;
  allFolded: boolean;
  count: number;
  total: { additions: number; deletions: number };
  /** The revision that the diff compares against, if the producer names it. */
  base?: string;
  heading?: ReactNode;
  onSplit: (split: boolean) => void;
  onWrap: () => void;
  onTree: () => void;
  onFold: () => void;
  hideTests: boolean;
  hideWhitespace: boolean;
  onTests: () => void;
  onWhitespace: () => void;
}

/**
 * The header row of a diff, laid out like the T3 Code review panel: its name, the app's
 * menus, the base it compares against and the line counts on the left; the app's
 * icon commands and the display toggles on the right; the app's icon menus last.
 */
function DiffToolbar({
  nodes,
  commands,
  onFind,
  split,
  wrap,
  tree,
  allFolded,
  count,
  total,
  base,
  heading,
  onSplit,
  onWrap,
  onTree,
  onFold,
  hideTests,
  hideWhitespace,
  onTests,
  onWhitespace,
}: DiffToolbarProps) {
  const icon = (node: LiveNode) => (node.type === 'menu' || node.type === 'button' ? node.icon : undefined);
  const menus = nodes.filter((node) => !icon(node));
  const iconActions = nodes.filter((node) => icon(node) && node.type === 'button');
  const iconMenus = nodes.filter((node) => icon(node) && node.type === 'menu');
  // The file headers stick below the toolbar, so the diff knows its height.
  const bar = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const node = bar.current;
    // The scroller takes it too: its scrollbar starts below the toolbar.
    const diff = node?.closest<HTMLElement>('[data-app-surface]') ?? node?.parentElement;
    if (!node || !diff || typeof ResizeObserver === 'undefined') return;
    const watch = new ResizeObserver(() =>
      diff.style.setProperty('--diff-toolbar-height', `${node.offsetHeight}px`),
    );
    watch.observe(node);
    return () => watch.disconnect();
  }, []);
  return (
    <div ref={bar} className="diff-toolbar" role="toolbar" aria-label="Diff display">
      <span className="diff-toolbar-summary">
        {heading}
        {menus.length > 0 ? (
          <span className="diff-commands" role="group" aria-label="Review commands">
            {menus.map((node) => (
              <DiffCommand key={node.id} node={node} commands={commands} />
            ))}
          </span>
        ) : (
          <span className="shrink-0">
            {count} {count === 1 ? 'file' : 'files'}
          </span>
        )}
        {menus.length > 0 && base && (
          <span className="diff-base min-w-0 truncate" title={`Compared with ${base}`}>
            vs {base}
          </span>
        )}
        <DiffStat {...total} />
      </span>
      <div className="diff-toolbar-controls">
        <button
          type="button"
          className="sr-only"
          data-diff-find
          tabIndex={-1}
          aria-label="Find in diff"
          aria-keyshortcuts="Meta+f Control+f"
          onClick={onFind}
        />
        {iconActions.map((node) => (
          <DiffCommand key={node.id} node={node} commands={commands} />
        ))}
        <button
          type="button"
          className="app-focus-ring"
          aria-label={allFolded ? 'Expand all' : 'Collapse all'}
          title={allFolded ? 'Expand all' : 'Collapse all'}
          onClick={onFold}
        >
          {allFolded ? <ExpandAllIcon className="size-3.5" /> : <CollapseAllIcon className="size-3.5" />}
        </button>
        <div className="diff-layout" role="group" aria-label="Diff layout">
          <button
            type="button"
            className="app-focus-ring"
            aria-label="Unified"
            title="Stacked"
            aria-pressed={!split}
            onClick={() => onSplit(false)}
          >
            <UnifiedDiffIcon className="size-3.5" />
          </button>
          <button
            type="button"
            className="app-focus-ring"
            aria-label="Split"
            title="Split"
            aria-pressed={split}
            onClick={() => onSplit(true)}
          >
            <SideBySideIcon className="size-3.5" />
          </button>
        </div>
        <button
          type="button"
          className="app-focus-ring"
          aria-label="Wrap"
          title="Wrap lines"
          aria-pressed={wrap}
          onClick={onWrap}
        >
          <WrapIcon className="size-3.5" />
        </button>
        <button
          type="button"
          className="app-focus-ring"
          aria-label="Hide whitespace changes"
          title={hideWhitespace ? 'Show whitespace changes' : 'Hide whitespace changes'}
          aria-pressed={hideWhitespace}
          onClick={onWhitespace}
        >
          <WhitespaceIcon className="size-3.5" />
        </button>
        <button
          type="button"
          className="app-focus-ring"
          aria-label="Hide test files"
          title={hideTests ? 'Show test files' : 'Hide test files'}
          aria-pressed={hideTests}
          onClick={onTests}
        >
          <TestFilesIcon className="size-3.5" />
        </button>
        <button
          type="button"
          className="app-focus-ring"
          aria-label="Files"
          title="File tree"
          aria-pressed={tree}
          onClick={onTree}
        >
          <FileTreeIcon className="size-3.5" />
        </button>
        {iconMenus.map((node) => (
          <DiffCommand key={node.id} node={node} commands={commands} />
        ))}
      </div>
    </div>
  );
}
