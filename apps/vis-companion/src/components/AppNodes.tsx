/**
 * The review nodes of an app attachment: a `diff` with threads on its lines, and
 * a `thread` of its own. Both read the same host state and save through the same
 * `draft` and `submit` actions. Without an app the threads are read only.
 */

import { Fragment, useEffect, useMemo, useRef, useState, type CSSProperties, type ReactNode } from 'react';
import { createPortal } from 'react-dom';

import {
  APP_ACTION_WORDS,
  SendKeys,
  isRetryable,
  type AppActionResult,
  type AppActionStatus,
} from '../lib/app-actions';
import {
  anchorKey,
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
import {
  LIVE_MESSAGE_CHARS,
  type LiveDiffNode,
  type LiveMessage,
  type LiveNode,
  type LiveShortcut,
  type LiveSubmission,
  type LiveThread,
  type LiveThreadNode,
} from '../lib/live-view';
import type { ViewAnchor } from '../lib/view';
import {
  ChevronIcon,
  DiffChangeIcon,
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
import { menuPosition, type MenuPosition } from '../lib/anchored-menu';
import { ariaShortcut } from '../lib/app-shortcuts';
import { Button, CopyChip } from './ui';
import { useReviewState } from '../lib/review-memory';

/** What an app gives its review nodes. Absent, the nodes are read only. */
export interface AppNodeActions {
  /** Save the draft of one thread, or start a thread on `anchor`. */
  draft: (
    nodeId: string,
    target: { threadId?: string; anchor?: ViewAnchor; body: string },
  ) => Promise<AppActionResult>;
  /** Send every saved draft of the node with this idempotency key. */
  send: (nodeId: string, idempotencyKey: string) => Promise<AppActionResult>;
  /** The newest receipts of the app, oldest first. */
  submissions: readonly LiveSubmission[];
  /** The app is closed or its owner is not connected: show, do not act. */
  isReadOnly: boolean;
}

const TEXTAREA =
  'block w-full resize-y border border-edge bg-input px-2.5 py-1 font-mono text-ui text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted';

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

/**
 * The state word of a Save or a Send. Nothing shows while it is idle. On a dialog
 * band the word takes the band's ink: a warning amber on the amber band is unreadable.
 */
export function ActionStatus({ status, isOnBand = false }: { status: AppActionStatus; isOnBand?: boolean }) {
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

function Messages({ messages }: { messages: readonly LiveMessage[] }) {
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

/**
 * The editor of one draft. Save keeps the draft on the host; Cancel closes the
 * editor and keeps the history and the saved draft. A blank Save on a new line
 * creates nothing.
 */
function DraftEditor({
  label,
  saved,
  memoryId,
  submitLabel = 'Save',
  onSave,
  onClose,
}: {
  label: string;
  saved: string;
  memoryId: string;
  submitLabel?: string;
  onSave: (body: string) => Promise<AppActionResult>;
  onClose: () => void;
}) {
  const [text, setText] = useReviewState(`${memoryId}:text`, saved);
  const close = () => {
    setText(saved);
    onClose();
  };
  const [status, setStatus] = useState<AppActionStatus>('idle');
  const isBusy = status === 'saving';
  const saving = useRef(false);
  const save = async () => {
    if (saving.current) return;
    if (text.trim() === '' && saved === '') {
      onClose();
      return;
    }
    saving.current = true;
    setStatus('saving');
    try {
      const result = await onSave(text.trim() === '' ? '' : text);
      setStatus(result.status);
      if (result.status === 'saved' && (text.trim() === '' || submitLabel === 'Comment')) {
        setText('');
        onClose();
      }
    } catch {
      setStatus('failed');
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
        placeholder="Add a comment…"
        className={TEXTAREA}
        value={text}
        maxLength={LIVE_MESSAGE_CHARS}
        aria-label={label}
        aria-keyshortcuts="Meta+Enter Control+Enter Escape"
        disabled={isBusy}
        onChange={(event) => {
          setText(event.target.value);
          if (status !== 'saving') setStatus('idle');
        }}
        onKeyDown={(event) => {
          if (
            !event.nativeEvent.isComposing && event.keyCode !== 229 &&
            event.key === 'Enter' &&
            (event.metaKey || event.ctrlKey) &&
            text.trim() !== '' &&
            !saving.current
          ) {
            event.preventDefault();
            void save();
          }
        }}
      />
      <div className="diff-draft-actions flex flex-wrap items-center gap-2">
        <span className="diff-submit-hint">⌘/Ctrl Enter to send</span>
        <Button type="button" variant="secondary" onClick={close}>
          Cancel
        </Button>
        <Button
          type="submit"
          variant="primary"
          disabled={isBusy || (submitLabel === 'Comment' && text.trim() === '')}
        >
          {isRetryable(status) ? 'Retry' : submitLabel}
        </Button>
        <ActionStatus status={status} />
      </div>
    </form>
  );
}

/** One thread: its history, its saved draft and a way to edit the draft. */
function ThreadBlock({
  thread,
  nodeId,
  actions,
  title,
}: {
  thread: LiveThread;
  nodeId: string;
  actions?: AppNodeActions;
  title?: string;
}) {
  const [isEditing, setEditing] = useReviewState(`${nodeId}:${thread.id}:editing`, false);
  const draft = thread.draft ?? '';
  const canEdit = Boolean(actions) && !actions?.isReadOnly;
  return (
    <section
      className="min-w-0 space-y-2 border-l-2 border-dialog-edge bg-panel-2 px-3 py-2"
      aria-label={title ?? 'Thread'}
      data-thread-id={thread.id}
    >
      <Messages messages={thread.messages} />
      {draft !== '' && !isEditing && (
        <div className="min-w-0">
          <span className="block font-mono text-ui text-dialog-hint">Draft</span>
          <p className="whitespace-pre-wrap font-mono text-body text-dialog-foreground [overflow-wrap:anywhere]">
            {draft}
          </p>
        </div>
      )}
      {isEditing && actions ? (
        <DraftEditor
          memoryId={`${nodeId}:${thread.id}`}
          label={draft === '' ? 'Reply' : 'Edit draft'}
          saved={draft}
          onSave={(body) => actions.draft(nodeId, { threadId: thread.id, body })}
          onClose={() => setEditing(false)}
        />
      ) : (
        canEdit && (
          <Button variant="secondary" density="compact" onClick={() => setEditing(true)}>
            {draft === '' ? 'Reply' : 'Edit draft'}
          </Button>
        )
      )}
    </section>
  );
}

const RECEIPT_WORDS: Record<LiveSubmission['status'], string> = {
  accepted: 'Accepted',
  delivered: 'Delivered',
  processing: 'Processing',
  answered: 'Answered',
  failed: 'Failed',
};

/** The Send control of a node and the receipts of its earlier Sends. */
function SendBar({
  nodeId,
  drafts,
  actions,
  label,
}: {
  nodeId: string;
  drafts: number;
  actions: AppNodeActions;
  label: string;
}) {
  const keys = useRef(new SendKeys());
  const [status, setStatus] = useState<AppActionStatus>('idle');
  const [isRetryPending, setRetryPending] = useState(false);
  const receipts = actions.submissions.filter((one) => one.node_id === nodeId).slice(-3);
  const send = async () => {
    setStatus('saving');
    const result = await actions.send(nodeId, keys.current.next());
    keys.current.settle(result);
    setRetryPending(isRetryable(result.status));
    setStatus(result.status);
  };
  const isBusy = status === 'saving';
  return (
    <div className="space-y-1">
      <div className="flex flex-wrap items-center gap-2">
        <Button
          variant="primary"
          disabled={actions.isReadOnly || isBusy || (drafts === 0 && !isRetryPending)}
          aria-label={label}
          onClick={() => void send()}
        >
          {isRetryable(status) ? 'Retry send' : drafts > 1 ? `Send ${drafts}` : 'Send'}
        </Button>
        {status !== 'saved' && <ActionStatus status={status} />}
      </div>
      {receipts.length > 0 && (
        <ul aria-label="Receipts" className="space-y-0.5">
          {receipts.map((receipt) => (
            <li
              key={receipt.id}
              data-submission-id={receipt.id}
              className={`font-mono text-ui ${
                receipt.status === 'failed'
                  ? 'text-err'
                  : receipt.status === 'answered'
                    ? 'text-ok'
                    : 'text-dialog-hint'
              }`}
            >
              {RECEIPT_WORDS[receipt.status]}
              {receipt.comments && receipt.comments.length > 0
                ? ` · ${receipt.comments.length} ${receipt.comments.length === 1 ? 'comment' : 'comments'}`
                : ''}
              {receipt.note ? ` · ${receipt.note}` : ''}
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}

function savedDrafts(threads: readonly LiveThread[]): number {
  return threads.filter((thread) => (thread.draft ?? '').trim() !== '').length;
}

/** A thread node: one discussion with its own Send. */
export function ThreadNodeView({ node, actions }: { node: LiveThreadNode; actions?: AppNodeActions }) {
  const thread: LiveThread = {
    id: node.id,
    messages: node.messages,
    ...(node.draft === undefined ? {} : { draft: node.draft }),
  };
  return (
    <div className="min-w-0 space-y-2">
      <ThreadBlock thread={thread} nodeId={node.id} actions={actions} title={node.label} />
      {actions && (
        <SendBar
          nodeId={node.id}
          drafts={savedDrafts([thread])}
          actions={actions}
          label={`Send ${node.label ?? 'thread'}`}
        />
      )}
    </div>
  );
}

/** A gutter reserves three character slots, and a smaller one for single digits. */
function numberWidth(digits: number): number {
  return digits === 1 ? 33.28 : Math.max(41.11, (digits + 2.25) * 7.83);
}

/** The number stays visible while the file scrolls sideways. */
function Gutter({
  line,
  digits,
  canComment,
  path,
  onActivate,
  side = 'new',
  selected,
  onComment,
}: {
  line: AppDiffLine;
  digits: number;
  canComment: boolean;
  path: string;
  onActivate: (line: AppDiffLine) => void;
  side?: 'old' | 'new';
  selected: boolean;
  onComment: (line: AppDiffLine) => void;
}) {
  return (
    <td data-kind={line.kind} className="diff-gutter" style={{ width: numberWidth(digits) }}>
      {canComment ? (
        <button
          type="button"
          className="diff-line-button app-focus-ring"
          aria-label={`Comment on ${path} ${line.anchor.side === 'old' ? 'old ' : ''}line ${line.anchor.line}`}
          onClick={() => onActivate(line)}
        >
          {side === 'old' ? line.old_line : line.anchor.line}
        </button>
      ) : side === 'old' ? (
        line.old_line
      ) : (
        line.anchor.line
      )}
      {selected && canComment && (
        <button
          type="button"
          className="diff-add-comment app-focus-ring"
          aria-label="Add comment"
          onClick={() => onComment(line)}
        >
          <PlusIcon className="size-4" />
        </button>
      )}
    </td>
  );
}

/** The current search occurrence does not erase a changed word's tint. */
function SearchText({ text, offset, match }: { text: string; offset: number; match?: DiffSearchMatch }) {
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

/** Changed words retain the same line box as the surrounding code. */
function LineCode({
  line,
  words,
  match,
}: {
  line: AppDiffLine;
  words?: readonly DiffSpan[];
  match?: DiffSearchMatch;
}) {
  const parts = words ?? [{ text: line.text, isChanged: false }];
  return (
    <td data-kind={line.kind} className="diff-code">
      {line.kind !== 'context' && (
        <span className="sr-only">{line.kind === 'add' ? 'Added: ' : 'Removed: '}</span>
      )}
      {parts.map((part, at) => {
        const start = parts.slice(0, at).reduce((offset, previous) => offset + previous.text.length, 0);
        const text = <SearchText text={part.text} offset={start} match={match} />;
        return part.isChanged ? (
          <span key={at} className="diff-word">
            {text}
          </span>
        ) : (
          <Fragment key={at}>{text}</Fragment>
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
  onActivate: (line: AppDiffLine) => void;
  onClose: () => void;
}

/** A thread occupies the file width, rather than the code's scroll width. */
function LineThreads({ lines, model }: { lines: AppDiffLine[]; model: HunkProps }) {
  const threads = lines.flatMap((line) => model.byAnchor.get(anchorKey(line.anchor)) ?? []);
  const selected = lines.find((line) => model.opened && anchorKey(line.anchor) === anchorKey(model.opened));
  if (!threads.length && !selected) return null;
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
              title={`Thread on ${model.file.path} line ${thread.anchor?.line}`}
            />
          ))}
          {selected && model.actions && (
            <DraftEditor
              memoryId={`${model.nodeId}:${anchorKey(selected.anchor)}`}
              label={`Comment on ${model.file.path} line ${selected.anchor.line}`}
              saved=""
              submitLabel="Comment"
              onSave={(body) =>
                model.actions!.draft(model.nodeId, {
                  anchor: selected.anchor,
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
          side={side}
          selected={Boolean(model.selected && anchorKey(line.anchor) === anchorKey(model.selected))}
          onComment={model.onComment}
        />
        <LineCode
          line={line}
          words={model.words.get(anchorKey(line.anchor))}
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

function DiffRow({
  old,
  next,
  split,
  lines,
  model,
  cell,
}: {
  old?: AppDiffLine;
  next?: AppDiffLine;
  split: boolean;
  lines: AppDiffLine[];
  model: HunkProps;
  cell: (line: AppDiffLine | undefined, side: 'old' | 'new') => ReactNode;
}) {
  return (
    <>
      <tr
        data-anchor={next ? anchorKey(next.anchor) : undefined}
        data-search-current={lines.some((line) => anchorKey(line.anchor) === model.searchAnchor) || undefined}
        data-line-kind={!split ? next?.kind : undefined}
        data-selected-line={
          lines.some((line) => model.selected && anchorKey(line.anchor) === anchorKey(model.selected))
            ? 'single'
            : undefined
        }
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

/** The +N and -N of a file or a diff: removals first. */
function DiffStat({ additions, deletions }: { additions: number; deletions: number }) {
  return (
    <span className="diff-stat flex shrink-0 items-center gap-1 font-mono tabular-nums">
      <span className="text-err-ink">-{deletions}</span>
      <span className="text-ok-ink">+{additions}</span>
    </span>
  );
}

/**
 * The band of one file: chevron, change mark, path, copy and the line counts. It
 * stays at the top while its lines scroll by, and a press folds the file.
 */
function FileHeader({
  file,
  isOpen,
  onToggle,
}: {
  file: AppDiffFile;
  isOpen: boolean;
  onToggle: () => void;
}) {
  const kind = fileChange(file);
  const change = CHANGE_MARK[kind];
  const renamed = file.old_path && file.new_path && file.old_path !== file.new_path;
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
      </button>
      <CopyChip
        value={file.path}
        label={`Copy path ${file.path}`}
        title="Copy file path"
        density="compact"
        className="diff-copy"
      />
      <span className="flex-1" />
      <DiffStat {...fileStat(file)} />
    </header>
  );
}

/** The quiet bar between two hunks: how many unchanged lines the diff leaves out. */
function HunkSeparator({ hunks, at }: { hunks: readonly AppDiffHunk[]; at: number }) {
  const hidden = hiddenLinesBefore(hunks, at);
  if (hidden === 0) return null;
  const header = hunks[at].header;
  return (
    <ContextBand
      title={`${header}; source context is not included in this patch`}
      label={hidden === null ? header : `${hidden} unmodified ${hidden === 1 ? 'line' : 'lines'}`}
    />
  );
}

/** A patch cannot reveal lines that its producer did not include. */
function ContextBand({ label, title }: { label: string; title: string }) {
  return (
    <div className="diff-context" title={title}>
      <span className="diff-context-marker" aria-hidden>
        <ExpandAllIcon className="size-3" />
      </span>
      <span className="diff-hunk">{label}</span>
    </div>
  );
}

/** Unified and split diffs share anchored comments, file controls and one scroller per file. */
export function DiffNodeView({
  node,
  actions,
  commands,
}: {
  node: LiveDiffNode;
  actions?: AppNodeActions;
  /** The click and keys of the toolbar buttons. Absent, they are read only. */
  commands?: DiffCommandActions;
}) {
  const {
    files: sourceFiles,
    words,
    digits,
    total,
    elsewhere,
  } = useMemo(() => diffModel(node.patch, node.threads), [node.patch, node.threads]);
  const [opened, setOpened] = useReviewState<ViewAnchor | null>(`${node.id}:opened`, null);
  const [selected, setSelected] = useReviewState<ViewAnchor | null>(`${node.id}:selected`, null);
  const [folded, setFolded] = useReviewState<ReadonlySet<string>>(`${node.id}:folded`, () => new Set());
  const [split, setSplit] = useReviewState(`${node.id}:split`, false);
  const [wrap, setWrap] = useReviewState(`${node.id}:wrap`, true);
  const [tree, setTree] = useReviewState(`${node.id}:tree`, false);
  const [pickedFile, setPickedFile] = useReviewState<string | null>(`${node.id}:pickedFile`, null);
  const [treeFolded, setTreeFolded] = useReviewState<ReadonlySet<string>>(`${node.id}:treeFolded`, () => new Set());
  const [hideTests, setHideTests] = useReviewState(`${node.id}:hideTests`, false);
  const [hideWhitespace, setHideWhitespace] = useReviewState(`${node.id}:hideWhitespace`, false);
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
  const revisions = [node.old_revision, node.new_revision].filter(Boolean).join(' → ');
  const allFolded = files.length > 0 && files.every((file) => folded.has(file.path));
  return (
    <div
      ref={container}
      tabIndex={0}
      className="app-diff min-w-0 space-y-3 app-focus-ring"
      data-wrap={wrap}
      data-split={split}
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
        revisions={revisions}
        commands={
          node.children?.length ? (
            <DiffCommands nodes={node.children} isSettled={commands?.isSettled ?? true} {...commands} />
          ) : undefined
        }
        hideTests={hideTests}
        hideWhitespace={hideWhitespace}
        onTests={() => setHideTests(!hideTests)}
        onWhitespace={() => setHideWhitespace(!hideWhitespace)}
        onSplit={setSplit}
        onWrap={() => setWrap(!wrap)}
        onTree={() => setTree(!tree)}
        onFold={() => setFolded(allFolded ? new Set() : new Set(files.map((file) => file.path)))}
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
                } as CSSProperties
              }
              aria-label={file.path}
            >
              <FileHeader file={file} isOpen={!folded.has(file.path)} onToggle={() => toggle(file.path)} />
              {!folded.has(file.path) && file.hunks.length > 0 && (
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
                      onActivate={(line) => {
                        setSelected(line.anchor);
                        setOpened(null);
                      }}
                      selected={selected}
                      searchAnchor={searchAnchor}
                      searchMatch={currentResult}
                      onComment={(line) => setOpened(line.anchor)}
                      onClose={() => setOpened(null)}
                    />
                  ))}
                  {fileChange(file) === 'modified' && (
                    <ContextBand
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
      {actions && (
        <SendBar
          nodeId={node.id}
          drafts={savedDrafts(node.threads)}
          actions={actions}
          label={`Send ${node.label ?? 'review'}`}
        />
      )}
    </div>
  );
}


/** What the toolbar of a diff needs from its view: the click and the key of each button. */
export interface DiffCommandActions {
  onActivate?: (id: string) => Promise<void> | void;
  keys?: ReadonlyMap<string, LiveShortcut>;
  isSettled: boolean;
}

/**
 * The commands an app puts in the toolbar of its diff (`view.json` `diff` `children`).
 * A button is one action. A group of buttons is one menu, and its label names the
 * current choice, like the `Changes ▾` picker of a code review.
 */
function DiffCommands({
  nodes,
  onActivate,
  keys,
  isSettled,
}: { nodes: readonly LiveNode[] } & DiffCommandActions) {
  const [pending, setPending] = useState<string | null>(null);
  const [menu, setMenu] = useState<{ id: string; at: MenuPosition } | null>(null);
  // Escape belongs to the open menu, not to the screen under it.
  useEffect(() => {
    if (!menu) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.stopPropagation();
      setMenu(null);
    };
    window.addEventListener('keydown', onKey, true);
    return () => window.removeEventListener('keydown', onKey, true);
  }, [menu]);
  const press = async (id: string) => {
    setMenu(null);
    if (!onActivate || pending) return;
    setPending(id);
    try {
      await onActivate(id);
    } finally {
      setPending(null);
    }
  };
  const inert = isSettled || !onActivate || pending !== null;
  const open = menu ? nodes.find((node) => node.id === menu.id) : undefined;
  return (
    <span className="diff-commands" role="group" aria-label="Review commands">
      {nodes.map((node) => {
        if (node.type === 'group') {
          const label = node.label ?? 'More';
          return (
            <button
              key={node.id}
              type="button"
              className="diff-command app-focus-ring"
              data-node-id={node.id}
              aria-haspopup="dialog"
              aria-expanded={menu?.id === node.id}
              title={label}
              disabled={inert}
              onClick={(event) => {
                const at = diffMenuAt(event.currentTarget);
                if (at) setMenu({ id: node.id, at });
              }}
            >
              <span className="min-w-0 truncate">{label}</span>
              <ChevronIcon open className="size-3 shrink-0" />
            </button>
          );
        }
        if (node.type !== 'button') return null;
        const shortcut = keys?.get(node.id);
        return (
          <button
            key={node.id}
            type="button"
            className="diff-command app-focus-ring"
            data-node-id={node.id}
            data-variant={node.variant}
            title={shortcut ? `${node.label} (${shortcut.key})` : node.label}
            aria-keyshortcuts={shortcut ? ariaShortcut(shortcut.key) : undefined}
            aria-busy={pending === node.id}
            disabled={inert || node.is_disabled}
            onClick={() => void press(node.id)}
          >
            <span className="min-w-0 truncate">{node.label}</span>
          </button>
        );
      })}
      {open?.type === 'group' && menu && (
        <DiffMenu
          label={open.label ?? 'More'}
          at={menu.at}
          items={open.children}
          keys={keys}
          onPress={(id) => void press(id)}
          onDismiss={() => setMenu(null)}
        />
      )}

    </span>
  );
}

/** The width of a toolbar menu, in px. It matches `.diff-menu`. */
const DIFF_MENU_WIDTH = 224;

/** Where a toolbar menu hangs: its left edge under its trigger, kept on the screen. */
function diffMenuAt(trigger: HTMLElement): MenuPosition | null {
  const box = trigger.getBoundingClientRect();
  return menuPosition({ top: box.top, bottom: box.bottom, right: box.left + DIFF_MENU_WIDTH }, DIFF_MENU_WIDTH);
}

/**
 * The compact popover of a toolbar menu. It is drawn like the rest of the review
 * surface, not like the app menus around it.
 */
function DiffMenu({
  label,
  at,
  items,
  keys,
  onPress,
  onDismiss,
}: {
  label: string;
  at: MenuPosition;
  items: readonly LiveNode[];
  keys?: ReadonlyMap<string, LiveShortcut>;
  onPress: (id: string) => void;
  onDismiss: () => void;
}) {
  const panel = useRef<HTMLDivElement>(null);
  // A keyboard opened this menu as often as a pointer did: focus its first choice.
  useEffect(() => {
    panel.current?.querySelector<HTMLButtonElement>('button:not(:disabled)')?.focus();
  }, []);
  return createPortal(
    <div className="fixed inset-0 z-50" role="presentation" onClick={onDismiss}>
      <div
        ref={panel}
        role="dialog"
        aria-label={label}
        className="diff-menu"
        style={{ left: at.left, top: at.top, bottom: at.bottom, maxHeight: at.maxHeight }}
        onClick={(event) => event.stopPropagation()}
      >
        {items.map((item) => {
          if (item.type !== 'button') return null;
          const shortcut = keys?.get(item.id);
          return (
            <button
              key={item.id}
              type="button"
              className="diff-menu-item"
              data-node-id={item.id}
              data-variant={item.variant}
              aria-keyshortcuts={shortcut ? ariaShortcut(shortcut.key) : undefined}
              disabled={item.is_disabled}
              onClick={() => onPress(item.id)}
            >
              <span className="min-w-0 flex-1 truncate">{item.label}</span>
              {shortcut && <kbd>{shortcut.key}</kbd>}
            </button>
          );
        })}
      </div>
    </div>,
    document.body,
  );
}

function DiffToolbar({
  commands,
  onFind,
  split,
  wrap,
  tree,
  allFolded,
  count,
  total,
  revisions,
  onSplit,
  onWrap,
  onTree,
  onFold,
  hideTests,
  hideWhitespace,
  onTests,
  onWhitespace,
}: {
  /** The app's own commands, first in the row. */
  commands?: ReactNode;
  onFind: () => void;
  split: boolean;
  wrap: boolean;
  tree: boolean;
  allFolded: boolean;
  count: number;
  total: { additions: number; deletions: number };
  revisions: string;
  onSplit: (split: boolean) => void;
  onWrap: () => void;
  onTree: () => void;
  onFold: () => void;
  hideTests: boolean;
  hideWhitespace: boolean;
  onTests: () => void;
  onWhitespace: () => void;
}) {
  return (
    <div className="diff-toolbar" role="toolbar" aria-label="Diff display">
      <span className="diff-toolbar-summary" title={revisions || undefined}>
        {commands}
        {!commands && (
          <span className="shrink-0">
            {count} {count === 1 ? 'file' : 'files'}
          </span>
        )}
        <DiffStat {...total} />
        {revisions && <span className="diff-revisions min-w-0 truncate">{revisions}</span>}
      </span>
      <div className="diff-toolbar-controls">
        <button
          type="button"
          className="app-focus-ring"
          data-diff-find
          title="Find in diff (⌘/Ctrl+F)"
          aria-label="Find in diff"
          aria-keyshortcuts="Meta+f Control+f"
          onClick={onFind}
        >
          Find
        </button>
        <button
          type="button"
          className="app-focus-ring"
          aria-label={allFolded ? 'Expand all' : 'Collapse all'}
          title={allFolded ? 'Expand all' : 'Collapse all'}
          onClick={onFold}
        >
          {allFolded ? <ExpandAllIcon className="size-4" /> : <CollapseAllIcon className="size-4" />}
        </button>
        <div className="diff-layout" role="group" aria-label="Diff layout">
          <button
            type="button"
            className="app-focus-ring"
            aria-label="Unified"
            title="Unified"
            aria-pressed={!split}
            onClick={() => onSplit(false)}
          >
            <UnifiedDiffIcon className="size-4" />
          </button>
          <button
            type="button"
            className="app-focus-ring"
            aria-label="Split"
            title="Split"
            aria-pressed={split}
            onClick={() => onSplit(true)}
          >
            <SideBySideIcon className="size-4" />
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
          <WrapIcon className="size-4" />
        </button>
        <button
          type="button"
          className="app-focus-ring"
          aria-label="Show whitespace changes"
          title={hideWhitespace ? 'Show whitespace changes' : 'Hide whitespace changes'}
          aria-pressed={!hideWhitespace}
          onClick={onWhitespace}
        >
          <WhitespaceIcon className="size-4" />
        </button>
        <button
          type="button"
          className="app-focus-ring"
          aria-label="Hide test files"
          title={hideTests ? 'Show test files' : 'Hide test files'}
          aria-pressed={hideTests}
          onClick={onTests}
        >
          <TestFilesIcon className="size-4" />
        </button>
        <button
          type="button"
          className="app-focus-ring"
          aria-label="Files"
          title="File tree"
          aria-pressed={tree}
          onClick={onTree}
        >
          <FileTreeIcon className="size-4" />
        </button>
      </div>
    </div>
  );
}
