/**
 * The review nodes of an app attachment: a `diff` with threads on its lines, and
 * a `thread` of its own. Both read the same host state and save through the same
 * `draft` and `submit` actions. Without an app the threads are read only.
 */

import { useMemo, useRef, useState, type CSSProperties, type ReactNode } from 'react';

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
  type LiveSubmission,
  type LiveThread,
  type LiveThreadNode,
} from '../lib/live-view';
import type { ViewAnchor } from '../lib/view';
import {
  ChevronIcon,
  CollapseAllIcon,
  ExpandAllIcon,
  FileTreeIcon,
  SideBySideIcon,
  UnifiedDiffIcon,
  WrapIcon,
} from './icons';
import { Button, CopyChip } from './ui';

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
  onSave,
  onClose,
}: {
  label: string;
  saved: string;
  onSave: (body: string) => Promise<AppActionResult>;
  onClose: () => void;
}) {
  const [text, setText] = useState(saved);
  const [status, setStatus] = useState<AppActionStatus>('idle');
  const isBusy = status === 'saving';
  const save = async () => {
    if (text.trim() === '' && saved === '') {
      onClose();
      return;
    }
    setStatus('saving');
    const result = await onSave(text.trim() === '' ? '' : text);
    setStatus(result.status);
    if (result.status === 'saved' && text.trim() === '') onClose();
  };
  return (
    <form
      className="space-y-2"
      onSubmit={(event) => {
        event.preventDefault();
        void save();
      }}
      onKeyDown={(event) => {
        if (event.key === 'Escape') {
          event.preventDefault();
          event.stopPropagation();
          onClose();
        }
      }}
    >
      <textarea
        autoFocus
        rows={3}
        className={TEXTAREA}
        value={text}
        maxLength={LIVE_MESSAGE_CHARS}
        aria-label={label}
        disabled={isBusy}
        onChange={(event) => {
          setText(event.target.value);
          if (status !== 'saving') setStatus('idle');
        }}
        onKeyDown={(event) => {
          if (event.key === 'Enter' && (event.metaKey || event.ctrlKey)) {
            event.preventDefault();
            void save();
          }
        }}
      />
      <div className="flex flex-wrap items-center gap-2">
        <Button type="submit" variant="primary" disabled={isBusy}>
          {isRetryable(status) ? 'Retry' : 'Save'}
        </Button>
        <Button type="button" variant="secondary" onClick={onClose}>
          Cancel
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
  const [isEditing, setEditing] = useState(false);
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

/** The number stays visible while the file scrolls sideways. */
function Gutter({
  line,
  digits,
  canComment,
  path,
  onActivate,
  side = 'new',
}: {
  line: AppDiffLine;
  digits: number;
  canComment: boolean;
  path: string;
  onActivate: (line: AppDiffLine) => void;
  side?: 'old' | 'new';
}) {
  return (
    <td data-kind={line.kind} className="diff-gutter" style={{ width: Math.max(41, digits * 8 + 25) }}>
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
    </td>
  );
}

/** Changed words retain the same line box as the surrounding code. */
function LineCode({ line, words }: { line: AppDiffLine; words?: readonly DiffSpan[] }) {
  return (
    <td data-kind={line.kind} className="diff-code">
      {line.kind !== 'context' && (
        <span className="sr-only">{line.kind === 'add' ? 'Added: ' : 'Removed: '}</span>
      )}
      {words
        ? words.map((part, at) =>
            part.isChanged ? (
              <span key={at} className="diff-word">
                {part.text}
              </span>
            ) : (
              part.text
            ),
          )
        : line.text}
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
              label={`Comment on ${model.file.path} line ${selected.anchor.line}`}
              saved=""
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
        />
        <LineCode line={line} words={model.words.get(anchorKey(line.anchor))} />
      </>
    ) : (
      <>
        <td className="diff-gutter diff-empty" style={{ width: Math.max(41, model.digits * 8 + 25) }} />
        <td className="diff-empty" />
      </>
    );
  return (
    <div className="min-w-0">
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
        data-line-kind={!split ? next?.kind : undefined}
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

/** The +N and -N of a file or a diff: removals first, as T3 puts them. */
function DiffStat({ additions, deletions }: { additions: number; deletions: number }) {
  return (
    <span className="flex shrink-0 items-center gap-2 font-mono text-ui tabular-nums">
      {deletions > 0 && <span className="text-err-ink">-{deletions}</span>}
      {(additions > 0 || deletions === 0) && <span className="text-ok-ink">+{additions}</span>}
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
  const change = CHANGE_MARK[fileChange(file)];
  const name =
    file.old_path && file.new_path && file.old_path !== file.new_path
      ? `${file.old_path} → ${file.new_path}`
      : file.path;
  return (
    <header
      className={`sticky top-0 z-[1] flex h-11 items-center gap-2 bg-panel pl-2 pr-3 mouse:h-8 ${isOpen ? 'border-b border-dialog-edge' : ''}`}
    >
      <button
        type="button"
        aria-expanded={isOpen}
        aria-label={`${isOpen ? 'Fold' : 'Unfold'} ${name}`}
        onClick={onToggle}
        className="diff-fold app-focus-ring flex min-w-0 flex-1 items-center gap-2 self-stretch text-left hover:text-accent-ink focus-visible:bg-current/10 focus-visible:outline-none"
      >
        <ChevronIcon open={isOpen} className="size-4 shrink-0 text-dialog-hint" />
        <span
          title={change.word}
          className={`w-4 shrink-0 text-center font-mono text-ui font-bold ${change.ink}`}
        >
          <span aria-hidden>{change.letter}</span>
          <span className="sr-only">{change.word}</span>
        </span>
        <span className="min-w-0 truncate font-sans text-[12px] leading-4 text-white">{name}</span>
      </button>
      <CopyChip value={file.path} label={`Copy path ${file.path}`} density="compact" />
      <DiffStat {...fileStat(file)} />
    </header>
  );
}

/** The quiet bar between two hunks: how many unchanged lines the diff leaves out. */
function HunkSeparator({ hunks, at }: { hunks: readonly AppDiffHunk[]; at: number }) {
  const hidden = hiddenLinesBefore(hunks, at);
  if (hidden === 0) return null;
  const header = hunks[at].header;
  const context = header.replace(/^@@[^@]*@@\s*/, '');
  return (
    <p
      title={header}
      className="diff-hunk mx-2 my-1 flex h-6 min-w-0 items-center gap-2 rounded-[6px] bg-[var(--color-diff-hunk)] px-2 font-sans text-ui text-dialog-hint"
    >
      <span className="shrink-0">
        {hidden === null ? header : `${hidden} unmodified ${hidden === 1 ? 'line' : 'lines'}`}
      </span>
      {hidden !== null && context && <span className="min-w-0 truncate text-footer-muted">{context}</span>}
    </p>
  );
}

/** Unified and split diffs share anchored comments, file controls and one scroller per file. */
export function DiffNodeView({ node, actions }: { node: LiveDiffNode; actions?: AppNodeActions }) {
  const { files, words, digits, total, elsewhere } = useMemo(
    () => diffModel(node.patch, node.threads),
    [node.patch, node.threads],
  );
  const [opened, setOpened] = useState<ViewAnchor | null>(null);
  const [folded, setFolded] = useState<ReadonlySet<string>>(() => new Set());
  const [split, setSplit] = useState(false);
  const [wrap, setWrap] = useState(true);
  const [tree, setTree] = useState(false);
  const refs = useRef(new Map<string, HTMLElement>());
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
    <div className="app-diff min-w-0 space-y-3" data-wrap={wrap} data-split={split}>
      <DiffToolbar
        split={split}
        wrap={wrap}
        tree={tree}
        allFolded={allFolded}
        count={files.length}
        total={total}
        revisions={revisions}
        onSplit={setSplit}
        onWrap={() => setWrap(!wrap)}
        onTree={() => setTree(!tree)}
        onFold={() => setFolded(allFolded ? new Set() : new Set(files.map((file) => file.path)))}
      />
      <div className={`min-w-0 ${tree ? 'diff-with-tree' : ''}`}>
        {tree && (
          <nav className="diff-file-tree" aria-label="Changed files">
            {fileTree(files).map((row) =>
              row.kind === 'folder' ? (
                <p
                  key={row.key}
                  className="px-2 py-1 text-ui text-dialog-hint"
                  style={{ paddingLeft: 8 + row.depth * 12 }}
                >
                  {row.name}
                </p>
              ) : (
                <button
                  key={row.key}
                  type="button"
                  aria-label={`Open ${row.file.path}`}
                  className="flex min-h-11 w-full min-w-0 items-center gap-2 px-2 text-left text-[12px] hover:bg-hover app-focus-ring"
                  style={{ paddingLeft: 8 + row.depth * 12 }}
                  onClick={() => {
                    setFolded((held) => {
                      const next = new Set(held);
                      next.delete(row.file.path);
                      return next;
                    });
                    refs.current.get(row.file.path)?.scrollIntoView({ block: 'nearest' });
                  }}
                >
                  <span className="min-w-0 flex-1 truncate" title={row.file.path}>
                    {row.name}
                  </span>
                  <DiffStat {...fileStat(row.file)} />
                </button>
              ),
            )}
          </nav>
        )}
        <div className="diff-files min-w-0 space-y-3">
          {!files.length && <p className="font-mono text-body text-dialog-hint">No changes.</p>}
          {files.map((file) => (
            <section
              key={file.path}
              ref={(el) => {
                if (el) refs.current.set(file.path, el);
                else refs.current.delete(file.path);
              }}
              className="diff-file min-w-0 border border-dialog-edge"
              style={
                {
                  '--diff-number-width': `${Math.max(41, (digits.get(file.path) ?? 1) * 8 + 25)}px`,
                } as CSSProperties
              }
              aria-label={file.path}
            >
              <FileHeader file={file} isOpen={!folded.has(file.path)} onToggle={() => toggle(file.path)} />
              {!folded.has(file.path) && (
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
                      onActivate={(line) => setOpened(line.anchor)}
                      onClose={() => setOpened(null)}
                    />
                  ))}
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

function DiffToolbar({
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
}: {
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
}) {
  return (
    <div className="diff-toolbar" role="toolbar" aria-label="Diff display">
      <span className="diff-toolbar-summary" title={revisions || undefined}>
        <span className="shrink-0">
          {count} {count === 1 ? 'file' : 'files'}
        </span>
        <DiffStat {...total} />
        {revisions && <span className="diff-revisions min-w-0 truncate">{revisions}</span>}
      </span>
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
        aria-label="Files"
        title="File tree"
        aria-pressed={tree}
        onClick={onTree}
      >
        <FileTreeIcon className="size-4" />
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
    </div>
  );
}
