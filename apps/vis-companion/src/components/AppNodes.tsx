/**
 * The review nodes of an app attachment: a `diff` with threads on its lines, and
 * a `thread` of its own. Both read the same host state and save through the same
 * `draft` and `submit` actions. Without an app the threads are read only.
 */

import { useMemo, useRef, useState, type ReactNode } from 'react';

import {
  APP_ACTION_WORDS,
  SendKeys,
  isRetryable,
  type AppActionResult,
  type AppActionStatus,
} from '../lib/app-actions';
import { anchorKey, parseUnifiedDiff, type AppDiffLine } from '../lib/app-diff';
import {
  LIVE_MESSAGE_CHARS,
  type LiveDiffNode,
  type LiveMessage,
  type LiveSubmission,
  type LiveThread,
  type LiveThreadNode,
} from '../lib/live-view';
import type { ViewAnchor } from '../lib/view';
import { Button } from './ui';

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
export function ThreadNodeView({
  node,
  actions,
}: {
  node: LiveThreadNode;
  actions?: AppNodeActions;
}) {
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

const LINE_TINT: Record<AppDiffLine['kind'], string> = {
  add: 'bg-ok/10 text-ok',
  remove: 'bg-err/10 text-error',
  context: 'text-dialog-foreground',
};

const LINE_MARK: Record<AppDiffLine['kind'], string> = { add: '+', remove: '-', context: ' ' };

/** One line-number cell. On the anchor side of a line it opens a new thread. */
function Gutter({
  number,
  isAnchor,
  label,
  onActivate,
}: {
  number?: number;
  isAnchor: boolean;
  label: string;
  onActivate: () => void;
}) {
  return (
    <td className="w-0 select-none whitespace-nowrap px-1 text-right text-dialog-hint">
      {isAnchor ? (
        <button
          type="button"
          className="min-w-6 px-0.5 text-right hover:text-accent-ink focus-visible:bg-hover focus-visible:text-accent-ink focus-visible:outline-none"
          aria-label={label}
          onClick={onActivate}
        >
          {number ?? ''}
        </button>
      ) : (
        (number ?? '')
      )}
    </td>
  );
}

/** Lines that scroll sideways together, between two threads. */
function LineRun({
  lines,
  path,
  canComment,
  onActivate,
}: {
  lines: AppDiffLine[];
  path: string;
  canComment: boolean;
  onActivate: (line: AppDiffLine) => void;
}) {
  return (
    <div className="min-w-0 overflow-x-auto" data-diff-lines>
      <table className="min-w-full border-collapse font-mono text-body">
        <tbody>
          {lines.map((line) => {
            const number = line.anchor.line;
            return (
              <tr key={anchorKey(line.anchor)} className={LINE_TINT[line.kind]} data-anchor={anchorKey(line.anchor)}>
                <Gutter
                  number={line.old_line}
                  isAnchor={canComment && line.anchor.side === 'old'}
                  label={`Comment on ${path} old line ${number}`}
                  onActivate={() => onActivate(line)}
                />
                <Gutter
                  number={line.new_line}
                  isAnchor={canComment && line.anchor.side === 'new'}
                  label={`Comment on ${path} line ${number}`}
                  onActivate={() => onActivate(line)}
                />
                <td className="w-0 select-none px-1" aria-hidden="true">
                  {LINE_MARK[line.kind]}
                </td>
                <td className="whitespace-pre pr-2">{line.text}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

/**
 * A unified diff with threads under the lines they name. A line number is a
 * button: Enter or a press opens a new thread on that line. The code scrolls
 * sideways; a thread keeps the full width under its line.
 */
export function DiffNodeView({ node, actions }: { node: LiveDiffNode; actions?: AppNodeActions }) {
  const files = useMemo(() => parseUnifiedDiff(node.patch), [node.patch]);
  const [opened, setOpened] = useState<ViewAnchor | null>(null);
  const canComment = Boolean(actions) && !actions?.isReadOnly;
  const byAnchor = useMemo(() => {
    const map = new Map<string, LiveThread[]>();
    for (const thread of node.threads) {
      if (!thread.anchor) continue;
      const key = anchorKey(thread.anchor);
      map.set(key, [...(map.get(key) ?? []), thread]);
    }
    return map;
  }, [node.threads]);
  const shown = new Set<string>();
  for (const file of files)
    for (const hunk of file.hunks) for (const line of hunk.lines) shown.add(anchorKey(line.anchor));
  const elsewhere = node.threads.filter(
    (thread) => !thread.anchor || !shown.has(anchorKey(thread.anchor)),
  );
  const openedKey = opened ? anchorKey(opened) : null;
  const revisions = [node.old_revision, node.new_revision].filter(Boolean).join(' → ');
  return (
    <div className="min-w-0 space-y-3">
      {revisions && <p className="font-mono text-ui text-dialog-hint">{revisions}</p>}
      {files.length === 0 && <p className="font-mono text-body text-dialog-hint">No changes.</p>}
      {files.map((file) => (
        <section key={file.path} className="min-w-0 border border-dialog-edge" aria-label={file.path}>
          <h4 className="border-b border-dialog-edge bg-panel-2 px-2 py-1 font-mono text-ui font-bold text-white [overflow-wrap:anywhere]">
            {file.old_path && file.new_path && file.old_path !== file.new_path
              ? `${file.old_path} → ${file.new_path}`
              : file.path}
          </h4>
          {file.hunks.map((hunk, hunkAt) => {
            const parts: ReactNode[] = [];
            let run: AppDiffLine[] = [];
            const flush = (key: string) => {
              if (run.length === 0) return;
              parts.push(
                <LineRun
                  key={`lines:${key}`}
                  lines={run}
                  path={file.path}
                  canComment={canComment}
                  onActivate={(line) => setOpened(line.anchor)}
                />,
              );
              run = [];
            };
            for (const line of hunk.lines) {
              run.push(line);
              const key = anchorKey(line.anchor);
              const threads = byAnchor.get(key) ?? [];
              const isOpen = openedKey === key && actions;
              if (threads.length === 0 && !isOpen) continue;
              flush(key);
              parts.push(
                <div key={`threads:${key}`} className="space-y-2 px-2 py-2">
                  {threads.map((thread) => (
                    <ThreadBlock
                      key={thread.id}
                      thread={thread}
                      nodeId={node.id}
                      actions={actions}
                      title={`Thread on ${file.path} line ${line.anchor.line}`}
                    />
                  ))}
                  {isOpen && (
                    <DraftEditor
                      label={`Comment on ${file.path} line ${line.anchor.line}`}
                      saved=""
                      onSave={(body) => actions.draft(node.id, { anchor: line.anchor, body })}
                      onClose={() => setOpened(null)}
                    />
                  )}
                </div>,
              );
            }
            flush(`end:${hunkAt}`);
            return (
              <div key={`${hunk.header}:${hunkAt}`} className="min-w-0">
                <p className="bg-panel-2 px-2 font-mono text-ui text-link-fg [overflow-wrap:anywhere]">
                  {hunk.header}
                </p>
                {parts}
              </div>
            );
          })}
        </section>
      ))}
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
