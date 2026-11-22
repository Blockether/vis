/**
 * The discussions of a live `canvas`: the threads on one item, or on the whole board.
 * A thread is a column of messages on one line. A person writes a comment or a reply
 * as a draft, and the next message of the session takes each saved draft. Without an
 * app the threads are read only.
 */

import { useState, type ReactNode } from 'react';

import type { AppActionStatus } from '../lib/app-actions';
import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import type { LiveCanvasCard, LiveCanvasNode, LiveMessage, LiveThread } from '../lib/live-view';
import { ActionStatus, DraftEditor, type AppNodeActions } from './AppNodes';
import { AppIcon, CheckIcon, PencilIcon, PlusIcon, TrashIcon } from './icons';
import { Button, CloseButton, IconButton } from './ui';

const draftLimit = viewSchema.$defs.component.oneOf
  .find((branch) => branch.properties.type?.const === 'canvas')
  ?.properties.threads?.items?.allOf?.find((part) => part.properties?.draft)
  ?.properties?.draft?.maxLength;
if (draftLimit === undefined) throw new Error('view.json has no canvas draft limit');

/** The first letters of the first two words of a name. */
function initials(name: string): string {
  return name
    .split(/\s+/)
    .filter(Boolean)
    .slice(0, 2)
    .map((word) => [...word][0]?.toUpperCase() ?? '')
    .join('');
}

const KIND_WORD: Record<LiveMessage['author']['kind'], string> = {
  human: 'Human',
  agent: 'Agent',
  extension: 'Extension',
};

/** The mark of each kind of author. The kind also stands in words next to the name. */
const KIND_FACE: Record<LiveMessage['author']['kind'], string> = {
  human: 'bg-accent text-accent-foreground',
  agent: 'bg-dialog-title text-dialog-title-foreground',
  extension: 'border border-edge-strong text-dialog-foreground',
};

const STAMP = new Intl.DateTimeFormat(undefined, {
  month: 'short',
  day: 'numeric',
  hour: '2-digit',
  minute: '2-digit',
});

/** One row of a thread: the mark of the author on the line of the thread, then the words. */
function Row({
  face,
  letters,
  isLast,
  children,
}: {
  face: string;
  letters: string;
  isLast: boolean;
  children: ReactNode;
}) {
  return (
    <li className="relative flex min-w-0 gap-2 pb-3 last:pb-0">
      {!isLast && <span aria-hidden="true" className="absolute bottom-0 left-[9px] top-5 w-px bg-edge-strong" />}
      <span
        aria-hidden="true"
        className={`relative flex size-5 shrink-0 items-center justify-center font-mono text-meta font-bold ${face}`}
      >
        {letters}
      </span>
      <div className="min-w-0 flex-1">{children}</div>
    </li>
  );
}

function Message({ message, isLast }: { message: LiveMessage; isLast: boolean }) {
  const kind = KIND_WORD[message.author.kind];
  const name = message.author.name ?? kind;
  return (
    <Row face={KIND_FACE[message.author.kind]} letters={initials(name)} isLast={isLast}>
      <div className="flex min-w-0 items-baseline gap-1.5" data-message-id={message.id}>
        <span className="min-w-0 truncate font-mono text-ui font-bold text-dialog-foreground">{name}</span>
        <span className="shrink-0 font-mono text-meta text-dialog-hint">
          {message.author.name ? `${kind.toLowerCase()} · ` : ''}
          <time dateTime={new Date(message.created_at).toISOString()}>
            {STAMP.format(message.created_at)}
          </time>
        </span>
      </div>
      <p className="whitespace-pre-wrap font-mono text-ui text-white [overflow-wrap:anywhere]">
        {message.body}
      </p>
    </Row>
  );
}

/** One thread: its messages, the saved draft of the person, and the reply control. */
function Thread({
  thread,
  nodeId,
  actions,
}: {
  thread: LiveThread;
  nodeId: string;
  actions?: AppNodeActions;
}) {
  const [isEditing, setEditing] = useState(false);
  const [status, setStatus] = useState<AppActionStatus>('idle');
  const canEdit = Boolean(actions) && !actions?.isReadOnly;
  const draft = thread.draft;
  const hasDraft = draft !== undefined;
  const count = thread.messages.length;
  // A blank reply adds the messages without a note.
  const save = (body: string | null) =>
    actions!.draft(nodeId, { threadId: thread.id, body: body === '' && count === 0 ? null : body });
  const remove = async () => {
    setStatus('saving');
    const result = await save(null);
    setStatus(result.status === 'saved' ? 'idle' : result.status);
  };
  return (
    <section
      aria-label={`Thread, ${count} ${count === 1 ? 'message' : 'messages'}${thread.is_resolved ? ', resolved' : ''}`}
      data-thread-id={thread.id}
      className="min-w-0 border border-edge bg-panel-2"
    >
      {thread.is_resolved && (
        <p className="flex items-center gap-1.5 border-b border-edge px-2.5 py-1 font-mono text-meta font-bold text-ok">
          <CheckIcon className="size-3" />
          Resolved
        </p>
      )}
      <ol className={`px-2.5 py-2.5 ${thread.is_resolved ? 'opacity-70' : ''}`}>
        {thread.messages.map((message, at) => (
          <Message key={message.id} message={message} isLast={at === count - 1 && !hasDraft} />
        ))}
        {hasDraft && !isEditing && (
          <Row face="border border-dashed border-accent text-accent" letters="Y" isLast>
            <div className="flex min-w-0 items-baseline gap-1.5">
              <span className="font-mono text-ui font-bold text-dialog-foreground">You</span>
              <span className="min-w-0 truncate font-mono text-meta text-dialog-hint">
                {draft === '' ? 'In the next message' : 'draft · in the next message'}
              </span>
            </div>
            {draft !== '' && (
              <p className="whitespace-pre-wrap font-mono text-ui text-dialog-foreground [overflow-wrap:anywhere]">
                {draft}
              </p>
            )}
          </Row>
        )}
      </ol>
      {canEdit && (
        <div className={`border-t border-edge ${isEditing ? 'px-2.5 py-2' : 'px-1 py-0.5'}`}>
          {isEditing ? (
            <DraftEditor
              memoryId={`${nodeId}:${thread.id}:${draft ?? ''}`}
              label={count > 0 ? 'Reply' : 'Edit comment'}
              saved={draft ?? ''}
              placeholder={count > 0 ? 'Add a note (optional)…' : undefined}
              isBlankAllowed={count > 0}
              maxLength={draftLimit}
              showFailureReason
              onSave={save}
              onClose={() => setEditing(false)}
            />
          ) : (
            <div className="flex items-center gap-1">
              <IconButton
                label={hasDraft ? 'Edit' : 'Reply'}
                title={hasDraft ? 'Edit' : 'Reply'}
                onClick={() => setEditing(true)}
              >
                <PencilIcon />
              </IconButton>
              {hasDraft && (
                <IconButton
                  variant="remove"
                  label="Remove"
                  title="Remove"
                  disabled={status === 'saving'}
                  onClick={() => void remove()}
                >
                  <TrashIcon />
                </IconButton>
              )}
              <ActionStatus status={status} />
            </div>
          )}
        </div>
      )}
    </section>
  );
}

/** The threads of one item, and the control that starts a new one. */
function ItemThreads({
  node,
  item,
  threads,
  actions,
  isComposing,
  onCompose,
}: {
  node: LiveCanvasNode;
  item: LiveCanvasCard;
  threads: LiveThread[];
  actions?: AppNodeActions;
  isComposing: boolean;
  onCompose: (isOpen: boolean) => void;
}) {
  const canEdit = Boolean(actions) && !actions?.isReadOnly;
  return (
    <div className="min-w-0 space-y-2">
      {threads.map((thread) => (
        <Thread key={thread.id} thread={thread} nodeId={node.id} actions={actions} />
      ))}
      {threads.length === 0 && !isComposing && (
        <p className="font-mono text-ui text-dialog-hint">No comments on this item.</p>
      )}
      {canEdit &&
        (isComposing ? (
          <DraftEditor
            memoryId={`${node.id}:${item.id}:new`}
            label={`Comment on ${item.title}`}
            saved=""
            submitLabel="Comment"
            maxLength={draftLimit}
            showFailureReason
            onSave={(body) => actions!.draft(node.id, { itemId: item.id, body })}
            onClose={() => onCompose(false)}
          />
        ) : (
          <IconButton
            label={threads.length > 0 ? 'New thread' : 'Comment'}
            title={threads.length > 0 ? 'New thread' : 'Comment'}
            onClick={() => onCompose(true)}
          >
            <PlusIcon />
          </IconButton>
        ))}
    </div>
  );
}

/**
 * The panel of the discussions. `itemId` names one item; without it the panel lists
 * the threads of the whole board under the title of each item.
 */
export function CanvasThreads({
  node,
  itemId,
  isSheet,
  bottom,
  isComposing,
  actions,
  onCompose,
  onOpen,
  onClose,
}: {
  node: LiveCanvasNode;
  itemId: string | null;
  /** A narrow board shows the panel as a sheet over its lower part. */
  isSheet: boolean;
  bottom: number;
  /** The editor of a new thread is open. */
  isComposing: boolean;
  actions?: AppNodeActions;
  onCompose: (isOpen: boolean) => void;
  /** Shows one item and its threads, or the whole board with null. */
  onOpen: (itemId: string | null) => void;
  onClose: () => void;
}) {
  const item = itemId === null ? undefined : node.cards.find((card) => card.id === itemId);
  const byItem = new Map<string, LiveThread[]>();
  for (const thread of node.threads) {
    const held = byItem.get(thread.item_id ?? '');
    if (held) held.push(thread);
    else byItem.set(thread.item_id ?? '', [thread]);
  }
  // The board order of the items is the order of the list.
  const talked = node.cards.filter((card) => byItem.has(card.id));
  const title = item ? item.title : 'Comments';
  return (
    <section
      data-canvas-control=""
      data-canvas-threads={itemId ?? ''}
      aria-label={item ? `Comments on ${item.title}` : 'Comments on the board'}
      className={`absolute z-20 flex cursor-default select-text flex-col border border-dialog-edge bg-panel text-left shadow-float ${
        isSheet ? 'inset-x-2 max-h-[62%]' : 'right-2 top-2 w-80 max-w-[calc(100%-1rem)]'
      }`}
      style={{ bottom, maxHeight: isSheet ? `min(62%, calc(100% - ${bottom + 8}px))` : undefined }}
      onKeyDown={(event) => {
        if (event.key === 'Escape') onClose();
      }}
    >
      <header className="flex min-w-0 shrink-0 items-center gap-2 border-b border-dialog-edge py-1 pl-3 pr-1">
        <AppIcon name="chat" className="size-3.5 shrink-0 text-dialog-hint" />
        <h3 className="min-w-0 flex-1 truncate font-mono text-ui font-bold text-dialog-foreground">{title}</h3>
        {item && node.threads.length > (byItem.get(item.id)?.length ?? 0) && (
          <Button variant="quiet" density="compact" onClick={() => onOpen(null)}>
            All
          </Button>
        )}
        <CloseButton label="Close comments" isStandalone onClick={onClose} />
      </header>
      <div className="min-h-0 flex-1 space-y-3 overflow-y-auto overscroll-contain px-3 py-2.5">
        {item ? (
          <ItemThreads
            node={node}
            item={item}
            threads={byItem.get(item.id) ?? []}
            actions={actions}
            isComposing={isComposing}
            onCompose={onCompose}
          />
        ) : talked.length === 0 ? (
          <p className="font-mono text-ui text-dialog-hint">No comments on this board.</p>
        ) : (
          talked.map((card) => (
            <div key={card.id} className="min-w-0 space-y-1.5">
              <Button variant="quiet" density="compact" className="max-w-full" onClick={() => onOpen(card.id)}>
                <span className="truncate">{card.title}</span>
              </Button>
              {(byItem.get(card.id) ?? []).map((thread) => (
                <Thread key={thread.id} thread={thread} nodeId={node.id} actions={actions} />
              ))}
            </div>
          ))
        )}
      </div>
    </section>
  );
}
