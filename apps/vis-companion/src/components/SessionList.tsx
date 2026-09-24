import {
  memo,
  useCallback,
  useEffect,
  useRef,
  useState,
  type DragEvent,
  type MouseEvent,
  type ReactNode,
} from 'react';
import type { Element, Root, Text } from 'hast';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';

import { Banner, ConfirmRow, LIST_EDGE } from './ui';
import { SessionHealth } from './SessionHealth';
import {
  EditableNameField,
  HeaderActions,
  HeaderTitle,
  LIST_EDGE_END,
  RowDisclosure,
  SectionHeader,
} from './SessionNavigator';
import { SwipeActions, type SwipeAction } from './SwipeActions';
import {
  ArchiveIcon,
  FolderPlusIcon,
  PencilIcon,
  ProjectsIcon,
  StarIcon,
  TrashIcon,
} from './icons';
import { GatewayClient, GatewayError, type SessionMatch } from '../lib/gateway';
import type { GatewayConn, Session, SessionGroup, SessionUsage } from '../lib/types';
import { draftMessageHasUnsent, type DraftMessage } from '../lib/draft-messages';
import type { PendingAttachment } from '../lib/attachments';
import { unreadTurnCount } from '../lib/unread';
import { isFavorite } from '../lib/favorites';
import { groupSwatch } from '../lib/group-colors';
import {
  sessionInputCount,
  sessionIsArchived,
  sessionIsLive,
  sessionNeedsInput,
  sessionWasInterrupted,
  timeLabel,
} from '../lib/fleet';
import { hasHardwarePointer } from '../lib/pointer';
import { SESSION_DRAG_MIME, useSessionLift } from '../lib/session-drag';

// Same frames as the session transcript's spinner and the TUI's
// `paint-content-loading!` — one vocabulary for "working" across the product.
// Two placeholder projects with ragged title widths: an even grid reads as a
// rendered table, a ragged one reads as text that has not arrived yet.
const SKELETON_GROUPS = [
  ['w-3/5', 'w-2/5', 'w-1/2'],
  ['w-1/2', 'w-2/3'],
];

// How long the row's disclosure takes to open or close. It is duplicated by the
// `duration-200` utilities below on purpose: the class drives the paint, this
// number only decides when the panel may leave the tree.
const STATS_MOTION_MS = 200;

// What a row says when the archive is aimed at work a human is still waiting on: an archived
// session takes no new turns, so putting a running one away would bury the turn it is holding.
const STILL_WORKING = 'This session is still active. Archive it once its turn is done.';

/**
 * Why the archive was refused, in the row's own words where it has them. A session that
 * started a turn between the press and the PATCH is refused by the GATEWAY for the reason
 * the row refuses it itself, so both answers read the same.
 */
function archiveRefusal(cause: unknown, wasAway: boolean): string {
  const busy =
    cause instanceof GatewayError &&
    cause.status === 409 &&
    (cause.body as { error?: { type?: string } } | null)?.error?.type === 'session-busy';
  if (busy) return STILL_WORKING;
  if (cause instanceof Error && cause.message) return cause.message;
  return wasAway ? 'Session could not be unarchived.' : 'Session could not be archived.';
}
export type SessionRowAction = {
  mode: 'delete';
  session: Session;
  conn: GatewayConn;
};

export type SessionRowCommands = {
  /**
   * Put the session away, or take it back — `away` says which — and answer the row the
   * gateway echoed. OPTIONAL: a row painted only because the GROUP holding it is archived
   * has nothing of its own to put away, so the band that paints it passes no verb.
   */
  archive?: (session: Session, conn: GatewayConn, away: boolean) => Promise<Session>;
  /**
   * File the session into a group, or take it out of one. OPTIONAL: only a grouped
   * project list offers it, and the anchor is the element the menu hangs under.
   */
  moveToGroup?: (session: Session, conn: GatewayConn, anchor: HTMLElement) => void;
  open: (conn: GatewayConn, sid: string, fresh?: boolean) => void | Promise<void>;
  rename: (session: Session, conn: GatewayConn, title: string) => Promise<void>;
  requestDelete: (session: Session, conn: GatewayConn) => void;
  toggleStar: (session: Session, conn: GatewayConn) => void;
  /**
   * Take the session straight out of its group, with nothing to choose. OPTIONAL, and
   * offered INSTEAD of `moveToGroup`: the only group its project has is the one it is
   * already in, so there is nowhere left to move it.
   */
  ungroup?: (session: Session, conn: GatewayConn) => void;
};

/** One stable contract shared by every feature that renders session rows. */
export type SessionListActions = {
  commands: SessionRowCommands;
  deletion: {
    target: Extract<SessionRowAction, { mode: 'delete' }> | null;
    isBusy: boolean;
    error: string | null;
    confirm: () => void;
    cancel: () => void;
  };
};

export type SessionRowDeletion = Omit<SessionListActions['deletion'], 'target'> | null;

/**
 * The row's slab keeps one geometry while its title changes from ink to a field.
 *
 * `data-row-surface` names it as the row's pressable half, so `SwipeActions` paints
 * the press across the whole row — chevron and menu cell included — instead of
 * stopping at this button's edge.
 *
 * The press is the row's own state, marked with `data-pressed` on pointer down and
 * cleared on release, rather than the browser's `:active`: every cell of the row is
 * painted from it, and a test can hold one press open and read what each cell wears.
 *
 * The paper arrives and leaves at once, with no transition on it. The row's other
 * cells take it instantly, so a button that eased its own background over 150ms was
 * still lit after they had let go, and the light appeared to wipe across the row.
 */
function SessionRowSurface({
  isEditing,
  isCurrent,
  isSelected,
  sessionId,
  onOpen,
  onSelectionClick,
  children,
}: {
  isEditing: boolean;
  /** This session is the one standing open in the pane beside the list. */
  isCurrent: boolean;
  isSelected: boolean;
  sessionId: string;
  onOpen: () => void;
  onSelectionClick?: (event: MouseEvent<HTMLButtonElement>) => boolean;
  children: ReactNode;
}) {
  const layout =
    'flex min-h-12 min-w-0 flex-1 items-center py-1.5 pl-4 pr-2 text-left mouse:min-h-8 mouse:py-1';
  const [isPressed, setIsPressed] = useState(false);
  useEffect(() => {
    if (!isPressed) return;
    // A drag swallows the pointer, so `dragend` is the release that arrives instead.
    const release = () => setIsPressed(false);
    window.addEventListener('pointerup', release);
    window.addEventListener('pointercancel', release);
    window.addEventListener('dragend', release);
    return () => {
      window.removeEventListener('pointerup', release);
      window.removeEventListener('pointercancel', release);
      window.removeEventListener('dragend', release);
    };
  }, [isPressed]);
  if (isEditing) {
    return (
      <div className={layout} data-session-id={sessionId}>
        {children}
      </div>
    );
  }
  return (
    <button
      type="button"
      aria-current={isCurrent ? 'page' : undefined}
      aria-pressed={onSelectionClick ? isSelected : undefined}
      className={`${layout} data-pressed:bg-hover active:bg-hover focus-visible:bg-hover focus-visible:outline-none`}
      data-session-id={sessionId}
      data-row-surface=""
      data-pressed={isPressed ? '' : undefined}
      onPointerDown={(event) => {
        if (event.button !== 0) return;
        setIsPressed(true);
      }}
      onClick={(event) => {
        if (!onSelectionClick?.(event)) onOpen();
      }}
    >
      {children}
    </button>
  );
}

/**
 * THE PICTURE A DRAG CARRIES. Left alone the browser paints one, and WebKit - the engine
 * behind the desktop window - paints it from the LAYER the row stands in rather than from
 * the row, so picking up one session put a ghost of every row below it under the cursor.
 * (Reported with a screenshot of the desktop window: one row taken, a column following
 * it.) A picture the row hands over is the picture the browser uses, so the row hands it
 * an opaque copy of ITSELF: one card, the row's own size, held where the pointer took it.
 */
function carryOneRow(row: HTMLElement, event: DragEvent<HTMLElement>, count = 1): void {
  // Older web views - and the suite's own carriers - have no picture to set.
  if (typeof event.dataTransfer.setDragImage !== 'function') return;
  const box = row.getBoundingClientRect();
  const picture = row.cloneNode(true) as HTMLElement;
  // The copy is a picture, not a row: nothing looking for this session may find it twice.
  picture.removeAttribute('data-session-row');
  picture.querySelectorAll('[data-session-id]').forEach((mark) => {
    mark.removeAttribute('data-session-id');
  });
  picture.setAttribute('aria-hidden', 'true');
  // Paper of its own, because a see-through card shows the very rows it is carried over,
  // and its EDGE is drawn inside the box (a border would push the row's own content in and
  // clip it). It stands exactly ON the row it copies, and a positioned element is a paint
  // layer of its own: what the engine photographs is this card, not the list it was cut
  // from. `@container` is what makes the copy read as the row it came from: this row
  // answers its width to the LIST it stands in, and out here the list is gone, so without
  // a container of its own the copy loses the second line and its group rail stops short
  // of the card's own end (reported from the desktop app).
  picture.className = `${picture.className} @container pointer-events-none overflow-hidden bg-panel ring-1 ring-edge ring-inset`;
  if (count > 1) {
    const badge = document.createElement('span');
    badge.className = 'absolute right-2 top-1 border border-accent bg-panel px-2 font-mono text-meta font-bold text-accent-ink';
    badge.textContent = `${count} sessions`;
    picture.append(badge);
  }
  picture.style.position = 'fixed';
  picture.style.top = `${box.top}px`;
  picture.style.left = `${box.left}px`;
  picture.style.width = `${box.width}px`;
  picture.style.height = `${box.height}px`;
  picture.style.zIndex = '9999';
  document.body.append(picture);
  event.dataTransfer.setDragImage(picture, event.clientX - box.left, event.clientY - box.top);
  // The picture is taken as this event returns, so the copy retires before the next frame
  // is painted and no reader ever sees two rows standing in one place.
  window.setTimeout(() => picture.remove(), 0);
}

export const SessionRow = memo(function SessionRow({
  session,
  group,
  draft,
  conn,
  match,
  needle,
  commands,
  deletion,
  isOpen = false,
  isSelected = false,
  onSelectionClick,
  dragIds,
  isDraggable,
}: {
  session: Session;
  /**
   * The GROUP this session is filed under — null while it is ungrouped, or while the
   * list has not read the groups yet. The row's mark and its name come from HERE: the
   * group owns both, so a rename or a recolour reaches every row wearing it at once.
   */
  group: SessionGroup | null;
  /** This device's unsent composer content for the session; EMPTY when there is none. */
  draft: DraftMessage;
  conn: GatewayConn;
  match: SessionMatch | null;
  needle: string;
  commands: SessionRowCommands;
  deletion: SessionRowDeletion;
  /**
   * This session is the one the pane beside the list is showing. The row wears the
   * standing paper — never a second rail down the leading edge, which belongs to the
   * group's colour and would read as two marks on one filed row.
   */
  isOpen?: boolean;
  /** Only project lists support Shift-click selection; other rows still open normally. */
  isSelected?: boolean;
  onSelectionClick?: (event: MouseEvent<HTMLButtonElement>) => boolean;
  /** The current project's selected rows, in their displayed order. */
  dragIds?: readonly string[];
  /**
   * Let a reader DRAG this row onto something that takes it — a group's band. The row
   * carries its own session id; lists that have nowhere to drop it leave this off.
   */
  isDraggable?: boolean;
}) {
  const timestamp = session.modified_at ?? session.created_at;
  // DIRTY: this device is holding composer content nobody has sent — words, a
  // picture, a file. When the session has no title of its own, that content names
  // the row, which otherwise reads "Untitled session" with nothing on screen to
  // say why it is worth opening. It is also the row's STATUS: the mark on the
  // right reads DIRTY where it would otherwise read IDLE, because a session
  // holding words you have not sent is not idle.
  const hasUnsent = draftMessageHasUnsent(draft);
  const title =
    session.title?.trim() ||
    (hasUnsent ? firstLine(draft.text) || attachmentSummary(draft.attachments) : '') ||
    'Untitled session';
  const live = sessionIsLive(session);
  const turns = Number(session.turn_count ?? 0);
  // Turns that finished while this session was closed: the one thing a relative
  // timestamp cannot announce. The GATEWAY counts them and says so on the row, so
  // every surface of that machine paints the same badge — and the poll that brings
  // the row back read is what retires it here.
  //
  // The session standing open in the pane beside the list is BEING READ: its row
  // drops NEW the moment it opens. The transcript reports the read mark through its
  // OWN gateway client, which this list only hears about on its next poll — and
  // until then the badge sat on the very conversation the reader was looking at.
  const unread = isOpen ? 0 : unreadTurnCount(session);
  // STOPPED: the newest turn was cut off — the operator cancelled it, or the
  // gateway died mid-answer and swept it on its next start. Gated on the unread
  // mark on purpose, so the flag is BOUNDED: it reports something you have not
  // seen, and opening the session retires it exactly the way it retires "new".
  // Ungated it would sit on the row until that session's next turn, which for an
  // abandoned session never comes. STOPPED takes the status mark for itself, ahead
  // of the NEW that mark would otherwise carry, so a cut-off row never says both.
  const stopped = !live && sessionWasInterrupted(session) && unread > 0;
  const isPutAway = sessionIsArchived(session) || group?.archived_at != null;
  const status = statusLabel(session, stopped, hasUnsent, unread, isPutAway);
  // The right chevron is a real DISCLOSURE, not decoration: it opens this
  // session's usage rollup in place. It stays a sibling of the open-session
  // button, never nested inside it, so "tell me more" cannot navigate away.
  const [statsOpen, setStatsOpen] = useState(false);
  // The rollup FETCHES on mount and aborts on unmount, so it cannot simply stay
  // mounted while closed. Dropping it on the same commit that closes the row
  // would pull the content out from under the collapse, so it outlives
  // `statsOpen` by exactly one transition and is only then let go.
  const [statsMounted, setStatsMounted] = useState(false);
  useEffect(() => {
    if (statsOpen || !statsMounted) return;
    const timer = window.setTimeout(() => setStatsMounted(false), STATS_MOTION_MS);
    return () => window.clearTimeout(timer);
  }, [statsOpen, statsMounted]);
  // Mount and open in ONE commit. The grid wrapper never leaves the tree, so
  // 0fr -> 1fr is a transition on a persistent element — no `@starting-style`,
  // which WebKit applies a frame late to freshly inserted nodes.
  const toggleStats = useCallback(() => {
    setStatsMounted(true);
    setStatsOpen((open) => !open);
  }, []);
  const [renameDraft, setRenameDraft] = useState<string | null>(null);
  const [renameBusy, setRenameBusy] = useState(false);
  const [renameError, setRenameError] = useState('');
  const renameInputRef = useRef<HTMLInputElement>(null);
  const renameSavingRef = useRef(false);
  const skipRenameBlurRef = useRef(false);
  const selectRenameOnFocusRef = useRef(false);
  const beginRename = useCallback(() => {
    skipRenameBlurRef.current = false;
    selectRenameOnFocusRef.current = true;
    setRenameError('');
    setRenameDraft(session.title?.trim() ?? '');
  }, [session.title]);
  const cancelRename = useCallback(() => {
    selectRenameOnFocusRef.current = false;
    setRenameError('');
    setRenameDraft(null);
  }, []);
  const commitRename = useCallback(async () => {
    if (renameDraft === null || renameSavingRef.current) return;
    const next = renameDraft.trim();
    if (!next) {
      setRenameError('A session name cannot be empty.');
      return;
    }
    if (next === (session.title?.trim() ?? '')) {
      cancelRename();
      return;
    }

    renameSavingRef.current = true;
    setRenameBusy(true);
    setRenameError('');
    try {
      await commands.rename(session, conn, next);
      setRenameDraft(null);
    } catch (cause) {
      setRenameError(cause instanceof Error ? cause.message : 'Session could not be renamed.');
      requestAnimationFrame(() => renameInputRef.current?.focus());
    } finally {
      renameSavingRef.current = false;
      setRenameBusy(false);
    }
  }, [cancelRename, commands, conn, renameDraft, session]);
  // The star is the GATEWAY's, and the row is holding the only copy of it there is:
  // `favorite_rank`, straight off this session. No device-side store can disagree
  // with it — which is what used to leave one screen starred and another plain.
  const isStarred = isFavorite(session);
  // Where the row GOES when this flips — the pinned band at the top of the project,
  // on page one — belongs to the group that pages it, so `ProjectGroup` owns the
  // follow. This is only the mark and the strip's verb.
  const toggleFavorite = useCallback(
    () => commands.toggleStar(session, conn),
    [commands, session, conn],
  );
  // WHICH SIDE OF THE ARCHIVE THIS ROW IS ON, and the verb that moves it to the other one.
  // A session still working keeps its row and is told so here, with nothing sent: archiving
  // it would hide work a human is waiting on, and the gateway refuses it for that same
  // reason (409 `session-busy`), which the row reads back the same way.
  const groupIsArchived = group?.archived_at != null;
  const [archiveError, setArchiveError] = useState('');
  const toggleArchive = useCallback(() => {
    const archive = commands.archive;
    if (!archive) return;
    if (!isPutAway && (sessionIsLive(session) || sessionNeedsInput(session))) {
      setArchiveError(STILL_WORKING);
      return;
    }
    setArchiveError('');
    void (async () => {
      try {
        await archive(session, conn, !isPutAway);
      } catch (cause) {
        setArchiveError(archiveRefusal(cause, isPutAway));
      }
    })();
  }, [commands, conn, isPutAway, session]);
  const archiving: SwipeAction[] = commands.archive && !groupIsArchived
    ? [
        {
          key: 'archive',
          label: isPutAway ? 'Unarchive' : 'Archive',
          icon: <ArchiveIcon className="size-4" />,
          onSelect: toggleArchive,
        },
      ]
    : [];
  // THE ROW'S FILING VERB, and there is at most one of it. A project with somewhere else
  // to put this session opens its sheet under the strip; when the only group it has is
  // the one this row is already under, there is nothing to ask and the verb takes it out.
  const filing: SwipeAction[] = commands.ungroup
    ? [
        {
          key: 'group',
          label: 'Ungroup',
          icon: <ProjectsIcon className="size-4" />,
          onSelect: () => commands.ungroup?.(session, conn),
        },
      ]
    : commands.moveToGroup
      ? [
          {
            key: 'group',
            label: 'Move to...',
            name: 'Move',
            icon: <FolderPlusIcon className="size-4" />,
            onSelect: (anchor: HTMLElement) => commands.moveToGroup?.(session, conn, anchor),
          },
        ]
      : [];
  // WHAT THIS ROW STANDS, measured as the question is asked. The two answers carry
  // a 48px floor of their own, and this row is TALLER than that floor whenever its
  // metadata stacks under the title — 52px on a phone — so the list lost those
  // pixels the moment the confirmation took the row's place. The trash verb is the
  // only way into that confirmation, so the row is still on screen right here, and
  // `clientHeight` is the height it stands inside the list's own rule.
  const rowRef = useRef<HTMLDivElement>(null);
  const [standingHeight, setStandingHeight] = useState<number | undefined>(undefined);
  const requestDelete = useCallback(() => {
    setStandingHeight(rowRef.current?.clientHeight);
    commands.requestDelete(session, conn);
  }, [commands, session, conn]);

  // A phone raises no drag events either, so a finger that RESTS on the row picks it
  // up instead (`lib/session-drag`); the row stays behind, dimmed, while it is out.
  const isCarried = useSessionLift(session.id, rowRef, Boolean(isDraggable));

  return (
    <div
      ref={rowRef}
      data-session-row={session.id}
      draggable={isDraggable}
      onDragStart={
        isDraggable
          ? (event) => {
              const ids = isSelected && dragIds?.length ? dragIds : [session.id];
              event.dataTransfer.setData('text/plain', session.id);
              if (ids.length > 1) event.dataTransfer.setData(SESSION_DRAG_MIME, JSON.stringify(ids));
              event.dataTransfer.effectAllowed = 'move';
              if (rowRef.current) carryOneRow(rowRef.current, event, ids.length);
            }
          : undefined
      }
      className={`${isSelected ? 'bg-accent/15' : isOpen ? 'bg-standing' : ''} ${isCarried ? 'opacity-40' : ''} [&+&]:border-t ${needle ? '[&+&]:border-dialog-hint' : '[&+&]:border-edge'}`}
    >
      {/* Rename is direct manipulation: the row stays put and only its title becomes ink
          with a caret. Metadata, status, and disclosure do not blink out around it. */}
      {deletion ? (
        <ConfirmRow
          question={`Delete ${title}?`}
          confirmLabel={deletion.isBusy ? 'Deleting...' : 'Yes, delete'}
          isBusy={deletion.isBusy}
          rowHeight={standingHeight}
          onKeep={deletion.cancel}
          onConfirm={deletion.confirm}
        />
      ) : (
        <SwipeActions
          label={title}
          isCurrent={isOpen}
          isSelected={isSelected}
          actions={
            renameDraft !== null
              ? []
              : [
                  {
                    key: 'favorite',
                    label: isStarred ? 'Unstar' : 'Star',
                    icon: <StarIcon filled={isStarred} className="size-4" />,
                    // The favorite keeps its accent fill; the caption uses readable ink.
                    tone: 'accent',
                    onSelect: toggleFavorite,
                  },
                  {
                    key: 'rename',
                    label: 'Rename',
                    icon: <PencilIcon className="size-4" />,
                    onSelect: beginRename,
                  },
                  ...filing,
                  ...archiving,
                  {
                    key: 'delete',
                    label: 'Delete',
                    icon: <TrashIcon className="size-4" />,
                    tone: 'danger',
                    onSelect: requestDelete,
                  },
                ]
          }
          trailing={
            <HeaderActions>
              <RowDisclosure
                isOpen={statsOpen}
                label={`${statsOpen ? 'Hide' : 'Show'} details for ${title}`}
                onClick={toggleStats}
              />
            </HeaderActions>
          }
        >
          {/* Hover changes the title ink; status marks retain their own meaning. */}
          <div className="group flex items-stretch">
            {/* THE GROUP'S OWN COLOUR, down the leading edge of the row. A filed session
                is filed wherever it is painted - pinned above the page, found by a
                search, sitting under its band - and without this mark the only place
                you could see it was the band, if the band happened to be on screen.
                Both the colour and the name are read off the GROUP at paint time; a row
                carrying a copy of them is a row that keeps painting a recoloured group
                in its old ink. */}
            {typeof session.group_id === 'string' && session.group_id !== '' && (
              <span className="flex shrink-0 items-stretch">
                <span aria-hidden className={`w-1 ${groupSwatch(group?.color ?? null)}`} />
                <span className="sr-only">{`In group ${group?.name ? group.name : 'unnamed'}`}</span>
              </span>
            )}
            <SessionRowSurface
              isEditing={renameDraft !== null}
              isCurrent={isOpen}
              isSelected={isSelected}
              sessionId={session.id}
              onOpen={() => void commands.open(conn, session.id)}
              onSelectionClick={onSelectionClick}
            >
              {/* Narrow lists stack metadata under the title. The container, not the
                  viewport, selects the full-width table so desktop sidebars stay readable.
                  The favorite shares the status cluster; no leading mark indents the title. */}
              <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-2 gap-y-1 @3xl:grid-cols-[minmax(0,1fr)_4.5rem_4.5rem_7.5rem_6rem] @3xl:gap-y-0">
                {/* The NAME, and nothing but the name. The badges used to ride inside this
              cell, so every row started its flags at a different x — the longer the
              title, the further right its `NEW` — and a long title pushed them off
              the line entirely. They have their own column now. */}
                <span className="col-start-1 row-start-1 flex min-w-0 items-center gap-1.5 @3xl:col-start-auto @3xl:row-start-auto">
                  {/* THE LEAF is the one line a human wrote. It keeps the same
                JetBrains Mono face as the whole app. Its 12px medium type completes
                the hierarchy below the larger machine, project and set names, while
                every fact beside it steps down to the metadata scale. A list is mostly
                titles, so one step down is one more row in the same glass. */}
                  {renameDraft !== null ? (
                    <EditableNameField
                      ref={renameInputRef}
                      autoFocus
                      aria-busy={renameBusy}
                      aria-label={`Rename ${title}`}
                      autoCapitalize="sentences"
                      autoCorrect="off"
                      face="text-body font-medium text-white placeholder:text-white/45"
                      fit="track"
                      placeholder="Untitled session"
                      readOnly={renameBusy}
                      value={renameDraft}
                      onBlur={() => {
                        if (skipRenameBlurRef.current) {
                          skipRenameBlurRef.current = false;
                          return;
                        }
                        void commitRename();
                      }}
                      onFocus={(event) => {
                        if (!selectRenameOnFocusRef.current) return;
                        selectRenameOnFocusRef.current = false;
                        const field = event.currentTarget;
                        const end = field.value.length;
                        if (!hasHardwarePointer()) {
                          field.setSelectionRange(end, end);
                          return;
                        }
                        field.setSelectionRange(0, end, 'backward');
                        requestAnimationFrame(() => {
                          if (renameInputRef.current === field) field.scrollLeft = 0;
                        });
                      }}
                      onChange={(event) => {
                        setRenameDraft(event.target.value);
                        setRenameError('');
                      }}
                      onKeyDown={(event) => {
                        if (event.key === 'Enter') {
                          event.preventDefault();
                          void commitRename();
                        }
                        if (event.key === 'Escape' && !renameBusy) {
                          event.preventDefault();
                          skipRenameBlurRef.current = true;
                          cancelRename();
                        }
                      }}
                    />
                  ) : (
                    <span
                      title={title}
                      className={`min-w-0 truncate text-body font-medium mouse:group-hover:text-accent-ink ${
                        session.title?.trim() ? 'text-white' : 'text-white/45'
                      }`}
                    >
                      {title}
                    </span>
                  )}
                </span>
                {/* `@3xl:contents` lets one DOM order cover three layouts. Under 24rem the
              row shows its title and status mark; from 24rem, including the desktop sidebar,
              the id, turns and time appear as one line of prose under the title. At
              48rem that line dissolves so the id and the turn count become columns in their
              own right. The facts are never dropped from the tree, only from the paint. */}
                {/* One rank, one ink: hierarchy is carried by SIZE (title 12px vs meta 10px),
              never by transparency — an id at 55% ink beside a `·` at 40% beside a full
              hint made one 9px line carry three different inks and none of them readable.
              A COUNT ends on its track's edge, not where its own digits run out:
              left-aligned, `9 turns` stopped 6px short of the `20 turns` one row below
              it, and a column of numbers that does not end together is not a column. */}
                <span className="col-start-1 row-start-2 hidden min-w-0 items-center gap-x-2 font-mono text-meta text-dialog-hint @sm:flex @3xl:contents">
                  <span className="truncate tabular-nums">{shortId(session.id)}</span>
                  <span className="@3xl:hidden" aria-hidden="true">
                    ·
                  </span>
                  <span className="whitespace-nowrap font-mono text-meta text-dialog-hint tabular-nums @3xl:justify-self-end">
                    {turns} {turns === 1 ? 'turn' : 'turns'}
                  </span>
                </span>
                <span className="col-start-3 row-start-1 inline-flex shrink-0 items-center gap-2 justify-self-end @3xl:col-start-auto @3xl:row-start-auto @3xl:justify-self-start">
                  <span
                    data-session-favorite-slot
                    className="grid size-3.5 shrink-0 place-items-center"
                    aria-hidden={!isStarred}
                  >
                    {isStarred && (
                      <>
                        <StarIcon filled className="size-3" />
                        <span className="sr-only">Favorite</span>
                      </>
                    )}
                  </span>
                  <span
                    data-session-status
                    role={renameBusy ? 'status' : undefined}
                    title={
                      status === 'DIRTY'
                        ? "Unsent message waiting in this session's composer"
                        : undefined
                    }
                    className={`shrink-0 items-center gap-1 font-mono text-chip font-bold tracking-[0.08em] ${
                      // Narrow sidebars show only live or input-needed marks.
                      status === 'IDLE' ? 'hidden @sm:inline-flex' : 'inline-flex'
                    } ${statusTone(session, stopped, hasUnsent, unread, isPutAway)}`}
                  >
                    <span
                      data-session-status-dot
                      aria-hidden="true"
                      className={`size-1.5 shrink-0 ${statusDot(session, stopped, hasUnsent, unread, isPutAway)} ${live ? 'animate-pulse motion-reduce:animate-none' : ''}`}
                    />
                    <span className="sr-only @sm:not-sr-only">
                      {renameBusy ? 'Saving' : status}
                    </span>
                  </span>
                </span>
                <span
                  className="col-start-2 col-end-4 row-start-2 hidden justify-self-end whitespace-nowrap font-mono text-meta text-dialog-hint tabular-nums @sm:block @3xl:col-start-auto @3xl:col-end-auto @3xl:row-start-auto"
                  title={formatExact(timestamp)}
                >
                  {timeLabel(timestamp)}
                </span>
              </span>
            </SessionRowSurface>
          </div>
        </SwipeActions>
      )}
      {(renameError || archiveError || deletion?.error) && (
        <div className="px-3 pb-2">
          <Banner kind="err">{renameError || archiveError || deletion?.error}</Banner>
        </div>
      )}
      {/* Height eases through a 0fr -> 1fr grid track: the one pure-CSS way to
          animate to CONTENT height without measuring it, and unlike a mount it
          plays in BOTH directions. The inner clip keeps the rollup from
          spilling over the next row while the track is still closing. */}
      <div
        aria-hidden={!statsOpen}
        className={`grid transition-[grid-template-rows] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
          statsOpen ? 'grid-rows-[1fr]' : 'grid-rows-[0fr]'
        }`}
      >
        <div className="overflow-hidden">
          <div
            className={`transition-[opacity,translate] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
              statsOpen ? 'opacity-100' : '-translate-y-1 opacity-0'
            }`}
          >
            {statsMounted && <SessionStats session={session} conn={conn} />}
          </div>
        </div>
      </div>
      {match && <MatchPreview match={match} needle={needle} />}
    </div>
  );
});

// The expanded half of a session row: everything the list cannot afford to
// carry for every session at once. It is fetched HERE only when the row opens,
// and aborted if the row closes first; the gateway memoizes decoded tool tallies.
function SessionStats({ session, conn }: { session: Session; conn: GatewayConn }) {
  const [usage, setUsage] = useState<SessionUsage | null>(null);
  const [phase, setPhase] = useState<'loading' | 'ready' | 'error'>('loading');

  useEffect(() => {
    const controller = new AbortController();
    setPhase('loading');
    new GatewayClient(conn)
      .sessionUsage(session.id, controller.signal)
      .then((next) => {
        if (controller.signal.aborted) return;
        setUsage(next);
        setPhase('ready');
      })
      .catch(() => {
        if (!controller.signal.aborted) setPhase('error');
      });
    return () => controller.abort();
  }, [conn, session.id]);

  return <SessionStatsPanel session={session} usage={usage} phase={phase} />;
}

/** The deterministic paint surface behind the row's on-demand usage read. */
export function SessionStatsPanel({
  session,
  usage,
  phase,
}: {
  session: Session;
  usage: SessionUsage | null;
  phase: 'loading' | 'ready' | 'error';
}) {
  const cacheReadShare = usage?.cache_read_share_percent;
  const reuseCoverage = usage?.reusable_prefix_coverage_percent;
  // The coverage number is only as good as the calls it was measured on, so the
  // card prints its sample beside it: a bold percentage over an undisclosed
  // denominator is the trick this pair exists to refuse.
  const reuseSamples = usage?.prompt_cache_sample_count;
  const reuseIsEstimated = usage?.reusable_prefix_estimated;

  return (
    <div
      className={`max-w-2xl border-t border-dialog-edge bg-panel-2 py-4 font-mono ${LIST_EDGE} ${LIST_EDGE_END}`}
    >
      {phase === 'loading' && (
        <p role="status" className="text-ui text-dialog-hint">
          Reading session metrics…
        </p>
      )}
      {phase === 'error' && (
        <p role="alert" className="text-ui text-warn">
          Session metrics unavailable. Close and reopen details to retry.
        </p>
      )}
      {phase === 'ready' && !usage && (
        <p className="text-ui text-dialog-hint">
          No measured calls yet. Metrics appear after the first model response.
        </p>
      )}
      {phase === 'ready' && usage && (
        <>
          <SessionHealth snapshot={usage.health} />
          <h3 className="border-t border-dialog-edge pt-4 text-title font-bold text-white">
            Session totals
          </h3>
          <p className="mt-1 text-ui text-dialog-hint">
            Across all calls, including repeated context.
          </p>
          <dl className="mt-3 grid grid-cols-4 gap-x-3 gap-y-3">
            <Stat label="Total input" value={compactCount(usage.input_tokens)} />
            <Stat label="Total output" value={compactCount(usage.output_tokens)} />
            <Stat label="Cost" value={formatUsd(usage.cost_usd)} />
            <Stat label="Folds" value={compactCount(usage.fold_count)} />
            <Stat label="Turns" value={compactCount(usage.turn_count)} />
            <Stat label="Calls" value={compactCount(usage.iteration_count)} />
            <Stat label="Tools" value={compactCount(usage.tool_call_count)} />
          </dl>
          <dl
            aria-label="Prompt cache"
            className="mt-2.5 grid grid-cols-2 gap-3 border-t border-dialog-edge/40 pt-2"
          >
            <CacheStat
              label="Cached input"
              value={typeof cacheReadShare === 'number' ? `${cacheReadShare}%` : '—'}
              explanation="Share of all input served from provider cache"
            />
            <CacheStat
              label="Reuse coverage"
              value={
                typeof reuseCoverage === 'number'
                  ? `${reuseIsEstimated ? '≈' : ''}${reuseCoverage}%`
                  : '—'
              }
              explanation={
                typeof reuseSamples === 'number'
                  ? `${reuseIsEstimated ? 'Estimated share' : 'Share'} of reusable prior input recovered from cache · ${compactCount(reuseSamples)} of ${compactCount(usage.iteration_count)} calls`
                  : `${reuseIsEstimated ? 'Estimated share' : 'Share'} of reusable prior input recovered from cache`
              }
            />
          </dl>
          <dl className="mt-2.5 flex flex-wrap items-baseline gap-x-3 gap-y-1 border-t border-dialog-edge/40 pt-2">
            {/* `/usage` names the model the session actually RAN on, but only
                once a turn has finished stamping it — a live session's newest
                turn has none. The pin (list row) and the state's root model are
                the standing answers, so fall back to those instead of a dash. */}
            <Meta
              label="Model"
              value={usage.model || session.model_pref?.model || session.model || '—'}
              title={usage.provider || session.model_pref?.provider}
            />
            <Meta
              label="Active"
              value={formatDuration(usage.duration_ms)}
              title="Time spent inside turns"
            />
          </dl>
        </>
      )}
    </div>
  );
}

function Stat({ label, value }: { label: string; value: string }) {
  return (
    <div className="min-w-0">
      <dt className="text-ui text-dialog-hint">{label}</dt>
      <dd className="mt-1 text-body font-bold tabular-nums text-white">{value}</dd>
    </div>
  );
}

function CacheStat({
  label,
  value,
  explanation,
}: {
  label: string;
  value: string;
  explanation: string;
}) {
  return (
    <div className="min-w-0">
      <dt className="text-ui text-dialog-hint">{label}</dt>
      <dd className="mt-1 text-body font-bold tabular-nums text-white">{value}</dd>
      <dd className="mt-1 text-pretty text-ui text-dialog-hint">{explanation}</dd>
    </div>
  );
}

// The grid above answers "how much"; this row answers "of what, for how long".
// It reuses the grid's dim-key/strong-value grammar so the two facts read as
// labelled data instead of one faint unlabelled sentence.
function Meta({ label, value, title }: { label: string; value: string; title?: string }) {
  return (
    <div className="flex min-w-0 items-baseline gap-1.5" title={title}>
      <dt className="shrink-0 text-ui text-dialog-hint">{label}</dt>
      <dd className="min-w-0 break-all text-ui font-bold text-white">{value}</dd>
    </div>
  );
}

function compactCount(value?: number): string {
  const n = Number(value ?? 0);
  if (!Number.isFinite(n)) return '—';
  if (n < 1_000) return String(n);
  if (n < 1_000_000) return `${(n / 1_000).toFixed(n < 10_000 ? 1 : 0)}k`;
  if (n < 1_000_000_000) return `${(n / 1_000_000).toFixed(n < 10_000_000 ? 1 : 0)}M`;
  return `${(n / 1_000_000_000).toFixed(1)}B`;
}

// Sub-cent totals must not read as "$0.00" — a session that cost something is
// never free.
function formatUsd(value?: number): string {
  const n = Number(value ?? 0);
  if (!Number.isFinite(n) || n <= 0) return '$0';
  if (n < 0.01) return '<$0.01';
  if (n < 1_000) return `$${n.toFixed(2)}`;
  return `$${Math.round(n).toLocaleString()}`;
}

function formatDuration(value?: number): string {
  const ms = Number(value ?? 0);
  if (!Number.isFinite(ms) || ms <= 0) return '0s';
  const seconds = Math.round(ms / 1_000);
  if (seconds < 60) return `${seconds}s`;
  const minutes = seconds / 60;
  if (minutes < 60) return `${Math.round(minutes)}m`;
  const hours = minutes / 60;
  return hours < 24 ? `${hours.toFixed(1)}h` : `${(hours / 24).toFixed(1)}d`;
}

// The list has nothing to paint yet. Two rules keep this honest:
//
// 1. Colour: the previous skeleton was drawn in panel tints — and `--panel2`
//    EQUALS `--surface` in the shipped themes (light: both #faf3eb), so it
//    rendered as invisible boxes. Bars use `--color-muted`, a mid grey that
//    separates from every gateway surface, and the Braille spinner says it in
//    words — the same one the transcript and the TUI use.
// 2. GEOMETRY: a placeholder that is not the exact height of the thing it
//    stands for makes the whole list jump when data lands. The skeleton mirrors
//    the compact 36px mouse project header and the session rows' density steps.
//    Each bar is centred inside an INVISIBLE glyph of the real type step, so its
//    line box — not a bare decorative bar — determines the same vertical rhythm.
// 3. ONE OWNER PER SEAM. The filter owns the line above the list, each skeleton
//    header owns the line below itself, sibling rows own their internal lines,
//    and the rows wrapper closes the group. No top border or negative margin is
//    needed, so loading and loaded states cannot stack adjacent rules.
function SkeletonBar({
  type,
  width,
  baz,
  tone,
}: {
  type: string;
  width: string;
  baz: string;
  tone: string;
}) {
  return (
    <span className={`grid ${width}`}>
      <span className={`col-start-1 row-start-1 invisible font-mono ${type}`}>&nbsp;</span>
      <span className={`col-start-1 row-start-1 self-center ${baz} ${tone}`} />
    </span>
  );
}

export function NavigatorSkeleton() {
  return (
    <div role="status" aria-live="polite" aria-label="Loading sessions">
      <div className="animate-pulse motion-reduce:animate-none" aria-hidden="true">
        {SKELETON_GROUPS.map((rows, group) => (
          <div key={group}>
            {/* The list's OWN header band, so a loading screen can never stand at a
                different height from the screen it turns into. */}
            <SectionHeader>
              {/* Two lines, because the header it stands in for is two: a name over
                  the path that qualifies it. A one-line skeleton grew by a line the
                  moment data landed, which is a layout jump on every cold open — so
                  the bars go through the same `name`/`qualifier` slots the real
                  header uses rather than a hand-stacked pair. */}
              <HeaderTitle
                name={<SkeletonBar type="text-title" width="w-28" baz="h-2.5" tone="bg-muted/40" />}
                qualifier={
                  <SkeletonBar type="text-chip" width="w-40" baz="h-1.5" tone="bg-muted/20" />
                }
              />
              <HeaderActions>
                <SkeletonBar type="text-chip" width="w-14" baz="h-1.5" tone="bg-muted/25" />
              </HeaderActions>
            </SectionHeader>
            {/* Mirrors `SessionRow` — the SAME grid, the same leading edge, the same
                trailing column — because a skeleton that stands anywhere else is a
                layout jump the user pays for on every cold open. It used to carry an
                `invisible` chevron and `px-3`, indenting its bars 32px in a list whose
                real rows started their titles at 8px. */}
            <div>
              {rows.map((width, row) => (
                <div
                  key={row}
                  className={`flex min-h-12 w-full items-center py-1.5 [&+&]:border-t [&+&]:border-edge mouse:min-h-8 mouse:py-1 ${LIST_EDGE}`}
                >
                  <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 @3xl:grid-cols-[minmax(0,1fr)_5.5rem_4.5rem_4.5rem_7.25rem_6rem] @3xl:gap-y-0">
                    <span className="col-start-1 row-start-1 @3xl:col-start-auto @3xl:row-start-auto">
                      <SkeletonBar type="text-meta" width={width} baz="h-2.5" tone="bg-muted/30" />
                    </span>
                    <span className="col-start-1 row-start-2 hidden items-center gap-x-2 @sm:flex @3xl:contents">
                      <SkeletonBar type="text-chip" width="w-14" baz="h-1.5" tone="bg-muted/20" />
                      <SkeletonBar type="text-chip" width="w-10" baz="h-1.5" tone="bg-muted/20" />
                    </span>
                    {/* The flag column a real row keeps for `NEW`.
                        Nothing is loading in it, but the track has to exist or the
                        columns shift the moment the rows arrive. */}
                    <span className="col-start-2 row-start-1 @3xl:col-start-auto @3xl:row-start-auto" />
                    <span className="col-start-3 row-start-1 justify-self-end @3xl:col-start-auto @3xl:row-start-auto @3xl:justify-self-start">
                      <SkeletonBar type="text-chip" width="w-12" baz="h-1.5" tone="bg-muted/25" />
                    </span>
                    <span className="col-start-3 row-start-2 hidden justify-self-end @sm:block @3xl:col-start-auto @3xl:row-start-auto @3xl:justify-self-start">
                      <SkeletonBar type="text-chip" width="w-12" baz="h-1.5" tone="bg-muted/20" />
                    </span>
                  </span>
                  <HeaderActions>
                    <span className="w-7 sm:w-8 mouse:w-6" />
                  </HeaderActions>
                </div>
              ))}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

export function shortId(id: string): string {
  return id.split('-')[0]?.slice(0, 8) || id.slice(0, 8);
}

function statusLabel(session: Session, stopped: boolean, hasUnsent: boolean, unread: number, isPutAway: boolean): string {
  // The DEMAND outranks liveness: a parked run is still live, and "LIVE" is
  // exactly what made the row look like it was getting on with it.
  // PUT AWAY outranks everything below: an archived session takes no new work at the
  // gateway, so whatever the row was doing when it was filed is no longer news.
  if (isPutAway) return 'ARCHIVED';
  if (sessionNeedsInput(session)) {
    // …and HOW MANY are open: answering one of two has to show, or the badge
    // reads exactly the same as it did before the answer.
    const open = sessionInputCount(session);
    return open > 1 ? `INPUT NEEDED ×${open}` : 'INPUT NEEDED';
  }
  if (sessionIsLive(session)) return 'LIVE';
  if (stopped) return 'STOPPED';
  // The gateway's own count of answers that landed since this reader last read the
  // session. It stands where IDLE and DIRTY stand, because it is the same fact about
  // the row: what this conversation is holding for you.
  if (unread > 0) return unread > 1 ? `NEW ×${unread}` : 'NEW';
  if (session.status === 'suspended') return 'WAITING';
  // Unsent words outrank IDLE and nothing else. What the session is doing is
  // news about the session; this is news about you, and it waits its turn.
  if (hasUnsent) return 'DIRTY';
  return 'IDLE';
}

function statusTone(session: Session, stopped: boolean, hasUnsent: boolean, unread: number, isPutAway: boolean): string {
  if (isPutAway) return 'text-muted';
  if (sessionNeedsInput(session)) return 'text-warn';
  if (sessionIsLive(session)) return 'text-ok';
  if (stopped) return 'text-err';
  if (unread > 0) return 'text-accent';
  if (session.status === 'suspended') return 'text-warn';
  if (hasUnsent) return 'text-dirty';
  return 'text-dialog-hint';
}

function statusDot(session: Session, stopped: boolean, hasUnsent: boolean, unread: number, isPutAway: boolean): string {
  // Filled and dimmed: put away is a state the row IS in, not the absence of one.
  if (isPutAway) return 'bg-muted';
  if (sessionNeedsInput(session)) return 'animate-pulse bg-warn-strong motion-reduce:animate-none';
  if (sessionIsLive(session)) return 'animate-pulse bg-ok motion-reduce:animate-none';
  // Solid, never pulsing: an interrupted session is the opposite of live.
  if (stopped) return 'bg-err';
  if (unread > 0) return 'bg-accent';
  if (session.status === 'suspended') return 'bg-warn-strong';
  // Filled like every mark that means something is waiting. The hollow square
  // is IDLE's alone, because it is the one that means nothing is.
  if (hasUnsent) return 'bg-dirty';
  return 'border border-dialog-hint';
}

export function draftSearchText(draft: DraftMessage | undefined): string {
  if (!draft) return '';
  return [draft.text, ...draft.attachments.map((attachment) => attachment.filename)]
    .join(' ')
    .toLowerCase();
}

export function sessionSearchText(session: Session): string {
  return [
    session.title,
    session.id,
    session.project_name,
    session.workspace?.label,
    session.workspace?.root,
    session.status,
    sessionNeedsInput(session) ? 'input needed waiting human' : '',
    sessionWasInterrupted(session) ? 'stopped interrupted' : '',
    sessionIsLive(session) ? 'live running' : 'idle',
  ]
    .filter(Boolean)
    .join(' ')
    .toLowerCase();
}

function dateMillis(value?: string): number {
  if (!value) return 0;
  const millis = new Date(value).getTime();
  return Number.isFinite(millis) ? millis : 0;
}

function formatExact(value?: string): string {
  const millis = dateMillis(value);
  return millis ? new Date(millis).toLocaleString() : '';
}

// Search hits stay subordinate to their session: compact transcript rows, not cards.
function MatchPreview({ match, needle }: { match: SessionMatch; needle: string }) {
  const rows =
    match.hits.length > 0
      ? match.hits
      : [
          {
            side: 'request' as const,
            snippet: match.requestSnippet?.trim() ?? '',
            at: null,
          },
          {
            side: 'reply' as const,
            snippet: match.replySnippet?.trim() ?? '',
            at: null,
          },
        ].filter((h) => h.snippet.length > 0);
  if (rows.length === 0) return null;
  return (
    <div className={`pb-1.5 ${LIST_EDGE} ${LIST_EDGE_END}`}>
      <div
        role="list"
        aria-label="Matching messages"
        className="divide-y divide-edge border-t border-edge"
      >
        {rows.map((hit, index) => (
          <div
            key={`${hit.side}-${hit.at ?? index}`}
            role="listitem"
            className="grid grid-cols-[2.5rem_minmax(0,1fr)] gap-2 py-1.5"
          >
            <span
              className={`font-mono text-ui font-bold mouse:text-meta ${
                hit.side === 'request' ? 'text-you-role' : 'text-vis-role'
              }`}
            >
              {hit.side === 'request' ? 'You' : 'Vis'}
            </span>
            <div className="line-clamp-2 whitespace-pre-wrap break-words font-mono text-ui text-dialog-foreground">
              <ReactMarkdown
                remarkPlugins={[remarkGfm]}
                rehypePlugins={[[searchPreviewMarkdown, needle]]}
                skipHtml
                allowedElements={['p', 'strong', 'em', 'del', 'code', 'br', 'mark']}
                unwrapDisallowed
                components={{
                  p: ({ children }) => <span className="block">{children}</span>,
                  strong: ({ children }) => <strong className="font-bold">{children}</strong>,
                  code: ({ children }) => (
                    <code className="bg-panel-2 px-0.5 font-mono">{children}</code>
                  ),
                  mark: ({ children }) => (
                    <mark className="bg-accent/20 px-0.5 font-bold text-white">{children}</mark>
                  ),
                }}
              >
                {hit.snippet}
              </ReactMarkdown>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

// Highlight parsed text, never Markdown syntax or URL targets. Images become their
// labels before rendering, so a search preview cannot fetch remote content.
function searchPreviewMarkdown(needle: string) {
  const pattern = needle ? new RegExp(`(${escapeRegExp(needle)})`, 'ig') : null;
  return (tree: Root) => {
    function visit(node: Root | Element) {
      // Work backwards so inserted marks are not visited or highlighted again.
      for (let index = node.children.length - 1; index >= 0; index -= 1) {
        let child = node.children[index];
        if (child.type === 'element' && child.tagName === 'img') {
          child = { type: 'text', value: String(child.properties.alt ?? '') };
          node.children[index] = child;
        }
        if (child.type === 'element') {
          visit(child);
        } else if (child.type === 'text' && pattern) {
          const parts = child.value.split(pattern);
          if (parts.length === 1) continue;
          const highlighted = parts.map<Element | Text>((value, part) =>
            part % 2 === 1
              ? {
                  type: 'element',
                  tagName: 'mark',
                  properties: {},
                  children: [{ type: 'text', value }],
                }
              : { type: 'text', value },
          );
          node.children.splice(index, 1, ...highlighted);
        }
      }
    }
    visit(tree);
  };
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// The first line of an unsent message, short enough to sit on one row. A dirty
// session has no other name, and a wall of pasted text must not become one.
export function firstLine(text: string): string {
  const line = text.split('\n', 1)[0]?.trim() ?? '';
  return line.length > 80 ? `${line.slice(0, 79)}\u2026` : line;
}

// An unsent message can be nothing but a picture. Then the attachment names the
// row, because "Untitled session" says nothing about what is waiting in it.
function attachmentSummary(attachments: PendingAttachment[]): string {
  const first = attachments[0];
  if (!first) return '';
  const name = firstLine(first.filename) || first.media_type;
  return attachments.length > 1 ? `${name} +${attachments.length - 1}` : name;
}
