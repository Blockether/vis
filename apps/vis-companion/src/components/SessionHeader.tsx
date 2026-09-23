import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';
import gatewaySchema from '../../../../packages/vis-contract/resources/vis-contract/schema/gateway.json';
import type { SessionGoal } from '../lib/types';
import { menuPosition, type MenuPosition } from '../lib/anchored-menu';
import { useDeskRail } from '../lib/fit-rows';
import { isIosNativeApp } from '../lib/host';
import { markSessionId } from '../lib/session-id';
import { Menu, MenuItem, MENU_WIDTH } from './Menu';
import { AlertIcon, CheckIcon, ClipIcon, CopyIcon, DotsIcon } from './icons';
import { BackButton, Button, DialogFrame, IconButton, Modal, SidebarToggle } from './ui';

const GOAL_STATUS = Object.fromEntries(
  gatewaySchema.$defs.session_goal.properties.status.oneOf.map(({ const: status, title }) => [
    status,
    title,
  ]),
);

/** Active wall time includes tools; inactive goals keep their persisted duration. */
function GoalTime({ goal }: { goal: SessionGoal }) {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    if (goal.status !== 'active') return;
    const timer = window.setInterval(() => setNow(Date.now()), 1_000);
    return () => window.clearInterval(timer);
  }, [goal.status]);
  const elapsed =
    goal.time_used_ms + (goal.status === 'active' ? Math.max(0, now - goal.updated_at) : 0);
  const seconds = Math.floor(elapsed / 1_000);
  const hours = Math.floor(seconds / 3_600);
  const minutes = Math.floor(seconds / 60) % 60;
  const duration = `${hours ? `${hours}h ` : ''}${hours || minutes ? `${minutes}m ` : ''}${seconds % 60}s`;
  return duration;
}

export type SessionHeaderModel = Readonly<{
  title: string;
  sessionId: string;
  connected: boolean;
  goal?: SessionGoal | null;
  artifacts: Readonly<{ count: number; isOpen: boolean }>;
}>;

export type SessionHeaderCommands = Readonly<{
  back: () => void;
  toggleArtifacts: () => void;
}>;

/**
 * The session's navigation and connection state. What this session IS — its id — and
 * what it has PRODUCED both stand behind one trailing kebab, so the band itself carries
 * only the sentence the screen is about. Behind it they are two rows, each a mark and
 * then a verb, in the same menu every row in the app hangs off the same mark.
 */
export function SessionHeader({
  model,
  commands,
  sidebar,
  team,
}: {
  model: SessionHeaderModel;
  commands: SessionHeaderCommands;
  /**
   * The desk's list toggle, when the shell has a list beside this transcript to
   * put away: it stands on the band's leading edge, where the phone's arrow does.
   */
  sidebar?: { isShown: boolean; onToggle: () => void };
  team?: ReactNode;
}) {
  // ON A DESK THE LIST IS BESIDE THIS HEADER, not behind it: there is nothing to go
  // back to, so the arrow goes. Its edge is not left bare, though — the toggle that
  // puts the list away, or brings it back, takes that leading column; only a desk
  // with no list to toggle lets the title claim the edge.
  const isDesk = useDeskRail();
  const nativeIos = isIosNativeApp();
  const [goalDetails, setGoalDetails] = useState(false);
  /** Where the session's own menu hangs, and `null` while it is closed. */
  const [menu, setMenu] = useState<MenuPosition | null>(null);
  // WHAT THE CLIPBOARD ANSWERED, worn by the row that asked it. Copying is silent, so
  // the row leaves the menu standing and reports in its own badge for a moment; a verb
  // that closes the panel under the thumb that pressed it reports nothing at all.
  const [copy, setCopy] = useState<'copied' | 'failed' | null>(null);
  const copyReset = useRef<number | undefined>(undefined);
  useEffect(() => () => window.clearTimeout(copyReset.current), []);
  const closeMenu = useCallback(() => {
    window.clearTimeout(copyReset.current);
    setCopy(null);
    setMenu(null);
  }, []);
  // ESCAPE BELONGS TO THE OPEN MENU. The screen under this header reads Escape as
  // "cancel the running turn", so the key is caught before it reaches that listener
  // and spent on the panel the reader can actually see.
  useEffect(() => {
    if (!menu) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.stopPropagation();
      closeMenu();
    };
    window.addEventListener('keydown', onKey, true);
    return () => window.removeEventListener('keydown', onKey, true);
  }, [menu, closeMenu]);
  // ONE DOOR, not a row of chips: the count the artifacts chip used to paint out loud
  // rides the trigger's own NAME, so a session that produced something still says so
  // to a reader who never sees the band.
  const produced = model.artifacts.count;
  const menuLabel = produced
    ? `Session actions, ${produced} artifact${produced === 1 ? '' : 's'}`
    : 'Session actions';
  // The first block of the id is what a reader recognises a session by, and it rides
  // the copy row's own line. The clipboard still gets the MARKED form: a bare uuid
  // pasted into a chat or an issue could be any identifier at all.
  const shortId = model.sessionId.slice(0, 8);
  async function copySessionId() {
    window.clearTimeout(copyReset.current);
    try {
      await navigator.clipboard.writeText(markSessionId(model.sessionId));
      setCopy('copied');
    } catch {
      setCopy('failed');
    }
    copyReset.current = window.setTimeout(() => setCopy(null), 1_500);
  }
  const goal = model.goal;
  return (
    /* On native iPhone the status bar is hidden. Put edge controls beside the
       island, then the session title below its safe-area inset. On iPad they
       share one row; browsers keep their visible status-bar padding. */
    <header
      className={`z-10 min-h-13 shrink-0 gap-0 border-b border-dialog-edge bg-panel-2 ${
        nativeIos
          ? 'grid grid-cols-[auto_minmax(0,1fr)_auto] grid-rows-[max(3.25rem,env(safe-area-inset-top))_auto] sm:flex sm:items-stretch'
          : 'flex items-stretch box-content pt-[env(safe-area-inset-top)] mouse:pt-0'
      }`}
    >
      {!isDesk && (
        <BackButton
          label="Back to sessions"
          onClick={commands.back}
          className={nativeIos ? 'row-start-1' : ''}
        />
      )}
      {isDesk && sidebar && <SidebarToggle isShown={sidebar.isShown} onClick={sidebar.onToggle} />}
      <div
        className={`min-w-0 flex-1 self-center px-4 py-1.5 mouse:py-1 ${
          nativeIos ? 'col-span-3 row-start-2 sm:col-auto sm:row-auto' : ''
        }`}
      >
        {/* The title is the sentence the screen is about. It stays one step above
            the connection line by size and weight — all in the app's one mono face. */}
        <h1 className="truncate text-title font-semibold text-white mouse:text-body">
          {model.title}
        </h1>
        <div className="flex min-w-0 items-center gap-1.5 font-mono text-ui text-dialog-hint mouse:text-meta">
          <span
            className={`size-1.5 shrink-0 ${
              model.connected ? 'bg-ok' : 'animate-pulse bg-turn-edge motion-reduce:animate-none'
            }`}
          />
          <span className="shrink-0">{model.connected ? 'Connected' : 'Reconnecting'}</span>
          {goal && (
            <Button
              variant="quiet"
              density="compact"
              pressEffect="none"
              className="-my-1.5 min-w-0 max-w-full"
              title={goal.objective}
              aria-haspopup="dialog"
              onClick={() => setGoalDetails(true)}
            >
              <span className="block max-w-full truncate">
                Goal: {GOAL_STATUS[goal.status]} - <GoalTime key={goal.id} goal={goal} />
              </span>
            </Button>
          )}
        </div>
      </div>
      <div
        className={`flex shrink-0 items-center gap-2 self-center pl-1 pr-[max(1rem,env(safe-area-inset-right))] ${
          nativeIos ? 'col-start-3 row-start-1 sm:col-auto sm:row-auto' : ''
        }`}
      >
        {team}
        <IconButton
          label={menuLabel}
          variant="quiet"
          aria-haspopup="dialog"
          aria-expanded={menu !== null}
          onClick={(event) =>
            setMenu(menuPosition(event.currentTarget.getBoundingClientRect(), MENU_WIDTH))
          }
        >
          <DotsIcon className="size-3.5" />
        </IconButton>
      </div>
      {menu && (
        <Menu label="Session actions" at={menu} onDismiss={closeMenu}>
          {model.artifacts.count > 0 && (
            <MenuItem
              // The row NAMES what pressing it does, and the count rides that name
              // rather than a badge: "Artifacts" with a bare "3" welded to it is what a
              // screen reader would have read out as one token.
              title={`${model.artifacts.isOpen ? 'Hide' : 'Open'} artifacts (${model.artifacts.count})`}
              icon={<ClipIcon className="size-3.5" />}
              onSelect={() => {
                closeMenu();
                commands.toggleArtifacts();
              }}
            />
          )}
          {/* THE ID IS A VERB HERE, not a caption with a chip beside it. Every row in
              this menu is a mark and then the thing pressing it does, one under the
              other; the id itself rides this row's own line as the fact it copies. */}
          <MenuItem
            title="Copy session id"
            meta={shortId}
            badge={copy ?? undefined}
            icon={
              copy === 'copied' ? (
                <CheckIcon className="size-3.5 text-ok" />
              ) : copy === 'failed' ? (
                <AlertIcon className="size-3.5 text-err-ink" />
              ) : (
                <CopyIcon className="size-3.5" />
              )
            }
            onSelect={() => {
              void copySessionId();
            }}
          />
        </Menu>
      )}
      {goal && goalDetails && (
        <Modal onDismiss={() => setGoalDetails(false)} size="fit">
          <DialogFrame
            title="Session goal"
            subtitle={GOAL_STATUS[goal.status]}
            closeLabel="Close session goal"
            onClose={() => setGoalDetails(false)}
          >
            <div className="space-y-3 p-4 text-body text-white">
              <p className="whitespace-pre-wrap break-words">{goal.objective}</p>
              <p className="text-ui text-dialog-hint">
                Iterations: {goal.iterations_used.toLocaleString('en-US')} /{' '}
                {goal.iteration_budget?.toLocaleString('en-US') ?? 'unlimited'}
              </p>
              <p className="text-ui text-dialog-hint">
                Time in goal: <GoalTime key={goal.id} goal={goal} />
              </p>
              {goal.reason && <p className="whitespace-pre-wrap break-words">{goal.reason}</p>}
              <p className="text-ui text-dialog-hint">
                Use /goal --pause, --resume or --cancel in the composer.
              </p>
            </div>
          </DialogFrame>
        </Modal>
      )}
    </header>
  );
}
