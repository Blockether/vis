/**
 * The debug log of an app, in the chrome of every app surface. The Log row of the
 * band menu shows only when the log has entries, and the menu counts the errors. Its panel lists
 * the entries oldest first, filters them by level and copies them.
 */

import { useId, useState } from 'react';

import {
  APP_LOG_FILTERS,
  logClock,
  logErrorCount,
  logFilterCounts,
  logText,
  shownLog,
  type AppLogFilter,
} from '../lib/app-log';
import type { LiveLogEntry, LiveLogLevel } from '../lib/live-view';
import { menuPosition, type MenuPosition } from '../lib/anchored-menu';
import { ChevronIcon, FullScreenIcon, LogIcon, MoreIcon } from './icons';
import { Menu, MenuItem, MENU_WIDTH } from './Menu';
import { BandButton, Chip, CopyChip } from './ui';

/** The ink of each level: errors and warnings stand out, debug steps back. */
const LEVEL_INK: Record<LiveLogLevel, string> = {
  debug: 'text-dialog-hint',
  info: 'text-white',
  warn: 'text-warn',
  error: 'text-err',
};

const FILTER_WORDS: Record<AppLogFilter, string> = {
  all: 'All',
  error: 'Error',
  warn: 'Warn',
  info: 'Info',
  debug: 'Debug',
};

/**
 * The ⋯ control of an app band: Log and Full screen, in a menu. Nothing when the
 * app has neither. The control counts the errors of the log.
 */
export function AppBandMenu({
  log,
  title,
  isLogOpen,
  onToggleLog,
  onFullScreen,
}: {
  log: readonly LiveLogEntry[] | undefined;
  title: string;
  isLogOpen: boolean;
  onToggleLog: () => void;
  onFullScreen?: () => void;
}) {
  const [menu, setMenu] = useState<MenuPosition | null>(null);
  const hasLog = log !== undefined && log.length > 0;
  if (!hasLog && !onFullScreen) return null;
  const errors = logErrorCount(log);
  const name = `More for ${title}${errors > 0 ? `, ${errors} log ${errors === 1 ? 'error' : 'errors'}` : ''}`;
  const pick = (act: () => void) => {
    setMenu(null);
    act();
  };
  return (
    <>
      <BandButton
        label={name}
        aria-haspopup="dialog"
        aria-expanded={menu !== null}
        onClick={(event) => setMenu(menuPosition(event.currentTarget.getBoundingClientRect(), MENU_WIDTH))}
        className={isLogOpen ? 'text-accent-ink' : ''}
        data-app-log-button
      >
        <span className="relative grid place-items-center">
          <MoreIcon className="size-3.5" />
          {errors > 0 && (
            <span
              aria-hidden
              data-log-errors
              className="absolute -right-3 -top-2.5 grid h-4 min-w-4 place-items-center border border-err-edge bg-err-surface px-0.5 font-mono text-meta font-bold leading-none tabular-nums text-err-ink"
            >
              {errors > 99 ? '99+' : errors}
            </span>
          )}
        </span>
      </BandButton>
      {menu && (
        <Menu label={`${title} actions`} at={menu} onDismiss={() => setMenu(null)}>
          {hasLog && (
            <MenuItem
              title={isLogOpen ? 'Hide log' : 'Show log'}
              badge={errors > 0 ? `${errors} ${errors === 1 ? 'error' : 'errors'}` : undefined}
              icon={<LogIcon className="size-3.5" />}
              onSelect={() => pick(onToggleLog)}
            />
          )}
          {onFullScreen && (
            <MenuItem
              title="Full screen"
              icon={<FullScreenIcon className="size-3.5" />}
              onSelect={() => pick(onFullScreen)}
            />
          )}
        </Menu>
      )}
    </>
  );
}

/**
 * The columns of one entry. Every row has the same slots, so the columns line up. A
 * touch row is 44px high, so the detail control of each row is a full touch target.
 */
const ROW =
  'flex min-h-11 w-full min-w-0 items-start gap-2 py-3 text-left font-mono text-ui normal-case tracking-normal mouse:min-h-6 mouse:py-1';

/** One entry: chevron slot, time, level, source and message; a detail opens under it in mono. */
function LogEntryRow({ entry }: { entry: LiveLogEntry }) {
  const [isOpen, setOpen] = useState(false);
  const detailId = useId();
  const columns = (
    <>
      <time className="shrink-0 tabular-nums text-dialog-hint" dateTime={new Date(entry.at).toISOString()}>
        {logClock(entry.at)}
      </time>
      <span className={`w-11 shrink-0 font-bold ${LEVEL_INK[entry.level]}`}>{entry.level}</span>
      <span className="w-9 shrink-0 text-dialog-hint">{entry.source}</span>
      <span className={`min-w-0 flex-1 whitespace-pre-wrap break-words ${entry.level === 'error' ? 'text-err-ink' : 'text-white'}`}>
        {entry.message}
      </span>
    </>
  );
  return (
    <li
      data-log-level={entry.level}
      className={`border-l-2 px-3 ${entry.level === 'error' ? 'border-err bg-err-surface' : entry.level === 'warn' ? 'border-warn' : 'border-transparent'}`}
    >
      {entry.detail ? (
        <>
          <button
            type="button"
            aria-expanded={isOpen}
            aria-controls={detailId}
            aria-label={`${isOpen ? 'Hide' : 'Show'} detail of ${entry.message}`}
            onClick={() => setOpen(!isOpen)}
            className={`${ROW} cursor-pointer select-none app-focus-ring`}
          >
            <span className="grid h-4 w-3 shrink-0 place-items-center text-dialog-hint">
              <ChevronIcon open={isOpen} className="size-3 opacity-70" />
            </span>
            {columns}
          </button>
          {isOpen && (
            <pre
              id={detailId}
              tabIndex={0}
              aria-label={`Detail of ${entry.message}`}
              className="mb-1 ml-5 max-h-64 overflow-auto whitespace-pre border border-dialog-edge bg-panel-2 p-2 font-mono text-ui text-code-result"
            >
              {entry.detail}
            </pre>
          )}
        </>
      ) : (
        <div className={ROW}>
          <span aria-hidden className="h-4 w-3 shrink-0" />
          {columns}
        </div>
      )}
    </li>
  );
}

/** The log panel of an app: filters, Copy all and the entries, oldest first. */
export function AppLogPanel({
  log,
  title,
  isPane = false,
}: {
  log: readonly LiveLogEntry[];
  title: string;
  /** The log is in a dialog that fills its place, so it takes at most half of it, not a fixed height. */
  isPane?: boolean;
}) {
  const [filter, setFilter] = useState<AppLogFilter>('all');
  const counts = logFilterCounts(log);
  // A filter whose entries left the log falls back to all of them.
  const active = counts[filter] > 0 ? filter : 'all';
  const shown = shownLog(log, active);
  return (
    <section
      aria-label={`Log of ${title}`}
      data-app-log
      className={`flex min-h-0 flex-col border-b border-dialog-edge bg-panel ${isPane ? 'max-h-[50%]' : ''}`}
    >
      <div className="flex flex-wrap items-center gap-1.5 px-3 py-2">
        <div role="group" aria-label="Filter log by level" className="flex flex-wrap gap-1.5">
          {APP_LOG_FILTERS.filter((one) => one === 'all' || counts[one] > 0).map((one) => (
            <Chip key={one} isOn={one === active} onClick={() => setFilter(one)}>
              {FILTER_WORDS[one]}
              <span className="font-normal tabular-nums">{counts[one]}</span>
            </Chip>
          ))}
        </div>
        <CopyChip value={logText(shown)} label="Copy all" className="ml-auto" />
      </div>
      <ol
        aria-label="Log entries"
        className={`m-0 list-none overflow-y-auto overscroll-contain border-t border-dialog-edge/60 p-0 py-1 ${isPane ? 'min-h-0 flex-1' : 'max-h-72'}`}
      >
        {shown.map((entry) => (
          <LogEntryRow key={entry.seq} entry={entry} />
        ))}
      </ol>
    </section>
  );
}
