import {
  startTransition,
  useEffect,
  useId,
  useMemo,
  useRef,
  useState,
  type HTMLAttributes,
} from 'react';
import { Button, Disclosure, Input, ListRow, LoadMore, PROSE, Spinner } from './ui';
import { InlineMarkdown } from './ChatContent';
import {
  ArrowOutIcon,
  CircleAlertIcon,
  CircleCheckIcon,
  CircleDashedIcon,
  CircleDotIcon,
  CircleXIcon,
  MARK_NUDGE,
} from './icons';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';
import { VIEW_CLOSE_EVENT, VIEW_PATCH_EVENT } from '../lib/view';
import {
  applyLiveViewEvent,
  LIVE_NOTE_CHARS,
  isLiveViewEvent,
  liveFraction,
  livePercent,
  orderedRows,
  type LiveLinkNode,
  type LiveLogNode,
  type LiveLogPage,
  type LiveNode,
  type LiveProgressNode,
  type LiveStatNode,
  type LiveStatusNode,
  type LiveStepsNode,
  type LiveRow,
  type LiveTableNode,
  type LiveTone,
  type LiveView as LiveViewModel,
} from '../lib/live-view';

/**
 * A run SHOWING its work, on the session it is running in.
 *
 * The other half of human input is a QUESTION: it blocks, it takes the screen,
 * it wants an answer. This one wants nothing. A scan sweeping a fleet, a build
 * draining a log, a table filling in — the operator watches it, or does not,
 * and the run finishes either way. So it is a panel in the session's own
 * column, never a dialog: a scrim over a screen for something nobody has to
 * answer is the app taking a hostage.
 *
 * It paints the same picture the terminal pane paints, node for node, because
 * both fold the same patches through the same rules (`lib/live-view`). What
 * differs is only what each surface is good at: the terminal scrolls one band
 * with a keyboard, the phone scrolls the page it already sits in.
 *
 * A view LEAVES when it ends: the record keeps every line, the model is handed
 * the closing picture as data, and the settled row returns to the transcript.
 * What one FORM did is painted nowhere here — that is `ActivityPanel`, which
 * belongs to the form and shares only a transport with this rail.
 */

/** One ink per tone. `idle` is the screen's ordinary ink: nothing is wrong with it. */
const TONE_INK: Record<LiveTone, string> = {
  idle: 'text-white',
  running: 'text-accent-ink',
  ok: 'text-ok',
  warn: 'text-warn',
  error: 'text-err',
};

/**
 * What a node with nothing in it SAYS. The engine's own sentences
 * (`view.materializer/empty-line`), so the phone, the terminal and the document
 * report an empty table with one wording rather than three.
 */
const EMPTY_LINE = {
  stat: 'nothing counted yet',
  steps: 'no steps yet',
  log: 'no output yet',
  table: 'no rows yet',
  link: 'no links',
} as const;

/** How many earlier lines one press of `Load earlier` reads out of the record. */
const LOG_PAGE = 200;

/**
 * The node's name, in the label voice the dialog's fields already use.
 *
 * CAPS CARRY THE NAME, and only the name. A live label often says what it is a
 * label OF — `Failure · vis-agent + vis-contract (PyPI packages)`, `Timeline ·
 * macos-latest` — and setting the whole line in caps shouts the very part that
 * has to be read: caps strip the ascenders and descenders a word is recognised
 * by, so a long tail stops being scannable and starts competing with the rows it
 * introduces. Everything up to the first `·` is the name; what follows is
 * ordinary type at the same size and colour.
 */
function NodeLabel({ children }: { children: string }) {
  const [name, ...rest] = children.split(' · ');
  return (
    <span className="block font-mono text-meta text-dialog-hint">
      <span className="uppercase tracking-[0.08em]">{name}</span>
      {rest.length > 0 && <span> · {rest.join(' · ')}</span>}
    </span>
  );
}

function Empty({ children }: { children: string }) {
  return <p className="font-mono text-meta italic text-dialog-hint">{children}</p>;
}

/**
 * The tone, as a SHAPE. A tone arrives as a word and used to leave as colour
 * alone — one dot, five inks — and colour alone is not a state: `ok` green
 * measures 2.9:1 on the light theme's panel, under the 3:1 a mark owes, and the
 * readers who cannot separate it from the amber beside it are a twentieth of
 * everyone. One ring with five interiors says WHICH state; the colour agrees.
 */
const TONE_MARK: Record<LiveTone, typeof CircleDotIcon> = {
  idle: CircleDashedIcon,
  running: CircleDotIcon,
  ok: CircleCheckIcon,
  warn: CircleAlertIcon,
  error: CircleXIcon,
};

function ToneMark({ tone, className = 'size-3' }: { tone: LiveTone; className?: string }) {
  const Drawn = TONE_MARK[tone];
  return <Drawn className={`${className} ${MARK_NUDGE} ${TONE_INK[tone]}`} />;
}

/**
 * The MARK says the state; the sentence beside it keeps the panel's ordinary ink.
 *
 * Colouring every line by its tone painted a green row for every job that merely
 * passed — five inks down one column, and the row actually doing something no
 * longer stood out. Only a FAILURE speaks in colour, because it is why the reader
 * opened the view.
 */
function rowInk(tone: LiveTone): string {
  return tone === 'error' ? TONE_INK.error : '';
}

/** A phase that has not run yet is not part of the picture, so it steps back. */
function stepInk(tone: LiveTone): string {
  return tone === 'idle' ? 'text-dialog-hint' : rowInk(tone);
}

/**
 * The verdict of a SEALED record. An open view needs no `Live` badge: it is already
 * changing on screen and exposed as a live region. Mark only the exception to that
 * default — the outcome once the picture cannot change again.
 */
function ViewState({ view, isSettled }: { view: LiveViewModel; isSettled: boolean }) {
  if (!isSettled) return null;
  const failed = view.nodes.some((node) => node.type === 'status' && node.tone === 'error');
  const [word, paint] = failed
    ? (['Failed', 'bg-err/15 text-err'] as const)
    : (['Done', 'bg-hover text-dialog-hint'] as const);
  return (
    <span
      className={`shrink-0 px-1.5 py-0.5 font-mono text-meta font-bold uppercase tracking-[0.08em] ${paint}`}
    >
      {word}
    </span>
  );
}

/**
 * The headline owns the whole column and its detail sits UNDER it. Sharing one
 * line cost the headline every character the detail claimed: on a phone, "1 of 2
 * jobs finished" beside a workflow and a job name broke to one word per line.
 */
function StatusRow({ node }: { node: LiveStatusNode }) {
  return (
    <div className="grid grid-cols-[auto_minmax(0,1fr)] items-baseline gap-x-2 gap-y-1 font-mono text-title">
      <ToneMark tone={node.tone} className="size-3.5" />
      <p className={`min-w-0 ${PROSE} ${TONE_INK[node.tone]}`}>
        <InlineMarkdown>{node.text}</InlineMarkdown>
      </p>
      {node.detail && (
        <p className="col-start-2 min-w-0 text-meta text-dialog-hint">
          <InlineMarkdown>{node.detail}</InlineMarkdown>
        </p>
      )}
    </div>
  );
}

const METER_SEGMENTS = 20;
const METER_CELLS = Array.from({ length: METER_SEGMENTS }, (_, cell) => cell);

/** The live view's segmented progress face, shared with its terminal rendering. */
function ProgressMeter({ value, label }: { value: number; label: string }) {
  const filled = Math.max(0, Math.min(METER_SEGMENTS, Math.round(value * METER_SEGMENTS)));
  return (
    <span
      role="progressbar"
      aria-label={label}
      aria-valuemin={0}
      aria-valuemax={100}
      aria-valuenow={Math.round(Math.max(0, Math.min(1, value)) * 100)}
      className="flex h-1.5 w-full gap-px mouse:h-1"
    >
      {METER_CELLS.map((cell) => (
        <span
          key={cell}
          className={`h-full flex-1 ${cell < filled ? 'bg-accent' : 'bg-dialog-edge'}`}
        />
      ))}
    </span>
  );
}

/** Selection belongs to the whole live-table row, not only its first cell. */
function SelectableTableRow({
  isSelected = false,
  className = '',
  ...props
}: HTMLAttributes<HTMLTableRowElement> & { isSelected?: boolean }) {
  return (
    <tr
      aria-selected={isSelected}
      className={`transition-colors duration-150 motion-reduce:transition-none ${
        isSelected ? 'bg-accent/10' : ''
      } ${className}`}
      {...props}
    />
  );
}

/**
 * A bar only when there is a fraction to draw. `done of total` is stated in
 * words beside it because a bar answers "how far", never "how many", and a scan
 * of 3 hosts and a scan of 3000 draw the same third of a bar.
 */
function ProgressRow({ node }: { node: LiveProgressNode }) {
  const fraction = liveFraction(node);
  const counted =
    node.done !== undefined && node.total !== undefined ? `${node.done}/${node.total}` : null;
  return (
    <div className="min-w-0">
      {fraction === null ? (
        <p className="font-mono text-ui text-dialog-hint">
          <span className="italic">working</span>
          {counted && <span className="ml-2 text-meta">{counted}</span>}
        </p>
      ) : (
        <>
          <ProgressMeter value={fraction} label={node.label ?? 'Progress'} />
          <p className="mt-1.5 flex items-baseline gap-2 font-mono text-meta text-dialog-hint">
            <span className="text-ui font-bold tabular-nums text-white">
              {livePercent(fraction)}%
            </span>
            {counted && <span className="tabular-nums">{counted}</span>}
          </p>
        </>
      )}
    </div>
  );
}

/** Counters that keep their slot as their numbers move. */
function StatRow({ node }: { node: LiveStatNode }) {
  if (node.stats.length === 0) return <Empty>{EMPTY_LINE.stat}</Empty>;
  return (
    <dl className="flex flex-wrap items-baseline gap-x-4 gap-y-1 font-mono">
      {node.stats.map((stat) => (
        <div key={stat.id} className="flex items-baseline gap-1.5">
          <dt className="text-meta text-dialog-hint">
            <InlineMarkdown>{stat.label}</InlineMarkdown>
          </dt>
          <dd className={`text-ui font-bold ${TONE_INK[stat.tone]}`}>{stat.value_text}</dd>
        </div>
      ))}
    </dl>
  );
}

function StepsRows({ node }: { node: LiveStepsNode }) {
  if (node.steps.length === 0) return <Empty>{EMPTY_LINE.steps}</Empty>;
  return (
    <ul className="space-y-1.5 font-mono text-ui">
      {node.steps.map((step) => (
        <li key={step.id} className="flex min-w-0 items-baseline gap-2">
          <ToneMark tone={step.tone} />
          <span className={`min-w-0 flex-1 truncate ${stepInk(step.tone)}`}>
            <InlineMarkdown>{step.label}</InlineMarkdown>
          </span>
          {step.value && (
            <span className="shrink-0 font-bold tabular-nums text-white">{step.value}</span>
          )}
          {step.detail && (
            <span className="shrink-0 text-meta text-dialog-hint">
              <InlineMarkdown>{step.detail}</InlineMarkdown>
            </span>
          )}
        </li>
      ))}
    </ul>
  );
}

function LogLines({
  lines,
  tones,
  numbers,
}: {
  lines: string[];
  tones?: (LiveTone | null)[];
  numbers?: number[];
}) {
  return lines.map((line, index) => {
    const tone = tones?.[index];
    return (
      <span
        key={index}
        className={tone ? TONE_INK[tone] : undefined}
        title={tone ? `Severity: ${tone}` : undefined}
      >
        {index > 0 && '\n'}
        {numbers ? `${numbers[index]}: ` : ''}
        {line}
      </span>
    );
  });
}

/**
 * Output as it arrives, and a way BACK past it.
 *
 * The node carries a window; the record behind it can be a hundred thousand
 * lines, which is precisely what a phone must not hold. So the walk back is a
 * page at a time out of the record itself, and when the window has slid on
 * while the operator was reading, the hole between what was fetched and what is
 * live is STATED rather than closed over.
 */
function LogRows({
  node,
  load,
}: {
  node: LiveLogNode;
  load?: (from: number, limit: number, query?: string) => Promise<LiveLogPage>;
}) {
  const [earlier, setEarlier] = useState<LiveLogPage | null>(null);
  const [isReading, setIsReading] = useState(false);
  const [readError, setReadError] = useState(false);
  const [draft, setDraft] = useState('');
  const [search, setSearch] = useState<{ query: string; from: number } | null>(null);
  const [result, setResult] = useState<{
    page: LiveLogPage;
    window: string[];
  } | null>(null);
  const [isSearching, setIsSearching] = useState(false);
  const [searchError, setSearchError] = useState(false);
  const searchRequest = useRef(0);
  const searchInput = useRef<HTMLInputElement>(null);
  useEffect(
    () => () => {
      searchRequest.current += 1;
    },
    [],
  );

  const windowStart = Math.max(0, node.total_lines - node.lines.length);
  const knownFrom = earlier ? earlier.from : windowStart;
  const hole = earlier ? Math.max(0, windowStart - (earlier.from + earlier.lines.length)) : 0;

  const clearSearch = () => {
    searchRequest.current += 1;
    setDraft('');
    setSearch(null);
    setResult(null);
    setIsSearching(false);
    setSearchError(false);
  };
  const find = async (query: string, from = 0) => {
    if (!query) {
      clearSearch();
      return;
    }
    const request = ++searchRequest.current;
    setSearch({ query, from });
    setResult(null);
    setSearchError(false);
    setIsSearching(true);
    try {
      const matches = load
        ? []
        : node.lines.flatMap((line, index) =>
            line.toLowerCase().includes(query.toLowerCase())
              ? [
                  {
                    line,
                    number: windowStart + index + 1,
                    tone: node.line_tones?.[index] ?? null,
                  },
                ]
              : [],
          );
      const slice = matches.slice(from, from + LOG_PAGE);
      const page = load
        ? await load(from, LOG_PAGE, query)
        : {
            node_id: node.id,
            from,
            total: node.total_lines,
            matched: matches.length,
            lines: slice.map((match) => match.line),
            line_numbers: slice.map((match) => match.number),
            line_tones: slice.map((match) => match.tone),
          };
      if (request === searchRequest.current) setResult({ page, window: node.lines });
    } catch {
      if (request === searchRequest.current) setSearchError(true);
    } finally {
      if (request === searchRequest.current) setIsSearching(false);
    }
  };

  const readEarlier = () => {
    if (!load || isReading) return;
    const from = Math.max(0, knownFrom - LOG_PAGE);
    const limit = knownFrom - from;
    if (limit <= 0) return;
    setIsReading(true);
    setReadError(false);
    load(from, limit)
      .then((page) => {
        // Keep one history page; older output stays in the retained record.
        setEarlier(page);
      })
      .catch(() => setReadError(true))
      .finally(() => setIsReading(false));
  };

  return (
    <div className="min-w-0">
      <form
        className="flex items-center gap-2"
        onSubmit={(event) => {
          event.preventDefault();
          void find(draft);
        }}
      >
        <Input
          ref={searchInput}
          type="search"
          className="min-w-0 flex-1"
          aria-label={`Search ${node.label || 'Output'}`}
          placeholder="Search log…"
          value={draft}
          onChange={(event) => setDraft(event.target.value)}
          onKeyDown={(event) => {
            if (event.key === 'Escape') {
              event.preventDefault();
              event.stopPropagation();
              clearSearch();
            }
          }}
        />
        <Button type="submit" variant="secondary" density="panel">
          Search
        </Button>
      </form>
      {search && (
        <div className="mt-2 space-y-2">
          <p role="status" className="break-words font-mono text-ui text-dialog-hint">
            {`“${search.query}” · `}
            {isSearching
              ? 'Searching…'
              : result
                ? `${result.page.matched} matches · ${result.page.total} recorded lines`
                : 'Search results'}
            {!load && ' · Loaded lines only'}
          </p>
          {result && result.window !== node.lines && (
            <p className="font-mono text-ui text-dialog-hint">Log changed. Refresh results.</p>
          )}
          {searchError && (
            <p role="alert" className="font-mono text-ui text-err">
              Could not read log. Try again.
            </p>
          )}
          <div className="flex flex-wrap gap-x-2 gap-y-5">
            <Button variant="secondary" onClick={() => void find(search.query)}>
              Refresh results
            </Button>
            <Button variant="secondary" onClick={clearSearch}>
              Clear search
            </Button>
            {(search.from > 0 || (result && result.page.matched > LOG_PAGE)) && (
              <>
                <Button
                  variant="secondary"
                  disabled={isSearching || search.from === 0}
                  onClick={() => void find(search.query, Math.max(0, search.from - LOG_PAGE))}
                >
                  Previous matches
                </Button>
                <Button
                  variant="secondary"
                  disabled={isSearching || !result || search.from + LOG_PAGE >= result.page.matched}
                  onClick={() => void find(search.query, search.from + LOG_PAGE)}
                >
                  Next matches
                </Button>
              </>
            )}
          </div>
          {result && result.page.lines.length === 0 && <Empty>No matching lines.</Empty>}
        </div>
      )}
      {!search && load && knownFrom > 0 && (
        <LoadMore
          label={`Load ${Math.min(LOG_PAGE, knownFrom)} earlier lines`}
          disabled={isReading}
          onClick={readEarlier}
        >
          {isReading ? 'Reading...' : `${knownFrom} earlier lines`}
        </LoadMore>
      )}
      {!search && readError && (
        <p role="alert" className="font-mono text-ui text-err">
          Could not read log. Try again.
        </p>
      )}
      {!search && node.lines.length === 0 && !earlier && <Empty>{EMPTY_LINE.log}</Empty>}
      <pre
        role="region"
        tabIndex={0}
        aria-label={`${node.label || 'Output'} output`}
        onKeyDown={(event) => {
          if (event.key === '/') {
            event.preventDefault();
            event.stopPropagation();
            searchInput.current?.focus();
          }
        }}
        className="mt-2 max-h-64 overflow-auto overscroll-contain whitespace-pre-wrap break-all border border-dialog-edge bg-panel-2 p-2 font-mono text-ui text-dialog-hint"
      >
        {search ? (
          result && (
            <LogLines
              lines={result.page.lines}
              tones={result.page.line_tones}
              numbers={result.page.line_numbers}
            />
          )
        ) : (
          <>
            {earlier && <LogLines lines={earlier.lines} tones={earlier.line_tones} />}
            {hole > 0 && `\n... ${hole} lines scrolled past while you were reading\n`}
            {earlier && hole === 0 && node.lines.length > 0 && '\n'}
            <LogLines lines={node.lines} tones={node.line_tones} />
          </>
        )}
      </pre>
    </div>
  );
}

/**
 * Rows keyed by id, in the order the view DECLARED.
 *
 * The phone does not offer its own sort: a live table is being written to while
 * it is read, and a column the operator sorted by would re-shuffle the rows
 * under the thumb on every patch. The order is the extension's statement, and
 * it is the same one the terminal paints.
 */
/**
 * A ROW IS A SENTENCE, and its columns are the words after the first.
 *
 * A live table is read DOWN its first column and glanced at across the rest, so
 * a phone stops scrolling sideways to see the rest: the row's own line carries
 * the name and, at the right edge, the VALUE — the last column, when that column
 * is right-aligned, because a number is what a row is measured by. Every column
 * between them stacks under the name as one detail line. From `sm` there is
 * width for the columns the run declared and they take their own cells back.
 *
 * No cell is fenced. A rule between rows is what the eye needs to keep a row
 * together, and forty boxed cells at 8px were a grid to decode before a run
 * could be read.
 */
function tableShape(node: LiveTableNode) {
  const last = node.columns.length - 1;
  const valueAt = last > 0 && node.columns[last]?.align === 'right' ? last : -1;
  const detailAt = node.columns
    .map((_, index) => index)
    .filter((index) => index > 0 && index !== valueAt);
  return { valueAt, detailAt };
}

/** The row as a phone reads it: the name, the value beside it, the rest beneath. */
function RowFace({
  node,
  row,
  isIndented,
}: {
  node: LiveTableNode;
  row: LiveRow;
  isIndented: boolean;
}) {
  const { valueAt, detailAt } = tableShape(node);
  const value = valueAt >= 0 ? (row.cells[valueAt] ?? '') : '';
  const detail = detailAt
    .map((index) => row.cells[index] ?? '')
    .filter((cell) => cell !== '')
    .join(' · ');
  const name =
    row.branch && row.cells[0]?.startsWith(`${row.branch} / `)
      ? row.cells[0].slice(row.branch.length + 3)
      : (row.cells[0] ?? '');
  return (
    <span className={`block min-w-0 ${isIndented ? 'pl-3' : ''}`}>
      <span className="flex min-w-0 items-baseline gap-2">
        <span className="min-w-0 flex-1 truncate">
          <InlineMarkdown>{name}</InlineMarkdown>
        </span>
        {value !== '' && (
          <span className="shrink-0 tabular-nums text-meta text-dialog-hint sm:hidden">
            {value}
          </span>
        )}
      </span>
      {detail !== '' && (
        <span className="mt-0.5 block truncate text-meta text-dialog-hint sm:hidden">
          <InlineMarkdown>{detail}</InlineMarkdown>
        </span>
      )}
    </span>
  );
}

function TableRows({
  node,
  onSelect,
}: {
  node: LiveTableNode;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
}) {
  const rows = orderedRows(node);
  const isSelectable = node.is_selectable && Boolean(onSelect);
  const { valueAt, detailAt } = tableShape(node);
  const selected = useMemo(() => new Set(node.selected_ids), [node.selected_ids]);
  const grouped = useMemo(() => {
    const counts = new Map<string, number>();
    for (const row of rows)
      if (row.branch) counts.set(row.branch, (counts.get(row.branch) ?? 0) + 1);
    return counts;
  }, [rows]);
  const selectedGroups = useMemo(
    () =>
      new Set(
        rows.filter((row) => selected.has(row.id) && row.branch).map((row) => row.branch as string),
      ),
    [rows, selected],
  );
  const selectedGroupKey = JSON.stringify([...selectedGroups].sort());
  const [openGroups, setOpenGroups] = useState<Set<string>>(() => selectedGroups);
  useEffect(() => {
    if (selectedGroups.size === 0) return;
    setOpenGroups((was) => {
      if ([...selectedGroups].every((group) => was.has(group))) return was;
      return new Set([...was, ...selectedGroups]);
    });
  }, [selectedGroupKey]);

  const visible: Array<{ kind: 'group'; label: string } | { kind: 'row'; row: LiveRow }> = [];
  const seen = new Set<string>();
  for (const row of rows) {
    const group = row.branch && (grouped.get(row.branch) ?? 0) > 1 ? row.branch : undefined;
    if (group && !seen.has(group)) {
      seen.add(group);
      visible.push({ kind: 'group', label: group });
    }
    if (!group || openGroups.has(group)) visible.push({ kind: 'row', row });
  }

  const span = Math.max(1, node.columns.length);
  return (
    <div className="-mx-3 overflow-x-auto">
      <table className="w-full min-w-0 border-collapse font-mono text-ui">
        <thead className="hidden sm:table-header-group">
          <tr>
            {node.columns.map((column) => (
              <th
                key={column.id}
                scope="col"
                className={`px-3 pb-1.5 font-normal uppercase tracking-[0.08em] text-meta text-dialog-hint ${
                  column.align === 'right' ? 'text-right' : 'text-left'
                }`}
              >
                <InlineMarkdown>{column.label}</InlineMarkdown>
              </th>
            ))}
          </tr>
        </thead>
        <tbody className="divide-y divide-dialog-edge">
          {rows.length === 0 && (
            <tr>
              <td className="px-3 py-2" colSpan={span}>
                <Empty>{EMPTY_LINE.table}</Empty>
              </td>
            </tr>
          )}
          {visible.map((item) => {
            if (item.kind === 'group') {
              const isOpen = openGroups.has(item.label);
              // The branch NAMES itself and then qualifies itself, the way every
              // label in this panel does: `Build native image · 3 variants`. The
              // name leads the row; the qualifier steps back to the right edge.
              const [name, ...rest] = item.label.split(' · ');
              return (
                <tr key={`group:${item.label}`}>
                  <td className="px-3 py-1" colSpan={span}>
                    <Disclosure
                      isOpen={isOpen}
                      tone="branch"
                      aria-label={name}
                      onClick={() =>
                        setOpenGroups((was) => {
                          const next = new Set(was);
                          if (isOpen) next.delete(item.label);
                          else next.add(item.label);
                          return next;
                        })
                      }
                    >
                      <span className="min-w-0 flex-1 truncate">{name}</span>
                      {rest.length > 0 && (
                        <span className="shrink-0 font-normal text-meta text-dialog-hint">
                          {rest.join(' · ')}
                        </span>
                      )}
                    </Disclosure>
                  </td>
                </tr>
              );
            }
            const row = item.row;
            const isSelected = selected.has(row.id);
            return (
              <SelectableTableRow
                key={row.id}
                isSelected={isSelected}
                className={`${rowInk(row.tone)} ${isSelectable ? 'cursor-pointer' : ''}`}
                onClick={isSelectable ? () => onSelect?.(node.id, [row.id]) : undefined}
              >
                <td className="p-0 align-top">
                  {isSelectable ? (
                    <ListRow
                      isSelected={isSelected}
                      aria-pressed={isSelected}
                      aria-label={`Select ${row.cells[0] || row.id}`}
                    >
                      <ToneMark tone={row.tone} />
                      <span className="min-w-0 flex-1 font-mono text-ui">
                        <RowFace node={node} row={row} isIndented={Boolean(row.branch)} />
                      </span>
                    </ListRow>
                  ) : (
                    <span className="flex min-w-0 items-start gap-2 px-3 py-2">
                      <ToneMark tone={row.tone} />
                      <RowFace node={node} row={row} isIndented={Boolean(row.branch)} />
                    </span>
                  )}
                </td>
                {detailAt.map((index) => (
                  <td
                    key={node.columns[index]?.id ?? index}
                    className="hidden px-3 py-2 align-top text-meta text-dialog-hint sm:table-cell"
                  >
                    <InlineMarkdown>{row.cells[index] ?? ''}</InlineMarkdown>
                  </td>
                ))}
                {valueAt >= 0 && (
                  <td className="hidden py-2 pr-3 pl-2 text-right align-top tabular-nums text-meta text-dialog-hint sm:table-cell">
                    <InlineMarkdown>{row.cells[valueAt] ?? ''}</InlineMarkdown>
                  </td>
                )}
              </SelectableTableRow>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

/**
 * Where the work also lives. A `url` is reachable from the phone and opens; a
 * path or an attachment names a place on the MACHINE, so it is stated as text
 * rather than dressed as a link that would do nothing under the thumb.
 */
function LinkRows({ node }: { node: LiveLinkNode }) {
  if (node.links.length === 0) return <Empty>{EMPTY_LINE.link}</Empty>;
  return (
    <ul className="space-y-1.5 font-mono text-ui">
      {node.links.map((link) => (
        <li key={link.id} className="flex min-w-0 items-baseline gap-2">
          <ArrowOutIcon className={`size-3 ${MARK_NUDGE} text-dialog-hint`} />
          {link.target_kind === 'url' ? (
            <a
              href={link.target}
              target="_blank"
              rel="noreferrer"
              className="min-w-0 flex-1 truncate text-accent-ink underline underline-offset-2"
            >
              <InlineMarkdown>{link.label}</InlineMarkdown>
            </a>
          ) : (
            <span className="min-w-0 flex-1 truncate text-white">
              <InlineMarkdown>{link.label}</InlineMarkdown>
            </span>
          )}
          {link.target_kind !== 'url' && (
            <span className="min-w-0 shrink truncate text-meta text-dialog-hint">
              {link.target}
            </span>
          )}
        </li>
      ))}
    </ul>
  );
}

/**
 * ONE node, painted where its view put it.
 *
 * A GROUP paints nothing of its own: it stands the nodes it holds side by side
 * (`row`) or one under the other (`column`) — the FORM's own layout vocabulary,
 * declared once and carried by no op, so an arrangement never rearranges itself
 * under a reader. It is the run's statement, and the terminal splits its band on
 * the same key. A phone has no width to split, so below `sm` a row stacks; the
 * reading order is the declared order either way.
 */
type NodePresentation = {
  isSettled: boolean;
  expanded: Record<string, boolean>;
  toggle: (id: string, value: boolean) => void;
  onActivate?: (id: string) => Promise<void> | void;
};

/** An operator button waits for acknowledgement and is inert in receipts. */
function ActionButton({
  node,
  onActivate,
  isSettled,
}: {
  node: Extract<LiveNode, { type: 'button' }>;
  onActivate?: NodePresentation['onActivate'];
  isSettled: boolean;
}) {
  const [pending, setPending] = useState(false);
  return (
    <Button
      variant="secondary"
      disabled={isSettled || node.is_disabled || pending || !onActivate}
      aria-busy={pending}
      onClick={async () => {
        if (!onActivate || pending) return;
        setPending(true);
        try {
          await onActivate(node.id);
        } finally {
          setPending(false);
        }
      }}
    >
      {node.label}
    </Button>
  );
}

function NodeCell({
  node,
  load,
  onSelect,
  presentation,
}: {
  node: LiveNode;
  load?: (nodeId: string, from: number, limit: number, query?: string) => Promise<LiveLogPage>;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
  presentation: NodePresentation;
}) {
  const contentId = useId();
  const isDisclosure = node.type === 'log' || (node.type === 'group' && node.is_collapsible);
  const isOpen = Object.hasOwn(presentation.expanded, node.id)
    ? presentation.expanded[node.id]
    : !presentation.isSettled &&
      (node.type === 'log' || node.type === 'group') &&
      node.default_expanded === true;
  const label = node.label || (node.type === 'log' ? 'Output' : node.id);
  const Heading =
    node.type === 'heading' ? (`h${node.level}` as 'h1' | 'h2' | 'h3' | 'h4' | 'h5' | 'h6') : 'h2';
  return (
    <div className="min-w-0 space-y-1.5">
      {isDisclosure ? (
        <Disclosure
          isOpen={isOpen}
          tone="branch"
          density="comfortable"
          aria-label={label}
          aria-controls={isOpen ? contentId : undefined}
          onClick={() => presentation.toggle(node.id, !isOpen)}
        >
          <NodeLabel>{label}</NodeLabel>
          {node.type === 'log' && (
            <span className="ml-2 text-ui text-dialog-hint">{node.total_lines} lines</span>
          )}
        </Disclosure>
      ) : (
        node.label && node.type !== 'button' && <NodeLabel>{node.label}</NodeLabel>
      )}
      {(!isDisclosure || isOpen) && (
        <div id={contentId} className="min-w-0">
          {node.type === 'group' && (
            <div
              className={
                node.direction === 'row'
                  ? 'grid min-w-0 gap-x-4 gap-y-3 sm:auto-cols-fr sm:grid-flow-col'
                  : 'min-w-0 space-y-3'
              }
            >
              {node.fields.map((child) => (
                <NodeCell
                  key={child.id}
                  node={child}
                  load={load}
                  onSelect={onSelect}
                  presentation={presentation}
                />
              ))}
            </div>
          )}
          {node.type === 'paragraph' && (
            <p className={`font-mono text-body text-white ${PROSE}`}>
              <InlineMarkdown>{node.text}</InlineMarkdown>
            </p>
          )}
          {node.type === 'heading' && (
            <Heading
              className={`font-mono font-bold text-white ${node.level === 1 ? 'text-head' : node.level === 2 ? 'text-subhead' : 'text-title'}`}
            >
              <InlineMarkdown>{node.text}</InlineMarkdown>
            </Heading>
          )}
          {node.type === 'code' && (
            <div className="min-w-0 bg-panel-2 p-2">
              {node.language && (
                <span className="font-mono text-ui text-dialog-hint">{node.language}</span>
              )}
              <pre
                tabIndex={0}
                aria-label={node.label || 'Code'}
                className="max-h-64 overflow-auto whitespace-pre font-mono text-body text-white"
              >
                <code>{node.text}</code>
              </pre>
            </div>
          )}
          {node.type === 'spinner' && (
            <p className="flex items-center gap-2 font-mono text-body text-white">
              {node.is_active && !presentation.isSettled && (
                <Spinner variant={node.variant} tone="accent" />
              )}
              <span>{node.text}</span>
            </p>
          )}
          {node.type === 'button' && (
            <ActionButton
              node={node}
              isSettled={presentation.isSettled}
              onActivate={presentation.onActivate}
            />
          )}
          {node.type === 'status' && <StatusRow node={node} />}
          {node.type === 'progress' && <ProgressRow node={node} />}
          {node.type === 'stat' && <StatRow node={node} />}
          {node.type === 'steps' && <StepsRows node={node} />}
          {node.type === 'log' && (
            <LogRows
              node={node}
              load={
                load &&
                ((from, limit, query) =>
                  query === undefined
                    ? load(node.id, from, limit)
                    : load(node.id, from, limit, query))
              }
            />
          )}
          {node.type === 'table' && <TableRows node={node} onSelect={onSelect} />}
          {node.type === 'link' && <LinkRows node={node} />}
        </div>
      )}
    </div>
  );
}

/**
 * ONE view, painted. Pure: everything it knows arrived as a prop, which is what
 * lets the whole picture be rendered from the engine's own fixture in a test.
 */
export function LiveViewPanel({
  view,
  onInterrupt,
  onSelect,
  onActivate,
  isInterrupting = false,
  error,
  load,
  isSettled = false,
}: {
  view: LiveViewModel;
  /** Stop the view, carrying the comment the human left — `null` when they left none. */
  onInterrupt?: (note: string | null) => void;
  /** Live selection, or local snapshot browsing in a receipt; omit when unavailable. */
  onSelect?: (nodeId: string, itemIds: string[]) => void;
  onActivate?: (nodeId: string) => Promise<void> | void;
  isInterrupting?: boolean;
  error?: string | null;
  load?: (nodeId: string, from: number, limit: number, query?: string) => Promise<LiveLogPage>;
  /**
   * The run is OVER and this is its record. Nothing spins, and the section stops
   * being a live region: a picture that cannot change again must not announce
   * itself to a screen reader as one that can.
   */
  isSettled?: boolean;
}) {
  // The stop is ARMED before it is sent, exactly as Escape arms it in the
  // terminal: the comment travels WITH the interrupt, so the run reads WHY it
  // was stopped and not merely that it was. `null` is "not armed" — an empty
  // string is an armed stop nobody has typed into yet.
  const phase = `${view.id}:${isSettled}`;
  const [disclosures, setDisclosures] = useState<{
    phase: string;
    values: Record<string, boolean>;
  }>({ phase, values: {} });
  const presentation: NodePresentation = {
    isSettled,
    onActivate,
    expanded: disclosures.phase === phase ? disclosures.values : {},
    toggle: (id, value) =>
      setDisclosures((held) => ({
        phase,
        values: { ...(held.phase === phase ? held.values : {}), [id]: value },
      })),
  };
  const [note, setNote] = useState<string | null>(null);
  const isArmed = note !== null;
  const typed = note ?? '';
  // One armed stop, however it is sent: the comment travels trimmed, and an
  // empty line is no comment at all rather than an empty one.
  const sendStop = (send: (note: string | null) => void) => {
    setNote(null);
    send(typed.trim() === '' ? null : typed.trim());
  };
  return (
    <section
      className="overflow-hidden border border-dialog-edge bg-panel"
      role={isSettled ? undefined : 'status'}
      aria-live={isSettled ? undefined : 'polite'}
    >
      <header className="flex items-start gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
        <span className="min-w-0 flex-1">
          <span className="block truncate font-mono text-title font-bold text-white">
            {view.title}
          </span>
          {view.description && (
            <span className="block truncate font-mono text-meta text-dialog-hint">
              <InlineMarkdown>{view.description}</InlineMarkdown>
            </span>
          )}
        </span>
        <ViewState view={view} isSettled={isSettled} />
        {!isSettled && onInterrupt && !isArmed && (
          <Button
            variant="secondary"
            className="shrink-0 self-center"
            onClick={() => setNote('')}
            disabled={isInterrupting}
          >
            {isInterrupting ? 'Stopping...' : 'Interrupt'}
          </Button>
        )}
      </header>
      {!isSettled && isArmed && onInterrupt && (
        <form
          className="flex flex-wrap items-center gap-x-2 gap-y-5 border-b border-dialog-edge bg-panel-2 px-3 py-2"
          onSubmit={(event) => {
            event.preventDefault();
            sendStop(onInterrupt);
          }}
          onKeyDown={(event) => {
            // Escape STOPS. It is the key that ARMED the interrupt, so it is the
            // key that sends it, note and all — the terminal answers the same
            // key the same way. `Keep watching` is the way back here, because a
            // phone has no Backspace to fall out of an empty line with.
            if (event.key !== 'Escape') return;
            event.preventDefault();
            event.stopPropagation();
            sendStop(onInterrupt);
          }}
        >
          <Input
            autoFocus
            className="min-w-40 flex-1"
            value={typed}
            maxLength={LIVE_NOTE_CHARS}
            onChange={(event) => setNote(event.target.value)}
            placeholder="why stop it? (optional)"
            aria-label={`Why are you stopping ${view.title}?`}
          />
          <Button type="submit" variant="primary" disabled={isInterrupting}>
            Interrupt
          </Button>
          <Button type="button" variant="secondary" onClick={() => setNote(null)}>
            Keep watching
          </Button>
        </form>
      )}
      {error && (
        <p className="border-b border-dialog-edge px-3 py-2 font-mono text-chip text-err">
          {error}
        </p>
      )}
      <ul className="divide-y divide-dialog-edge">
        {view.nodes.map((node) => (
          // Table cells own their padding; an outer inset makes the first and last
          // rows uneven relative to the internal separators. Keep labelled headings inset.
          <li
            key={node.id}
            className={`min-w-0 px-3 ${node.type === 'table' ? (node.label ? 'pt-2.5' : '') : 'py-2.5'}`}
          >
            <NodeCell node={node} load={load} onSelect={onSelect} presentation={presentation} />
          </li>
        ))}
      </ul>
    </section>
  );
}

/**
 * Every live view this session is showing, kept current.
 *
 * The snapshot is read on mount and on every reconnect, and the three events
 * fold into it as they arrive — the same two doors the parked-form dialog uses,
 * for the same reason: a phone woken by a push never saw the frames that opened
 * the view it is about to paint.
 *
 * A HOOK rather than the panel's own state, because an open view is not only a
 * panel: the running row above the transcript stops saying "Vis is thinking"
 * and names what is on screen instead, and both must read one list.
 */
export function useLiveViews(
  client: GatewayClient,
  subscriptions: SessionSubscriptionHub,
  sid: string,
  onRecordFiled?: () => void,
): LiveViewModel[] {
  const [views, setViews] = useState<LiveViewModel[]>([]);
  const revision = useRef(0);

  useEffect(() => {
    revision.current += 1;
    let cancelled = false;
    const controller = new AbortController();
    const reload = () => {
      const requestedAt = revision.current;
      client
        .liveViews(sid, controller.signal)
        .then((open) => {
          if (!cancelled && requestedAt === revision.current) setViews(open);
        })
        .catch(() => undefined);
    };
    reload();
    const stopConnection = subscriptions.subscribeConnection((connected) => {
      if (connected) reload();
    });
    const recordRefreshTimers: ReturnType<typeof setTimeout>[] = [];
    let patchTimer: ReturnType<typeof setTimeout> | null = null;
    let pendingPatches: SseEvent[] = [];
    const flushPatches = () => {
      if (patchTimer !== null) clearTimeout(patchTimer);
      patchTimer = null;
      if (pendingPatches.length === 0) return;
      const batch = pendingPatches;
      pendingPatches = [];
      // Activity is informative, not input-critical. Let elapsed clocks, touch and
      // streamed prose interrupt this render instead of waiting behind a large table.
      startTransition(() => {
        setViews((current) => batch.reduce(applyLiveViewEvent, current));
      });
    };
    const revealRecord = () => {
      onRecordFiled?.();
      // A close on the gateway thread files into the running block's collector
      // immediately, but that collector reaches the persisted iteration only when
      // the block returns. The close frame can therefore beat one transcript read.
      // Re-read across that short handoff so the settled picture gives way to
      // the durable transcript record without a manual refresh.
      for (const delay of [250, 1_000, 3_000, 8_000]) {
        recordRefreshTimers.push(setTimeout(() => onRecordFiled?.(), delay));
      }
    };
    const stopEvents = subscriptions.subscribeSession(sid, (event) => {
      if (!isLiveViewEvent(event)) return;
      revision.current += 1;
      if (event.type === VIEW_PATCH_EVENT) {
        pendingPatches.push(event);
        // Activity can emit much faster than WKWebView can paint. Fold a burst in
        // memory and give React one picture, leaving the clock and touch handling
        // enough main-thread time to move independently.
        patchTimer ??= setTimeout(flushPatches, 80);
        return;
      }
      flushPatches();
      setViews((current) => applyLiveViewEvent(current, event));
      if (event.type === VIEW_CLOSE_EVENT) revealRecord();
    });
    return () => {
      cancelled = true;
      revision.current += 1;
      controller.abort();
      stopConnection();
      stopEvents();
      if (patchTimer !== null) clearTimeout(patchTimer);
      pendingPatches = [];
      for (const timer of recordRefreshTimers) clearTimeout(timer);
    };
  }, [client, sid, subscriptions, onRecordFiled]);

  // A patch frame that skipped a seq means frames were LOST — the coalescing
  // window states the range it stands for precisely so this is knowable. The
  // picture is behind, so it is RE-READ rather than patched further: a table
  // quietly missing a row is the failure this whole numbering exists to catch.
  const isStale = views.some((view) => view.is_stale === true);
  useEffect(() => {
    if (!isStale) return;
    let cancelled = false;
    const controller = new AbortController();
    const requestedAt = revision.current;
    client
      .liveViews(sid, controller.signal)
      .then((open) => {
        if (!cancelled && requestedAt === revision.current) setViews(open);
      })
      .catch(() => undefined);
    return () => {
      cancelled = true;
      controller.abort();
    };
  }, [isStale, client, sid]);

  return views;
}

/** Every open view of this session, painted where the transcript ends. */
export function LiveView({
  views,
  client,
  sid,
}: {
  views: LiveViewModel[];
  client: GatewayClient;
  sid: string;
}) {
  const [stopping, setStopping] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);

  if (views.length === 0) return null;

  // The note the human typed rides WITH the stop: one call, so the run never
  // resumes on an interrupt whose reason is still in flight behind it.
  const interrupt = (viewId: string, note: string | null) => {
    setStopping(viewId);
    setError(null);
    client
      .viewAction(sid, viewId, {
        action: 'interrupt',
        ...(note ? { note } : {}),
      })
      .catch(() => setError('That view would not stop. It may have just finished.'))
      .finally(() => setStopping(null));
  };

  const select = (viewId: string, nodeId: string, itemIds: string[]) => {
    setError(null);
    client
      .viewAction(sid, viewId, {
        action: 'select',
        node_id: nodeId,
        item_ids: itemIds,
      })
      .catch(() => setError('That job could not be selected. It may have just finished.'));
  };

  const activate = async (viewId: string, nodeId: string) => {
    setError(null);
    try {
      const outcome = await client.viewAction(sid, viewId, {
        action: 'activate',
        node_id: nodeId,
      });
      if (!outcome.is_accepted)
        setError('That action is unavailable. The button may be disabled or the run finished.');
    } catch {
      setError('That action could not be confirmed. Check the run before trying again.');
    }
  };

  const readLog =
    (viewId: string) => (nodeId: string, from: number, limit: number, query?: string) =>
      client.liveViewLog(sid, viewId, nodeId, from, limit, query);

  return (
    <div className="space-y-3">
      {views.map((view) => (
        <LiveViewPanel
          key={view.id}
          view={view}
          error={stopping === null ? error : null}
          isInterrupting={stopping === view.id}
          onInterrupt={(note) => interrupt(view.id, note)}
          onSelect={(nodeId, itemIds) => select(view.id, nodeId, itemIds)}
          onActivate={(nodeId) => activate(view.id, nodeId)}
          load={readLog(view.id)}
        />
      ))}
    </div>
  );
}
