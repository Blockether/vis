import {
  startTransition,
  useEffect,
  useId,
  useMemo,
  useRef,
  useState,
  type HTMLAttributes,
  type ReactNode,
} from 'react';
import {
  BandLabel,
  Button,
  DialogFrame,
  Disclosure,
  ExecutionAction,
  Input,
  ListRow,
  LoadMore,
  Modal,
  PROSE,
  Spinner,
  TextButton,
  ViewHeading,
  ViewLayout,
  ViewParagraph,
} from './ui';
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
  type LiveGroup,
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
 * and the run finishes either way. In the transcript it is ONE ROW — what is
 * running and how far it has come — and selecting RUN opens the picture in a
 * transient screen; closing that screen leaves both the row and the running work
 * alone.
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
 * WEIGHT CARRIES THE NAME, not caps. A live label often says what it is a label
 * OF — `Failure · vis-agent + vis-contract (PyPI packages)`, `Timeline ·
 * macos-latest` — and the name itself is whatever the extension called the thing:
 * `Last observed pool state`, `glms-tests/glms-test-data #6043`. Caps strip the
 * ascenders and descenders a word is recognised by, so a shouted build identifier
 * stops being scannable and starts competing with the rows it introduces. The name
 * keeps the case its author wrote it in, exactly as the terminal paints it, and
 * stands out by ink and weight instead. Everything up to the first `·` is the name;
 * what follows is ordinary type at the same size.
 */
function NodeLabel({ children }: { children: string }) {
  const [name, ...rest] = children.split(' · ');
  return (
    <span className="block font-mono text-meta text-dialog-hint">
      <span className="font-bold text-white">{name}</span>
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
 * Rules FENCE the blocks of a run. A status line, a meter and a table are separate
 * readings, and in the embedded card they stack close enough that spacing alone
 * lets them run together; a hairline between two nodes keeps each one its own row.
 * A divider is a rule the view ASKED for, so neither side of one is ruled twice.
 */
function rowRule(previous: LiveNode | undefined, node: LiveNode): string {
  if (!previous || previous.type === 'divider' || node.type === 'divider') return '';
  return 'border-t border-dialog-edge';
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
  const name = row.cells[0] ?? '';
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
  /**
   * A parent stays SHUT until the reader opens it, unless the producer DECLARED it
   * open: only they know which group is the interesting one. Opening the parents
   * that held the live selection sounded helpful and was not: `watch` selects every
   * running job, so a matrix in flight stood open on the first paint and sprang back
   * open on the next poll, seconds after the reader had closed it. This set holds the
   * folds the reader TOUCHED, so their own hand always wins over the declaration.
   */
  const [toggledGroups, setToggledGroups] = useState<Set<string>>(() => new Set());

  type LiveTableItem =
    | { kind: 'group'; id: string; label: string; tone?: LiveTone; count: number; isOpen: boolean }
    | { kind: 'row'; row: LiveRow };

  // A parent OWNS its legs wherever the producer listed them. GitHub returns a matrix
  // interleaved with the rest of the run, so walking the rows in order put the head
  // above the first leg it met and left the other four stranded as parentless rows
  // further down the table — under whichever head happened to come next.
  const legs = new Map<string, LiveRow[]>();
  for (const row of rows) {
    if (!row.parent) continue;
    const kept = legs.get(row.parent);
    if (kept) kept.push(row);
    else legs.set(row.parent, [row]);
  }

  // The heads paint in the order the table DECLARED them — by `order`, then by the
  // order they were declared — and a group nobody declared comes after them, keeping
  // the place its first row gave it. Order is declared rather than discovered.
  const declared = new Map(node.groups.map((group) => [group.id, group]));
  const ordered: LiveGroup[] = [
    ...node.groups
      .map((group, index) => ({ group, index }))
      .sort((a, b) => (a.group.order ?? 0) - (b.group.order ?? 0) || a.index - b.index)
      .map(({ group }) => group),
    ...[...legs.keys()].filter((id) => !declared.has(id)).map((id) => ({ id })),
  ];

  const heads: LiveTableItem[] = [];
  for (const group of ordered) {
    const held = legs.get(group.id) ?? [];
    if (held.length === 0) continue;
    const isOpen = toggledGroups.has(group.id) ? group.is_open !== true : group.is_open === true;
    heads.push({
      kind: 'group',
      id: group.id,
      label: group.label || group.id,
      tone: group.tone,
      count: held.length,
      isOpen,
    });
    if (isOpen) for (const leg of held) heads.push({ kind: 'row', row: leg });
  }

  // The heads stand TOGETHER, where the first grouped row was listed; a row that names
  // no parent is simply itself, in the place the producer gave it.
  const visible: LiveTableItem[] = [];
  let isSpliced = false;
  for (const row of rows) {
    if (!row.parent) {
      visible.push({ kind: 'row', row });
      continue;
    }
    if (isSpliced) continue;
    isSpliced = true;
    visible.push(...heads);
  }

  const span = Math.max(1, node.columns.length);
  return (
    <div className="-mx-(--live-view-inset) overflow-x-auto">
      <table className="w-full min-w-0 border-collapse font-mono text-ui">
        <thead className="hidden sm:table-header-group">
          <tr className="border-b border-dialog-edge">
            {node.columns.map((column) => (
              <th
                key={column.id}
                scope="col"
                className={`px-(--live-view-inset) py-2 font-bold text-meta text-dialog-hint ${
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
              <td className="px-(--live-view-inset) py-2" colSpan={span}>
                <Empty>{EMPTY_LINE.table}</Empty>
              </td>
            </tr>
          )}
          {visible.map((item) => {
            if (item.kind === 'group') {
              // The head NAMES itself and then says how much its fold holds. The count is
              // COUNTED from the rows right here: a producer DECLARES a group through the
              // live interface and every surface renders that declaration the same way,
              // instead of a label smuggled through the field that IDENTIFIES the group.
              // The mark is how a head says a leg failed without being opened at all.
              const held = `${item.count} row${item.count === 1 ? '' : 's'}`;
              return (
                <tr key={`group:${item.id}`}>
                  <td className="px-(--live-view-inset) py-1" colSpan={span}>
                    <Disclosure
                      isOpen={item.isOpen}
                      tone="branch"
                      aria-label={item.label}
                      onClick={() =>
                        setToggledGroups((was) => {
                          const next = new Set(was);
                          if (next.has(item.id)) next.delete(item.id);
                          else next.add(item.id);
                          return next;
                        })
                      }
                    >
                      {item.tone ? <ToneMark tone={item.tone} /> : null}
                      <span className={`min-w-0 flex-1 truncate ${rowInk(item.tone ?? 'idle')}`}>
                        {item.label}
                      </span>
                      <span className="shrink-0 font-normal text-meta text-dialog-hint">
                        {held}
                      </span>
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
                className={`${rowInk(row.tone)} ${isSelectable ? 'cursor-pointer focus-within:bg-hover' : ''}`}
                onClick={isSelectable ? () => onSelect?.(node.id, [row.id]) : undefined}
              >
                <td className="p-0 align-middle">
                  {/* A ROW IS A BAND, NOT A TOUCH CELL. The regular row stands 48px tall so a
                      thumb can land anywhere on it; on a desk that turned twenty variants into a
                      column of boxes taller than the run's own header. The compact band is 36px
                      under a thumb — its invisible slop still answers at 44px — and 32px under a
                      pointer, the height the head and the parent rows beside it already keep. */}
                  {isSelectable ? (
                    <ListRow
                      inset="live-view"
                      density="compact"
                      aria-pressed={isSelected}
                      aria-label={`Select ${row.cells[0] || row.id}`}
                    >
                      <ToneMark tone={row.tone} />
                      <span className="min-w-0 flex-1 font-mono text-ui">
                        <RowFace node={node} row={row} isIndented={Boolean(row.parent)} />
                      </span>
                    </ListRow>
                  ) : (
                    <span className="flex min-w-0 items-start gap-2 px-(--live-view-inset) py-1.5">
                      <ToneMark tone={row.tone} />
                      <RowFace node={node} row={row} isIndented={Boolean(row.parent)} />
                    </span>
                  )}
                </td>
                {detailAt.map((index) => (
                  <td
                    key={node.columns[index]?.id ?? index}
                    className="hidden px-(--live-view-inset) py-2 align-middle text-meta text-dialog-hint sm:table-cell"
                  >
                    <InlineMarkdown>{row.cells[index] ?? ''}</InlineMarkdown>
                  </td>
                ))}
                {valueAt >= 0 && (
                  <td className="hidden py-2 pr-(--live-view-inset) pl-2 text-right align-middle tabular-nums text-meta text-dialog-hint sm:table-cell">
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
 * A result set shares one frame and fills columns in source order when they fit.
 */
function LinkRows({ node }: { node: LiveLinkNode }) {
  if (node.links.length === 0) return <Empty>{EMPTY_LINE.link}</Empty>;
  const isGrid = node.links.length > 1;
  return (
    <ul
      className={`font-mono text-ui ${isGrid ? 'grid grid-cols-[repeat(auto-fit,minmax(min(100%,16rem),1fr))] gap-2 border border-dialog-hint p-2' : 'space-y-1.5'}`}
    >
      {node.links.map((link) => (
        <li
          key={link.id}
          className={`flex min-w-0 gap-2 ${isGrid ? 'min-h-11 items-center mouse:min-h-7' : 'items-baseline'}`}
        >
          <ArrowOutIcon className={`size-3 ${MARK_NUDGE} text-dialog-hint`} />
          {link.target_kind === 'url' ? (
            <a
              href={link.target}
              target="_blank"
              rel="noreferrer"
              className={`min-w-0 flex-1 text-accent-ink underline underline-offset-2 ${isGrid ? 'flex min-h-11 items-center wrap-anywhere mouse:min-h-7' : 'truncate'}`}
            >
              <span className="min-w-0">
                <InlineMarkdown>{link.label}</InlineMarkdown>
              </span>
            </a>
          ) : (
            <span className={`min-w-0 flex-1 text-white ${isGrid ? 'wrap-anywhere' : 'truncate'}`}>
              <InlineMarkdown>{link.label}</InlineMarkdown>
            </span>
          )}
          {link.target_kind !== 'url' && (
            <span
              className={`min-w-0 shrink text-meta text-dialog-hint ${isGrid ? 'basis-1/2 wrap-anywhere' : 'truncate'}`}
            >
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
        <div id={contentId} className={`min-w-0 ${isDisclosure ? 'pl-4.5' : ''}`}>
          {node.type === 'group' && (
            <ViewLayout direction={node.direction}>
              {node.fields.map((child) => (
                <NodeCell
                  key={child.id}
                  node={child}
                  load={load}
                  onSelect={onSelect}
                  presentation={presentation}
                />
              ))}
            </ViewLayout>
          )}
          {node.type === 'divider' && (
            <hr className="m-0 w-full border-0 border-t border-dialog-hint" />
          )}
          {node.type === 'paragraph' && (
            <ViewParagraph>
              <InlineMarkdown>{node.text}</InlineMarkdown>
            </ViewParagraph>
          )}
          {node.type === 'heading' && (
            <ViewHeading level={node.level}>
              <InlineMarkdown>{node.text}</InlineMarkdown>
            </ViewHeading>
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
 * The one line a run is worth while its picture is folded away: the newest status
 * it carries, else how far it has come. The terminal's collapsed row reads the
 * same way (`live-view/status-summary`), so a run says the same thing on both
 * surfaces.
 */
function runSummary(nodes: LiveNode[]): string {
  const status = findNode(
    nodes,
    (node): node is LiveStatusNode => node.type === 'status' && node.text.trim() !== '',
  );
  if (status) return status.text;
  const progress = findNode(nodes, (node): node is LiveProgressNode => node.type === 'progress');
  const fraction = progress ? liveFraction(progress) : null;
  return fraction === null ? '' : `${livePercent(fraction)}%`;
}

/** Depth first, because a status inside a group still says what the run is doing. */
function findNode<T extends LiveNode>(
  nodes: LiveNode[],
  pick: (node: LiveNode) => node is T,
): T | undefined {
  for (const node of nodes) {
    if (pick(node)) return node;
    if (node.type === 'group') {
      const inner = findNode(node.fields, pick);
      if (inner) return inner;
    }
  }
  return undefined;
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
  embedded = false,
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
  /** Frame RUN below ACTIVITY without enclosing the other execution content. */
  embedded?: boolean;
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
  const [opened, setOpened] = useState(false);
  // What the row SAYS while the picture is folded away — see `runSummary`.
  const summary = embedded ? runSummary(view.nodes) : '';
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
    <>
      {/* Embedded, a run is a band like the TUI draws it: no frame of its own, the
          execution group's column, and the same row rhythm as the CODE and ACTIVITY
          bands beside it. Only the standalone dialog keeps a frame. */}
      <section
        className={`live-view-panel min-w-0 overflow-hidden ${
          embedded ? '' : 'border border-dialog-edge bg-panel'
        }`}
        data-execution-run={embedded || undefined}
        role={isSettled ? undefined : 'status'}
        aria-live={isSettled ? undefined : 'polite'}
      >
        <header
          className={
            embedded
              ? 'flex min-h-11 min-w-0 items-center gap-2 mouse:min-h-7'
              : 'flex items-start gap-2 border-b border-dialog-edge bg-panel-2 px-(--live-view-inset) py-2.5'
          }
        >
          {embedded ? (
            <ExecutionAction
              className="min-w-0 flex-1"
              aria-label={`Open run ${view.title}`}
              onClick={() => setOpened(true)}
            >
              {/* The band's own word never gives way: a squeezed row printed "R…" and spent the
                  width on the name beside it. Furniture keeps its width; the PROSE truncates. */}
              <BandLabel className="shrink-0">RUN</BandLabel>
              <span className="min-w-0 shrink truncate">{view.title}</span>
              {summary && (
                <span className="min-w-0 flex-1 truncate text-dialog-hint">{summary}</span>
              )}
            </ExecutionAction>
          ) : (
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
          )}
          {!isSettled &&
            onInterrupt &&
            !isArmed &&
            (embedded ? (
              <TextButton
                isBand
                className="shrink-0 self-center"
                onClick={() => setNote('')}
                disabled={isInterrupting}
              >
                {isInterrupting ? 'Stopping...' : 'Interrupt'}
              </TextButton>
            ) : (
              <Button
                variant="secondary"
                className="shrink-0 self-center"
                onClick={() => setNote('')}
                disabled={isInterrupting}
              >
                {isInterrupting ? 'Stopping...' : 'Interrupt'}
              </Button>
            ))}
          {/* The band ends INTERRUPT | LIVE: the verb, the rule a terminal prints between two
              words, and the run's state last. The touch target keeps its 44px reach in an
              invisible pseudo-element, so the row stays a line of text under the finger too. */}
          {embedded && !isSettled && (
            <>
              {onInterrupt && !isArmed && (
                <span
                  aria-hidden="true"
                  className="shrink-0 select-none font-mono text-ui text-dialog-hint"
                >
                  |
                </span>
              )}
              <BandLabel weight="state" className="shrink-0">
                LIVE
              </BandLabel>
            </>
          )}
          <ViewState view={view} isSettled={isSettled} />
        </header>
        {!isSettled && isArmed && onInterrupt && (
          <form
            className={`flex flex-wrap items-center gap-x-2 gap-y-5 py-2 ${
              embedded ? '' : 'border-b border-dialog-edge bg-panel-2 px-(--live-view-inset)'
            }`}
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
          <p
            className={`py-2 font-mono text-chip text-err ${
              embedded ? '' : 'border-b border-dialog-edge px-(--live-view-inset)'
            }`}
          >
            {error}
          </p>
        )}
        {/* THE TRANSCRIPT STATES A RUN; IT DOES NOT PAINT IT. A build observation
            painted in place stood taller than the turn that opened it and pushed the
            answer off the screen — the same rule a settled run obeys one row above
            (`LiveRunRow`). The picture is one press away, in the run's own screen. */}
        {!embedded && (
          <ul>
            {view.nodes.map((node, index) => (
              // Table cells own their padding; an outer inset makes the first and last
              // rows uneven relative to the internal separators. Keep labelled headings inset.
              <li
                key={node.id}
                className={`min-w-0 ${rowRule(view.nodes[index - 1], node)} ${node.type === 'divider' ? '' : 'px-(--live-view-inset)'} ${node.type === 'table' ? (node.label ? 'pt-2.5' : '') : 'py-2.5'}`}
              >
                <NodeCell node={node} load={load} onSelect={onSelect} presentation={presentation} />
              </li>
            ))}
          </ul>
        )}
      </section>
      {embedded && opened && (
        <RunDialog title={view.title} onClose={() => setOpened(false)}>
          <div className="min-h-0 flex-1 overflow-y-auto py-3">
            <LiveViewPanel
              view={view}
              onInterrupt={onInterrupt}
              onSelect={onSelect}
              onActivate={onActivate}
              error={error}
              isInterrupting={isInterrupting}
              load={load}
              isSettled={isSettled}
            />
          </div>
        </RunDialog>
      )}
    </>
  );
}

/**
 * AN OPENED RUN IS A DIALOG, NOT THE WHOLE APPLICATION.
 *
 * A run used to open in the artifact overlay — the viewport-pinned layer a document
 * opens in — which is right for a document and wrong for a run: on a desktop one
 * click papered the session list, the transcript and the composer with a single
 * view. It opens in the app's ONE dialog instead (`Modal` + `DialogFrame`), and that
 * dialog belongs to the session it came from (`within="session"`): the whole glass on
 * a phone, the whole chat pane on a desk, with the list beside it neither dimmed nor
 * covered. It FILLS that pane: held to the desktop question box it opened as a small
 * window in the middle of the session it reports on.
 *
 * The run keeps its own border in here, so the box the transcript shows is the box
 * the dialog shows.
 */
export function RunDialog({
  title,
  subtitle,
  onClose,
  children,
}: {
  title: string;
  /** What the band REPORTS under the run's name — a settled run's verdict. */
  subtitle?: ReactNode;
  onClose: () => void;
  children: ReactNode;
}) {
  // Escape is the way out a keyboard has. An ARMED interrupt inside the run stops the
  // key before it reaches here, so Escape sends that stop rather than closing the run.
  useEffect(() => {
    function onKey(event: KeyboardEvent) {
      if (event.key === 'Escape') onClose();
    }
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);
  return (
    <Modal within="session" onDismiss={onClose}>
      <DialogFrame title={title} subtitle={subtitle} onClose={onClose}>
        {/* The run is a BOX in here, so the dialog's edge is never mistaken for the run's. */}
        <div className="flex min-h-0 min-w-0 flex-1 flex-col px-3">{children}</div>
      </DialogFrame>
    </Modal>
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

/** Open views, either within their owning Activity or in an unmatched fallback. */
export function LiveView({
  views,
  client,
  sid,
  embedded = false,
}: {
  views: LiveViewModel[];
  client: GatewayClient;
  sid: string;
  embedded?: boolean;
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

  const panels = views.map((view) => (
    <LiveViewPanel
      key={view.id}
      view={view}
      embedded={embedded}
      error={stopping === null ? error : null}
      isInterrupting={stopping === view.id}
      onInterrupt={(note) => interrupt(view.id, note)}
      onSelect={(nodeId, itemIds) => select(view.id, nodeId, itemIds)}
      onActivate={(nodeId) => activate(view.id, nodeId)}
      load={readLog(view.id)}
    />
  ));
  return embedded ? panels : <div className="min-w-0 space-y-3">{panels}</div>;
}
