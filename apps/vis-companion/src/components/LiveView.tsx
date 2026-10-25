import {
  Fragment,
  memo,
  startTransition,
  useCallback,
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
  ChoiceRow,
  CopyChip,
  DialogFrame,
  Disclosure,
  ExecutionAction,
  Input,
  ListRow,
  Modal,
  PROSE,
  Spinner,
  TextButton,
  ViewHeading,
  ViewLayout,
  ViewParagraph,
} from './ui';
import { InlineMarkdown } from './ChatContent';
import { HumanInputFieldRow } from './HumanInputPrompt';
import { humanInputDefaultValue, type HumanInputValue } from '../lib/human-input';
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
import { VIEW_CLOSE_EVENT, VIEW_OPEN_EVENT, VIEW_PATCH_EVENT, type ViewAction } from '../lib/view';
import {
  applyLiveViewEvent,
  LIVE_NOTE_CHARS,
  isLiveInputNode,
  isLiveViewEvent,
  liveInputNodes,
  liveFraction,
  livePercent,
  orderedRows,
  type LiveCardNode,
  type LiveInputNode,
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
 * The ink of a tone MARK. Words beside a mark keep the body ink, because text never
 * wears a data colour. A mark that says "nothing happened yet" steps back to the hint.
 */
const MARK_INK: Record<LiveTone, string> = {
  idle: 'text-dialog-hint',
  running: 'text-accent-ink',
  ok: 'text-ok',
  warn: 'text-warn',
  error: 'text-err',
};

/** The tone as a WORD, so a state reads without colour and without the icon (#417). */
const TONE_WORD: Record<LiveTone, string> = {
  idle: 'Waiting',
  running: 'Running',
  ok: 'Passed',
  warn: 'Warning',
  error: 'Failed',
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

/** How many matching lines one page of search results holds. */
const LOG_PAGE = 200;

/**
 * How many earlier lines one read takes out of the record. The gateway caps a page
 * at the default log window (`gateway.view/live-log-page`), so this is the largest
 * read that comes back whole.
 */
const LOG_READ_PAGE = 2000;

/** A value at least this long is an identifier rather than a figure, so it gets a copy action. */
const LONG_VALUE = 24;

/**
 * An identifier breaks AFTER its separators, so `platform/services/deploy` wraps
 * at a slash rather than in the middle of a word. Any break still wins over a row
 * that scrolls sideways.
 */
function Breakable({ children }: { children: string }) {
  const parts = children.split(/(?<=[/._-])/);
  return parts.map((part, index) => (
    <Fragment key={index}>
      {part}
      {index < parts.length - 1 && <wbr />}
    </Fragment>
  ));
}

/** A single stat this short is a figure, so it stands as the view's large number. */
const HERO_VALUE = 12;

/**
 * A section's name, in the case its author wrote it.
 *
 * WEIGHT CARRIES THE NAME, not caps: a shouted build identifier stops being
 * scannable. Everything up to the first `·` is the name; the rest follows as a
 * plain phrase joined by commas, because a run of dot-joined meta reads as noise.
 *
 * THE SIZE LADDER. A name is always larger than the caption under it, and a value
 * is never smaller than the label over it. A section label is `text-ui`, and what it
 * names is `text-body` or larger. A fact label is `text-meta` over a `text-body`
 * value. A step or a row is `text-body` over a `text-meta` caption. A link is
 * `text-ui` over a `text-meta` place: links are where the work also lives, not
 * the work itself, so they read one step quieter than the rows above them. The headline is
 * `text-subhead` over a `text-body` detail.
 */
function NodeLabel({ children }: { children: string }) {
  const [name, ...rest] = children.split(' · ');
  return (
    <span className="flex min-w-0 flex-wrap items-baseline gap-x-2 font-mono text-ui">
      <span className="font-bold text-white [overflow-wrap:anywhere]">{name}</span>
      {rest.length > 0 && (
        <span className="text-dialog-hint [overflow-wrap:anywhere]">{rest.join(', ')}</span>
      )}
    </span>
  );
}

function Empty({ children }: { children: string }) {
  return <p className="font-mono text-ui italic text-dialog-hint">{children}</p>;
}

/**
 * The tone, as a SHAPE. One ring with five interiors says WHICH state, and the
 * colour agrees: colour alone is not a state for the readers who cannot separate
 * green from the amber beside it.
 */
const TONE_MARK: Record<LiveTone, typeof CircleDotIcon> = {
  idle: CircleDashedIcon,
  running: CircleDotIcon,
  ok: CircleCheckIcon,
  warn: CircleAlertIcon,
  error: CircleXIcon,
};

export function ToneMark({ tone, className = 'size-3' }: { tone: LiveTone; className?: string }) {
  const Drawn = TONE_MARK[tone];
  return <Drawn className={`${className} shrink-0 ${MARK_NUDGE} ${MARK_INK[tone]}`} />;
}

/**
 * A mark ON ITS LINE. The box is one line tall in the type of the words beside it,
 * and the mark is centred in it. The mark then stands on the first line of those
 * words, however they wrap and whatever size the mark is. A padding guessed for one
 * size put the marks of a step rail below the labels they belong to.
 */
function LineMark({ tone, className = 'size-3' }: { tone: LiveTone; className?: string }) {
  const Drawn = TONE_MARK[tone];
  return (
    <span className="flex h-[1lh] shrink-0 items-center">
      <Drawn className={`${className} ${MARK_INK[tone]}`} />
    </span>
  );
}

/** A state as a mark and a word, at caption size. Only a failure colours its word. */
function ToneWord({ tone, word = TONE_WORD[tone] }: { tone: LiveTone; word?: string }) {
  return (
    <span className="inline-flex items-start gap-1.5 font-mono text-meta text-dialog-hint">
      <LineMark tone={tone} />
      <span className={tone === 'error' ? 'text-err' : ''}>{word}</span>
    </span>
  );
}

/**
 * The MARK says the state; the sentence beside it keeps the panel's ordinary ink.
 * Only a FAILURE speaks in colour, because it is why the reader opened the view.
 */
function rowInk(tone: LiveTone): string {
  return tone === 'error' ? TONE_INK.error : '';
}

/**
 * The verdict of a SEALED record, as a word and a mark in sentence case. An open view
 * needs no `Live` badge: it is already changing on screen and is a live region.
 */
function ViewState({ view, isSettled }: { view: LiveViewModel; isSettled: boolean }) {
  if (!isSettled) return null;
  const failed = view.nodes.some((node) => node.type === 'status' && node.tone === 'error');
  return (
    // The box is one line of the title beside it, so the word stands on the title's line.
    <span className="flex h-[1lh] shrink-0 items-center font-mono text-title">
      <ToneWord tone={failed ? 'error' : 'ok'} word={failed ? 'Failed' : 'Done'} />
    </span>
  );
}

/**
 * The HEADLINE: the run's one sentence at reading size. Its state stands over it as
 * a word, and its detail sits under it one size down, so the sentence owns the
 * whole column on a phone.
 */
function StatusRow({ node }: { node: LiveStatusNode }) {
  return (
    <div className="min-w-0 space-y-1 font-mono">
      <ToneWord tone={node.tone} />
      <p
        className={`min-w-0 text-subhead font-bold ${PROSE} [overflow-wrap:anywhere] ${
          node.tone === 'error' ? 'text-err' : 'text-white'
        }`}
      >
        <InlineMarkdown>{node.text}</InlineMarkdown>
      </p>
      {node.detail && (
        <p className="min-w-0 text-body text-dialog-hint [overflow-wrap:anywhere]">
          <InlineMarkdown>{node.detail}</InlineMarkdown>
        </p>
      )}
    </div>
  );
}

/**
 * How far, as ONE track. A bar answers "how far", so it draws a fraction and no
 * count. It is drawn, not animated: only a declared spinner moves in a live view,
 * so the fill steps to each new value and needs no reduced-motion variant.
 */
function ProgressMeter({ value, label }: { value: number; label: string }) {
  const clamped = Math.max(0, Math.min(1, value));
  return (
    <span
      role="progressbar"
      aria-label={label}
      aria-valuemin={0}
      aria-valuemax={100}
      aria-valuenow={Math.round(clamped * 100)}
      className="block h-1.5 w-full mouse:h-1"
    >
      <svg
        aria-hidden="true"
        viewBox="0 0 1000 1"
        preserveAspectRatio="none"
        className="block size-full"
      >
        <rect width="1000" height="1" className="fill-dialog-edge" />
        <rect width={Math.round(clamped * 1000)} height="1" className="fill-accent" />
      </svg>
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
 * The percent LEADS, the count says how many, the track says how far. `done of
 * total` is in words because a scan of 3 hosts and a scan of 3000 draw the same
 * third of a bar.
 */
function ProgressRow({ node }: { node: LiveProgressNode }) {
  const fraction = liveFraction(node);
  const counted =
    node.done !== undefined && node.total !== undefined ? `${node.done} of ${node.total}` : null;
  if (fraction === null) {
    return (
      <p className="font-mono text-body text-dialog-hint">
        <span className="italic">Working</span>
        {counted && <span className="tabular-nums">, {counted}</span>}
      </p>
    );
  }
  return (
    <div className="min-w-0 space-y-2 font-mono">
      <p className="flex flex-wrap items-baseline gap-x-3">
        <span className="text-head font-bold tabular-nums text-white">
          {livePercent(fraction)}%
        </span>
        {counted && <span className="text-body tabular-nums text-dialog-hint">{counted}</span>}
      </p>
      <ProgressMeter value={fraction} label={node.label ?? 'Progress'} />
    </div>
  );
}

/**
 * Counters as FACTS: each label over its value, in a grid that wraps to the width.
 * A long identifier wraps instead of scrolling sideways and keeps a copy action on
 * its first line. One short figure alone is the view's large number.
 */
function StatRow({ node }: { node: LiveStatNode }) {
  if (node.stats.length === 0) return <Empty>{EMPTY_LINE.stat}</Empty>;
  const [only] = node.stats;
  if (node.stats.length === 1 && only.value_text.length <= HERO_VALUE) {
    return (
      <div className="min-w-0 font-mono">
        <p className="text-display font-bold tabular-nums text-white">{only.value_text}</p>
        <p className="flex min-w-0 items-start gap-1.5 text-body text-dialog-hint">
          {only.tone !== 'idle' && <LineMark tone={only.tone} />}
          <span className="min-w-0 [overflow-wrap:anywhere]">
            <InlineMarkdown>{only.label}</InlineMarkdown>
          </span>
          {only.tone !== 'idle' && <span className="sr-only">{TONE_WORD[only.tone]}</span>}
        </p>
      </div>
    );
  }
  return (
    <dl className="grid grid-cols-[repeat(auto-fill,minmax(min(100%,9rem),1fr))] gap-x-5 gap-y-3 font-mono">
      {node.stats.map((stat) => (
        <div key={stat.id} className="min-w-0">
          <dt className="text-meta text-dialog-hint [overflow-wrap:anywhere]">
            <InlineMarkdown>{stat.label}</InlineMarkdown>
          </dt>
          <dd className="flex min-w-0 items-start gap-1.5 text-body font-bold tabular-nums text-white">
            {stat.tone !== 'idle' && <LineMark tone={stat.tone} />}
            <span className="min-w-0 [overflow-wrap:anywhere]">
              <Breakable>{stat.value_text}</Breakable>
            </span>
            {stat.tone !== 'idle' && <span className="sr-only">{TONE_WORD[stat.tone]}</span>}
            {stat.value_text.length > LONG_VALUE && (
              <span className="flex h-[1lh] shrink-0 items-center">
                <CopyChip value={stat.value_text} label={`Copy ${stat.label}`} />
              </span>
            )}
          </dd>
        </div>
      ))}
    </dl>
  );
}

/**
 * Steps on a RAIL. Each mark stands on the first line of its label (`LineMark`).
 * The rail runs from under one mark to the line box of the next, with the same
 * margin at both ends, so every gap on it is equal whether a step has a detail or
 * not. The step in flight, or the one that failed, is the loud one; a step not
 * reached yet steps back.
 */
function StepsRows({ node }: { node: LiveStepsNode }) {
  if (node.steps.length === 0) return <Empty>{EMPTY_LINE.steps}</Empty>;
  return (
    <ol className="min-w-0 font-mono text-body">
      {node.steps.map((step, index) => {
        const isLast = index === node.steps.length - 1;
        const isCurrent = step.tone === 'running' || step.tone === 'error';
        const ink =
          step.tone === 'error'
            ? 'text-err'
            : step.tone === 'idle'
              ? 'text-dialog-hint'
              : 'text-white';
        return (
          <li key={step.id} className="grid grid-cols-[auto_minmax(0,1fr)_auto] gap-x-2.5">
            <span className="flex flex-col items-center">
              <LineMark tone={step.tone} className="size-3.5" />
              {!isLast && (
                <span aria-hidden="true" className="my-0.5 w-px flex-1 bg-dialog-edge" />
              )}
            </span>
            <span className={`min-w-0 ${isLast ? '' : 'pb-3'}`}>
              <span
                className={`block [overflow-wrap:anywhere] ${ink} ${isCurrent ? 'font-bold' : ''}`}
              >
                <InlineMarkdown>{step.label}</InlineMarkdown>
                <span className="sr-only">, {TONE_WORD[step.tone]}</span>
              </span>
              {step.detail && (
                <span className="mt-0.5 block text-meta text-dialog-hint [overflow-wrap:anywhere]">
                  <InlineMarkdown>{step.detail}</InlineMarkdown>
                </span>
              )}
            </span>
            <span className="tabular-nums text-dialog-hint">{step.value}</span>
          </li>
        );
      })}
    </ol>
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
 * Output as it arrives — ALL of it.
 *
 * The node carries a WINDOW onto a record that can be a hundred thousand lines, and
 * a live view shows the LOG, never a tail of it: the pane reads the record back to
 * its first line on its own, a page at a time, and keeps every line it is sent
 * afterwards. Nothing here asks the operator to press for output the view already
 * holds — search is for jumping straight to a match in a long run.
 */
function LogRows({
  node,
  load,
}: {
  node: LiveLogNode;
  load?: (from: number, limit: number, query?: string) => Promise<LiveLogPage>;
}) {
  const [head, setHead] = useState<{
    from: number;
    lines: string[];
    tones: (LiveTone | null)[];
  } | null>(null);
  const [isReading, setIsReading] = useState(false);
  const [readError, setReadError] = useState(false);
  const [readAttempt, setReadAttempt] = useState(0);
  const loadRef = useRef(load);
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
  const output = useRef<HTMLPreElement>(null);
  const [isFollowing, setIsFollowing] = useState(true);
  useEffect(
    () => () => {
      searchRequest.current += 1;
    },
    [],
  );

  const windowStart = Math.max(0, node.total_lines - node.lines.length);
  const hasLoad = Boolean(load);

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

  // The panel builds its reader inline, so the effect below reads the LATEST one
  // instead of re-running on an identity that changes with every paint.
  useEffect(() => {
    loadRef.current = load;
  });

  /**
   * What the pane was never sent, read out of the record and painted as each page
   * lands. `windowStart` moves only when a fresh snapshot or a `clear` re-bases the
   * node — exactly when the earlier lines have to be read again.
   */
  useEffect(() => {
    setHead(null);
    setReadError(false);
    if (!hasLoad || windowStart <= 0) return;
    let isWanted = true;
    setIsReading(true);
    void (async () => {
      const reader = loadRef.current;
      if (!reader) {
        setIsReading(false);
        return;
      }
      let from = windowStart;
      let earlier: string[] = [];
      let tones: (LiveTone | null)[] = [];
      while (isWanted && from > 0) {
        const start = Math.max(0, from - LOG_READ_PAGE);
        try {
          const page = await reader(start, from - start);
          if (!isWanted) return;
          earlier = page.lines.concat(earlier);
          tones = (page.line_tones ?? page.lines.map(() => null)).concat(tones);
          from = start;
          setHead({ from, lines: earlier, tones });
          if (page.lines.length === 0) break;
        } catch {
          if (isWanted) {
            setReadError(true);
            setIsReading(false);
          }
          return;
        }
      }
      if (isWanted) setIsReading(false);
    })();
    return () => {
      isWanted = false;
    };
  }, [hasLoad, node.id, windowStart, readAttempt]);

  // FOLLOW keeps the newest line in sight as lines arrive; a paused pane stays where
  // the reader left it. Search results are a fixed page, so they never follow.
  const lineCount = node.lines.length + (head?.lines.length ?? 0);
  useEffect(() => {
    const pane = output.current;
    if (!isFollowing || search || !pane) return;
    pane.scrollTop = pane.scrollHeight;
  }, [isFollowing, search, lineCount]);

  return (
    <div className="min-w-0 space-y-2">
      <form
        className="flex min-w-0 flex-wrap items-center gap-2"
        onSubmit={(event) => {
          event.preventDefault();
          void find(draft);
        }}
      >
        <Input
          ref={searchInput}
          type="search"
          className="min-w-40 flex-1"
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
        {/* The actions wrap as ONE group: a copy icon alone on a line read as a stray mark. */}
        <span className="flex shrink-0 items-center gap-2">
        <Button type="submit" variant="secondary" density="panel">
          Search
        </Button>
        {!search && (
          <Button
            variant="secondary"
            density="panel"
            onClick={() => setIsFollowing((follow) => !follow)}
          >
            {isFollowing ? 'Pause' : 'Follow'}
          </Button>
        )}
        <CopyChip
          label="Copy output"
          value={async () =>
            (search
              ? (result?.page.lines ?? [])
              : (head?.lines ?? []).concat(node.lines)
            ).join('\n')
          }
        />
        </span>
      </form>
      {search && (
        <div className="space-y-2">
          <p role="status" className="break-words font-mono text-ui text-dialog-hint">
            {`“${search.query}”: `}
            {isSearching
              ? 'Searching…'
              : result
                ? `${result.page.matched} matches in ${result.page.total} recorded lines`
                : 'Search results'}
            {!load && '. Loaded lines only'}
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
      {!search && isReading && (
        <p role="status" className="font-mono text-ui text-dialog-hint">
          Reading earlier lines…
        </p>
      )}
      {!search && readError && (
        <div className="flex flex-wrap items-baseline gap-x-2 gap-y-5">
          <p role="alert" className="font-mono text-ui text-err">
            Could not read the earlier lines.
          </p>
          <Button variant="secondary" onClick={() => setReadAttempt((count) => count + 1)}>
            Try again
          </Button>
        </div>
      )}
      {!search && node.lines.length === 0 && !head && <Empty>{EMPTY_LINE.log}</Empty>}
      <pre
        ref={output}
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
        className="max-h-64 overflow-auto overscroll-contain whitespace-pre-wrap break-all bg-panel-2 p-2 font-mono text-ui text-dialog-hint"
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
            {head && <LogLines lines={head.lines} tones={head.tones} />}
            {head && head.lines.length > 0 && node.lines.length > 0 && '\n'}
            <LogLines lines={node.lines} tones={node.line_tones} />
          </>
        )}
      </pre>
    </div>
  );
}

/**
 * A folded log still SAYS its failures: its last error lines stand under its name,
 * so folding the output never hides why a run failed (#417).
 */
function LogErrors({ node, label }: { node: LiveLogNode; label: string }) {
  const first = Math.max(0, node.total_lines - node.lines.length) + 1;
  const errors = node.lines
    .flatMap((line, index) =>
      node.line_tones?.[index] === 'error' ? [{ line, number: first + index }] : [],
    )
    .slice(-3);
  if (errors.length === 0) return null;
  return (
    <pre
      aria-label={`Latest errors in ${label}`}
      className="whitespace-pre-wrap break-all bg-panel-2 p-2 font-mono text-ui"
    >
      <LogLines
        lines={errors.map((error) => error.line)}
        tones={errors.map(() => 'error' as const)}
        numbers={errors.map((error) => error.number)}
      />
    </pre>
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
function tableShape(columns: LiveTableNode['columns']) {
  const last = columns.length - 1;
  const valueAt = last > 0 && columns[last]?.align === 'right' ? last : -1;
  const detailAt = columns
    .map((_, index) => index)
    .filter((index) => index > 0 && index !== valueAt);
  return { valueAt, detailAt };
}

/** The row as a phone reads it: the name, the value beside it, the rest beneath. */
function RowFace({ columns, row }: { columns: LiveTableNode['columns']; row: LiveRow }) {
  const { valueAt, detailAt } = tableShape(columns);
  const value = valueAt >= 0 ? (row.cells[valueAt] ?? '') : '';
  const detail = detailAt
    .map((index) => row.cells[index] ?? '')
    .filter((cell) => cell !== '')
    .join(', ');
  const name = row.cells[0] ?? '';
  return (
    <span className="block min-w-0 flex-1">
      <span className="flex min-w-0 items-baseline gap-2">
        <span className="min-w-0 flex-1 [overflow-wrap:anywhere]">
          <InlineMarkdown>{name}</InlineMarkdown>
          <span className="sr-only sm:hidden">, {TONE_WORD[row.tone]}</span>
        </span>
        {value !== '' && (
          <span className="shrink-0 tabular-nums text-meta text-dialog-hint sm:hidden">
            {value}
          </span>
        )}
      </span>
      {detail !== '' && (
        <span className="mt-0.5 block text-meta text-dialog-hint [overflow-wrap:anywhere] sm:hidden">
          <InlineMarkdown>{detail}</InlineMarkdown>
        </span>
      )}
    </span>
  );
}

/**
 * One table row, memoized by its own props. A patch that appends one row or
 * changes one state then paints only that row, not the whole table again.
 */
const LiveTableRow = memo(function LiveTableRow({
  nodeId,
  columns,
  row,
  isSelected,
  isSelectable,
  hasState,
  onSelect,
}: {
  nodeId: string;
  columns: LiveTableNode['columns'];
  row: LiveRow;
  isSelected: boolean;
  isSelectable: boolean;
  hasState: boolean;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
}) {
  const { valueAt, detailAt } = tableShape(columns);
  // A LEG moves one whole step in, mark and words together, so its mark stands
  // under its head's name. Indenting only the words left the mark on the outer
  // column, with a wider gap after it than any other row has.
  const mark = row.parent ? 'ml-5' : '';
  return (
    <SelectableTableRow
      isSelected={isSelected}
      className={`${rowInk(row.tone)} ${isSelectable ? 'cursor-pointer focus-within:bg-hover' : ''}`}
      onClick={isSelectable ? () => onSelect?.(nodeId, [row.id]) : undefined}
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
            <span className={`flex min-w-0 flex-1 items-start gap-2 font-mono text-body ${mark}`}>
              <LineMark tone={row.tone} />
              <RowFace columns={columns} row={row} />
            </span>
          </ListRow>
        ) : (
          <span className="flex min-w-0 items-start gap-2 py-1.5 sm:pr-(--live-view-inset)">
            <span className={`flex min-w-0 flex-1 items-start gap-2 ${mark}`}>
              <LineMark tone={row.tone} />
              <RowFace columns={columns} row={row} />
            </span>
          </span>
        )}
      </td>
      {detailAt.map((index) => (
        <td
          key={columns[index]?.id ?? index}
          className="hidden px-(--live-view-inset) py-2 align-baseline text-meta text-dialog-hint [overflow-wrap:anywhere] sm:table-cell"
        >
          <InlineMarkdown>{row.cells[index] ?? ''}</InlineMarkdown>
        </td>
      ))}
      {valueAt >= 0 && (
        <td className="hidden py-2 pr-(--live-view-inset) pl-2 text-right align-baseline tabular-nums text-ui text-white sm:table-cell">
          <InlineMarkdown>{row.cells[valueAt] ?? ''}</InlineMarkdown>
        </td>
      )}
      {hasState && (
        <td className="hidden py-2 pr-(--live-view-inset) align-baseline whitespace-nowrap sm:table-cell">
          <ToneWord tone={row.tone} />
        </td>
      )}
    </SelectableTableRow>
  );
});

type LiveTableItem =
  | { kind: 'group'; id: string; label: string; tone?: LiveTone; count: number; isOpen: boolean }
  | { kind: 'row'; row: LiveRow };

/**
 * The rows one memoized chunk paints. A patch on a 5000-row table then renders
 * the chunk that holds the change, and each other chunk costs one shallow check.
 */
const TABLE_CHUNK = 64;
/** The chunks the first paint holds; the rest of a long table follows in steps. */
const TABLE_FIRST_CHUNKS = 4;
/** The chunks each later step adds, so no single commit lays out thousands of rows. */
const TABLE_STEP_CHUNKS = 8;

function chunked<T>(items: T[], size: number): T[][] {
  const out: T[][] = [];
  for (let at = 0; at < items.length; at += size) out.push(items.slice(at, at + size));
  return out;
}

function sameItem(a: LiveTableItem, b: LiveTableItem): boolean {
  if (a.kind === 'row' || b.kind === 'row')
    return a.kind === 'row' && b.kind === 'row' && a.row === b.row;
  return (
    a.id === b.id &&
    a.label === b.label &&
    a.tone === b.tone &&
    a.count === b.count &&
    a.isOpen === b.isOpen
  );
}

type LiveTableChunkProps = {
  items: LiveTableItem[];
  nodeId: string;
  columns: LiveTableNode['columns'];
  selected: Set<string>;
  isSelectable: boolean;
  hasState: boolean;
  span: number;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
  onToggleGroup: (id: string) => void;
};

function sameChunk(was: LiveTableChunkProps, now: LiveTableChunkProps): boolean {
  return (
    was.nodeId === now.nodeId &&
    was.columns === now.columns &&
    was.selected === now.selected &&
    was.isSelectable === now.isSelectable &&
    was.hasState === now.hasState &&
    was.span === now.span &&
    was.onSelect === now.onSelect &&
    was.onToggleGroup === now.onToggleGroup &&
    was.items.length === now.items.length &&
    was.items.every((item, index) => sameItem(item, now.items[index]!))
  );
}

const LiveTableChunk = memo(function LiveTableChunk({
  items,
  nodeId,
  columns,
  selected,
  isSelectable,
  hasState,
  span,
  onSelect,
  onToggleGroup,
}: LiveTableChunkProps) {
  return (
    <>
      {items.map((item) => {
        if (item.kind === 'group') {
        // The head NAMES itself and then says how much its fold holds. The count is
        // COUNTED from the rows right here: a producer DECLARES a group through the
        // live interface and every surface renders that declaration the same way,
        // instead of a label smuggled through the field that IDENTIFIES the group.
        // The mark is how a head says a leg failed without being opened at all. It
        // FOLLOWS the name: in front of it, the mark moved the name off the column
        // of the rows, and heads with and without one started in two places.
        const held = `${item.count} row${item.count === 1 ? '' : 's'}`;
        return (
          <tr key={`group:${item.id}`}>
            <td className="px-(--live-view-inset) py-1" colSpan={span}>
              <Disclosure
                isOpen={item.isOpen}
                tone="branch"
                alignsWithMarks
                aria-label={item.label}
                onClick={() => onToggleGroup(item.id)}
              >
                <span className={`min-w-0 truncate ${rowInk(item.tone ?? 'idle')}`}>
                  {item.label}
                </span>
                {item.tone ? <ToneMark tone={item.tone} /> : null}
                {/* The count keeps a step of its own between it and the name, so a
                    head too long for the band trims the NAME and still reads as two
                    things instead of one run-on line. */}
                <span className="ml-auto shrink-0 pl-2 font-normal text-meta text-dialog-hint">
                  {held}
                </span>
              </Disclosure>
            </td>
          </tr>
        );
        }
        const row = item.row;
        return (
          <LiveTableRow
            key={row.id}
            nodeId={nodeId}
            columns={columns}
            row={row}
            isSelected={selected.has(row.id)}
            isSelectable={isSelectable}
            hasState={hasState}
            onSelect={onSelect}
          />
        );
      })}
    </>
  );
}, sameChunk);

function TableRows({
  node,
  onSelect,
}: {
  node: LiveTableNode;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
}) {
  const rows = orderedRows(node);
  const isSelectable = node.is_selectable && Boolean(onSelect);
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
  // A long table paints its first chunks at once and the rest in transition steps,
  // which yield to input, so 5000 rows never hold one frame of the view.
  const [shownChunks, setShownChunks] = useState(TABLE_FIRST_CHUNKS);
  const toggleGroup = useCallback(
    (id: string) =>
      setToggledGroups((was) => {
        const next = new Set(was);
        if (next.has(id)) next.delete(id);
        else next.add(id);
        return next;
      }),
    [],
  );

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

  // A STATE column only when a row has a state to report: a table of plain facts
  // would otherwise grow a column of "Waiting" down its whole length.
  // A producer that already declared its own state column keeps it alone.
  const hasState =
    rows.some((row) => row.tone !== 'idle') &&
    !node.columns.some((column) => /^(state|status)$/i.test(column.label.trim()));
  const span = Math.max(1, node.columns.length + (hasState ? 1 : 0));
  const chunks = chunked(visible, TABLE_CHUNK);
  useEffect(() => {
    if (shownChunks >= chunks.length) return;
    startTransition(() => setShownChunks((was) => was + TABLE_STEP_CHUNKS));
  }, [shownChunks, chunks.length]);
  return (
    // Only a SELECTABLE table bleeds to the view's edges, because its rows are press
    // bands. A table that is only read keeps its rules on the column of the words, so
    // its lines end where the links and the log under it end.
    <div className={`${isSelectable ? '-mx-(--live-view-inset) ' : ''}overflow-x-auto`}>
      <table
        className={`w-full min-w-0 border-collapse font-mono text-body ${
          isSelectable
            ? ''
            : '[&_td:first-child]:pl-0 [&_td:last-child]:pr-0 [&_th:first-child]:pl-0 [&_th:last-child]:pr-0'
        }`}
      >
        <thead className="hidden sm:table-header-group">
          <tr className="border-b border-dialog-edge/40">
            {node.columns.map((column) => (
              <th
                key={column.id}
                scope="col"
                className={`px-(--live-view-inset) py-2 font-normal text-meta text-dialog-hint ${
                  column.align === 'right' ? 'text-right' : 'text-left'
                }`}
              >
                <InlineMarkdown>{column.label}</InlineMarkdown>
              </th>
            ))}
            {hasState && (
              <th
                scope="col"
                className="py-2 pr-(--live-view-inset) text-left font-normal text-meta text-dialog-hint"
              >
                State
              </th>
            )}
          </tr>
        </thead>
        <tbody className="divide-y divide-dialog-edge/40">
          {rows.length === 0 && (
            <tr>
              <td className="px-(--live-view-inset) py-2" colSpan={span}>
                <Empty>{EMPTY_LINE.table}</Empty>
              </td>
            </tr>
          )}
          {chunks.slice(0, shownChunks).map((items, index) => (
            <LiveTableChunk
              key={index}
              items={items}
              nodeId={node.id}
              columns={node.columns}
              selected={selected}
              isSelectable={isSelectable}
              hasState={hasState}
              span={span}
              onSelect={onSelect}
              onToggleGroup={toggleGroup}
            />
          ))}
        </tbody>
      </table>
    </div>
  );
}

/** What a target opens, in words, so a link says where it goes before it is pressed. */
const TARGET_WORD = { url: 'Web page', path: 'File', attachment: 'Attachment' } as const;

/**
 * Where the work also lives. A `url` is reachable from the phone and opens; a path
 * or an attachment names a place on the MACHINE, so it is stated as text, with the
 * place under its name, rather than dressed as a link that does nothing. What a
 * link opens follows as a word, never an arrow glued to the label.
 */
function LinkRows({ node }: { node: LiveLinkNode }) {
  if (node.links.length === 0) return <Empty>{EMPTY_LINE.link}</Empty>;
  return (
    <ul className="min-w-0 divide-y divide-dialog-edge/40 font-mono text-ui">
      {node.links.map((link) => (
        <li key={link.id} className="flex min-w-0 items-start gap-2 py-1.5 first:pt-0 last:pb-0">
          {link.tone && link.tone !== 'idle' && <LineMark tone={link.tone} />}
          <span className="min-w-0 flex-1">
            {link.target_kind === 'url' ? (
              <a
                href={link.target}
                target="_blank"
                rel="noreferrer"
                className="text-white underline decoration-dialog-hint underline-offset-2 [overflow-wrap:anywhere]"
              >
                <InlineMarkdown>{link.label}</InlineMarkdown>
              </a>
            ) : (
              <>
                <span className="block text-white [overflow-wrap:anywhere]">
                  <InlineMarkdown>{link.label}</InlineMarkdown>
                </span>
                {/* A place that only repeats the label says nothing new. */}
                {link.target !== link.label && (
                  <span className="block text-meta text-dialog-hint [overflow-wrap:anywhere]">
                    {link.target}
                  </span>
                )}
              </>
            )}
          </span>
          <span className="flex h-[1lh] shrink-0 items-center gap-1.5 text-dialog-hint">
            <span className="text-meta">{TARGET_WORD[link.target_kind]}</span>
            {link.target_kind === 'url' && <ArrowOutIcon className="size-3" />}
          </span>
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
  /** The values the operator typed and the view has not echoed yet. */
  values: Record<string, HumanInputValue>;
  onChange?: (id: string, value: HumanInputValue) => void;
  /** Send a typed value now: the input lost focus or its own Enter fired. */
  onCommit?: (id: string) => void;
  /** Send every input of the view with the submit button `id`. */
  onSubmit?: (id: string) => Promise<void> | void;
};

/** Wait this long after the last keystroke before a `change` goes out. */
export const LIVE_CHANGE_DEBOUNCE_MS = 400;

/** What an input shows: the typed value, the view's value, or the field's default. */
function liveInputValue(node: LiveInputNode, typed: Record<string, HumanInputValue>): HumanInputValue {
  if (Object.hasOwn(typed, node.id)) return typed[node.id];
  const value = node.value;
  if (
    typeof value === 'string' ||
    typeof value === 'number' ||
    typeof value === 'boolean' ||
    (Array.isArray(value) && value.every((item) => typeof item === 'string'))
  ) {
    return value as HumanInputValue;
  }
  return humanInputDefaultValue(node.field);
}

/** Every input value a submit sends. A secret the host already holds stays there. */
function liveSubmitValues(
  nodes: readonly LiveNode[],
  typed: Record<string, HumanInputValue>,
): Record<string, HumanInputValue> {
  return Object.fromEntries(
    liveInputNodes(nodes)
      .filter((node) => Object.hasOwn(typed, node.id) || !node.is_set)
      .map((node) => [node.id, liveInputValue(node, typed)]),
  );
}

/** A form input inside a live view: the same control a question uses. */
function LiveInput({ node, presentation }: { node: LiveInputNode; presentation: NodePresentation }) {
  const { onChange, onCommit } = presentation;
  return (
    <div className="min-w-0" onBlur={() => onCommit?.(node.id)}>
      <HumanInputFieldRow
        field={node.field}
        values={{ [node.field.id]: liveInputValue(node, presentation.values) }}
        errors={{}}
        disabled={presentation.isSettled || !onChange}
        onChange={(_, value) => onChange?.(node.id, value)}
        onSubmit={() => onCommit?.(node.id)}
      />
    </div>
  );
}

/** A choice tile. A selectable card sends one click, like a button. */
function CardTile({
  node,
  onActivate,
  isSettled,
}: {
  node: LiveCardNode;
  onActivate?: NodePresentation['onActivate'];
  isSettled: boolean;
}) {
  const [pending, setPending] = useState(false);
  const tone = node.tone ?? 'idle';
  const body = (
    <>
      <span className="flex min-w-0 items-center gap-2">
        {node.tone && <ToneMark tone={tone} />}
        <span className="min-w-0 truncate text-body font-semibold text-white">{node.title}</span>
      </span>
      {node.text && (
        <span className="block text-ui text-dialog-foreground [overflow-wrap:anywhere]">
          <InlineMarkdown>{node.text}</InlineMarkdown>
        </span>
      )}
      {node.meta && (
        <span className="block font-mono text-meta text-dialog-hint [overflow-wrap:anywhere]">
          {node.meta}
        </span>
      )}
    </>
  );
  const frame = 'block w-full min-w-0 space-y-1 border border-dialog-edge bg-panel-2 px-3 py-2.5 text-left';
  if (!node.is_selectable) return <div className={frame}>{body}</div>;
  return (
    <ChoiceRow
      isOn={false}
      mark={null}
      disabled={isSettled || pending || !onActivate}
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
      <span className="block space-y-1">{body}</span>
    </ChoiceRow>
  );
}

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
    <div className="min-w-0 space-y-2">
      {isLiveInputNode(node) ? (
        <LiveInput node={node} presentation={presentation} />
      ) : isDisclosure ? (
        // The name starts on the column of every other section name, and the chevron
        // follows it: a chevron in front pushed `Output` 20px in from `Services`.
        // The press face stops at its words, so its focus ring frames `Output 200 lines`
        // and not an empty band across the whole view.
        <Disclosure
          className="max-w-fit"
          isOpen={isOpen}
          tone="branch"
          inlineChevron
          density="comfortable"
          aria-label={label}
          aria-controls={isOpen ? contentId : undefined}
          onClick={() => presentation.toggle(node.id, !isOpen)}
          tally={
            node.type === 'log' ? (
              <span className="font-normal text-ui text-dialog-hint">
                {node.total_lines} lines
              </span>
            ) : undefined
          }
        >
          <NodeLabel>{label}</NodeLabel>
        </Disclosure>
      ) : (
        node.label &&
        node.type !== 'button' &&
        node.type !== 'card' && <NodeLabel>{node.label}</NodeLabel>
      )}
      {node.type === 'log' && !isOpen && <LogErrors node={node} label={label} />}
      {/* A log's output starts at the column of the view, not under its name: an
          indent there only took width from the lines. */}
      {!isLiveInputNode(node) && (!isDisclosure || isOpen) && (
        <div
          id={contentId}
          className={`min-w-0 ${isDisclosure && node.type !== 'log' ? 'pl-5' : ''}`}
        >
          {node.type === 'group' && (
            <ViewLayout direction={node.direction} maxColumns={node.max_columns}>
              {node.children.map((child) => (
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
            <hr className="m-0 w-full border-0 border-t border-dialog-edge/40" />
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
              onActivate={node.action === 'submit' ? presentation.onSubmit : presentation.onActivate}
            />
          )}
          {node.type === 'card' && (
            <CardTile
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
export function runSummary(nodes: LiveNode[]): string {
  const status = findNode(
    nodes,
    (node): node is LiveStatusNode => node.type === 'status' && node.text.trim() !== '',
  );
  if (status) return status.text;
  const progress = findNode(nodes, (node): node is LiveProgressNode => node.type === 'progress');
  const fraction = progress ? liveFraction(progress) : null;
  return fraction === null ? '' : `${livePercent(fraction)}%`;
}

/** The tone of the newest status, the state a pin or a folded row shows. */
export function runTone(nodes: LiveNode[]): LiveTone {
  return findNode(nodes, (node): node is LiveStatusNode => node.type === 'status')?.tone ?? 'idle';
}

/** Depth first, because a status inside a group still says what the run is doing. */
function findNode<T extends LiveNode>(
  nodes: LiveNode[],
  pick: (node: LiveNode) => node is T,
): T | undefined {
  for (const node of nodes) {
    if (pick(node)) return node;
    if (node.type === 'group') {
      const inner = findNode(node.children, pick);
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
  onChange,
  onSubmit,
  isInterrupting = false,
  error,
  load,
  isSettled = false,
  embedded = false,
  inDialog = false,
}: {
  view: LiveViewModel;
  /** Stop the view, carrying the comment the human left — `null` when they left none. */
  onInterrupt?: (note: string | null) => void;
  /** Live selection, or local snapshot browsing in a receipt; omit when unavailable. */
  onSelect?: (nodeId: string, itemIds: string[]) => void;
  onActivate?: (nodeId: string) => Promise<void> | void;
  /** Send one input's new value. The panel waits for a pause in the typing. */
  onChange?: (nodeId: string, value: HumanInputValue) => Promise<void> | void;
  /** Send every input value with the submit button `nodeId`. */
  onSubmit?: (nodeId: string, values: Record<string, HumanInputValue>) => Promise<void> | void;
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
  /**
   * The run fills its own dialog. The dialog's band already names it, so the panel
   * paints no second title and no frame: a box inside the dialog's box.
   */
  inDialog?: boolean;
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
  const [typed, setTyped] = useState<Record<string, HumanInputValue>>({});
  const unsent = useRef(new Map<string, { value: HumanInputValue; timer: ReturnType<typeof setTimeout> }>());
  const sendRef = useRef(onChange);
  useEffect(() => {
    sendRef.current = onChange;
  });
  const sendChange = (id: string) => {
    const held = unsent.current.get(id);
    if (!held) return;
    clearTimeout(held.timer);
    unsent.current.delete(id);
    void sendRef.current?.(id, held.value);
  };
  useEffect(() => {
    const held = unsent.current;
    return () => {
      for (const id of [...held.keys()]) {
        const entry = held.get(id);
        if (entry) clearTimeout(entry.timer);
        held.delete(id);
        if (entry) void sendRef.current?.(id, entry.value);
      }
    };
  }, []);
  const presentation: NodePresentation = {
    isSettled,
    onActivate,
    values: typed,
    onChange: onChange
      ? (id, value) => {
          setTyped((held) => ({ ...held, [id]: value }));
          const held = unsent.current.get(id);
          if (held) clearTimeout(held.timer);
          unsent.current.set(id, {
            value,
            timer: setTimeout(() => sendChange(id), LIVE_CHANGE_DEBOUNCE_MS),
          });
        }
      : undefined,
    onCommit: sendChange,
    onSubmit: onSubmit
      ? async (id) => {
          for (const held of [...unsent.current.keys()]) sendChange(held);
          await onSubmit(id, liveSubmitValues(view.nodes, typed));
        }
      : undefined,
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
  const noteText = note ?? '';
  // One armed stop, however it is sent: the comment travels trimmed, and an
  // empty line is no comment at all rather than an empty one.
  const sendStop = (send: (note: string | null) => void) => {
    setNote(null);
    send(noteText.trim() === '' ? null : noteText.trim());
  };
  const canInterrupt = !isSettled && Boolean(onInterrupt) && !isArmed;
  const isCard = !embedded && !inDialog;
  return (
    <>
      {/* Embedded, a run is a band like the TUI draws it: no frame of its own, the
          execution group's column, and the same row rhythm as the CODE and ACTIVITY
          bands beside it. Only the standalone dialog keeps a frame. */}
      <section
        className={`live-view-panel min-w-0 overflow-hidden ${
          isCard ? 'border border-dialog-edge bg-panel' : ''
        }`}
        data-execution-run={embedded || undefined}
        role={isSettled ? undefined : 'status'}
        aria-live={isSettled ? undefined : 'polite'}
      >
        {(!inDialog || canInterrupt || isSettled) && (
        <header
          className={
            embedded
              ? 'flex min-h-9 min-w-0 items-center gap-2 mouse:min-h-7'
              : inDialog
                ? 'flex min-w-0 flex-row-reverse items-center justify-between gap-2 px-(--live-view-inset) pb-2'
                : 'flex items-start gap-2 border-b border-dialog-edge px-(--live-view-inset) py-2.5'
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
            isCard && (
              <span className="min-w-0 flex-1">
                <span className="block font-mono text-title font-bold text-white [overflow-wrap:anywhere]">
                  {view.title}
                </span>
                {view.description && (
                  <span className="block font-mono text-ui text-dialog-hint [overflow-wrap:anywhere]">
                    <InlineMarkdown>{view.description}</InlineMarkdown>
                  </span>
                )}
              </span>
            )
          )}
          {canInterrupt &&
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
              words, and the run's state last. Both words PRESS — LIVE opens the run the row is
              reporting — and each keeps its 44px reach in an invisible pseudo-element, so the
              row stays a line of text under the finger too. */}
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
              <TextButton
                isBand
                className="shrink-0"
                aria-label={`Open the live run ${view.title}`}
                onClick={() => setOpened(true)}
              >
                LIVE
              </TextButton>
            </>
          )}
          <ViewState view={view} isSettled={isSettled} />
        </header>
        )}
        {!isSettled && isArmed && onInterrupt && (
          <form
            className={`flex flex-wrap items-center gap-x-2 gap-y-5 py-2 ${
              embedded ? '' : 'px-(--live-view-inset)'
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
              value={noteText}
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
            className={`py-2 font-mono text-ui text-err ${
              embedded ? '' : 'px-(--live-view-inset)'
            }`}
          >
            {error}
          </p>
        )}
        {/* THE TRANSCRIPT STATES A RUN; IT DOES NOT PAINT IT. A build observation
            painted in place stood taller than the turn that opened it and pushed the
            answer off the screen — the same rule a settled run obeys one row above
            (`LiveRunRow`). The picture is one press away, in the run's own screen. */}
        {/* SPACE separates the sections, not a rule under every node: a hairline between
            each pair fenced the run into a stack of boxes. Only the log, the secondary
            reading, keeps a rule above it. */}
        {!embedded && (
          <ul className={`space-y-6 ${inDialog ? 'pt-2 pb-4' : 'py-4'}`}>
            {view.nodes.map((node, index) => (
              <li
                key={node.id}
                className={`min-w-0 ${node.type === 'divider' ? '' : 'px-(--live-view-inset)'} ${
                  node.type === 'log' && index > 0
                    ? 'relative pt-4 before:absolute before:inset-x-(--live-view-inset) before:top-0 before:border-t before:border-dialog-edge/40'
                    : ''
                }`}
              >
                <NodeCell node={node} load={load} onSelect={onSelect} presentation={presentation} />
              </li>
            ))}
          </ul>
        )}
      </section>
      {embedded && opened && (
        <OpenedRun
          view={view}
          onInterrupt={onInterrupt}
          onSelect={onSelect}
          onActivate={onActivate}
          onChange={onChange}
          onSubmit={onSubmit}
          error={error}
          isInterrupting={isInterrupting}
          load={load}
          isSettled={isSettled}
          onClose={() => setOpened(false)}
        />
      )}
    </>
  );
}

/** An opened run: its panel in the session's dialog. Its row and a closed step digest open it. */
function OpenedRun({
  onClose,
  ...panel
}: Omit<Parameters<typeof LiveViewPanel>[0], 'embedded'> & { onClose: () => void }) {
  return (
    <RunDialog
      title={panel.view.title}
      subtitle={
        panel.view.description ? <InlineMarkdown>{panel.view.description}</InlineMarkdown> : undefined
      }
      onClose={onClose}
    >
      <div className="min-h-0 flex-1 overflow-y-auto py-3">
        <LiveViewPanel {...panel} inDialog />
      </div>
    </RunDialog>
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
 * The run paints no frame and no second title in here (`inDialog`): the dialog is
 * its box, and the dialog's band is its one title.
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
 * A snapshot describes the moment the gateway answered, so the events that arrive
 * while it is in flight are folded OVER it, never a reason to drop it. A running
 * watch patches every few seconds, and a dropped snapshot left the panel empty
 * until the next view opened. A patch that names a view this panel never mounted
 * means its open was missed, so it reads the snapshot again.
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
  const reloadRef = useRef<(() => void) | null>(null);

  useEffect(() => {
    let cancelled = false;
    const controller = new AbortController();
    // The live events since the oldest snapshot read still in flight, in order.
    let journal: SseEvent[] | null = null;
    let inFlight = 0;
    let requested = 0;
    let applied = 0;
    const mounted = new Set<string>();
    const reload = () => {
      requested += 1;
      const request = requested;
      journal ??= [];
      const from = journal.length;
      inFlight += 1;
      client
        .liveViews(sid, controller.signal)
        .then((open) => {
          if (cancelled || request < applied) return;
          applied = request;
          const settled = (journal ?? []).slice(from).reduce(applyLiveViewEvent, open);
          for (const view of settled) mounted.add(view.id);
          setViews(settled);
        })
        .catch(() => undefined)
        .finally(() => {
          inFlight -= 1;
          if (inFlight === 0) journal = null;
        });
    };
    reloadRef.current = reload;
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
      const viewId = typeof event.view_id === 'string' ? event.view_id : '';
      if (event.type === VIEW_OPEN_EVENT) {
        const opened = event.view as { id?: unknown } | null | undefined;
        if (typeof opened?.id === 'string') mounted.add(opened.id);
      } else if (event.type === VIEW_PATCH_EVENT && viewId && !mounted.has(viewId) && inFlight === 0) {
        reload();
      }
      journal?.push(event);
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
      reloadRef.current = null;
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
    if (isStale) reloadRef.current?.();
  }, [isStale]);

  return views;
}

/** Open views as one-line runs, or in the session's dialog when `onClose` is given. */
export function LiveView({
  views,
  client,
  sid,
  onClose,
  inPanel = false,
}: {
  views: LiveViewModel[];
  client: GatewayClient;
  /** The session that shows the views. */
  sid: string;
  /** Paint each view without a frame or a title, inside the side panel. */
  inPanel?: boolean;
  /** Show the views already opened in the session's dialog, and call this when it closes. */
  onClose?: () => void;
}) {
  const [stopping, setStopping] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);

  if (views.length === 0) return null;

  const send = (viewId: string, action: ViewAction) => client.viewAction(sid, viewId, action);

  // The note the human typed rides WITH the stop: one call, so the run never
  // resumes on an interrupt whose reason is still in flight behind it.
  const interrupt = (viewId: string, note: string | null) => {
    setStopping(viewId);
    setError(null);
    send(viewId, {
        action: 'interrupt',
        ...(note ? { note } : {}),
      })
      .catch(() => setError('That view would not stop. It may have just finished.'))
      .finally(() => setStopping(null));
  };

  const select = (viewId: string, nodeId: string, itemIds: string[]) => {
    setError(null);
    send(viewId, {
        action: 'select',
        node_id: nodeId,
        item_ids: itemIds,
      })
      .catch(() => setError('That job could not be selected. It may have just finished.'));
  };

  const activate = async (viewId: string, nodeId: string) => {
    setError(null);
    try {
      const outcome = await send(viewId, {
        action: 'activate',
        node_id: nodeId,
      });
      if (!outcome.is_accepted)
        setError('That action is unavailable. The button may be disabled or the run finished.');
    } catch {
      setError('That action could not be confirmed. Check the run before trying again.');
    }
  };

  const change = async (viewId: string, nodeId: string, value: HumanInputValue) => {
    try {
      const outcome = await send(viewId, { action: 'change', node_id: nodeId, value });
      if (!outcome.is_accepted) setError('That value was refused, or the view has ended.');
    } catch {
      setError('That value could not be sent. Check the connection and try again.');
    }
  };

  const submit = async (viewId: string, nodeId: string, values: Record<string, HumanInputValue>) => {
    setError(null);
    try {
      const outcome = await send(viewId, { action: 'submit', node_id: nodeId, values });
      if (!outcome.is_accepted) {
        const named = Object.values(outcome.errors ?? {});
        setError(named[0] ?? outcome.reason ?? 'The view did not accept these values.');
      }
    } catch {
      setError('These values could not be sent. Check the connection and try again.');
    }
  };

  const readLog = (viewId: string) => (nodeId: string, from: number, limit: number, query?: string) =>
    client.liveViewLog(sid, viewId, nodeId, from, limit, query);

  const panels = views.map((view) => {
    const panel = {
      view,
      error: stopping === null ? error : null,
      isInterrupting: stopping === view.id,
      onInterrupt: (note: string | null) => interrupt(view.id, note),
      onSelect: (nodeId: string, itemIds: string[]) => select(view.id, nodeId, itemIds),
      onActivate: (nodeId: string) => activate(view.id, nodeId),
      onChange: (nodeId: string, value: HumanInputValue) => change(view.id, nodeId, value),
      onSubmit: (nodeId: string, values: Record<string, HumanInputValue>) =>
        submit(view.id, nodeId, values),
      load: readLog(view.id),
    };
    if (inPanel) return <LiveViewPanel key={view.id} {...panel} inDialog />;
    return onClose ? (
      <OpenedRun key={view.id} {...panel} onClose={onClose} />
    ) : (
      <LiveViewPanel key={view.id} {...panel} embedded />
    );
  });
  return panels;
}
