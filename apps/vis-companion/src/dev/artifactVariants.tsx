/**
 * WHAT BELONGS IN THE SESSION HEADER'S RIGHT SLOT?
 *
 * Today that slot is `Share`: it copies a `vis://s/<sid>` link — a thing you
 * need about once per session, parked where the eye lands every time the screen
 * opens. Meanwhile the session's most valuable output has NO index at all: the
 * figures, PDFs, HTML reports, clips and files the model PRODUCED are scattered
 * down the transcript in the tool call that happened to make them, so "show me
 * that chart again" is a scroll hunt through 40 turns.
 *
 * So the slot is repurposed: it becomes ARTIFACTS — one control that answers
 * "what has this session produced", and one surface that opens each one in the
 * viewer that can already zoom it, draw on it, and attach the drawing back to
 * the next message. Attachments produced by the model, under the word a human
 * uses for them.
 *
 * Each proposal has to survive its own falsifier:
 *
 *   * `empty` — a session that produced NOTHING. The control must not exist:
 *     no dead chip, no gallery that opens onto "No artifacts yet". A feature
 *     that costs a fresh session one pixel is not free.
 *   * `docs` — a session whose output is a PDF, an HTML report and two logs.
 *     Nothing to thumbnail: a design that only reads as a photo grid falls
 *     apart here, which is exactly where documents live.
 *   * `default` — eight mixed artifacts over four turns, including a clip and a
 *     two-digit count, so the layout is measured at its widest.
 */

import type { ReactNode } from 'react';

type ArtifactKind = 'image' | 'video' | 'doc' | 'file';

interface Artifact {
  name: string;
  kind: ArtifactKind;
  /** The chip beside the name: what KIND of thing this is, in one word. */
  media: string;
  /** Human size, `<n> kB` or `<n> MB` — parsed back by `totalLabel`. */
  size: string;
  /** Which turn produced it, and the tool call inside it. Provenance is data. */
  turn: number;
  tool: string;
  at: string;
  pages?: number;
  runtime?: string;
  /**
   * A thumbnail stands in for a real raster here: literal Tailwind height
   * classes, because Tailwind scans source TEXT and a computed `h-${n}` is a
   * class that never gets generated.
   */
  bars?: string[];
  /** The artifact's own hue, so two figures from one turn stay distinct. */
  hue: string;
}

/**
 * One session's production, newest first: two figures from the same python
 * block, a screencast, a paginated PDF, an HTML dashboard, and the recorded
 * files a shell run left behind.
 */
export const ARTIFACTS: Artifact[] = [
  {
    name: 'sales-forecast.png',
    kind: 'image',
    media: 'PNG',
    size: '96 kB',
    turn: 7,
    tool: 'python_execution',
    at: '12:11',
    bars: ['h-4', 'h-6', 'h-7', 'h-9', 'h-8', 'h-11', 'h-10', 'h-12'],
    hue: 'bg-machine-violet',
  },
  {
    name: 'revenue-2024.png',
    kind: 'image',
    media: 'PNG',
    size: '214 kB',
    turn: 6,
    tool: 'python_execution',
    at: '12:04',
    bars: ['h-3', 'h-5', 'h-8', 'h-6', 'h-10', 'h-7', 'h-12', 'h-9'],
    hue: 'bg-machine-teal',
  },
  {
    name: 'latency-hist.png',
    kind: 'image',
    media: 'PNG',
    size: '88 kB',
    turn: 6,
    tool: 'python_execution',
    at: '12:04',
    bars: ['h-10', 'h-12', 'h-9', 'h-6', 'h-4', 'h-3', 'h-2', 'h-2'],
    hue: 'bg-machine-orange',
  },
  {
    name: 'residuals.png',
    kind: 'image',
    media: 'PNG',
    size: '74 kB',
    turn: 6,
    tool: 'python_execution',
    at: '12:03',
    bars: ['h-6', 'h-3', 'h-8', 'h-4', 'h-7', 'h-3', 'h-6', 'h-5'],
    hue: 'bg-machine-aqua',
  },
  {
    name: 'session-run.mp4',
    kind: 'video',
    media: 'MP4',
    size: '4.1 MB',
    turn: 5,
    tool: 'shell',
    at: '11:58',
    runtime: '0:42',
    bars: ['h-6', 'h-7', 'h-6', 'h-8', 'h-6', 'h-7', 'h-6', 'h-7'],
    hue: 'bg-machine-indigo',
  },
  {
    name: 'q3-report.pdf',
    kind: 'doc',
    media: 'PDF',
    size: '1.2 MB',
    turn: 5,
    tool: 'python_execution',
    at: '11:57',
    pages: 24,
    hue: 'bg-machine-rose',
  },
  {
    name: 'dashboard.html',
    kind: 'doc',
    media: 'HTML',
    size: '48 kB',
    turn: 5,
    tool: 'python_execution',
    at: '11:56',
    hue: 'bg-machine-azure',
  },
  {
    name: 'flame-p3.png',
    kind: 'image',
    media: 'PNG',
    size: '180 kB',
    turn: 4,
    tool: 'shell',
    at: '11:40',
    bars: ['h-12', 'h-4', 'h-9', 'h-3', 'h-7', 'h-5', 'h-10', 'h-4'],
    hue: 'bg-machine-brass',
  },
  {
    name: 'audit-trail.pdf',
    kind: 'doc',
    media: 'PDF',
    size: '320 kB',
    turn: 4,
    tool: 'shell',
    at: '11:41',
    pages: 6,
    hue: 'bg-machine-coral',
  },
  {
    name: 'build.log',
    kind: 'file',
    media: 'TEXT',
    size: '12 kB',
    turn: 4,
    tool: 'shell',
    at: '11:39',
    hue: 'bg-machine-olive',
  },
  {
    name: 'coverage.json',
    kind: 'file',
    media: 'JSON',
    size: '34 kB',
    turn: 3,
    tool: 'run_tests',
    at: '11:20',
    hue: 'bg-machine-cyan',
  },
  {
    name: 'bench.csv',
    kind: 'file',
    media: 'CSV',
    size: '8 kB',
    turn: 3,
    tool: 'run_tests',
    at: '11:19',
    hue: 'bg-machine-bronze',
  },
];

/** The photographed states, per proposal. The page owns the matrix. */
export const ARTIFACT_STATES: Record<string, string[]> = {
  'artifacts-sheet': ['default', 'docs', 'shut', 'empty'],
  'artifacts-turns': ['default', 'docs', 'empty'],
  'artifacts-dock': ['default', 'docs', 'empty'],
};

export function artifactsFor(state: string): Artifact[] {
  if (state === 'empty') return [];
  if (state === 'docs') {
    return ARTIFACTS.filter((entry) => entry.kind === 'doc' || entry.kind === 'file');
  }
  return ARTIFACTS;
}

/** kB, so `1.2 MB` and `96 kB` can be added up honestly. */
function kilobytes(size: string): number {
  const [value, unit] = size.split(' ');
  return Number(value) * (unit === 'MB' ? 1024 : 1);
}

export function totalLabel(list: Artifact[]): string {
  const total = list.reduce((sum, entry) => sum + kilobytes(entry.size), 0);
  return total >= 1024 ? `${(total / 1024).toFixed(1)} MB` : `${Math.round(total)} kB`;
}

const KIND_GLYPH: Record<ArtifactKind, string> = {
  image: '▣',
  video: '▶',
  doc: '▤',
  file: '≡',
};

/**
 * The thumbnail. An image or a clip shows its own picture; a document shows a
 * page with its page count; a recorded file has no picture and does not pretend
 * to — it wears its extension and stays a row of text.
 */
function Thumb({ artifact, className }: { artifact: Artifact; className: string }) {
  if (artifact.bars) {
    return (
      <span
        className={`relative flex items-end justify-center gap-0.5 overflow-hidden border-b border-dialog-edge bg-code px-2 pb-2 ${className}`}
      >
        {artifact.bars.map((bar, index) => (
          <span key={index} className={`w-1.5 ${bar} ${artifact.hue} opacity-80`} />
        ))}
        {artifact.kind === 'video' && (
          <span className="absolute inset-0 grid place-items-center font-mono text-subhead text-white">
            ▶
          </span>
        )}
        {artifact.runtime && (
          <span className="absolute right-1 bottom-1 bg-ink/80 px-1 font-mono text-chip text-white">
            {artifact.runtime}
          </span>
        )}
      </span>
    );
  }
  if (artifact.kind === 'doc') {
    return (
      <span
        className={`relative flex flex-col justify-center gap-1 overflow-hidden border-b border-dialog-edge bg-panel-2 px-3 ${className}`}
      >
        <span className={`h-1 w-2/3 ${artifact.hue}`} />
        <span className="h-0.5 w-full bg-dialog-hint/50" />
        <span className="h-0.5 w-full bg-dialog-hint/50" />
        <span className="h-0.5 w-4/5 bg-dialog-hint/50" />
        <span className="h-0.5 w-full bg-dialog-hint/50" />
        <span className="h-0.5 w-1/2 bg-dialog-hint/50" />
        {artifact.pages && (
          <span className="absolute right-1 bottom-1 bg-ink/80 px-1 font-mono text-chip text-white">
            {artifact.pages} pp
          </span>
        )}
      </span>
    );
  }
  return (
    <span
      className={`grid place-items-center border-b border-dialog-edge bg-code font-mono text-subhead text-dialog-hint ${className}`}
    >
      {KIND_GLYPH[artifact.kind]}
    </span>
  );
}

/** `PNG · 214 kB · turn 6` — the line that makes an artifact citable. */
function Meta({ artifact, withTool }: { artifact: Artifact; withTool?: boolean }) {
  return (
    <span className="block truncate font-mono text-chip text-dialog-hint">
      {[artifact.media, artifact.size, `turn ${artifact.turn}`, withTool ? artifact.tool : null]
        .filter(Boolean)
        .join(' · ')}
    </span>
  );
}

/** A tile in a grid: picture on top, name and provenance under it. */
function Tile({ artifact, thumb }: { artifact: Artifact; thumb: string }) {
  return (
    <span className="flex min-h-11 min-w-0 flex-col border border-dialog-edge bg-panel text-left transition-colors hover:bg-hover">
      <Thumb artifact={artifact} className={thumb} />
      <span className="min-w-0 px-2 py-1.5">
        <span className="block truncate font-mono text-meta font-bold text-white">
          {artifact.name}
        </span>
        <Meta artifact={artifact} />
      </span>
    </span>
  );
}

/**
 * THE REPURPOSED SLOT. Same geometry Share had, and the same loud tone — but it
 * counts, so it says something true about THIS session, and with nothing
 * produced it renders nothing at all.
 */
function ArtifactsChip({ count, open }: { count: number; open: boolean }) {
  if (!count) return null;
  const tone = open
    ? 'border-accent bg-accent text-accent-foreground'
    : 'border-dialog-title bg-dialog-title text-dialog-title-foreground hover:bg-accent-2';
  return (
    <span
      className={`inline-flex min-h-6 shrink-0 items-center gap-1 border px-2 font-mono text-chip font-bold uppercase tracking-[0.08em] ${tone}`}
    >
      <span aria-hidden="true">▣</span>
      <span className="hidden sm:inline">Artifacts</span>
      <span>{count}</span>
    </span>
  );
}

/** The session header, with the artifacts control where Share used to be. */
function SessionHeader({ count, open }: { count: number; open: boolean }) {
  return (
    <header className="z-10 flex min-h-13 shrink-0 items-stretch border-b border-dialog-edge bg-panel-2">
      <span className="grid w-11 shrink-0 place-items-center border-r border-dialog-edge bg-dialog-title font-mono text-subhead font-bold text-dialog-title-foreground mouse:w-10">
        ‹
      </span>
      <span className="min-w-0 flex-1 self-center px-3 py-1.5">
        <span className="block truncate font-mono text-body font-bold text-white">
          Quarterly revenue review
        </span>
        <span className="flex items-center gap-1.5 font-mono text-meta text-dialog-hint">
          <span className="size-1.5 bg-ok" />
          Connected
        </span>
      </span>
      <span className="flex shrink-0 items-center gap-1 self-center pr-2 pl-1 sm:pr-3">
        <span className="hidden max-w-36 min-h-6 items-center gap-1 border border-dialog-edge px-2 font-mono text-chip text-dialog-hint sm:inline-flex">
          <span className="opacity-50">#</span>5ca90155
        </span>
        <ArtifactsChip count={count} open={open} />
      </span>
    </header>
  );
}

/**
 * Enough transcript to prove the artifacts surface is layered over a session
 * that is still readable, plus the composer holding the picture that came BACK
 * out of the viewer — the round trip the whole feature exists for.
 */
function TranscriptMock({ pending }: { pending: boolean }) {
  return (
    <div className="min-h-0 flex-1 overflow-hidden">
      <div className="mx-auto w-full max-w-3xl px-3.5 pt-4 sm:px-6">
        <p className="border-l-2 border-you-role bg-you-message/10 px-3 py-2 font-mono text-body text-white">
          Chart 2024 revenue by quarter and give me the report as a PDF.
        </p>
        <p className="mt-3 font-mono text-body text-white">
          Four quarters, one figure each and the paginated report:
        </p>
        <div className="mt-2 border border-code-edge bg-code px-3 py-2 font-mono text-meta text-code-foreground">
          <span className="block text-code-syntax-comment"># python_execution</span>
          <span className="block">
            <span className="text-code-syntax-keyword">for</span> quarter{' '}
            <span className="text-code-syntax-keyword">in</span> quarters:
          </span>
          <span className="block pl-4">plot(quarter).savefig(out)</span>
        </div>
        <p className="mt-2 font-mono text-chip text-footer-muted">
          ↗ revenue-2024.png · latency-hist.png · q3-report.pdf
        </p>
      </div>
      {pending && (
        <div className="mx-auto mt-4 w-full max-w-3xl px-3.5 sm:px-6">
          <div className="flex items-center gap-2 border border-accent bg-hover px-2 py-1.5">
            <span className="grid size-8 shrink-0 place-items-center bg-machine-teal/30 font-mono text-meta text-white">
              ▣
            </span>
            <span className="min-w-0 flex-1">
              <span className="block truncate font-mono text-meta font-bold text-white">
                revenue-2024-marked.png
              </span>
              <span className="block font-mono text-chip text-dialog-hint">
                drawn on · attached to this message
              </span>
            </span>
            <span className="shrink-0 font-mono text-ui text-dialog-hint">✕</span>
          </div>
        </div>
      )}
    </div>
  );
}

function ComposerMock() {
  return (
    <div className="shrink-0 border-t border-dialog-edge bg-panel-2 px-3 py-2">
      <div className="flex items-center gap-2">
        <span className="grid size-9 shrink-0 place-items-center border border-dialog-edge font-mono text-ui text-dialog-hint">
          +
        </span>
        <span className="min-h-9 flex-1 border border-dialog-edge bg-input px-2 py-1.5 font-mono text-body text-dialog-hint">
          Which quarter is the outlier?
        </span>
        <span className="grid min-h-9 shrink-0 place-items-center border border-accent bg-accent px-3 font-mono text-ui font-bold text-accent-foreground">
          Send
        </span>
      </div>
    </div>
  );
}

/**
 * The session screen every proposal is photographed inside. `overlay` covers
 * only the transcript, never the header: whichever surface a proposal opens,
 * the control that opened it stays visible and lit.
 */
function SessionMock({
  count,
  open,
  pending = true,
  under,
  aside,
  overlay,
}: {
  count: number;
  open: boolean;
  pending?: boolean;
  under?: ReactNode;
  aside?: ReactNode;
  overlay?: ReactNode;
}) {
  return (
    <section className="flex h-full min-h-0 flex-col overflow-hidden bg-ink">
      <SessionHeader count={count} open={open} />
      {under}
      <div className="relative flex min-h-0 flex-1 overflow-hidden">
        <TranscriptMock pending={pending} />
        {aside}
        {overlay}
      </div>
      <ComposerMock />
    </section>
  );
}

/** The sheet's own header: what this is, how much of it there is, and out. */
function SurfaceHeader({ list, children }: { list: Artifact[]; children?: ReactNode }) {
  return (
    <header className="flex shrink-0 items-start justify-between gap-3 border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4">
      <span className="min-w-0">
        <span className="block font-mono text-title font-bold text-white">Artifacts</span>
        <span className="block font-mono text-meta text-dialog-hint">
          {list.length} produced by the model · {totalLabel(list)}
        </span>
        {children}
      </span>
      <span className="grid min-h-8 min-w-8 shrink-0 place-items-center border border-dialog-edge font-mono text-ui text-dialog-hint">
        ✕
      </span>
    </header>
  );
}

const FILTERS: { label: string; kinds: ArtifactKind[] }[] = [
  { label: 'All', kinds: ['image', 'video', 'doc', 'file'] },
  { label: 'Pictures', kinds: ['image', 'video'] },
  { label: 'Documents', kinds: ['doc'] },
  { label: 'Files', kinds: ['file'] },
];

/**
 * The kind filter. A count per chip, and a chip with nothing behind it is drawn
 * disabled rather than hidden: a strip that changes shape per session is a strip
 * you have to re-read every time.
 */
function FilterStrip({ list }: { list: Artifact[] }) {
  return (
    <div className="flex shrink-0 items-center gap-1.5 overflow-x-auto border-b border-dialog-edge bg-panel px-3 py-2 sm:px-4">
      {FILTERS.map((filter, index) => {
        const count = list.filter((entry) => filter.kinds.includes(entry.kind)).length;
        const on = index === 0;
        return (
          <span
            key={filter.label}
            className={`inline-flex min-h-6 shrink-0 items-center gap-1.5 border px-2 font-mono text-meta ${
              on
                ? 'border-accent bg-hover font-bold text-white'
                : count
                  ? 'border-edge text-dialog-hint'
                  : 'border-edge text-dialog-hint opacity-40'
            }`}
          >
            {filter.label}
            <span className={on ? 'text-accent-ink' : ''}>{count}</span>
          </span>
        );
      })}
    </div>
  );
}

/** The one line that says what a tap buys: zoom, draw, send it back. */
function SurfaceFooter() {
  return (
    <footer className="shrink-0 border-t border-dialog-edge bg-panel-2 px-3 py-1.5 font-mono text-chip text-dialog-hint sm:px-4">
      Tap to open · pinch to zoom · draw on it and{' '}
      <span className="font-bold text-white">Attach to message</span> sends the picture back to the
      model
    </footer>
  );
}

/**
 * A — GRID SHEET. The chip opens a full-width sheet over the transcript: kind
 * filter, then every artifact newest-first as a tile, two columns on a phone,
 * three on a tablet, five under a mouse. The Photos mental model, so nothing has
 * to be learned; and it scales to the session that produced ninety figures.
 *
 * What it gives up: order is time, not meaning. A grid of eight PNGs from four
 * different turns says nothing about WHICH question produced which picture — the
 * turn number in the meta line is all the provenance there is.
 */
export function ArtifactsSheetVariant({ state }: { state: string }) {
  const list = artifactsFor(state);
  const open = state !== 'empty' && state !== 'shut';
  return (
    <SessionMock
      count={list.length}
      open={open}
      overlay={
        open ? (
          <div className="absolute inset-0 flex flex-col border-t border-dialog-edge bg-ink">
            <SurfaceHeader list={list} />
            <FilterStrip list={list} />
            <div className="min-h-0 flex-1 overflow-y-auto p-3 sm:p-4">
              <div className="grid grid-cols-2 gap-2 sm:grid-cols-3 sm:gap-3 mouse:grid-cols-5">
                {list.map((artifact) => (
                  <Tile key={artifact.name} artifact={artifact} thumb="h-24 sm:h-28" />
                ))}
              </div>
            </div>
            <SurfaceFooter />
          </div>
        ) : null
      }
    />
  );
}

/**
 * B — BY TURN. The same sheet, indexed the way the session actually happened:
 * one band per turn, carrying the question that produced the batch and the tool
 * that made it, with a jump back into the transcript. Provenance is the primary
 * key, so "the chart from when I asked about latency" is findable by MEMORY
 * rather than by scanning thumbnails.
 *
 * What it gives up: density. Forty turns of one figure each is forty banners,
 * and the same list under A's grid would have fit on one screen.
 */
export function ArtifactsTurnsVariant({ state }: { state: string }) {
  const list = artifactsFor(state);
  const open = state !== 'empty';
  const turns = Array.from(new Set(list.map((entry) => entry.turn)));
  const asked: Record<number, string> = {
    6: 'Chart 2024 revenue by quarter',
    5: 'Give me the report as a PDF',
    4: 'Profile the slow endpoint',
    3: 'Run the suite with coverage',
  };
  return (
    <SessionMock
      count={list.length}
      open={open}
      overlay={
        open ? (
          <div className="absolute inset-0 flex flex-col border-t border-dialog-edge bg-ink">
            <SurfaceHeader list={list}>
              <span className="block font-mono text-meta text-dialog-hint">
                newest turn first · {turns.length} turns produced something
              </span>
            </SurfaceHeader>
            <div className="min-h-0 flex-1 overflow-y-auto">
              {turns.map((turn) => {
                const batch = list.filter((entry) => entry.turn === turn);
                const head = batch[0];
                return (
                  <section key={turn}>
                    <header className="flex items-center justify-between gap-2 border-b border-dialog-edge bg-panel px-3 py-1.5 sm:px-4">
                      <span className="min-w-0">
                        <span className="block truncate font-mono text-meta font-bold tracking-[0.08em] text-white uppercase">
                          Turn {turn} · {head.at} · {head.tool}
                        </span>
                        <span className="block truncate font-mono text-chip text-dialog-hint">
                          “{asked[turn]}”
                        </span>
                      </span>
                      <span className="shrink-0 font-mono text-chip text-link">In transcript ↗</span>
                    </header>
                    <div className="grid grid-cols-2 gap-2 p-3 sm:grid-cols-3 sm:gap-3 sm:p-4 mouse:grid-cols-5">
                      {batch.map((artifact) => (
                        <Tile key={artifact.name} artifact={artifact} thumb="h-24 sm:h-28" />
                      ))}
                    </div>
                  </section>
                );
              })}
            </div>
            <SurfaceFooter />
          </div>
        ) : null
      }
    />
  );
}

/**
 * C — DOCK. No overlay at all. The chip pins a band that lives WITH the
 * transcript: a swipeable filmstrip under the header on a phone, and a real
 * column beside the transcript once there is room, so you keep reading while
 * you scan what came out. Nothing is covered, nothing has to be dismissed, and
 * a fresh session never grows the band at all.
 *
 * What it gives up: the phone strip is one row — the ninth artifact is a swipe
 * away and there is no filter, so the session that produced ninety figures
 * needs the sheet after all.
 */
export function ArtifactsDockVariant({ state }: { state: string }) {
  const list = artifactsFor(state);
  const open = state !== 'empty';
  return (
    <SessionMock
      count={list.length}
      open={open}
      under={
        open ? (
          <div className="shrink-0 border-b border-dialog-edge bg-panel sm:hidden">
            <div className="flex items-center justify-between gap-2 px-3 pt-1.5">
              <span className="font-mono text-chip font-bold tracking-[0.08em] text-white uppercase">
                Artifacts {list.length}
              </span>
              <span className="font-mono text-chip text-dialog-hint">
                {totalLabel(list)} · swipe →
              </span>
            </div>
            <div className="flex gap-2 overflow-x-auto px-3 py-2">
              {list.map((artifact) => (
                <span key={artifact.name} className="w-28 shrink-0">
                  <Tile artifact={artifact} thumb="h-16" />
                </span>
              ))}
            </div>
          </div>
        ) : null
      }
      aside={
        open ? (
          <aside className="hidden w-64 shrink-0 flex-col border-l border-dialog-edge bg-panel sm:flex mouse:w-72">
            <header className="shrink-0 border-b border-dialog-edge px-3 py-2">
              <span className="block font-mono text-ui font-bold text-white">Artifacts</span>
              <span className="block font-mono text-chip text-dialog-hint">
                {list.length} produced · {totalLabel(list)}
              </span>
            </header>
            <div className="min-h-0 flex-1 overflow-y-auto">
              {list.map((artifact) => (
                <span
                  key={artifact.name}
                  className="flex min-h-14 items-center gap-2 border-b border-dialog-edge px-2 py-1.5 hover:bg-hover"
                >
                  <Thumb artifact={artifact} className="size-11 shrink-0 border" />
                  <span className="min-w-0 flex-1">
                    <span className="block truncate font-mono text-meta font-bold text-white">
                      {artifact.name}
                    </span>
                    <Meta artifact={artifact} withTool />
                  </span>
                </span>
              ))}
            </div>
            <footer className="shrink-0 border-t border-dialog-edge px-3 py-1.5 font-mono text-chip text-dialog-hint">
              Open · zoom · draw · attach back
            </footer>
          </aside>
        ) : null
      }
    />
  );
}
