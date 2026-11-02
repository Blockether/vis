import type { Options } from 'react-markdown';
import { approxBytes, registerMemorySource } from './perf';

/**
 * A SESSION OPENED AGAIN DOES NOT PARSE ITS MARKDOWN AGAIN. Every switch remounts
 * the session screen, and react-markdown parses each message when it mounts. In a
 * long transcript that parse is most of the time between the tap and the frame
 * that shows the session. This cache keeps the syntax tree of each text the app
 * parsed, keyed by the text, so a later parse of the same text takes a copy.
 *
 * The cache wraps remark-parse and runs before every transform. Each plugin list
 * that carries it parses with remark-gfm; remark-breaks only transforms. A plugin
 * that changes how text parses needs a cache of its own.
 *
 * Transforms edit the tree in place: remark-breaks splits text at each newline.
 * The cache therefore keeps its own copy and gives every parse a fresh clone,
 * which costs about a tenth of the parse it replaces.
 *
 * A STREAMED TEXT PARSES ONLY ITS OPEN BLOCKS. Each flush of a stream gives the
 * message a longer text. A parse of the whole text made each flush cost more than
 * the flush before it. In markdown only the last top-level block of a text can
 * still change. The parse therefore notes where each top-level block starts, and
 * `advanceSplit` cuts a growing text after its closed blocks.
 */

/** Characters of source text the kept trees may cover. A tree takes 8 to 25 bytes per character. */
export const MARKDOWN_TREE_BUDGET = 1_000_000;

/** What one kept tree costs beyond its text, in characters: the map slot and the root. */
const ENTRY_COST = 256;

/** Texts that can grow at the same time: an answer, its reasoning and a tool result. */
const STREAMS = 4;

const trees = new Map<string, unknown>();
let held = 0;
let recent: string[] = [];

function cost(text: string): number {
  return text.length + ENTRY_COST;
}

/** Keep `tree` as the newest entry and drop the least recently used past the budget. */
function keep(text: string, tree: unknown): void {
  if (cost(text) > MARKDOWN_TREE_BUDGET) return;
  trees.set(text, tree);
  held += cost(text);
  for (const oldest of trees.keys()) {
    if (held <= MARKDOWN_TREE_BUDGET) break;
    trees.delete(oldest);
    held -= cost(oldest);
  }
}

/**
 * The tree of `text`: a copy of the kept tree, or what `parse` returns.
 *
 * A streamed text grows by appending, so each state of a stream extends the one
 * before it. Such a state is parsed but not kept: it does not render again, and
 * one long stream would otherwise push every finished text out. A stream's text
 * is kept when it is parsed again unchanged, as the transcript shows it.
 */
export function cachedTree<T>(text: string, parse: () => T): T {
  const kept = trees.get(text);
  if (kept !== undefined) {
    trees.delete(text);
    trees.set(text, kept);
    return structuredClone(kept) as T;
  }
  const tree = parse();
  if (!text) return tree;
  const stream = recent.findIndex((before) => text.length > before.length && text.startsWith(before));
  if (stream >= 0) {
    recent[stream] = text;
    return tree;
  }
  recent = [text, ...recent.filter((before) => before !== text)].slice(0, STREAMS);
  keep(text, structuredClone(tree));
  return tree;
}

type RemarkPlugin = NonNullable<Options['remarkPlugins']>[number];

/** Recent texts whose block starts stay known: the stream tails and their earlier states. */
const NOTED_STARTS = 32;

const starts = new Map<string, readonly number[]>();

type ParsedRoot = { children?: Array<{ position?: { start?: { offset?: number } } }> };

/** Note where each top-level block of `text` starts in its parsed `tree`. */
function noteStarts(text: string, tree: unknown): void {
  const offsets: number[] = [];
  for (const child of (tree as ParsedRoot).children ?? []) {
    const offset = child.position?.start?.offset;
    if (offset === undefined) return;
    offsets.push(offset);
  }
  starts.delete(text);
  starts.set(text, offsets);
  for (const oldest of starts.keys()) {
    if (starts.size <= NOTED_STARTS) break;
    starts.delete(oldest);
  }
}

/** Where each top-level block of `text` starts, if the app parsed `text` recently. */
export function blockStartsOf(text: string): readonly number[] | undefined {
  return starts.get(text);
}

/**
 * A remark plugin that sends the parser of its processor through `cachedTree`. It also
 * notes the block starts of each parsed text for `advanceSplit`.
 */
export const remarkParseCache: RemarkPlugin = function remarkParseCache() {
  const parse = this.parser;
  if (!parse) return;
  this.parser = (text, file) => {
    const tree = cachedTree(text, () => parse(text, file));
    noteStarts(text, tree);
    return tree;
  };
};

/** A run of whole top-level blocks, at `start` in the text that it comes from. */
export interface SplitPiece {
  start: number;
  text: string;
}

/** `text` in pieces that join to it. Only the last piece can still change. */
export interface MarkdownSplit {
  text: string;
  pieces: readonly SplitPiece[];
}

/** `text` as one piece. */
export function wholeSplit(text: string): MarkdownSplit {
  return { text, pieces: [{ start: 0, text }] };
}

/**
 * A line that can define a link or a footnote, such as `[guide]: https://…` or `[^1]: …`.
 * A definition serves the links of all blocks, so a text with a definition stays whole.
 */
const DEFINITION = /^[ \t>*+\-\d.)]*\[(?:[^\\\]]|\\[\s\S])*\]:/m;

/**
 * Where the closed blocks of `text` end: at the start of the line where its last block opens.
 *
 * A block that opens on the unfinished last line can still change. `#` alone is a heading,
 * but `#x` continues the paragraph above it. Then that block and the block before it stay open.
 */
function closedLength(text: string, blocks: readonly number[] | undefined): number {
  if (!blocks || blocks.length < 2) return 0;
  let last = blocks.length - 1;
  if (!text.includes('\n', blocks[last])) last -= 1;
  if (last < 1) return 0;
  const cut = text.lastIndexOf('\n', blocks[last] - 1) + 1;
  return cut > 0 && /^[ \t]*$/.test(text.slice(cut, blocks[last])) ? cut : 0;
}

/**
 * The split of `text`, after the split of an earlier state of the same text.
 *
 * In markdown only the last top-level block of a text can still change. A block closes
 * when the next block opens on a finished line, and no later line opens it again. Thus
 * a closed block renders alone as it renders in the whole text. `startsOf` gives the
 * block starts of a parsed text. A text that does not extend the earlier state is one
 * piece again. The parse offsets do not count a byte order mark, so such a text stays whole.
 */
export function advanceSplit(
  previous: MarkdownSplit,
  text: string,
  startsOf: (text: string) => readonly number[] | undefined,
): MarkdownSplit {
  if (text === previous.text) return previous;
  if (!text.startsWith(previous.text) || text.startsWith('\uFEFF') || DEFINITION.test(text))
    return wholeSplit(text);
  const closed = previous.pieces.slice(0, -1);
  const tail = previous.pieces[previous.pieces.length - 1];
  const cut = closedLength(tail.text, startsOf(tail.text));
  const open = { start: tail.start + cut, text: text.slice(tail.start + cut) };
  if (!cut) return { text, pieces: [...closed, open] };
  return { text, pieces: [...closed, { start: tail.start, text: tail.text.slice(0, cut) }, open] };
}

registerMemorySource('markdown trees', () => {
  let bytes = 0;
  for (const [text, tree] of trees) bytes += approxBytes(text) + approxBytes(tree);
  return [{ source: 'markdown trees', bytes, entries: trees.size }];
});
