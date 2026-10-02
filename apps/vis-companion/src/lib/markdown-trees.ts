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

/** A remark plugin that sends the parser of its processor through `cachedTree`. */
export const remarkParseCache: RemarkPlugin = function remarkParseCache() {
  const parse = this.parser;
  if (parse) this.parser = (text, file) => cachedTree(text, () => parse(text, file));
};

registerMemorySource('markdown trees', () => {
  let bytes = 0;
  for (const [text, tree] of trees) bytes += approxBytes(text) + approxBytes(tree);
  return [{ source: 'markdown trees', bytes, entries: trees.size }];
});
