/**
 * Where the words of a session search appear in the text the search found.
 *
 * The gateway's full-text index reads words without case or accents and treats each
 * query word as the start of a word, so "cafe" finds "Café" and "sess" finds
 * "sessions". A highlight follows the same rules. Each query word is marked on its
 * own, wherever a word in the text starts with it — never the query as one phrase,
 * which the index does not require.
 */

const WORD_CHARACTER = /[\p{L}\p{N}]/u;
const WORD_SEPARATOR = /[^\p{L}\p{N}]+/u;

/** One part of a text: either words the search asked for, or the text between them. */
export type SearchSegment = { text: string; isMatch: boolean };

/**
 * Lower case without accents, one UTF-16 unit for each unit of `text`, so an index into
 * the result is the same index into the original.
 */
function foldSearchText(text: string): string {
  let folded = '';
  for (const character of text) {
    folded +=
      character.length === 1
        ? (character.toLowerCase().normalize('NFD')[0] ?? character)
        : character;
  }
  return folded;
}

/** The distinct words of a query, folded, longest first so a longer word wins a tie. */
export function searchTerms(query: string): string[] {
  const words = foldSearchText(query).split(WORD_SEPARATOR).filter(Boolean);
  return [...new Set(words)].sort((left, right) => right.length - left.length);
}

function startsWord(folded: string, index: number): boolean {
  return index === 0 || !WORD_CHARACTER.test(folded[index - 1]);
}

/** Where the terms start words in `text`, as ordered `[start, end)` ranges that never overlap. */
export function searchRanges(text: string, terms: readonly string[]): [number, number][] {
  if (!text || terms.length === 0) return [];
  const folded = foldSearchText(text);
  const found: [number, number][] = [];
  for (const term of terms) {
    for (let at = folded.indexOf(term); at !== -1; at = folded.indexOf(term, at + 1)) {
      if (startsWord(folded, at)) found.push([at, at + term.length]);
    }
  }
  found.sort((left, right) => left[0] - right[0] || right[1] - left[1]);
  const merged: [number, number][] = [];
  for (const [start, end] of found) {
    const last = merged[merged.length - 1];
    if (last && start <= last[1]) last[1] = Math.max(last[1], end);
    else merged.push([start, end]);
  }
  return merged;
}

/** `text` cut into the words the search asked for and the text between them. */
export function searchSegments(text: string, terms: readonly string[]): SearchSegment[] {
  const segments: SearchSegment[] = [];
  let from = 0;
  for (const [start, end] of searchRanges(text, terms)) {
    if (start > from) segments.push({ text: text.slice(from, start), isMatch: false });
    segments.push({ text: text.slice(start, end), isMatch: true });
    from = end;
  }
  if (from < text.length) segments.push({ text: text.slice(from), isMatch: false });
  return segments;
}
