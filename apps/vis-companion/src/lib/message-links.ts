/**
 * BARE URLS IN A MESSAGE THAT THE USER SENDS (#446).
 *
 * The bubble shows the text as the user typed it. Each bare `http://` or `https://` URL
 * becomes a link; the text that the model gets and the session keeps stays the same.
 * The TUI uses the same rule, and `vis-contract/fixtures/message-links.json` holds the
 * cases of both clients.
 */

/** A bare URL that does not continue a word; group 2 is the URL. */
const BARE_URL = /(^|[^A-Za-z0-9_])(https?:\/\/[^\s<>"`]+)/gi;
/** A code span keeps its text, as in Markdown. */
const CODE_SPAN = /(`+)[\s\S]*?\1/g;
const TRAILING_PUNCTUATION = new Set(['.', ',', ':', ';', '!', '?', "'", '"', '*', '_', '~']);
const OPENING: Readonly<Record<string, string>> = { ')': '(', ']': '[', '}': '{' };

const count = (text: string, ch: string) => text.split(ch).length - 1;

/** Drop the trailing punctuation of `url`, and each closing bracket that it does not open. */
function trimUrl(url: string): string {
  let trimmed = url;
  for (;;) {
    const last = trimmed.at(-1) ?? '';
    const opening = OPENING[last];
    if (!TRAILING_PUNCTUATION.has(last) && !(opening && count(trimmed, last) > count(trimmed, opening)))
      return trimmed;
    trimmed = trimmed.slice(0, -1);
  }
}

export type MessageTextPart = string | { url: string };

/** The parts of `text`: plain text, and each bare URL outside a code span as a link. */
export function messageLinkParts(text: string): MessageTextPart[] {
  const code = [...text.matchAll(CODE_SPAN)].map((m) => [m.index, m.index + m[0].length]);
  const parts: MessageTextPart[] = [];
  let at = 0;
  for (const match of text.matchAll(BARE_URL)) {
    const start = match.index + match[1].length;
    const url = trimUrl(match[2]);
    if (code.some(([from, to]) => start >= from && start < to) || !/^https?:\/\/./i.test(url))
      continue;
    if (start > at) parts.push(text.slice(at, start));
    parts.push({ url });
    at = start + url.length;
  }
  if (at < text.length) parts.push(text.slice(at));
  return parts;
}
