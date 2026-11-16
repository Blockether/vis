/**
 * BARE URLS IN A MESSAGE THAT THE USER SENDS (#446).
 *
 * The bubble shows the text as the user typed it. Each bare `http://` or `https://` URL
 * becomes a link; the text that the model gets and the session keeps stays the same.
 * A bare `file:///` URL names a file on the gateway machine (#449). It is a link too, but
 * the client opens it in its own file preview, not in a browser.
 * The TUI uses the same rule, and `vis-contract/fixtures/message-links.json` holds the
 * cases of both clients.
 */

/** A bare URL ends before a space, an angle bracket, a quote or a backtick. */
const URL_TEXT = '[^\\s<>"`]';
/** A bare URL that does not continue a word; group 2 is the URL. */
const BARE_URL = new RegExp(`(^|[^A-Za-z0-9_])((?:https?://|file:///)${URL_TEXT}+)`, 'gi');
const URL_START = new RegExp(`^${URL_TEXT}*`);
/** A link needs more than its scheme. A file URL must name an absolute path. */
const LINK = /^(?:https?:\/\/|file:\/\/\/)./i;
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

/** The bare URL that `text` starts with: it ends before a quote, without its trailing punctuation. */
export function bareUrlAt(text: string): string {
  return trimUrl(URL_START.exec(text)?.[0] ?? '');
}

type MarkdownNode = {
  type: string;
  value?: string;
  url?: string;
  children?: MarkdownNode[];
  position?: { start: { offset?: number }; end: { offset?: number } };
};

/** Cut each bare link below `parent` to `bareUrlAt` and give the rest back as text and links. */
function trimBareLinks(parent: MarkdownNode): void {
  const nodes = parent.children ?? [];
  for (let i = 0; i < nodes.length; i += 1) {
    const node = nodes[i];
    const label = node.children?.length === 1 ? node.children[0] : undefined;
    const span = (node.position?.end.offset ?? NaN) - (node.position?.start.offset ?? NaN);
    // The source of a bare link is its label alone: `<url>` and `[label](url)` are longer.
    const bare =
      node.type === 'link' && label?.type === 'text' && !!label.value && span === label.value.length;
    if (!bare || !label?.value || !node.url?.endsWith(label.value)) {
      trimBareLinks(node);
      continue;
    }
    const kept = bareUrlAt(label.value);
    const rest = label.value.slice(kept.length);
    if (!rest) continue;
    node.url = node.url.slice(0, -rest.length);
    label.value = kept;
    // The rest can hold the next URL: `"a":"https://a.example/x","b":"https://b.example/y"`.
    const tail = messageLinkParts(rest).map((part) =>
      typeof part === 'string'
        ? { type: 'text', value: part }
        : { type: 'link', url: part.url, children: [{ type: 'text', value: part.url }] },
    );
    nodes.splice(i + 1, 0, ...tail);
  }
}

/**
 * A BARE URL IN MARKDOWN ENDS AS IT ENDS IN A SENT MESSAGE. GitHub markdown ends a bare URL
 * only at a space or at `<`, so the link of `{"url": "https://a.example/x"}` also took `"}`.
 * This remark plugin cuts each bare link to the rule of this file. A link that the author
 * marked, `<url>` or `[label](url)`, stays as written.
 */
export function remarkBareLinks() {
  return (tree: MarkdownNode): void => trimBareLinks(tree);
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
    if (code.some(([from, to]) => start >= from && start < to) || !LINK.test(url))
      continue;
    if (start > at) parts.push(text.slice(at, start));
    parts.push({ url });
    at = start + url.length;
  }
  if (at < text.length) parts.push(text.slice(at));
  return parts;
}

/**
 * The absolute path that a `file:///` URL names, with its percent encoding decoded.
 * Returns `null` for another URL or for an encoding that does not decode.
 */
export function filePathOfUrl(url: string): string | null {
  if (!/^file:\/\/\/./i.test(url)) return null;
  try {
    return decodeURIComponent(new URL(url).pathname);
  } catch {
    return null;
  }
}
