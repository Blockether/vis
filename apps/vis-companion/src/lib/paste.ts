import type { ClipboardEvent } from 'react';

export const PASTE_INLINE_MAX_CHARS = 80;

export interface ComposerPaste {
  id: number;
  content: string;
  token: string;
}

export type UserMessagePart =
  | { type: 'text'; text: string; key: string }
  | { type: 'paste'; summary: string; content: string; key: string }
  | { type: 'image'; summary: string; key: string };

const PLACEHOLDER = /\[Pasted #(\d+): [^\]]*?\]/g;
// Both `vis-paste` (large text) and `vis-image` (a dropped/pasted picture) collapse
// to a four-backtick fence whose FIRST body line is the `[Pasted #N]` / `[Image #N]`
// summary token. The image fence's remaining lines are the temp source path + metadata
// that the TUI reads to redraw the picture — never shown verbatim to the user.
const FENCE = /(?:^|\n)````vis-(paste|image)\n([^\n]*)\n([\s\S]*?)\n````(?=\n|$)/g;
// A BARE image path in the prose: what a channel sends when the picture was not
// collapsed into a `vis-image` fence (an older turn, or a path typed by hand).
// Mirrors the engine's `attachments/image-path-token-pattern`, but requires a
// path separator so an ordinary `logo.png` mentioned in a sentence stays text.
const IMAGE_PATH =
  /(?:^|\s)([^\s"'<>|]*\/[^\s"'<>|]*\.(?:png|jpe?g|gif|webp|bmp))(?=$|[\s.,;:!?)\]}'"])/gi;

export function shouldCollapsePaste(text: string): boolean {
  return text.includes('\n') || text.length > PASTE_INLINE_MAX_CHARS;
}

export function createComposerPaste(id: number, content: string): ComposerPaste {
  return { id, content, token: pasteSummary(id, content) };
}

export function pasteSummary(id: number, content: string): string {
  const lines = content.split('\n').length;
  return `[Pasted #${id}: ${lines} ${lines === 1 ? 'line' : 'lines'}, ${formatBytes(new TextEncoder().encode(content).byteLength)}]`;
}

export function expandPastePlaceholders(text: string, pastes: Map<number, ComposerPaste>): string {
  return text.replace(
    PLACEHOLDER,
    (whole, rawId: string) => pastes.get(Number(rawId))?.content ?? whole,
  );
}

export function collapsePastePlaceholders(
  text: string,
  pastes: Map<number, ComposerPaste>,
): string {
  return text.replace(PLACEHOLDER, (whole, rawId: string) => {
    const paste = pastes.get(Number(rawId));
    if (!paste) return whole;
    return `\n\`\`\`\`vis-paste\n${pasteSummary(paste.id, paste.content)}\n${paste.content}\n\`\`\`\`\n`;
  });
}

export function parseUserMessage(text: string): UserMessagePart[] {
  const parts: UserMessagePart[] = [];
  let offset = 0;
  let index = 0;
  for (const match of text.matchAll(FENCE)) {
    const start = match.index ?? 0;
    const prefixStart = text[start] === '\n' ? start + 1 : start;
    if (start > offset)
      parts.push({ type: 'text', text: text.slice(offset, start), key: `text-${index++}` });
    if (match[1] === 'image') {
      // The picture itself renders below from the DB-owned base64 attachment;
      // here we only surface the `[Image #N: ...]` caption, never the temp path.
      parts.push({
        type: 'image',
        summary: match[2].trim(),
        key: `image-${index++}-${prefixStart}`,
      });
    } else {
      parts.push({
        type: 'paste',
        summary: match[2].trim(),
        content: match[3],
        key: `paste-${index++}-${prefixStart}`,
      });
    }
    offset = start + match[0].length;
    if (text[offset] === '\n') offset += 1;
  }
  if (offset < text.length)
    parts.push({ type: 'text', text: text.slice(offset), key: `text-${index}` });
  const flat = parts.length ? parts : [{ type: 'text', text, key: 'text-0' } as UserMessagePart];
  return flat.flatMap((part) => (part.type === 'text' ? splitImagePaths(part) : [part]));
}

// Collapse every bare image path inside ONE text part into its own chip part,
// keeping the prose around it. The raw text is never mutated upstream, so edit /
// re-send still ships the path that re-attaches the picture.
function splitImagePaths(part: { type: 'text'; text: string; key: string }): UserMessagePart[] {
  const out: UserMessagePart[] = [];
  let offset = 0;
  let index = 0;
  for (const match of part.text.matchAll(IMAGE_PATH)) {
    const path = match[1];
    const start = (match.index ?? 0) + match[0].indexOf(path);
    const before = part.text.slice(offset, start);
    if (before) out.push({ type: 'text', text: before, key: `${part.key}-t${index++}` });
    out.push({
      type: 'image',
      summary: path.slice(path.lastIndexOf('/') + 1),
      key: `${part.key}-i${index++}`,
    });
    offset = start + path.length;
  }
  if (!out.length) return [part];
  const rest = part.text.slice(offset);
  if (rest) out.push({ type: 'text', text: rest, key: `${part.key}-t${index}` });
  return out;
}

// The tags of a user message row: one authored line, or one paste.
const ROW_TAGS = new Set(['P', 'DIV', 'DETAILS']);

/**
 * Copy a selection inside the parts of one user message with each closed paste written
 * out whole: the clipboard gets the pasted text, never its `[Pasted #N]` summary. A
 * selection that leaves the message bubble or touches no paste keeps the native copy.
 */
export function copyWithPastes(event: ClipboardEvent<HTMLElement>): void {
  const root = event.currentTarget;
  const selection = root.ownerDocument.getSelection();
  if (!selection || selection.isCollapsed || !selection.rangeCount) return;
  const range = selection.getRangeAt(0);
  // Selecting the whole bubble starts the range on the bubble, around the parts.
  if (!(root.parentElement ?? root).contains(range.commonAncestorContainer)) return;
  const pastes = Array.from(root.querySelectorAll('details[data-paste]'));
  if (!pastes.some((paste) => range.intersectsNode(paste))) return;
  let text = '';
  let hasRow = false;
  let afterBlock = false;
  for (const child of Array.from(root.childNodes)) {
    if (!range.intersectsNode(child)) continue;
    const block = child instanceof Element && ROW_TAGS.has(child.tagName);
    if (hasRow && (block || afterBlock)) text += '\n';
    text += selectedText(child, range);
    hasRow = true;
    afterBlock = block;
  }
  event.preventDefault();
  event.clipboardData.setData('text/plain', text);
}

function selectedText(node: Node, range: Range): string {
  if (!range.intersectsNode(node)) return '';
  if (node.nodeType === Node.TEXT_NODE) {
    const text = node.textContent ?? '';
    const start = node === range.startContainer ? range.startOffset : 0;
    return text.slice(start, node === range.endContainer ? range.endOffset : text.length);
  }
  if (!(node instanceof Element) || node.tagName === 'SUMMARY') return '';
  if (node instanceof HTMLDetailsElement && node.hasAttribute('data-paste')) {
    // A closed paste, or a selection of only its summary, stands for the whole paste.
    const body = node.querySelector('pre');
    if (!node.open || !body || !range.intersectsNode(body))
      return node.querySelector('code')?.textContent ?? '';
  }
  return Array.from(node.childNodes, (child) => selectedText(child, range)).join('');
}

function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes}B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)}KB`;
  return `${(bytes / 1024 / 1024).toFixed(1)}MB`;
}
