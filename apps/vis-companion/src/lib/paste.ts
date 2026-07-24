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
  return text.replace(PLACEHOLDER, (whole, rawId: string) => pastes.get(Number(rawId))?.content ?? whole);
}

export function collapsePastePlaceholders(text: string, pastes: Map<number, ComposerPaste>): string {
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
    if (start > offset) parts.push({ type: 'text', text: text.slice(offset, start), key: `text-${index++}` });
    if (match[1] === 'image') {
      // The picture itself renders below from the DB-owned base64 attachment;
      // here we only surface the `[Image #N: ...]` caption, never the temp path.
      parts.push({ type: 'image', summary: match[2].trim(), key: `image-${index++}-${prefixStart}` });
    } else {
      parts.push({ type: 'paste', summary: match[2].trim(), content: match[3], key: `paste-${index++}-${prefixStart}` });
    }
    offset = start + match[0].length;
    if (text[offset] === '\n') offset += 1;
  }
  if (offset < text.length) parts.push({ type: 'text', text: text.slice(offset), key: `text-${index}` });
  return parts.length ? parts : [{ type: 'text', text, key: 'text-0' }];
}

function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes}B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)}KB`;
  return `${(bytes / 1024 / 1024).toFixed(1)}MB`;
}
