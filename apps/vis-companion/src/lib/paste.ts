export const PASTE_INLINE_MAX_CHARS = 80;

export interface ComposerPaste {
  id: number;
  content: string;
  token: string;
}

export type UserMessagePart =
  | { type: 'text'; text: string; key: string }
  | { type: 'paste'; summary: string; content: string; key: string };

const PLACEHOLDER = /\[Pasted #(\d+): [^\]]*?\]/g;
const PASTE_FENCE = /(?:^|\n)````vis-paste\n([^\n]*)\n([\s\S]*?)\n````(?=\n|$)/g;

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
  for (const match of text.matchAll(PASTE_FENCE)) {
    const start = match.index ?? 0;
    const prefixStart = text[start] === '\n' ? start + 1 : start;
    if (start > offset) parts.push({ type: 'text', text: text.slice(offset, start), key: `text-${index++}` });
    parts.push({
      type: 'paste',
      summary: match[1].trim(),
      content: match[2],
      key: `paste-${index++}-${prefixStart}`,
    });
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
