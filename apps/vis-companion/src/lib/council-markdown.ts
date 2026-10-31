const FENCE = /^ {0,3}(`{3,}|~{3,})/;
const ITEM = /^(?:[-+*]|\d{1,9}[.)])(?:[ \t]|$)/;
const INDENT = /^[ \t]/;

function closesFence(fence: string, line: string): boolean {
  const match = FENCE.exec(line);
  return Boolean(
    match &&
      match[1][0] === fence[0] &&
      match[1].length >= fence.length &&
      !line.slice(match[0].length).trim(),
  );
}

/**
 * True when an authored line break between two lines starts a new block. Indented
 * lines, list items after list lines, table rows and quotes stay joined.
 */
function blockBreak(prev: string, line: string): boolean {
  return (
    prev.trim() !== '' &&
    line.trim() !== '' &&
    !INDENT.test(line) &&
    !(ITEM.test(line) && (ITEM.test(prev) || INDENT.test(prev))) &&
    !(line.includes('|') && prev.includes('|')) &&
    !(line.startsWith('>') && prev.startsWith('>')) &&
    !/^(?:=+|-+)[ \t]*$/.test(line)
  );
}

/**
 * Council text as Markdown where each authored line break outside a fence is a block
 * break. Agents write one line for each point; CommonMark would run those lines
 * together. Mirrors com.blockether.vis.tui.presentation/council-markdown.
 */
export function councilMarkdown(text: string): string {
  const out: string[] = [];
  let prev: string | null = null;
  let fence = '';
  for (const line of text.replace(/\r\n?/g, '\n').split('\n')) {
    if (!fence && prev !== null && blockBreak(prev, line)) out.push('');
    out.push(line);
    fence = fence ? (closesFence(fence, line) ? '' : fence) : (FENCE.exec(line)?.[1] ?? '');
    prev = line;
  }
  return out.join('\n');
}
