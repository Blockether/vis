import type { SuggestRow } from "./VisClient";

/* Composer-side trigger smarts for the gateway's canonical suggestion service.
   The gateway owns ranking (/v1/sessions/:sid/suggest); the client only decides
   when the operator is currently typing a file sigil. `@@` escapes a literal @,
   matching the gateway comment in server.clj. */
export type FileMention = { start: number; end: number; q: string };

export const activeFileMention = (text: string, cursor = text.length): FileMention | null => {
  const head = text.slice(0, cursor);
  const match = /(^|\s)@([^@\s]*)$/.exec(head);
  if (!match || match.index == null) return null;
  const at = match.index + match[1]!.length;
  if (at > 0 && text[at - 1] === "@") return null;
  return { start: at, end: cursor, q: match[2] ?? "" };
};

export const applyFileMention = (text: string, mention: FileMention, row: SuggestRow): string => {
  const value = row.name.trim();
  const before = text.slice(0, mention.start);
  const after = text.slice(mention.end);
  return `${before}@${value} ${after.replace(/^\s*/, "")}`;
};

export const suggestLabel = (row: SuggestRow): string => {
  const bits = [row.status, row.age, typeof row.size === "number" ? `${row.size}b` : undefined].filter(
    Boolean
  );
  return bits.join(" · ");
};
