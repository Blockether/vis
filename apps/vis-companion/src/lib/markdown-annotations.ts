/**
 * COMMENTS ON A MARKDOWN ARTIFACT, KEPT IN THE ARTIFACT ITSELF.
 *
 * A note the model wrote is a file, and the human's remarks about it have to
 * survive being read on another device, exported, or opened by the model again
 * next turn. So an annotation is not a side table keyed by a character offset
 * that the next revision invalidates: it is markdown, appended to the document
 * under one `## Comments` heading, and saving it is simply the NEXT VERSION of
 * that same filename.
 *
 * One line per comment — the quoted selection, then what the human said — so
 * the section reads as prose in any markdown renderer, this one included, and
 * round-trips through `parseAnnotated`/`renderAnnotated` unchanged.
 */

export interface MarkdownComment {
  /** The passage the human selected, collapsed onto one line. */
  quote: string;
  /** What they said about it. */
  body: string;
}

export const COMMENTS_HEADING = "## Comments";

const COMMENT_LINE = /^- \*\*“(.*)”\*\* — (.*)$/;

/**
 * A remark about the WHOLE note, not about one passage.
 *
 * Not every thing a human has to say is about a sentence — "this plan is out of
 * date" is about the document. Such a comment carries no quote, and is written
 * out under its own marker so the section still round-trips.
 */
export const GENERAL_LABEL = "Whole document";

const GENERAL_LINE = /^- \*\*Whole document\*\* — (.*)$/;

/** One line, no markers that could close the ones this format opens. */
function oneLine(text: string): string {
  return text.replace(/[“”]/g, '"').replace(/\s+/g, " ").trim();
}

/** The document split into what it says and what has been said ABOUT it. */
export function parseAnnotated(text: string): {
  body: string;
  comments: MarkdownComment[];
} {
  const at = text.lastIndexOf(`\n${COMMENTS_HEADING}\n`);
  if (at < 0) return { body: text, comments: [] };
  const comments: MarkdownComment[] = [];
  for (const line of text.slice(at + COMMENTS_HEADING.length + 2).split("\n")) {
    const general = GENERAL_LINE.exec(line.trim());
    if (general) {
      comments.push({ quote: "", body: general[1] });
      continue;
    }
    const hit = COMMENT_LINE.exec(line.trim());
    if (hit) comments.push({ quote: hit[1], body: hit[2] });
  }
  // A `## Comments` section this app did not write is left where it is.
  if (comments.length === 0) return { body: text, comments: [] };
  return { body: text.slice(0, at).replace(/\s+$/, ""), comments };
}

/** The document to SAVE: the prose, then the comments section, or no section. */
export function renderAnnotated(
  body: string,
  comments: MarkdownComment[],
): string {
  const prose = body.replace(/\s+$/, "");
  const kept = comments.filter((entry) => oneLine(entry.body).length > 0);
  if (kept.length === 0) return `${prose}\n`;
  const lines = kept.map((entry) =>
    oneLine(entry.quote).length === 0
      ? `- **${GENERAL_LABEL}** — ${oneLine(entry.body)}`
      : `- **“${oneLine(entry.quote)}”** — ${oneLine(entry.body)}`,
  );
  return `${prose}\n\n${COMMENTS_HEADING}\n\n${lines.join("\n")}\n`;
}

/**
 * The selection a human made, as a quote worth reading back. Long passages are
 * elided in the MIDDLE so both ends still identify the sentence.
 */
export const QUOTE_LIMIT = 160;

export function quoteOf(selection: string): string {
  const text = oneLine(selection);
  if (text.length <= QUOTE_LIMIT) return text;
  const half = Math.floor((QUOTE_LIMIT - 1) / 2);
  return `${text.slice(0, half).trim()}…${text.slice(-half).trim()}`;
}
