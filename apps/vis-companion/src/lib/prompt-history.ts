/**
 * Shell-like recall of earlier prompts in the composer (#372).
 *
 * The rules match the terminal app's recall ring: the newest 20 prompts of the
 * session, oldest first, with an immediate repeat kept once. Slash commands stay
 * out, except saveable ones such as `/goal` (#360). A Council turn is not recalled,
 * because the human did not write it.
 */

export const PROMPT_HISTORY_LIMIT = 20;

export interface PromptHistorySource {
  request?: string;
  request_kind?: string;
}

/** The position in the history, and the draft that the first ↑ put aside. */
export interface PromptRecall {
  index: number;
  draft: string;
}

/** True when `text` has the command shape: one leading slash, no slash in its first token. */
export function isCommandText(text: string): boolean {
  return /^\/[^\s/]+(?:\s|$)/.test(text.trimStart());
}

function isRemembered(text: string, saveable: ReadonlySet<string>): boolean {
  if (!isCommandText(text)) return true;
  const command = text.trimStart().split(/\s/, 1)[0].slice(1).toLowerCase();
  return saveable.has(command);
}

/** The recallable prompts of `turns`, oldest first. `saveable` holds command names without the slash. */
export function promptHistory(
  turns: readonly PromptHistorySource[],
  saveable: ReadonlySet<string> = new Set(),
): string[] {
  const texts: string[] = [];
  for (const turn of turns) {
    if (turn.request_kind === 'council') continue;
    const text = (turn.request ?? '').trim();
    if (!text || !isRemembered(text, saveable)) continue;
    if (texts[texts.length - 1] !== text) texts.push(text);
  }
  return texts.slice(-PROMPT_HISTORY_LIMIT);
}

/** True when the caret has no line above it, so ↑ cannot move it. */
export function isCaretOnFirstLine(value: string, start: number, end: number): boolean {
  return start === end && !value.slice(0, start).includes('\n');
}

/** True when the caret has no line below it, so ↓ cannot move it. */
export function isCaretOnLastLine(value: string, start: number, end: number): boolean {
  return start === end && !value.slice(end).includes('\n');
}

export interface PromptRecallStep {
  recall: PromptRecall | null;
  text: string;
}

/**
 * One ↑ (`-1`) or ↓ (`1`) press. Returns the new recall state and the composer
 * text, or null when the press has nothing to do: ↑ at the oldest prompt or
 * with no history, and ↓ outside a recall. The ↓ after the newest prompt gives
 * back the draft that the first ↑ put aside.
 */
export function stepPromptRecall(
  history: readonly string[],
  recall: PromptRecall | null,
  direction: -1 | 1,
  current: string,
): PromptRecallStep | null {
  if (direction < 0) {
    if (!history.length) return null;
    if (!recall) {
      const index = history.length - 1;
      return { recall: { index, draft: current }, text: history[index] };
    }
    if (recall.index <= 0) return null;
    const index = Math.min(recall.index, history.length) - 1;
    return { recall: { ...recall, index }, text: history[index] };
  }
  if (!recall) return null;
  const index = recall.index + 1;
  if (index >= history.length) return { recall: null, text: recall.draft };
  return { recall: { ...recall, index }, text: history[index] };
}
