import { describe, expect, it } from 'vitest';

import {
  PROMPT_HISTORY_LIMIT,
  isCaretOnFirstLine,
  isCaretOnLastLine,
  promptHistory,
  stepPromptRecall,
} from './prompt-history';

describe('promptHistory', () => {
  it('keeps the human prompts, oldest first, without Council turns or blanks', () => {
    expect(
      promptHistory([
        { request: 'first' },
        { request: '  ' },
        { request: 'from a peer', request_kind: 'council' },
        { request: ' second ', request_kind: 'user' },
      ]),
    ).toEqual(['first', 'second']);
  });

  it('keeps an immediate repeat once, but a later repeat again', () => {
    expect(
      promptHistory([{ request: 'a' }, { request: 'a' }, { request: 'b' }, { request: 'a' }]),
    ).toEqual(['a', 'b', 'a']);
  });

  it('leaves commands out, except saveable ones and absolute paths', () => {
    expect(
      promptHistory(
        [
          { request: '/reload' },
          { request: '/goal ship it' },
          { request: '/GOAL again' },
          { request: '/tmp/log.txt explains the crash' },
        ],
        new Set(['goal']),
      ),
    ).toEqual(['/goal ship it', '/GOAL again', '/tmp/log.txt explains the crash']);
  });

  it('keeps only the newest prompts', () => {
    const turns = Array.from({ length: PROMPT_HISTORY_LIMIT + 5 }, (_, i) => ({ request: `p${i}` }));
    const history = promptHistory(turns);
    expect(history).toHaveLength(PROMPT_HISTORY_LIMIT);
    expect(history[0]).toBe('p5');
    expect(history.at(-1)).toBe(`p${PROMPT_HISTORY_LIMIT + 4}`);
  });
});

describe('stepPromptRecall', () => {
  const history = ['one', 'two', 'three'];

  it('steps back like a shell, then forward to the draft that ↑ put aside', () => {
    let step = stepPromptRecall(history, null, -1, 'half typed')!;
    expect(step.text).toBe('three');
    step = stepPromptRecall(history, step.recall, -1, step.text)!;
    expect(step.text).toBe('two');
    step = stepPromptRecall(history, step.recall, -1, step.text)!;
    expect(step.text).toBe('one');
    expect(stepPromptRecall(history, step.recall, -1, step.text)).toBeNull();

    step = stepPromptRecall(history, step.recall, 1, step.text)!;
    expect(step.text).toBe('two');
    step = stepPromptRecall(history, step.recall, 1, step.text)!;
    expect(step.text).toBe('three');
    step = stepPromptRecall(history, step.recall, 1, step.text)!;
    expect(step).toEqual({ recall: null, text: 'half typed' });
  });
});

describe('caret edges', () => {
  it('lets ↑ recall only from the first line and ↓ only from the last line', () => {
    const text = 'line one\nline two';
    expect(isCaretOnFirstLine(text, 3, 3)).toBe(true);
    expect(isCaretOnFirstLine(text, 12, 12)).toBe(false);
    expect(isCaretOnLastLine(text, 12, 12)).toBe(true);
    expect(isCaretOnLastLine(text, 3, 3)).toBe(false);
  });

  it('leaves a selection to the browser', () => {
    expect(isCaretOnFirstLine('abc', 0, 2)).toBe(false);
    expect(isCaretOnLastLine('abc', 0, 2)).toBe(false);
  });
});
