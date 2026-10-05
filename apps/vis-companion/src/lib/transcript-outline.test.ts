import { describe, expect, it } from 'vitest';

import { EDGE_MARGIN } from './anchored-menu';
import { pasteSummary } from './paste';
import {
  answerExcerpt,
  mergeOutline,
  outlineEntry,
  outlineStatus,
  previewPosition,
  promptLabel,
  readingTurnId,
  scrubIndex,
  scrubStep,
  type OutlineEntry,
  type OutlineStatus,
} from './transcript-outline';

/** A desktop window with room for the card and its preview. */
const DESKTOP = { width: 1440, height: 900 };

/** A paste as the composer sends it: a fence with its summary, then its content. */
const pasted = (id: number, content: string) =>
  ['````vis-paste', pasteSummary(id, content), content, '````'].join('\n');

describe('promptLabel', () => {
  it('cuts the image tokens of the composer out of the words', () => {
    expect(promptLabel('[IMAGE #1] cut this token')).toBe('cut this token');
    expect(promptLabel('look [IMAGE #2] at [Image #3] this')).toBe('look at this');
  });

  it('drops image captions and image paths', () => {
    const image = '````vis-image\n[Image #1: shot.png]\n/tmp/vis/shot.png\n````\nfix this';
    expect(promptLabel(image)).toBe('fix this');
    expect(promptLabel('compare /tmp/shots/a.png with this')).toBe('compare with this');
  });

  it('reads a short paste as its words and a long paste as its summary', () => {
    expect(promptLabel(`see this\n${pasted(1, 'a\nb\nc')}\nand fix it`)).toBe('see this a b c and fix it');
    const log = Array.from({ length: 60 }, (_, line) => `line ${line} of the log`).join('\n');
    expect(promptLabel(`${pasted(1, log)}\nwhy does this fail?`)).toBe(`${pasteSummary(1, log)} why does this fail?`);
  });

  it('reads each short paste of a prompt as its words, in order', () => {
    const quotes = [pasted(1, 'the notice\nafter the tests'), pasted(2, 'an attempt key\nper call'), 'yes, both'];
    expect(promptLabel(quotes.join('\n'))).toBe('the notice after the tests an attempt key per call yes, both');
  });

  it('names a prompt without words by what it holds', () => {
    expect(promptLabel('[IMAGE #1]')).toBe('Attachments only');
    expect(promptLabel('', 2)).toBe('Attachments only');
    expect(promptLabel('')).toBe('Empty message');
  });
});

describe('answerExcerpt', () => {
  it('keeps the words of Markdown and drops its marks', () => {
    expect(
      answerExcerpt('## Plan\n\n**Bold** and *italic* with `code` and [a link](https://x.y).'),
    ).toBe('Plan Bold and italic with code and a link.');
    expect(answerExcerpt('![chart](a.png) see <https://x.y> and ~~old~~ new')).toBe(
      'chart see https://x.y and old new',
    );
  });

  it('drops code blocks, also one that the cut left open', () => {
    expect(answerExcerpt('Before\n```clj\n(+ 1 2)\n```\nAfter')).toBe('Before After');
    expect(answerExcerpt('Start\n~~~\nopen code')).toBe('Start');
  });

  it('drops the marks of lists, quotes, tasks, tables and dividers', () => {
    expect(answerExcerpt('- one\n- [x] two\n> quoted\n1. three\n* four')).toBe(
      'one two quoted three four',
    );
    expect(answerExcerpt('| a | b |\n|---|:--:|\n| 1 | 2 |')).toBe('a b 1 2');
    expect(answerExcerpt('One\n\n---\n\nTwo\n***')).toBe('One Two');
  });

  it('keeps stars and underscores that are not emphasis', () => {
    expect(answerExcerpt('a * b * c and snake_case_name')).toBe('a * b * c and snake_case_name');
    expect(answerExcerpt('Use \\*stars\\* here')).toBe('Use *stars* here');
  });

  it('cuts a long answer with an ellipsis', () => {
    const excerpt = answerExcerpt('word '.repeat(200));
    expect(excerpt).toHaveLength(400);
    expect(excerpt.endsWith('…')).toBe(true);
    expect(answerExcerpt('')).toBe('');
  });
});

describe('outlineEntry', () => {
  it('reads the answer and the state of a transcript row', () => {
    expect(
      outlineEntry({
        turn_id: 't1',
        request: '[IMAGE #1] why?',
        attachments: [{}],
        status: 'done',
        content: [
          { id: '1', type: 'tool' },
          { id: '2', type: 'prose', markdown: '**Because** it is.' },
          { id: '3', type: 'speech', text: 'Because it is.' },
        ],
      }),
    ).toEqual({ id: 't1', label: 'why?', answer: 'Because it is. Because it is.', status: 'done', isCouncil: false });
  });

  it('reads the answer and the state of a fork point from the gateway', () => {
    expect(
      outlineEntry({ turn_id: 't2', request: 'hi', request_kind: 'council', answer: '## Yes\n\nIt is.', status: 'failed' }),
    ).toEqual({ id: 't2', label: 'hi', answer: 'Yes It is.', status: 'failed', isCouncil: true });
  });
});

describe('outlineStatus', () => {
  it('names each wire status of a turn', () => {
    const wire = ['completed', 'streaming', 'running', 'queued', 'pending', 'suspended'];
    const ended = ['cancelled', 'interrupted', 'failed', 'error'];
    expect([...wire, ...ended].map((status) => outlineStatus(status))).toEqual([
      'done',
      'running',
      'running',
      'queued',
      'queued',
      'waiting',
      'cancelled',
      'cancelled',
      'failed',
      'failed',
    ]);
  });

  it('names each durable status that the transcript sends', () => {
    expect(['done', 'running', 'error', 'interrupted'].map((status) => outlineStatus(status))).toEqual([
      'done',
      'running',
      'failed',
      'cancelled',
    ]);
  });

  it('gives no state for a missing or unknown status', () => {
    expect(outlineStatus()).toBeNull();
    expect(outlineStatus('sent')).toBeNull();
    expect(outlineStatus('constructor')).toBeNull();
  });

  it('marks a settled turn that ended in a cancel as cancelled, as the transcript does', () => {
    expect(outlineStatus('completed', 'cancelled')).toBe('cancelled');
    expect(outlineStatus(undefined, 'cancelled')).toBe('cancelled');
    expect(outlineStatus('streaming', 'cancelled')).toBe('running');
  });
});

describe('mergeOutline', () => {
  const entry = (id: string, answer: string, status: OutlineStatus | null = null): OutlineEntry => ({
    id,
    label: id.toUpperCase(),
    answer,
    status,
    isCouncil: false,
  });

  it('fills a missing listed answer from the held row and appends new held rows', () => {
    expect(
      mergeOutline(
        [entry('a', ''), entry('b', 'listed')],
        [entry('a', 'held a'), entry('b', 'held b'), entry('c', '')],
      ),
    ).toEqual([entry('a', 'held a'), entry('b', 'listed'), entry('c', '')]);
  });

  it('takes the state of the held row, which follows the live turn', () => {
    expect(
      mergeOutline(
        [entry('a', 'x', 'running'), entry('b', 'y', 'done')],
        [entry('a', 'x', 'done'), entry('b', 'y')],
      ),
    ).toEqual([entry('a', 'x', 'done'), entry('b', 'y', 'done')]);
  });
});

describe('previewPosition', () => {
  it('stands left of its anchor, centred on it', () => {
    expect(previewPosition({ left: 700, top: 300, bottom: 336 }, 120, DESKTOP)).toEqual({
      left: 372,
      top: 258,
      width: 320,
    });
  });

  it('stays inside the screen at both ends', () => {
    expect(previewPosition({ left: 700, top: 880, bottom: 890 }, 200, DESKTOP)?.top).toBe(
      900 - EDGE_MARGIN - 200,
    );
    expect(previewPosition({ left: 700, top: 0, bottom: 10 }, 200, DESKTOP)?.top).toBe(EDGE_MARGIN);
  });

  it('paints narrower on a narrow screen, and not at all without room', () => {
    expect(previewPosition({ left: 300, top: 300, bottom: 336 }, 120, DESKTOP)).toEqual({
      left: EDGE_MARGIN,
      top: 258,
      width: 280,
    });
    expect(previewPosition({ left: 215, top: 300, bottom: 336 }, 120, DESKTOP)).toBeNull();
  });
});

describe('scrub', () => {
  it('gives each turn a step that a finger can hit and tell apart', () => {
    expect(scrubStep(400, 5)).toBe(32);
    expect(scrubStep(400, 40)).toBe(10);
    expect(scrubStep(400, 1000)).toBe(6);
    expect(scrubStep(400, 0)).toBe(32);
  });

  it('moves one turn for each step, up to older turns, inside the list', () => {
    expect(scrubIndex(10, -26, 10, 20)).toBe(7);
    expect(scrubIndex(10, 4, 10, 20)).toBe(10);
    expect(scrubIndex(2, -100, 10, 20)).toBe(0);
    expect(scrubIndex(18, 100, 10, 20)).toBe(19);
    expect(scrubIndex(0, 0, 10, 0)).toBe(-1);
  });
});

describe('readingTurnId', () => {
  /** A turn of the transcript column, with the edges that a browser gives it. */
  const turn = (id: string, top: number, bottom: number) =>
    ({ dataset: { turnId: id }, getBoundingClientRect: () => ({ top, bottom }) }) as unknown as HTMLElement;
  const column = (...turns: HTMLElement[]) => ({ querySelectorAll: () => turns }) as unknown as HTMLElement;
  /** A scroller with its top edge at 100px, scrolled to `scrollTop` in 3000px of content. */
  const scroller = (scrollTop: number) =>
    ({ scrollTop, scrollHeight: 3000, clientHeight: 800, getBoundingClientRect: () => ({ top: 100 }) }) as unknown as HTMLElement;

  it('names the first turn that reaches below the top of the scroller', () => {
    expect(readingTurnId(scroller(0), column(turn('t1', 100, 600), turn('t2', 600, 1200)))).toBe('t1');
    // t1 ends 20px below the top edge, inside the inset, so the reader is in t2.
    const turns = column(turn('t1', -400, 120), turn('t2', 120, 900), turn('t3', 900, 1600));
    expect(readingTurnId(scroller(1000), turns)).toBe('t2');
  });

  it('names the last turn when the end of the transcript is on screen', () => {
    expect(readingTurnId(scroller(2200), column(turn('t1', 100, 600), turn('t2', 600, 900)))).toBe('t2');
  });

  it('names no turn without a column or turns', () => {
    expect(readingTurnId(scroller(0), null)).toBeNull();
    expect(readingTurnId(scroller(0), column())).toBeNull();
  });
});
