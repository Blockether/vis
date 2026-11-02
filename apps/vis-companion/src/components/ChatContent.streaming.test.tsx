// @vitest-environment jsdom
import { render } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import type { IterationAttachment, TranscriptIteration } from '../lib/types';

// Regression, user report ("scrolling a big live session hangs on iOS"): one
// streaming delta rebuilds the turn's `iterations` array to grow its tail, so
// `buildSegments` returned a fresh object for every segment and the memo
// boundary — which compared identity — re-rendered the WHOLE trace on every
// flush, ~7 times a second. Measured on a 60-iteration turn: 63 segment renders
// per flush against 1. The settled part of a trace cannot change; only the
// iteration being streamed can.
//
// The rail is the seam that proves it: each segment builds its own attachment
// page, so a segment that re-rendered asks `pageBySize` for one again.
const railPages = vi.fn();
vi.mock('../lib/artifacts', async (importOriginal) => {
  const original = await importOriginal<typeof import('../lib/artifacts')>();
  return {
    ...original,
    pageBySize: (...args: Parameters<typeof original.pageBySize>) => {
      railPages();
      return original.pageBySize(...args);
    },
  };
});

// react-markdown parses its text on every render, so each render is one parse.
const parsed: string[] = [];
vi.mock('react-markdown', async (importOriginal) => {
  const original = await importOriginal<typeof import('react-markdown')>();
  return {
    ...original,
    default: (options: Parameters<typeof original.default>[0]) => {
      parsed.push(options.children ?? '');
      return original.default(options);
    },
  };
});

const { IterationTrace, Markdown } = await import('./ChatContent');

function attachment(position: number): IterationAttachment {
  return {
    index: 0,
    iteration_id: `i${position}`,
    name: `notes-${position}.txt`,
    size: 12,
    mime: 'text/plain',
  } as unknown as IterationAttachment;
}

function iteration(position: number, thinking: string): TranscriptIteration {
  return {
    position,
    id: `i${position}`,
    thinking,
    assistant_prose: `step ${position}`,
    forms: [],
    attachments: [attachment(position)],
  } as unknown as TranscriptIteration;
}

/** The settled iterations keep their identity across a flush, as the reducer leaves them. */
const settled = [iteration(0, 'first'), iteration(1, 'second')];

/** What `updateRunningIteration` hands the screen for one `content.block.delta`. */
function streamed(tick: number): TranscriptIteration[] {
  return [...settled, iteration(2, `thinking${'.'.repeat(tick)}`)];
}

const client = {
  base: 'http://gateway.example.com',
  retainAttachment: () => () => {},
  attachmentUrl: () => Promise.resolve(null),
} as never;

describe('a trace while its turn streams', () => {
  it('re-renders only the iteration being streamed', () => {
    const view = render(
      <IterationTrace iterations={streamed(1)} live whole client={client} sid="s1" />,
    );
    railPages.mockClear();

    view.rerender(<IterationTrace iterations={streamed(2)} live whole client={client} sid="s1" />);

    expect(railPages).toHaveBeenCalledTimes(1);
  });

  it('re-renders a settled segment when its own iteration changes', () => {
    const view = render(
      <IterationTrace iterations={streamed(1)} live whole client={client} sid="s1" />,
    );
    railPages.mockClear();

    view.rerender(
      <IterationTrace
        iterations={[iteration(0, 'first'), settled[1], iteration(2, 'x')]}
        live
        whole
        client={client}
        sid="s1"
      />,
    );

    expect(railPages).toHaveBeenCalledTimes(2);
  });
});

/** The states of `text` that deltas of these sizes show, with the whole text last. */
function streamStates(text: string, sizes = [3, 1, 7, 2, 5, 11, 4]): string[] {
  const states: string[] = [];
  for (let end = 0; end < text.length; ) {
    end = Math.min(text.length, end + sizes[states.length % sizes.length]);
    states.push(text.slice(0, end));
  }
  return states;
}

const ANSWER = [
  '## Diagnosis',
  '',
  'Each flush parsed the **whole** answer again, see `cachedTree` and https://example.com/docs.',
  '',
  '- The stream sends a flush every 150 ms.',
  '- Each flush parsed `6,308` characters:',
  '  - nested item one',
  '  - nested item two',
  '',
  '  The second paragraph makes this item loose.',
  '- A third item.',
  '',
  '### Measurements',
  '',
  '| Step | Cost |',
  '| ---- | ---: |',
  '| parse | 1275 ms |',
  '| commit | 376 ms |',
  '',
  '```text',
  'flush 1: 120 chars',
  '',
  'flush 50: 6,308 chars',
  '```',
  '',
  '> Only the last block of a text can still change.',
  '',
  'Done ~~soon~~ now.',
].join('\n');

/** Lines that change a block above them: a continued paragraph, list, heading and table. */
const LATE_MARKERS = [
  'Intro line',
  '#not a heading, so the paragraph goes on',
  '',
  '1. Check the logs',
  '',
  '2. Fix the cause; the list goes on after a blank line',
  '',
  'Setext title',
  '============',
  '',
  'Header | Value',
  '--- | ---',
  'parse | 1275 ms',
  '',
  '- item',
  '---',
  '',
  '> A quote that',
  'goes on lazily.',
  '',
  '```ts',
  'const a = 1;',
  '',
  'const b = 2;',
  '```',
  '',
  'Last paragraph.',
].join('\n');

describe('an answer while its turn streams', () => {
  // Regression, user report ("rendering that big output took a while, and the app did not
  // scroll to the bottom; a lag of a few seconds"): each flush of a streamed answer parsed
  // the whole answer again, so every flush cost more than the one before it.
  it('parses only the blocks that can still change', () => {
    const point = (index: number) => `Point ${index}: ${'word '.repeat(12)}`;
    const answer = Array.from({ length: 24 }, (_, index) => point(index)).join('\n\n');
    const states = streamStates(answer);
    const view = render(<Markdown>{states[0]}</Markdown>);

    const work = states.slice(1).map((state) => {
      parsed.length = 0;
      view.rerender(<Markdown>{state}</Markdown>);
      return parsed.reduce((sum, text) => sum + text.length, 0);
    });

    // Each flush parses its open point, so a flush that parses nothing means a broken seam.
    expect(Math.min(...work)).toBeGreaterThan(0);
    // A flush parses at most the last two points and its delta, never the whole answer.
    expect(Math.max(...work)).toBeLessThan(3 * point(10).length);
  });

  it.each([
    ['an answer', ANSWER, undefined, 'Done soon now.'],
    // One character at a time, so a delta ends at each place where a block can change.
    ['a text whose late lines change the blocks above them', LATE_MARKERS, [1], 'Last paragraph.'],
  ])('renders each state of %s as the whole text renders it', (_, text, sizes, ending) => {
    const states = streamStates(text, sizes);
    const view = render(<Markdown>{states[0]}</Markdown>);
    for (const state of states) {
      view.rerender(<Markdown>{state}</Markdown>);
      const whole = render(<Markdown>{state}</Markdown>);
      expect(view.container.innerHTML, state).toBe(whole.container.innerHTML);
      whole.unmount();
    }
    expect(view.container.textContent).toContain(ending);
  });
});
