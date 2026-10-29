// @vitest-environment jsdom
import { act, render } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { Markdown, UserMessage } from './ChatContent';
import { JustifiedProse } from './JustifiedProse';

/** Every glyph and natural space is this wide; every line is this tall. */
const CHAR = 8;
const LINE = 20;
const paragraph =
  'A paragraph should keep the lines that the browser chose for it. The final line can remain naturally short.';
/** At 240px the code does not fit after "Look at", which then stretches one gap 192px. */
const coded = 'Look at `src/components/Prose.tsx` for the details of the change today.';
const codedText = coded.replaceAll('`', '');
let width = 240;
let resize: ResizeObserverCallback;
let disconnect: ReturnType<typeof vi.fn>;
let reads: ReturnType<typeof vi.fn>;
let fonts: EventTarget & { ready: Promise<void>; status: 'loaded' | 'loading' };
const originalFonts = Object.getOwnPropertyDescriptor(document, 'fonts');
const originalRects = Object.getOwnPropertyDescriptor(Range.prototype, 'getClientRects');
const nativeStyle = window.getComputedStyle;

type Box = { line: number; left: number; width: number };

/**
 * Lay out the prose as a browser does: greedy breaks at spaces outside code, every line
 * but the last justified by stretching its gaps.
 */
function layoutOf(root: Element): Box[] {
  const text = root.textContent ?? '';
  const glued: boolean[] = [];
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  while (walker.nextNode()) {
    const leaf = walker.currentNode as Text;
    const code = !!leaf.parentElement?.closest('code');
    for (let index = 0; index < leaf.length; index++) glued.push(code);
  }
  const breakable = (index: number) => /\s/.test(text[index]) && !glued[index];
  const words: [number, number][] = [];
  for (let index = 0; index < text.length; ) {
    while (index < text.length && breakable(index)) index++;
    const start = index;
    while (index < text.length && !breakable(index)) index++;
    if (index > start) words.push([start, index]);
  }
  const lines: [number, number][][] = [];
  /** The lines inside a code box wider than the column keep natural gaps. */
  const boxed = new Set<[number, number][]>();
  /** Every placed word, in reading order. */
  const order: [number, number][] = [];
  let x = 0;
  for (const word of words) {
    let end = word[0];
    while (end < word[1] && glued[end]) end++;
    if ((end - word[0]) * CHAR > width) {
      // Inline code is an inline-block of at most the column. Wider code takes its own
      // lines and wraps at its own spaces; the text after it starts a new line.
      let line: [number, number][] | null = null;
      for (const match of text.slice(word[0], end).matchAll(/\S+/g)) {
        const piece: [number, number] = [
          word[0] + match.index,
          word[0] + match.index + match[0].length,
        ];
        const size = (piece[1] - piece[0]) * CHAR;
        if (line && x + CHAR + size <= width) {
          line.push(piece);
          x += CHAR + size;
        } else {
          line = [piece];
          lines.push(line);
          boxed.add(line);
          x = size;
        }
        order.push(piece);
      }
      x = width;
      if (end < word[1]) {
        lines.push([[end, word[1]]]);
        order.push(lines.at(-1)![0]);
        x = (word[1] - end) * CHAR;
      }
      continue;
    }
    const size = (word[1] - word[0]) * CHAR;
    const line = lines.at(-1);
    if (line && x + CHAR + size <= width) {
      line.push(word);
      x += CHAR + size;
    } else {
      lines.push([word]);
      x = size;
    }
    order.push(word);
  }
  const boxes: Box[] = Array.from(text, () => ({ line: 0, left: 0, width: 0 }));
  for (const [number, line] of lines.entries()) {
    const used = line.reduce((sum, [start, end]) => sum + (end - start) * CHAR, 0);
    const gap =
      number < lines.length - 1 && line.length > 1 && !boxed.has(line)
        ? (width - used) / (line.length - 1)
        : CHAR;
    let left = 0;
    for (const [index, [start, end]] of line.entries()) {
      for (let at = start; at < end; at++)
        boxes[at] = { line: number, left: left + (at - start) * CHAR, width: CHAR };
      left += (end - start) * CHAR;
      const next = line[index + 1] ?? order[order.indexOf(line[index]) + 1];
      for (let at = end; at < (next?.[0] ?? text.length); at++) {
        const first = at === end && index < line.length - 1;
        boxes[at] = { line: number, left, width: first ? gap : 0 };
      }
      left += gap;
    }
  }
  return boxes;
}

function offsetIn(root: Element, node: Node, offset: number) {
  let total = 0;
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  while (walker.nextNode()) {
    if (walker.currentNode === node) return total + offset;
    total += (walker.currentNode as Text).length;
  }
  return total;
}

function clientRects(this: Range) {
  const root = (this.startContainer.parentElement ?? this.startContainer as Element).closest(
    'p, li',
  )!;
  const boxes = layoutOf(root);
  const start = offsetIn(root, this.startContainer, this.startOffset);
  const end = offsetIn(root, this.endContainer, this.endOffset);
  const lines = new Map<number, { left: number; right: number }>();
  for (const box of boxes.slice(start, end)) {
    const line = lines.get(box.line);
    if (line) line.right = Math.max(line.right, box.left + box.width);
    else lines.set(box.line, { left: box.left, right: box.left + box.width });
  }
  return [...lines].map(([line, { left, right }]) => {
    const top = line * LINE;
    const bottom = top + LINE;
    return { left, right, x: left, y: top, top, bottom, width: right - left, height: LINE };
  });
}

beforeEach(() => {
  width = 240;
  disconnect = vi.fn();
  fonts = Object.assign(new EventTarget(), {
    ready: Promise.resolve(),
    status: 'loaded' as 'loaded' | 'loading',
  });
  Object.defineProperty(document, 'fonts', { configurable: true, value: fonts });
  vi.stubGlobal(
    'ResizeObserver',
    class {
      constructor(callback: ResizeObserverCallback) {
        resize = callback;
      }
      observe() {}
      unobserve() {}
      disconnect = disconnect;
    },
  );
  vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) =>
    setTimeout(callback, 0),
  );
  vi.stubGlobal('cancelAnimationFrame', clearTimeout);
  vi.stubGlobal('getComputedStyle', (element: Element) =>
    Object.assign(nativeStyle(element), {
      width: `${width}px`,
      direction: 'ltr',
      writingMode: 'horizontal-tb',
      whiteSpace: 'normal',
      textTransform: 'none',
      textIndent: '0px',
      hyphens: 'manual',
      wordBreak: 'normal',
      overflowWrap: 'normal',
    }),
  );
  vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue({
    font: '',
    measureText: () => ({ width: CHAR }),
  } as unknown as CanvasRenderingContext2D);
  vi.spyOn(HTMLElement.prototype, 'getBoundingClientRect').mockImplementation(
    () => ({ left: 0, top: 0, right: width, bottom: LINE, width, height: LINE }) as DOMRect,
  );
  reads = vi.fn(clientRects);
  Object.defineProperty(Range.prototype, 'getClientRects', { configurable: true, value: reads });
});

afterEach(() => {
  window.getSelection()?.removeAllRanges();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
  if (originalFonts) Object.defineProperty(document, 'fonts', originalFonts);
  else Reflect.deleteProperty(document, 'fonts');
  if (originalRects) Object.defineProperty(Range.prototype, 'getClientRects', originalRects);
  else Reflect.deleteProperty(Range.prototype, 'getClientRects');
});

/** Let the placement that follows a commit run, as it does before the browser paints. */
async function settle() {
  await act(async () => {});
}

/** Let a scheduled frame and its placement run. */
async function frame(ms = 0) {
  await act(() => new Promise((resolve) => setTimeout(resolve, ms)));
}

/** Report the paragraph's current width to the shared ResizeObserver. */
function delivered(element: Element) {
  act(() => resize([{ target: element } as ResizeObserverEntry], {} as ResizeObserver));
}

/** The text alignment of each composed line: `justify`, or `start` where it stays ragged. */
function endings(prose: Element) {
  return [...prose.children]
    .filter(
      (line): line is HTMLElement =>
        line instanceof HTMLElement && line.style.display === 'inline-block',
    )
    .map((line) => line.style.textAlignLast);
}

describe('justified prose', () => {
  it('leaves ordinary prose native without reading a single line', async () => {
    const view = render(<Markdown>{paragraph}</Markdown>);
    await settle();
    const prose = view.getByRole('paragraph');
    expect(prose).not.toHaveAttribute('data-justified-lines');
    expect(prose.textContent).toBe(paragraph);
    expect(reads).not.toHaveBeenCalled();
  });

  // #282 follow-up: opening Markdown must not paint native text, then reflow it.
  it('sets a hole beside inline code ragged before the first frame, on the native breaks', async () => {
    vi.stubGlobal('requestAnimationFrame', () => 0);
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    const prose = view.getByRole('paragraph');
    expect(prose).toHaveAttribute('data-justified-lines');
    expect(endings(prose)).toEqual(['start', 'justify', 'justify', 'start']);
    expect(prose.textContent).toBe(codedText);
    expect(prose.querySelector('code')?.textContent).toBe('src/components/Prose.tsx');
  });

  // Regression, user report: code wider than the column took its own lines, and the short
  // line before it stayed natively justified, with gaps across the whole column.
  it('sets the line before code wider than the column ragged and lets the code wrap', async () => {
    const wide =
      'Read it in `c6a8ef275 fix(trace): show the running step below a closed digest`. It stays short.';
    const view = render(<Markdown>{wide}</Markdown>);
    await settle();
    const prose = view.getByRole('paragraph');
    expect(prose).toHaveAttribute('data-justified-lines');
    expect(prose.textContent).toBe(wide.replaceAll('`', ''));
    const lines = [...prose.children].filter(
      (line): line is HTMLElement =>
        line instanceof HTMLElement && line.style.display === 'inline-block',
    );
    expect(lines.map((line) => line.textContent)).toEqual([
      'Read it in',
      'c6a8ef275 fix(trace): show the running step below a closed digest',
      '. It stays short.',
    ]);
    expect(endings(prose)).toEqual(['start', 'start', 'start']);
    expect(lines.map((line) => line.style.whiteSpace)).toEqual(['nowrap', 'normal', 'nowrap']);
  });

  it('reads a paragraph with a long token even without code', async () => {
    const text = 'Read it in averyveryverylongidentifier now please.';
    const view = render(<JustifiedProse>{text}</JustifiedProse>);
    await settle();
    const prose = view.getByRole('paragraph');
    expect(endings(prose)).toEqual(['start', 'justify', 'start']);
    expect(prose.textContent).toBe(text);
  });

  it('keeps native lines when the risky token opens no wide gap', async () => {
    width = 400;
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    expect(view.getByRole('paragraph')).not.toHaveAttribute('data-justified-lines');
    expect(reads).toHaveBeenCalled();
  });

  it('keeps native text when it is disabled or cannot be measured', async () => {
    const disabled = render(<JustifiedProse enabled={false}>{coded}</JustifiedProse>);
    await settle();
    expect(disabled.getByRole('paragraph')).not.toHaveAttribute('data-justified-lines');
    expect(reads).not.toHaveBeenCalled();
    disabled.unmount();
    reads.mockImplementation(() => []);
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    const prose = view.getByRole('paragraph');
    expect(prose).not.toHaveAttribute('data-justified-lines');
    expect(prose.textContent).toBe(codedText);
  });

  it('keeps its lines when a parent repeats the same prose with new elements', async () => {
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    const line = view.getByRole('paragraph').firstElementChild;
    const before = reads.mock.calls.length;
    view.rerender(<Markdown>{coded}</Markdown>);
    await settle();
    expect(view.getByRole('paragraph').firstElementChild).toBe(line);
    expect(reads.mock.calls.length).toBe(before);
  });

  it('reads streamed text again and shows it at once', async () => {
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    const streamed = `${coded} More words arrive while the answer streams.`;
    view.rerender(<Markdown>{streamed}</Markdown>);
    await settle();
    const prose = view.getByRole('paragraph');
    expect(prose.textContent).toBe(streamed.replaceAll('`', ''));
    expect(endings(prose)[0]).toBe('start');
  });

  it('reads one step of its column again and returns to native lines that read well', async () => {
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    const prose = view.getByRole('paragraph');
    // The first delivery reports the width that the placement already read.
    delivered(prose);
    expect(prose).toHaveAttribute('data-justified-lines');
    width = 400;
    delivered(prose);
    expect(prose).not.toHaveAttribute('data-justified-lines');
    width = 240;
    await frame(100);
    delivered(prose);
    expect(endings(prose)).toEqual(['start', 'justify', 'justify', 'start']);
  });

  it('rides native lines while its column moves and reads them where it rests', async () => {
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    const prose = view.getByRole('paragraph');
    delivered(prose);
    width = 400;
    delivered(prose);
    width = 240;
    delivered(prose);
    expect(prose).not.toHaveAttribute('data-justified-lines');
    await frame(120);
    await frame();
    expect(endings(prose)).toEqual(['start', 'justify', 'justify', 'start']);
  });

  it('keeps the lines a selection covers until the selection clears', async () => {
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    const prose = view.getByRole('paragraph');
    delivered(prose);
    const range = document.createRange();
    // A reader's selection runs across the paragraph, as jsdom's `containsNode` requires.
    range.setStartBefore(prose);
    range.setEndAfter(prose);
    window.getSelection()!.addRange(range);
    const markup = prose.innerHTML;
    width = 400;
    delivered(prose);
    expect(prose.innerHTML).toBe(markup);
    expect(window.getSelection()!.toString()).toBe(codedText);
    window.getSelection()!.removeAllRanges();
    act(() => {
      document.dispatchEvent(new Event('selectionchange'));
    });
    await frame();
    expect(prose).not.toHaveAttribute('data-justified-lines');
  });

  it('reads its lines again after fonts load', async () => {
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    const before = reads.mock.calls.length;
    act(() => {
      fonts.dispatchEvent(new Event('loadingdone'));
    });
    await frame();
    expect(reads.mock.calls.length).toBeGreaterThan(before);
    expect(view.getByRole('paragraph')).toHaveAttribute('data-justified-lines');
  });

  it('releases its observer on unmount', async () => {
    const view = render(<Markdown>{coded}</Markdown>);
    await settle();
    view.unmount();
    expect(disconnect).toHaveBeenCalled();
  });
});

describe('request and Markdown prose', () => {
  it('keeps tabs and indented commands left-aligned', async () => {
    const text = `${paragraph}
  git  status
git\tstatus`;
    const view = render(<UserMessage>{text}</UserMessage>);
    await settle();
    const lines = [...view.container.querySelectorAll('article p')];
    for (const line of lines.slice(1)) {
      expect(line).not.toHaveAttribute('data-justified-lines');
    }
    expect(lines.map((line) => line.textContent)).toEqual(text.split('\n'));
  });

  it('leaves fenced code literal while justifying the surrounding request', async () => {
    const request = [paragraph, '```ts', 'const value = getValue(item);', '```', paragraph].join('\n');
    const view = render(<UserMessage>{request}</UserMessage>);
    await settle();
    const lines = [...view.container.querySelectorAll('article p')];
    expect(lines.map((line) => line.textContent)).toEqual([
      paragraph,
      '```ts',
      'const value = getValue(item);',
      '```',
      paragraph,
    ]);
  });

  it('keeps nested block structure and task controls native while reading child paragraphs', async () => {
    const view = render(
      <Markdown>{`- [ ] **Keep this control**\n\n- ${coded}\n\n  Another paragraph in the same item.`}</Markdown>,
    );
    await settle();
    expect(view.getByRole('checkbox')).not.toBeChecked();
    for (const item of view.getAllByRole('listitem'))
      expect(item).not.toHaveAttribute('data-justified-lines');
    const [, prose] = view.getAllByRole('paragraph');
    expect(prose).toHaveAttribute('data-justified-lines');
    expect(prose.textContent).toBe(codedText);
  });

  it('reads Markdown paragraphs and list items, but not compact results or rich hard breaks', async () => {
    const view = render(<Markdown>{`${coded}\n\n- ${coded}`}</Markdown>);
    await settle();
    expect(view.getByRole('paragraph')).toHaveAttribute('data-justified-lines');
    expect(view.getByRole('listitem')).toHaveAttribute('data-justified-lines');
    view.rerender(<Markdown nested>{coded}</Markdown>);
    await settle();
    expect(view.getByRole('paragraph')).not.toHaveAttribute('data-justified-lines');
    view.rerender(<Markdown hardBreaks>{`${coded}\nSecond line with **emphasis**.`}</Markdown>);
    await settle();
    expect(view.getByRole('paragraph')).not.toHaveAttribute('data-justified-lines');
    expect(view.container.querySelectorAll('br')).toHaveLength(1);
    expect(view.getByText('emphasis').tagName).toBe('STRONG');
  });
});
