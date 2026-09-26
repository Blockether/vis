// @vitest-environment jsdom
import { act, fireEvent, render, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { Markdown, UserMessage } from './ChatContent';
import { JustifiedProse } from './JustifiedProse';
import { MarkdownAnnotator } from './MarkdownArtifact';
import { lineText, prepare, solve } from '@kitlangton/justice';
import { useLayoutEffect } from 'react';
import { noteReaderGesture, releaseReaderScroll } from '../lib/reader-gesture';

const paragraph =
  'A paragraph should choose its line breaks together rather than treating every line as an isolated decision. The final line can remain naturally short.';
let width = 240;
let glyphWidth = 8;
let resize: ResizeObserverCallback;
let disconnect: ReturnType<typeof vi.fn>;
let measure: ReturnType<typeof vi.spyOn>;
let fonts: EventTarget & { ready: Promise<void>; status: 'loaded' | 'loading' };
const originalFonts = Object.getOwnPropertyDescriptor(document, 'fonts');
const originalRangeRect = Object.getOwnPropertyDescriptor(Range.prototype, 'getBoundingClientRect');
const nativeStyle = window.getComputedStyle;

beforeEach(() => {
  width = 240;
  glyphWidth = 8;
  disconnect = vi.fn();
  fonts = Object.assign(new EventTarget(), {
    ready: Promise.resolve(),
    status: 'loading' as 'loaded' | 'loading',
  });
  Object.defineProperty(document, 'fonts', { configurable: true, value: fonts });
  vi.stubGlobal(
    'ResizeObserver',
    class {
      constructor(callback: ResizeObserverCallback) {
        resize = callback;
      }
      observe() {}
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
      boxSizing: 'content-box',
      direction: 'ltr',
      writingMode: 'horizontal-tb',
      whiteSpace: 'normal',
      textTransform: 'none',
      textIndent: '0px',
      font: '16px sans-serif',
    }),
  );
  measure = vi.spyOn(HTMLElement.prototype, 'getBoundingClientRect').mockImplementation(function (
    this: HTMLElement,
  ) {
    return { width: (this.textContent?.length ?? 0) * glyphWidth } as DOMRect;
  });
  Object.defineProperty(Range.prototype, 'getBoundingClientRect', {
    configurable: true,
    value: vi.fn(function (this: Range) {
      return { width: this.toString().length * glyphWidth } as DOMRect;
    }),
  });
});

afterEach(() => {
  releaseReaderScroll();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
  if (originalFonts) Object.defineProperty(document, 'fonts', originalFonts);
  else Reflect.deleteProperty(document, 'fonts');
  if (originalRangeRect)
    Object.defineProperty(Range.prototype, 'getBoundingClientRect', originalRangeRect);
  else Reflect.deleteProperty(Range.prototype, 'getBoundingClientRect');
});

async function composed(element: Element) {
  await waitFor(() => expect(element).toHaveAttribute('data-justice'));
}

/** Let the placement that follows a commit run, as it does before the browser paints. */
async function settle() {
  await act(async () => {});
}

/** Words measured so far; placing a paragraph reads only the paragraph's own box. */
function wordMeasurements() {
  return measure.mock.instances.filter((node: Element) => node.tagName !== 'P').length;
}

function resized() {
  act(() => resize([], {} as ResizeObserver));
}

describe('Justice prose', () => {
  // #282 follow-up: opening Markdown must not paint native text, then reflow it.
  it('composes the opening render without waiting for an animation frame', async () => {
    vi.stubGlobal('requestAnimationFrame', () => 0);
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    await settle();
    const prose = view.getByRole('paragraph');
    expect(prose).toHaveAttribute('data-justice');
    expect(prose.children.length).toBeGreaterThan(1);
    expect(prose.textContent).toBe(paragraph);
  });

  it('composes a plain user request with Justice on the first paint', async () => {
    const view = render(<UserMessage>{paragraph}</UserMessage>);
    await settle();
    const prose = view.container.querySelector('article p');
    expect(prose).toHaveAttribute('data-justice');
    expect(prose?.textContent).toBe(paragraph);
  });

  it('keeps hard line breaks and literal spacing in a user request', async () => {
    const view = render(<UserMessage>{`${paragraph}\n\n  git  status\n${paragraph}`}</UserMessage>);
    await settle();
    const lines = [...view.container.querySelectorAll('article p')];
    expect(lines.map((line) => line.textContent)).toEqual([paragraph, '', '  git  status', paragraph]);
    expect(lines.map((line) => line.hasAttribute('data-justice'))).toEqual([
      true,
      false,
      false,
      true,
    ]);
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
    expect(lines.map((line) => line.hasAttribute('data-justice'))).toEqual([
      true,
      false,
      false,
      false,
      true,
    ]);
  });

  it('justifies request prose around a collapsed paste without opening its literal body', async () => {
    const request = [
      paragraph,
      '````vis-paste',
      '[Pasted #1: 1 line, 3B]',
      'abc',
      '````',
      paragraph,
    ].join('\n');
    const view = render(<UserMessage>{request}</UserMessage>);
    await settle();
    expect(
      [...view.container.querySelectorAll('article p[data-justice]')].map((line) => line.textContent),
    ).toEqual([paragraph, paragraph]);
    expect(view.container.querySelector('details code')?.textContent).toBe('abc');
  });

  it('keeps an inline image chip beside the original text rather than moving it into a paragraph', () => {
    const view = render(<UserMessage>{'Look at /art/chart.png and describe it.'}</UserMessage>);
    const article = view.container.querySelector('article')!;
    expect(article.querySelector('p')).toBeNull();
    expect(article).toHaveTextContent('Look at chart.png and describe it.');
    expect(article).not.toHaveTextContent('/art/chart.png');
  });

  it('keeps an empty request bubble without introducing a blank paragraph', () => {
    const view = render(<UserMessage>{''}</UserMessage>);
    expect(view.container.querySelector('article p')).toBeNull();
    expect(view.container.querySelector('article .border-you-role')).toBeInTheDocument();
  });

  it('measures plain prose without laying out every word separately', async () => {
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await composed(prose);
    expect(prose.textContent).toBe(paragraph);
    expect(measure.mock.calls.length).toBeLessThan(6);
  });

  it('defers distant prose while keeping its source text and composes before it enters view', async () => {
    let notify: IntersectionObserverCallback = () => {};
    const options: (IntersectionObserverInit | undefined)[] = [];
    const observe = vi.fn();
    const unobserve = vi.fn();
    const disconnect = vi.fn();
    vi.stubGlobal(
      'IntersectionObserver',
      class {
        constructor(callback: IntersectionObserverCallback, init?: IntersectionObserverInit) {
          options.push(init);
          notify = callback;
        }
        observe = observe;
        unobserve = unobserve;
        disconnect = disconnect;
      },
    );
    measure.mockImplementation(function (this: HTMLElement) {
      if (this.dataset.testid === 'scroller') return { top: 0, bottom: 800 } as DOMRect;
      if (this.tagName === 'P') return { width, top: 3000, bottom: 3040 } as DOMRect;
      return { width: (this.textContent?.length ?? 0) * glyphWidth } as DOMRect;
    });
    const transcript = (both: boolean) => (
      <div data-testid="scroller" style={{ overflowY: 'auto' }}>
        <JustifiedProse>{paragraph}</JustifiedProse>
        {both && <JustifiedProse>{`${paragraph} It follows the first one.`}</JustifiedProse>}
      </div>
    );

    const view = render(transcript(true));
    await settle();
    const scroller = view.getByTestId('scroller');
    const [prose, next] = view.getAllByRole('paragraph');
    for (const element of [prose, next]) {
      expect(element).not.toHaveAttribute('data-justice');
      expect(observe).toHaveBeenCalledWith(element);
    }
    expect(prose.textContent).toBe(paragraph);
    // One observer per scroller, looking a scroller height ahead: the page viewport
    // cannot see prose that the scroller still clips.
    expect(options).toEqual([{ root: scroller, rootMargin: '100% 0px' }]);
    // Placing prose reads where it stands, never the words inside it.
    const placed = [scroller, prose, next];
    expect(measure.mock.instances.every((node: HTMLElement) => placed.includes(node))).toBe(true);

    act(() =>
      notify(
        [{ isIntersecting: true, target: prose } as unknown as IntersectionObserverEntry],
        {} as IntersectionObserver,
      ),
    );
    await composed(prose);
    expect(prose.textContent).toBe(paragraph);
    expect(next).not.toHaveAttribute('data-justice');
    view.rerender(transcript(false));
    expect(unobserve).toHaveBeenCalledWith(next);
    expect(disconnect).not.toHaveBeenCalled();
    view.unmount();
    expect(unobserve).toHaveBeenCalledWith(prose);
    expect(disconnect).toHaveBeenCalledOnce();
  });

  // Opening a session scrolls its transcript to the newest turn in a layout effect.
  // Prose measured before that scroll would take the lines on screen for distant ones.
  it('composes prose that its screen scrolls into view as it opens, before any frame', async () => {
    vi.stubGlobal('requestAnimationFrame', () => 0);
    let scrolled = 0;
    measure.mockImplementation(function (this: HTMLElement) {
      if (this.dataset.testid === 'scroller') return { top: 0, bottom: 800 } as DOMRect;
      if (this.tagName === 'P')
        return { width, top: 3000 - scrolled, bottom: 3040 - scrolled } as DOMRect;
      return { width: (this.textContent?.length ?? 0) * glyphWidth } as DOMRect;
    });
    function Screen() {
      // A parent's layout effect runs after those of the prose inside it.
      useLayoutEffect(() => {
        scrolled = 2900;
      }, []);
      return (
        <div data-testid="scroller" style={{ overflowY: 'auto' }}>
          <JustifiedProse>{paragraph}</JustifiedProse>
        </div>
      );
    }
    const view = render(<Screen />);
    await settle();
    expect(view.getByRole('paragraph')).toHaveAttribute('data-justice');
  });

  it('composes prose above the reader once they stop scrolling, without moving what they read', async () => {
    let notify: IntersectionObserverCallback = () => {};
    vi.stubGlobal(
      'IntersectionObserver',
      class {
        constructor(callback: IntersectionObserverCallback) {
          notify = callback;
        }
        observe() {}
        unobserve() {}
        disconnect() {}
      },
    );
    let scroller: HTMLElement | null = null;
    let top = -2000;
    measure.mockImplementation(function (this: HTMLElement) {
      if (this.dataset.testid === 'scroller') return { top: 0, bottom: 800 } as DOMRect;
      if (this.tagName === 'P') {
        // Here the composed lines stand taller than the native wrap they replace.
        const height = this.hasAttribute('data-justice') ? 64 : 40;
        const start = top - (scroller?.scrollTop ?? 0);
        return { width, top: start, bottom: start + height } as DOMRect;
      }
      return { width: (this.textContent?.length ?? 0) * glyphWidth } as DOMRect;
    });
    const view = render(
      <div data-testid="scroller" style={{ overflowY: 'auto' }}>
        <JustifiedProse>{paragraph}</JustifiedProse>
      </div>,
    );
    scroller = view.getByTestId('scroller');
    const prose = view.getByRole('paragraph');
    await settle();
    expect(prose).not.toHaveAttribute('data-justice');

    // The reader scrolls up, and the paragraph comes within reach above what they see.
    noteReaderGesture();
    top = -600;
    act(() =>
      notify(
        [{ isIntersecting: true, target: prose } as unknown as IntersectionObserverEntry],
        {} as IntersectionObserver,
      ),
    );
    await act(() => new Promise((resolve) => setTimeout(resolve, 100)));
    expect(prose).not.toHaveAttribute('data-justice');
    expect(scroller.scrollTop).toBe(0);

    await composed(prose);
    expect(prose.textContent).toBe(paragraph);
    expect(scroller.scrollTop).toBe(24);
  });

  it('keeps its lines when a parent repeats the same prose with new handlers', async () => {
    const first = vi.fn();
    const second = vi.fn();
    const prose = (open: () => void) => (
      <JustifiedProse>
        {paragraph} Then{' '}
        <button type="button" onClick={open}>
          open
        </button>{' '}
        it.
      </JustifiedProse>
    );
    const view = render(prose(first));
    const element = view.getByRole('paragraph');
    await composed(element);
    const markup = element.innerHTML;
    const measurements = measure.mock.calls.length;
    view.rerender(prose(second));
    await settle();
    expect(element).toHaveAttribute('data-justice');
    expect(element.innerHTML).toBe(markup);
    expect(measure.mock.calls.length).toBe(measurements);
    fireEvent.click(view.getByRole('button', { name: 'open' }));
    expect(second).toHaveBeenCalledOnce();
    expect(first).not.toHaveBeenCalled();
  });

  it('keeps the initial composition on the first ResizeObserver delivery', async () => {
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await composed(prose);
    const markup = prose.innerHTML;
    resized();
    expect(prose).toHaveAttribute('data-justice');
    expect(prose.innerHTML).toBe(markup);
  });

  it('keeps readable native text when the initial measurement fails', async () => {
    measure.mockImplementation(() => {
      throw new Error('Measurement unavailable');
    });
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    await settle();
    const prose = view.getByRole('paragraph');
    expect(measure).toHaveBeenCalled();
    expect(prose).not.toHaveAttribute('data-justice');
    expect(prose.textContent).toBe(paragraph);
    expect(prose.querySelector('[aria-hidden]')).toBeNull();
  });

  it('preserves the source text and paragraph semantics, including soft whitespace', async () => {
    const text = paragraph.replace('should choose', 'should\nchoose');
    const view = render(<JustifiedProse>{text}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await composed(prose);
    expect(prose.textContent).toBe(text);
    expect(prose.querySelector('[aria-hidden]')).toBeNull();
  });

  it('reuses measurements on resize and remeasures after fonts load', async () => {
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await composed(prose);
    const narrowLines = prose.children.length;
    const measurements = wordMeasurements();
    width = 480;
    resized();
    await composed(prose);
    expect(prose.children.length).toBeLessThan(narrowLines);
    expect(wordMeasurements()).toBe(measurements);
    glyphWidth = 12;
    act(() => fonts.dispatchEvent(new Event('loadingdone')));
    await waitFor(() => expect(measure.mock.calls.length).toBeGreaterThan(measurements));
    expect(prose.textContent).toBe(paragraph);
  });

  it('does not remeasure when fonts were already loaded at composition', async () => {
    fonts.status = 'loaded';
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    await settle();
    const prose = view.getByRole('paragraph');
    expect(prose).toHaveAttribute('data-justice');
    const measurements = measure.mock.calls.length;
    await act(async () => {
      await fonts.ready;
      await new Promise((resolve) => setTimeout(resolve, 20));
    });
    expect(measure.mock.calls.length).toBe(measurements);
    act(() => fonts.dispatchEvent(new Event('loadingdone')));
    await waitFor(() => expect(measure.mock.calls.length).toBeGreaterThan(measurements));
  });

  it('wraps natively while its column is moving and composes where it lands', async () => {
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await composed(prose);
    const narrowLines = prose.children.length;
    const measurements = wordMeasurements();

    // The desk rail riding off its seam hands this column a new width on every frame
    // of the ride. Solving and rewriting every line for each of them composes for a
    // width nobody reads, and that work is what the ride stutters on.
    for (const next of [300, 360, 420]) {
      width = next;
      resized();
    }

    expect(prose).not.toHaveAttribute('data-justice');
    expect(prose.textContent).toBe(paragraph);

    // The rail lands, and the paragraph is composed once, for the width it kept.
    width = 480;
    resized();
    await composed(prose);

    expect(prose.children.length).toBeLessThan(narrowLines);
    expect(wordMeasurements()).toBe(measurements);
  });

  it('keeps lines composed for a width the observer reports later, on new line nodes', async () => {
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await composed(prose);
    const narrow = [...prose.children];

    // Fonts finish loading in the frame that widens the column, before its resize arrives.
    width = 480;
    act(() => fonts.dispatchEvent(new Event('loadingdone')));
    await waitFor(() => expect(prose.children.length).toBeLessThan(narrow.length));
    const wide = [...prose.children];
    // Chrome can keep a space collapsed in a line rewritten in place, leaving it short.
    expect(wide.filter((line) => narrow.includes(line))).toEqual([]);

    // The delivery of the width these lines were composed for is not a move.
    resized();
    expect(prose).toHaveAttribute('data-justice');
    expect(prose.firstElementChild).toBe(wide[0]);
  });

  it('refits a passage its selection held through a resize once the selection clears', async () => {
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await composed(prose);
    const narrowLines = prose.children.length;
    const selectedMarkup = prose.innerHTML;
    // jsdom does not report a node inside a range that selects its contents.
    let selected = true;
    vi.spyOn(window, 'getSelection').mockImplementation(
      () => ({ isCollapsed: !selected, containsNode: () => selected }) as unknown as Selection,
    );

    width = 480;
    resized();
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 20));
    });
    expect(prose.innerHTML).toBe(selectedMarkup);

    selected = false;
    act(() => document.dispatchEvent(new Event('selectionchange')));
    await waitFor(() => expect(prose.children.length).toBeLessThan(narrowLines));
  });

  it('shows the latest streamed text immediately and composes updated rich markup', async () => {
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await composed(prose);
    const next = `${paragraph} New words arrive while the response is running.`;
    view.rerender(<JustifiedProse>{next}</JustifiedProse>);
    expect(prose.textContent).toBe(next);
    await composed(prose);
    view.rerender(
      <JustifiedProse>
        Read <a href="https://example.com">the source</a>.
      </JustifiedProse>,
    );
    await composed(prose);
    expect(view.getByRole('link', { name: 'the source' })).toHaveAttribute(
      'href',
      'https://example.com',
    );
  });

  it.each([
    'مرحبا بكم في هذا النص الذي يحتفظ باتجاهه الأصلي',
    '日本語の文章はブラウザーが改行します。',
    'Keep\u00a0these words together.',
    'Directional \u202etext remains native.',
    'word '.repeat(401).trim(),
    'x'.repeat(16_385),
    `${'a-'.repeat(17)}z`,
    Array(129).fill('well-known').join(' '),
    '',
  ])('keeps unsupported text native (case %#)', (text) => {
    const view = render(<JustifiedProse>{text}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    expect(prose).not.toHaveAttribute('data-justice');
    expect(prose.textContent).toBe(text);
    expect(measure).not.toHaveBeenCalled();
  });

  it.each(['measurement', 'segmentation'])('keeps native text without %s support', (capability) => {
    if (capability === 'measurement') vi.stubGlobal('ResizeObserver', undefined);
    else vi.stubGlobal('Intl', { Segmenter: undefined });
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    expect(view.getByRole('paragraph')).not.toHaveAttribute('data-justice');
    expect(view.getByRole('paragraph').textContent).toBe(paragraph);
    expect(measure).not.toHaveBeenCalled();
  });

  it('waits for a hidden column to gain width and releases observers on unmount', async () => {
    width = 0;
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    expect(prose).not.toHaveAttribute('data-justice');
    width = 240;
    resized();
    await composed(prose);
    view.unmount();
    expect(disconnect).toHaveBeenCalledOnce();
    const measurements = measure.mock.calls.length;
    act(() => fonts.dispatchEvent(new Event('loadingdone')));
    expect(measure.mock.calls.length).toBe(measurements);
  });

  it('falls back when an unbreakable token cannot fit, then composes when widened', async () => {
    const text = `Read ${'longidentifier'.repeat(8)} carefully.`;
    const view = render(<JustifiedProse>{text}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await waitFor(() => expect(measure).toHaveBeenCalled());
    expect(prose).not.toHaveAttribute('data-justice');
    expect(prose.textContent).toBe(text);
    width = 1200;
    resized();
    await composed(prose);
  });

  it('keeps explicit hyphen breaks in the original source, without inserting spaces', async () => {
    const text =
      'Well-known paragraph-wide choices keep source-authored hyphens intact across lines.';
    const view = render(<JustifiedProse>{text}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    await composed(prose);
    expect(prose.textContent).toBe(text);
  });

  it('optimizes formatted artifact paragraphs without losing annotations or inline semantics', async () => {
    const text =
      'Artifact paragraphs keep **bold words**, *emphasis*, ~~old wording~~, [a link](https://example.com) and `git  status --short` while choosing their line breaks together.';
    const view = render(
      <MarkdownAnnotator text={text} onSave={async () => 2} chrome={({ body }) => <>{body}</>} />,
    );
    const prose = view.getByRole('paragraph');
    const original = prose.textContent;
    await composed(prose);
    expect(prose.textContent).toBe(original);
    expect([...prose.querySelectorAll('strong')].map((node) => node.textContent).join(' ')).toBe(
      'bold words',
    );
    expect(prose.querySelector('em')).toHaveTextContent('emphasis');
    expect([...prose.querySelectorAll('del')].map((node) => node.textContent).join(' ')).toBe(
      'old wording',
    );
    expect(view.getByRole('link', { name: 'a link' })).toHaveAttribute(
      'href',
      'https://example.com',
    );
    expect(prose.querySelectorAll('code')).toHaveLength(1);
    expect(prose.querySelector('code')?.textContent).toBe('git  status --short');
  });

  it('optimizes formatted list items and quotations in artifact Markdown', async () => {
    const view = render(
      <Markdown>{`- A **formatted** list item chooses all its line breaks together.

> A quoted paragraph with *emphasis* uses the same paragraph optimizer.`}</Markdown>,
    );
    await composed(view.getByRole('listitem'));
    await composed(view.container.querySelector('blockquote p')!);
    expect(view.container.querySelector('strong')).toHaveTextContent('formatted');
    expect(view.container.querySelector('em')).toHaveTextContent('emphasis');
  });

  it('feeds Justice distinct advances for identical words in different styles', async () => {
    const measured: { text: string; width: number }[] = [];
    const advance = (node: Node, bold = false): number => {
      if (node.nodeType === Node.TEXT_NODE)
        return (node.textContent?.length ?? 0) * (bold ? 16 : 8);
      const strong = bold || (node instanceof HTMLElement && node.tagName === 'STRONG');
      return [...node.childNodes].reduce((sum, child) => sum + advance(child, strong), 0);
    };
    measure.mockImplementation(function (this: HTMLElement) {
      const width = advance(this);
      // Placing the paragraph reads its own box, not an advance.
      if (this.tagName !== 'P') measured.push({ text: this.textContent ?? '', width });
      return { width } as DOMRect;
    });
    vi.spyOn(Range.prototype, 'getBoundingClientRect').mockImplementation(function (this: Range) {
      const width = advance(this.cloneContents());
      measured.push({ text: this.toString(), width });
      return { width } as DOMRect;
    });
    const text = 'Read echo echo echo carefully and keep each repeated word in its own style.';
    const view = render(
      <JustifiedProse>
        Read echo <strong>echo</strong> echo carefully and keep each repeated word in its own style.
      </JustifiedProse>,
    );
    const prose = view.getByRole('paragraph');
    await composed(prose);
    expect(measured).toContainEqual({ text: 'echo', width: 32 });
    expect(measured).toContainEqual({ text: 'echo', width: 64 });
    const expected = prepare(text, (word) => word.length * 8);
    for (let index = 3; index < expected.widths.length; index++) expected.widths[index] += 32;
    const layout = solve(expected, width, { hanging: 0, opening: 0 });
    expect([...prose.children].map((line) => line.textContent)).toEqual(
      layout.lines.map((line) => lineText(expected, line)),
    );
    const count = measured.length;
    width = 480;
    resized();
    await composed(prose);
    expect(measured).toHaveLength(count);
    view.rerender(<JustifiedProse>{text}</JustifiedProse>);
    await composed(prose);
    expect(prose.querySelector('strong')).toBeNull();
    expect(measured.length).toBeGreaterThan(count);
    expect(prose.textContent).toBe(text);
  });

  it('measures rich prose without rebuilding layout for each word', async () => {
    const text =
      'Measure every styled word while repeated requests make the whole transcript expensive to compose again and again.';
    const view = render(
      <JustifiedProse>
        Measure every <strong>styled word</strong> while repeated requests make the whole transcript
        expensive to compose again and again.
      </JustifiedProse>,
    );
    const prose = view.getByRole('paragraph');
    await composed(prose);
    expect(prose.textContent).toBe(text);
    expect(measure.mock.calls.length).toBeLessThan(6);
  });

  it('measures prose around inline code without reflowing every ordinary word', async () => {
    const boxes: string[] = [];
    measure.mockImplementation(function (this: HTMLElement) {
      boxes.push(this.textContent ?? '');
      return { width: (this.textContent?.length ?? 0) * glyphWidth } as DOMRect;
    });
    const text =
      'The plain words before and after inline code still need one shared layout when this transcript opens again.';
    const view = render(
      <Markdown>
        {'The plain words before and after `inline code` still need one shared layout when this transcript opens again.'}
      </Markdown>,
    );
    const prose = view.getByRole('paragraph');
    await composed(prose);
    expect(prose.textContent).toBe(text);
    expect(prose.querySelector('code')).toHaveTextContent('inline code');
    // Besides the paragraph, only the space and the code box are laid out: the box whole,
    // and on each side of its break.
    expect(new Set(boxes)).toEqual(new Set([text, ' ', 'inline code', 'inline', 'code']));
  });

  it('retains styled partial words, explicit hyphens, graphemes and link actions', async () => {
    const open = vi.fn();
    const view = render(
      <JustifiedProse>
        A para<strong>graph</strong> keeps well-<em>known</em> words, naïve café é 👩‍💻 👍🏽 and{' '}
        <button onClick={open}>a note</button> intact while wrapping.
      </JustifiedProse>,
    );
    const prose = view.getByRole('paragraph');
    const original = prose.textContent;
    await composed(prose);
    expect(prose.textContent).toBe(original);
    expect(prose.querySelector('strong')).toHaveTextContent('graph');
    expect(prose.querySelector('em')).toHaveTextContent('known');
    fireEvent.click(view.getByRole('button', { name: 'a note' }));
    expect(open).toHaveBeenCalledOnce();
    expect(prose.innerHTML).not.toContain('�');
  });

  it('keeps oversized inline code native instead of changing its spaces or hyphens', async () => {
    const code = 'git  status --short-with-a-long-identifier-name';
    const view = render(<Markdown>{`Read \`${code}\` without changing the command.`}</Markdown>);
    const prose = view.getByRole('paragraph');
    const original = prose.textContent;
    await waitFor(() => expect(measure).toHaveBeenCalled());
    // The hyphenated option alone is wider than the column.
    expect(prose).not.toHaveAttribute('data-justice');
    expect(prose.textContent).toBe(original);
    width = 720;
    resized();
    await composed(prose);
    expect(prose.querySelectorAll('code')).toHaveLength(1);
    expect(prose.querySelector('code')?.textContent).toBe(code);
  });

  it('breaks inline code only after spaces and slashes or before a dot', async () => {
    const path = 'apps/vis-companion/src/components/well-known-name.tsx';
    const text = `Open ${path} and run npm test -- --run --reporter dot before you commit.`;
    const view = render(
      <Markdown>
        {`Open \`${path}\` and run \`npm test -- --run --reporter dot\` before you commit.`}
      </Markdown>,
    );
    const prose = view.getByRole('paragraph');
    await composed(prose);
    expect(prose.textContent).toBe(text);
    expect([...prose.children].map((line) => line.textContent)).toEqual([
      'Open apps/vis-companion/src/',
      'components/well-known-name.tsx',
      'and run npm test -- --run',
      '--reporter dot before you',
      'commit.',
    ]);
    // A space at a break stays between the lines, outside both code boxes.
    for (const part of prose.querySelectorAll('code')) expect(part.textContent).not.toMatch(/^\s|\s$/);
  });

  it('leaves a line short at natural spacing rather than opening holes beside long code', async () => {
    const view = render(
      <Markdown>
        {'Open `src/components/JustifiedProse.tsx` and read how every line is measured before you change it.'}
      </Markdown>,
    );
    const prose = view.getByRole('paragraph');
    await composed(prose);
    const lines = [...prose.children] as HTMLElement[];
    expect(lines.map((line) => line.textContent)).toEqual([
      'Open src/components/',
      'JustifiedProse.tsx and read',
      'how every line is measured',
      'before you change it.',
    ]);
    // One gap beside the path would have to open ten glyphs wide.
    expect(parseFloat(lines[0].style.wordSpacing)).toBe(0);
    expect(parseFloat(lines[1].style.wordSpacing)).toBeGreaterThan(0);
  });

  it('keeps nested block structure and task controls native while optimizing child paragraphs', async () => {
    const view = render(
      <Markdown>
        {
          '- [ ] **Keep this control**\n\n- A paragraph with **emphasis**.\n\n  Another paragraph in the same item.\n\n  - Nested list'
        }
      </Markdown>,
    );
    expect(view.getByRole('checkbox')).not.toBeChecked();
    const outer = view.getAllByRole('listitem').slice(0, 2);
    for (const item of outer) expect(item).not.toHaveAttribute('data-justice');
    for (const prose of view
      .getAllByRole('paragraph')
      .filter((node) => !node.querySelector('input')))
      await composed(prose);
    expect(view.getAllByRole('list')).toHaveLength(2);
  });

  it('enhances Markdown paragraphs and list items, but not compact results or rich hard breaks', async () => {
    const view = render(<Markdown>{`${paragraph}\n\n- ${paragraph}`}</Markdown>);
    await composed(view.getByRole('paragraph'));
    await composed(view.getByRole('listitem'));
    view.rerender(<Markdown nested>{paragraph}</Markdown>);
    expect(view.getByRole('paragraph')).not.toHaveAttribute('data-justice');
    view.rerender(<Markdown hardBreaks>{'First line.\nSecond line with **emphasis**.'}</Markdown>);
    expect(view.getByRole('paragraph')).not.toHaveAttribute('data-justice');
    expect(view.container.querySelectorAll('br')).toHaveLength(1);
    expect(view.getByText('emphasis').tagName).toBe('STRONG');
  });
});
