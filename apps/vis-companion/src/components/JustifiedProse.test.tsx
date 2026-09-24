// @vitest-environment jsdom
import { act, fireEvent, render, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { Markdown } from './ChatContent';
import { JustifiedProse } from './JustifiedProse';
import { MarkdownAnnotator } from './MarkdownArtifact';
import { lineText, prepare, solve } from '@kitlangton/justice';

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

function resized() {
  act(() => resize([], {} as ResizeObserver));
}

describe('Justice prose', () => {
  // #282 follow-up: opening Markdown must not paint native text, then reflow it.
  it('composes the opening render without waiting for an animation frame', () => {
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    expect(prose).toHaveAttribute('data-justice');
    expect(prose.children.length).toBeGreaterThan(1);
    expect(prose.textContent).toBe(paragraph);
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
    const observe = vi.fn();
    const unobserve = vi.fn();
    const disconnect = vi.fn();
    const create = vi.fn();
    vi.stubGlobal(
      'IntersectionObserver',
      class {
        constructor(callback: IntersectionObserverCallback) {
          create();
          notify = callback;
        }
        observe = observe;
        unobserve = unobserve;
        disconnect = disconnect;
      },
    );
    measure.mockImplementation(function (this: HTMLElement) {
      if (this.tagName === 'P')
        return { width, top: 3000, bottom: 3040 } as DOMRect;
      return { width: (this.textContent?.length ?? 0) * glyphWidth } as DOMRect;
    });

    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const prose = view.getByRole('paragraph');
    expect(prose).not.toHaveAttribute('data-justice');
    expect(prose.textContent).toBe(paragraph);
    expect(observe).toHaveBeenCalledWith(prose);
    expect(measure.mock.instances).toEqual([prose]);
    const second = render(<JustifiedProse>{paragraph}</JustifiedProse>);
    const next = second.container.querySelector('p')!;
    expect(create).toHaveBeenCalledOnce();
    expect(observe).toHaveBeenCalledWith(next);
    expect(measure.mock.instances).toEqual([prose, next]);

    act(() =>
      notify(
        [{ isIntersecting: true, target: prose } as unknown as IntersectionObserverEntry],
        {} as IntersectionObserver,
      ),
    );
    await composed(prose);
    expect(prose.textContent).toBe(paragraph);
    expect(unobserve).toHaveBeenCalledWith(prose);
    expect(disconnect).not.toHaveBeenCalled();
    expect(next).not.toHaveAttribute('data-justice');
    second.unmount();
    expect(unobserve).toHaveBeenCalledWith(next);
    view.unmount();
    expect(disconnect).toHaveBeenCalledOnce();
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

  it('keeps readable native text when the initial measurement fails', () => {
    measure.mockImplementation(() => {
      throw new Error('Measurement unavailable');
    });
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
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
    const measurements = measure.mock.calls.length;
    width = 480;
    resized();
    await waitFor(() => expect(prose.children.length).toBeLessThan(narrowLines));
    expect(measure.mock.calls.length).toBe(measurements);
    glyphWidth = 12;
    act(() => fonts.dispatchEvent(new Event('loadingdone')));
    await waitFor(() => expect(measure.mock.calls.length).toBeGreaterThan(measurements));
    expect(prose.textContent).toBe(paragraph);
  });

  it('does not remeasure when fonts were already loaded at composition', async () => {
    fonts.status = 'loaded';
    const view = render(<JustifiedProse>{paragraph}</JustifiedProse>);
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
    const measurements = measure.mock.calls.length;

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
    expect(measure.mock.calls.length).toBe(measurements);
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
      measured.push({ text: this.textContent ?? '', width });
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
    expect(measure.mock.calls.length).toBeLessThan(6);
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
    const code = 'git  status --short-with-a-long-identifier';
    const view = render(<Markdown>{`Read \`${code}\` without changing the command.`}</Markdown>);
    const prose = view.getByRole('paragraph');
    const original = prose.textContent;
    await waitFor(() => expect(measure).toHaveBeenCalled());
    expect(prose).not.toHaveAttribute('data-justice');
    expect(prose.textContent).toBe(original);
    width = 640;
    resized();
    await composed(prose);
    expect(prose.querySelectorAll('code')).toHaveLength(1);
    expect(prose.querySelector('code')?.textContent).toBe(code);
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
