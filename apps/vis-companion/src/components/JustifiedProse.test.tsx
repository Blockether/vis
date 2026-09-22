// @vitest-environment jsdom
import { act, render, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { Markdown } from './ChatContent';
import { JustifiedProse } from './JustifiedProse';

const paragraph =
  'A paragraph should choose its line breaks together rather than treating every line as an isolated decision. The final line can remain naturally short.';
let width = 240;
let glyphWidth = 8;
let resize: ResizeObserverCallback;
let disconnect: ReturnType<typeof vi.fn>;
let measure: ReturnType<typeof vi.spyOn>;
let fonts: EventTarget & { ready: Promise<void> };
const originalFonts = Object.getOwnPropertyDescriptor(document, 'fonts');
const nativeStyle = window.getComputedStyle;

beforeEach(() => {
  width = 240;
  glyphWidth = 8;
  disconnect = vi.fn();
  fonts = Object.assign(new EventTarget(), { ready: Promise.resolve() });
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
});

afterEach(() => {
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
  if (originalFonts) Object.defineProperty(document, 'fonts', originalFonts);
  else Reflect.deleteProperty(document, 'fonts');
});

async function composed(element: Element) {
  await waitFor(() => expect(element).toHaveAttribute('data-justice'));
}

function resized() {
  act(() => resize([], {} as ResizeObserver));
}

describe('Justice prose', () => {
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

  it('shows the latest streamed text immediately and drops composition for rich markup', async () => {
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
    expect(prose).not.toHaveAttribute('data-justice');
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

  it('enhances Markdown paragraphs and list items, but not compact results or rich hard breaks', async () => {
    const view = render(<Markdown>{`${paragraph}\n\n- ${paragraph}`}</Markdown>);
    await composed(view.getByRole('paragraph'));
    await composed(view.getByRole('listitem'));
    view.rerender(<Markdown nested>{paragraph}</Markdown>);
    expect(view.getByRole('paragraph')).not.toHaveAttribute('data-justice');
    view.rerender(<Markdown hardBreaks>{'First line.\nSecond line with **emphasis**.'}</Markdown>);
    expect(view.getByRole('paragraph')).not.toHaveAttribute('data-justice');
    expect(view.container.querySelector('br')).not.toBeNull();
    expect(view.getByText('emphasis').tagName).toBe('STRONG');
  });
});
