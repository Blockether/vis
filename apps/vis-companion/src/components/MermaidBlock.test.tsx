// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { MermaidBlock } from './MermaidBlock';

// The real library needs a laid-out browser to measure text, and it is loaded
// lazily on purpose; the contract this component owns is what it asks mermaid
// for and what it paints when mermaid cannot answer.
const mermaid = vi.hoisted(() => ({
  initialize: vi.fn(),
  parse: vi.fn(),
  render: vi.fn(),
}));

vi.mock('mermaid', () => ({ default: mermaid }));

const SOURCE = 'flowchart TD\n  A[Start] --> B[Stop]';

const fallback = <pre data-testid="fence">{SOURCE}</pre>;

beforeEach(() => {
  mermaid.initialize.mockReset();
  mermaid.parse.mockReset().mockResolvedValue(true);
  mermaid.render.mockReset().mockImplementation(async (id: string) => ({
    svg: `<svg data-drawn="${id}"><g class="node"></g></svg>`,
  }));
  document.documentElement.removeAttribute('data-theme');
});

afterEach(cleanup);

describe('a mermaid fence', () => {
  it('is painted as the picture mermaid draws from it', async () => {
    render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);

    const picture = await screen.findByRole('img', { name: 'Diagram' });
    expect(picture.querySelector('svg')).toBeInTheDocument();
    expect(mermaid.render).toHaveBeenCalledWith(expect.stringContaining('vis-diagram-'), SOURCE);
    expect(screen.queryByTestId('fence')).toBeNull();
  });

  it('asks mermaid for the transcript palette and refuses HTML in the labels', async () => {
    render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);
    await screen.findByRole('img', { name: 'Diagram' });

    expect(mermaid.initialize).toHaveBeenCalledWith(
      expect.objectContaining({
        startOnLoad: false,
        securityLevel: 'strict',
        theme: 'base',
        flowchart: { htmlLabels: false, useMaxWidth: true },
      }),
    );
    const [config] = mermaid.initialize.mock.calls[0] as [
      { themeVariables: Record<string, string>; fontFamily: string },
    ];
    expect(config.fontFamily).toContain('JetBrains Mono');
    expect(Object.keys(config.themeVariables)).toEqual(
      expect.arrayContaining(['background', 'lineColor', 'nodeBorder', 'textColor']),
    );
  });

  it('carries a copy control for the source, and drops it inside a card', async () => {
    const { unmount } = render(
      <MermaidBlock source={SOURCE} compact={false} fallback={fallback} />,
    );
    await screen.findByRole('img', { name: 'Diagram' });
    expect(screen.getByRole('button', { name: 'Copy diagram source' })).toBeInTheDocument();

    unmount();
    render(<MermaidBlock source={SOURCE} compact fallback={fallback} frameless />);
    await screen.findByRole('img', { name: 'Diagram' });
    expect(screen.queryByRole('button', { name: 'Copy diagram source' })).toBeNull();
  });

  it('draws the diagram again when the session changes palette', async () => {
    render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);
    await screen.findByRole('img', { name: 'Diagram' });
    expect(mermaid.render).toHaveBeenCalledTimes(1);

    document.documentElement.dataset.theme = 'blockether-dark';

    await waitFor(() => expect(mermaid.render).toHaveBeenCalledTimes(2));
  });

  it('shows the fence itself when mermaid rejects the source', async () => {
    mermaid.parse.mockResolvedValue(false);
    render(<MermaidBlock source={'flowchart TD\n  A[' } compact={false} fallback={fallback} />);

    expect(await screen.findByTestId('fence')).toBeInTheDocument();
    expect(mermaid.render).not.toHaveBeenCalled();
  });

  it('shows the fence itself when the drawing fails', async () => {
    mermaid.render.mockRejectedValue(new Error('no layout'));
    render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);

    expect(await screen.findByTestId('fence')).toBeInTheDocument();
  });

  it('paints nothing while the library is still loading', () => {
    mermaid.parse.mockReturnValue(new Promise(() => {}));
    const { container } = render(
      <MermaidBlock source={SOURCE} compact={false} fallback={fallback} />,
    );

    expect(container).toBeEmptyDOMElement();
  });
});

describe('copying a diagram as a picture', () => {
  // User request (paraphrased: a mermaid diagram cannot be copied as an image).
  it('puts a PNG of the drawing on the clipboard, sized from its viewBox', async () => {
    mermaid.render.mockResolvedValue({
      svg: '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 120" width="100%" style="max-width: 300px"><g></g></svg>',
    });
    const sources: string[] = [];
    vi.stubGlobal(
      'Image',
      class {
        src = '';
        decode() {
          sources.push(this.src);
          return Promise.resolve();
        }
      },
    );
    const paint = { fillRect: vi.fn(), drawImage: vi.fn(), fillStyle: '' };
    vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue(
      paint as unknown as CanvasRenderingContext2D,
    );
    const sizes: Array<[number, number]> = [];
    vi.spyOn(HTMLCanvasElement.prototype, 'toBlob').mockImplementation(function (
      this: HTMLCanvasElement,
      done: BlobCallback,
    ) {
      sizes.push([this.width, this.height]);
      done(new Blob(['png'], { type: 'image/png' }));
    });
    let copied: Blob | undefined;
    vi.stubGlobal(
      'ClipboardItem',
      class {
        readonly data: Record<string, Promise<Blob>>;
        constructor(data: Record<string, Promise<Blob>>) {
          this.data = data;
        }
      },
    );
    const write = vi.fn(async (items: Array<{ data: Record<string, Promise<Blob>> }>) => {
      copied = await items[0].data['image/png'];
    });
    vi.stubGlobal('navigator', { ...navigator, clipboard: { write } });

    try {
      render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);
      fireEvent.click(await screen.findByRole('button', { name: 'Copy diagram as image' }));

      await waitFor(() => expect(copied?.type).toBe('image/png'));
      expect(sizes).toEqual([[600, 240]]);
      const drawn = decodeURIComponent(sources[0].replace(/^data:image\/svg\+xml;charset=utf-8,/u, ''));
      expect(drawn).toContain('width="300"');
      expect(drawn).toContain('height="120"');
      expect(drawn).not.toContain('max-width');
      expect(paint.fillRect).toHaveBeenCalledWith(0, 0, 600, 240);
      expect(await screen.findByRole('button', { name: 'Copied' })).toBeInTheDocument();
    } finally {
      vi.unstubAllGlobals();
      vi.restoreAllMocks();
    }
  });
});
