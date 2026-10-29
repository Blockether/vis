// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { MermaidBlock } from './MermaidBlock';

// These tests run the REAL renderer: Pele draws synchronously without browser
// layout, so the contract is checked against the picture it actually produces.

const SOURCE = 'flowchart TD\n  A[Start] --> B[Stop]';

const fallback = <pre data-testid="fence">{SOURCE}</pre>;

afterEach(() => {
  cleanup();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  document.documentElement.removeAttribute('data-theme');
});

describe('a mermaid fence', () => {
  it('is painted as the picture Pele draws from it', async () => {
    render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);

    const picture = await screen.findByRole('img', { name: 'Diagram' });
    expect(picture.textContent).toContain('Start');
    expect(picture.textContent).toContain('Stop');
    expect(screen.queryByTestId('fence')).toBeNull();
  });

  // Regression, user report (paraphrased: the diagram renders again from scratch
  // every time it shows): each mount waited for the library and drew anew.
  it('paints a drawn source in the first frame when it mounts again', async () => {
    const first = render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);
    const drawn = (await screen.findByRole('img', { name: 'Diagram' })).innerHTML;
    first.unmount();

    render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);

    expect(screen.getByRole('img', { name: 'Diagram' }).innerHTML).toBe(drawn);
  });

  it('takes its colours from the code palette and keeps the drawing on a palette change', async () => {
    render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);
    const picture = await screen.findByRole('img', { name: 'Diagram' });
    const drawn = picture.innerHTML;
    expect(drawn).toContain('var(--pele-bg');

    act(() => {
      document.documentElement.dataset.theme = 'blockether-dark';
    });

    expect(screen.getByRole('img', { name: 'Diagram' }).innerHTML).toBe(drawn);
  });
});

describe('before the renderer is loaded', () => {
  it('shows the fence itself when the library cannot load', async () => {
    vi.resetModules();
    vi.doMock('pele', () => {
      throw new Error('chunk gone');
    });
    const { MermaidBlock: Fresh } = await import('./MermaidBlock');
    try {
      render(<Fresh source={SOURCE} compact={false} fallback={fallback} />);
      expect(await screen.findByTestId('fence')).toBeInTheDocument();
    } finally {
      vi.doUnmock('pele');
    }
  });
});

describe('copying a diagram as a picture', () => {
  // User request (paraphrased: a mermaid diagram cannot be copied as an image).
  it('puts a PNG of the drawing on the clipboard, sized from its viewBox and with its colours baked in', async () => {
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

    render(<MermaidBlock source={SOURCE} compact={false} fallback={fallback} />);
    const svg = (await screen.findByRole('img', { name: 'Diagram' })).querySelector('svg');
    const [, , width, height] = (svg?.getAttribute('viewBox') ?? '').split(/\s+/u).map(Number);
    fireEvent.click(screen.getByRole('button', { name: 'Copy diagram as image' }));

    await waitFor(() => expect(copied?.type).toBe('image/png'));
    expect(sizes).toEqual([[Math.round(width * 2), Math.round(height * 2)]]);
    const drawn = decodeURIComponent(sources[0].replace(/^data:image\/svg\+xml;charset=utf-8,/u, ''));
    expect(drawn).toContain(`width="${width}"`);
    expect(drawn).not.toContain('var(--pele-');
    expect(paint.fillRect).toHaveBeenCalledWith(0, 0, Math.round(width * 2), Math.round(height * 2));
    expect(await screen.findByRole('button', { name: 'Copied' })).toBeInTheDocument();
  });
});
