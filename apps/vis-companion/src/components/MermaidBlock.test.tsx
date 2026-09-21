// @vitest-environment jsdom
import { cleanup, render, screen, waitFor } from '@testing-library/react';
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
