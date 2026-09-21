// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { beforeEach, describe, expect, it, vi } from 'vitest';
import { Markdown } from './ChatContent';

// The library is loaded lazily and needs a laid-out browser to measure text, so
// the drawing itself is stubbed here: what this suite checks is that a `mermaid`
// fence reaches the diagram block at all, and that an unsupported one is still
// readable as source.
const mermaid = vi.hoisted(() => ({
  initialize: vi.fn(),
  parse: vi.fn(),
  render: vi.fn(),
}));

vi.mock('mermaid', () => ({ default: mermaid }));

const FENCE = '```mermaid\nflowchart TD\n  A[Start] --> B[Stop]\n```';

beforeEach(() => {
  mermaid.initialize.mockReset();
  mermaid.parse.mockReset().mockResolvedValue(true);
  mermaid.render.mockReset().mockResolvedValue({ svg: '<svg><g class="node"></g></svg>' });
});

describe('a mermaid fence in a message', () => {
  it('becomes a diagram instead of a code listing', async () => {
    render(<Markdown>{FENCE}</Markdown>);

    const picture = await screen.findByRole('img', { name: 'Diagram' });
    expect(picture.querySelector('svg')).toBeInTheDocument();
    expect(mermaid.render).toHaveBeenCalledWith(
      expect.any(String),
      'flowchart TD\n  A[Start] --> B[Stop]',
    );
    expect(screen.queryByText('flowchart TD')).toBeNull();
  });

  it('keeps the source readable when mermaid will not draw it', async () => {
    mermaid.parse.mockResolvedValue(false);
    render(<Markdown>{'```mermaid\nsequenceDiagram\n  A->>B: hi\n```'}</Markdown>);

    expect(await screen.findByText(/sequenceDiagram/)).toBeInTheDocument();
    expect(screen.queryByRole('img', { name: 'Diagram' })).toBeNull();
  });
});
