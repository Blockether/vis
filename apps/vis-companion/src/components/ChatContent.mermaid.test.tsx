// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { Markdown } from './ChatContent';

// The real renderer draws here: Pele needs no browser layout. This suite checks
// that a `mermaid` fence reaches the diagram block at all, and that a fence Pele
// cannot draw is still readable as source.

const FENCE = '```mermaid\nflowchart TD\n  A[Start] --> B[Stop]\n```';

describe('a mermaid fence in a message', () => {
  it('becomes a diagram instead of a code listing', async () => {
    render(<Markdown>{FENCE}</Markdown>);

    const picture = await screen.findByRole('img', { name: 'Diagram' });
    expect(picture.textContent).toContain('Stop');
    expect(screen.queryByText('flowchart TD')).toBeNull();
  });

  it('keeps the source readable when Pele cannot draw it', async () => {
    render(<Markdown>{'```mermaid\nsequenceDiagram\n  A->>\n```'}</Markdown>);

    expect(await screen.findByText(/sequenceDiagram/)).toBeInTheDocument();
    expect(screen.queryByRole('img', { name: 'Diagram' })).toBeNull();
  });
});
