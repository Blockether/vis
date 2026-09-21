// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { STORY_GATEWAYS, STORY_SESSION_ROW, STORY_SESSION_SEARCH_MATCH } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import type { SessionMatch } from '../lib/gateway';
import { SessionRow } from './SessionList';

function row(match: SessionMatch = STORY_SESSION_SEARCH_MATCH, needle = 'windows') {
  return render(
    <SessionRow
      session={STORY_SESSION_ROW}
      group={null}
      draft={EMPTY_DRAFT_MESSAGE}
      conn={STORY_GATEWAYS[0]}
      match={match}
      needle={needle}
      commands={{
        open: vi.fn(),
        rename: vi.fn(async () => {}),
        requestDelete: vi.fn(),
        toggleStar: vi.fn(),
      }}
      deletion={null}
    />,
  );
}

function snippet(text: string): SessionMatch {
  return { ...STORY_SESSION_SEARCH_MATCH, hits: [{ side: 'reply', snippet: text, at: null }] };
}

describe('session search previews', () => {
  // Regression, user screenshot: search displayed raw Markdown and highlighted its source.
  it('renders inline Markdown and highlights visible text within each mark', () => {
    const { container } = row();

    expect(container.querySelector('strong')).toHaveTextContent('Windows');
    expect(container.querySelector('em')).toHaveTextContent('macOS');
    expect(container.querySelector('del')).toHaveTextContent('skip Windows');
    expect(container.querySelector('code')).toHaveTextContent('WINDOWS');
    expect(container.querySelector('strong mark')).toHaveTextContent('Windows');
    expect(container.querySelector('code mark')).toHaveTextContent('WINDOWS');
    expect(container.querySelector('del mark')).toHaveTextContent('Windows');
    expect(container.querySelectorAll('mark')).toHaveLength(6);
    expect(container.textContent).not.toMatch(/\*\*|~~|`|https:\/\//);
    expect(screen.getAllByText('Vis')).toHaveLength(2);
    expect(screen.getByText('You')).toBeInTheDocument();
  });

  it('keeps search text literal and case-insensitive, including regex punctuation', () => {
    const { container } = row(snippet('**win(dows)+** and WIN(DOWS)+, not windows'), 'win(dows)+');

    expect([...container.querySelectorAll('mark')].map((mark) => mark.textContent)).toEqual([
      'win(dows)+',
      'WIN(DOWS)+',
    ]);
    expect(container.querySelector('strong mark')).toBeInTheDocument();
  });

  it('keeps code literal and external content inert while preserving labels', () => {
    const { container } = row(
      snippet(
        '`<Windows>` [Windows docs](javascript:alert%281%29) ![Windows screenshot](https://example.com/image.png) <script>alert(1)</script>',
      ),
    );

    expect(container.querySelector('code')).toHaveTextContent('<Windows>');
    expect(container.querySelector('a, img, script')).toBeNull();
    expect(container.textContent).toContain('Windows docs');
    expect(container.textContent).toContain('Windows screenshot');
    // Skipped HTML tags leave harmless text, never an executable element.
    expect(container.textContent).toContain('alert(1)');
    expect(container.querySelectorAll('mark')).toHaveLength(3);
  });

  it('renders without highlights for an empty query or a match only in a URL', () => {
    const { container, unmount } = row(
      snippet('**Windows** ![diagram](https://example.com/image.png)'),
      '',
    );
    expect(container.querySelector('strong')).toHaveTextContent('Windows');
    expect(container.querySelector('mark, img')).toBeNull();
    expect(container.textContent).toContain('diagram');
    unmount();

    const next = row(snippet('[Documentation](https://example.com/Windows)'));
    expect(next.container.textContent).toContain('Documentation');
    expect(next.container.querySelector('mark, a')).toBeNull();
  });

  it('renders fallback request and reply snippets through the same Markdown path', () => {
    const { container } = row({
      ...STORY_SESSION_SEARCH_MATCH,
      hits: [],
      requestSnippet: '  **Windows** request  ',
      replySnippet: '_Windows_ reply',
    });

    expect(container.querySelector('strong mark')).toHaveTextContent('Windows');
    expect(container.querySelector('em mark')).toHaveTextContent('Windows');
    expect(screen.getByText('You')).toBeInTheDocument();
    expect(screen.getByText('Vis')).toBeInTheDocument();
  });

  it('keeps block Markdown compact and preserves its words', () => {
    const { container } = row(
      snippet('# Windows checks\n\n- **Windows** passes\n- `Windows` is enabled'),
    );

    expect(container.textContent).toContain('Windows checks');
    expect(container.textContent).toContain('Windows passes');
    expect(container.textContent).toContain('Windows is enabled');
    expect(container.querySelector('h1, ul, pre')).toBeNull();
    expect(container.querySelectorAll('mark')).toHaveLength(3);
  });

  it('does not add an empty preview for a title-only match', () => {
    const { container } = row({
      ...STORY_SESSION_SEARCH_MATCH,
      hits: [],
      requestSnippet: '  ',
      replySnippet: null,
    });

    expect(screen.queryByText('You')).toBeNull();
    expect(screen.queryByText('Vis')).toBeNull();
    expect(container.querySelector('mark')).toBeNull();
  });
});
