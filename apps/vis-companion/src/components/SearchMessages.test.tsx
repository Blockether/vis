// @vitest-environment jsdom
import { fireEvent, render, screen, within } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { STORY_SESSION_SEARCH_MATCH } from '../dev/story-data';
import { timeLabel } from '../lib/fleet';
import type { SessionMatch } from '../lib/gateway';
import { SearchMessages } from './SearchMessages';

function pane({
  match = STORY_SESSION_SEARCH_MATCH,
  query = 'windows',
  title = 'Release checks',
  isSearching = false,
}: { match?: SessionMatch | null; query?: string; title?: string; isSearching?: boolean } = {}) {
  const onOpen = vi.fn();
  const view = render(
    <SearchMessages
      title={title}
      match={match}
      query={query}
      isSearching={isSearching}
      onOpen={onOpen}
    />,
  );
  return { ...view, onOpen };
}

function snippet(text: string): SessionMatch {
  return { ...STORY_SESSION_SEARCH_MATCH, hits: [{ side: 'reply', snippet: text, at: null }] };
}

const marks = (container: HTMLElement) =>
  [...container.querySelectorAll('mark')].map((mark) => mark.textContent);

describe('search messages pane', () => {
  // Regression, user screenshot: search displayed raw Markdown and highlighted its source.
  it('renders inline Markdown and highlights visible text within each mark', () => {
    const { container } = pane();

    expect(container.querySelector('strong')).toHaveTextContent('Windows');
    expect(container.querySelector('em')).toHaveTextContent('macOS');
    expect(container.querySelector('del')).toHaveTextContent('skip Windows');
    expect(container.querySelector('code')).toHaveTextContent('WINDOWS');
    expect(container.querySelector('strong mark')).toHaveTextContent('Windows');
    expect(container.querySelector('code mark')).toHaveTextContent('WINDOWS');
    expect(container.querySelector('del mark')).toHaveTextContent('Windows');
    expect(container.querySelectorAll('mark')).toHaveLength(6);
    expect(container.textContent).not.toMatch(/\*\*|~~|`|https:\/\//);
  });

  it('labels each message with who wrote it, where and when', () => {
    pane();
    const items = screen.getAllByRole('listitem');

    expect(items.map((item) => within(item).getByText(/^(You|Vis)$/).textContent)).toEqual([
      'You',
      'Vis',
      'Vis',
    ]);
    expect(within(items[2]).getByText('thinking')).toBeVisible();
    const at = new Date(STORY_SESSION_SEARCH_MATCH.hits[0].at!).toISOString();
    expect(within(items[0]).getByText(timeLabel(at))).toBeVisible();
  });

  it('marks each query word on its own, without case, and reads punctuation as a break', () => {
    const { container } = pane({
      match: snippet('**win(dows)+** and WIN(DOWS)+, not windows'),
      query: 'win(dows)+',
    });

    expect(marks(container)).toEqual(['win', 'dows', 'WIN', 'DOWS', 'win']);
    expect(container.querySelector('strong mark')).toBeInTheDocument();
  });

  it('keeps code literal and external content inert while preserving labels', () => {
    const { container } = pane({
      match: snippet(
        '`<Windows>` [Windows docs](javascript:alert%281%29) ![Windows screenshot](https://example.com/image.png) <script>alert(1)</script>',
      ),
    });

    expect(container.querySelector('code')).toHaveTextContent('<Windows>');
    expect(container.querySelector('a, img, script')).toBeNull();
    expect(container.textContent).toContain('Windows docs');
    expect(container.textContent).toContain('Windows screenshot');
    // Skipped HTML tags leave harmless text, never an executable element.
    expect(container.textContent).toContain('alert(1)');
    expect(container.querySelectorAll('mark')).toHaveLength(3);
  });

  it('renders without highlights for an empty query or a match only in a URL', () => {
    const { container, unmount } = pane({
      match: snippet('**Windows** ![diagram](https://example.com/image.png)'),
      query: '',
    });
    expect(container.querySelector('strong')).toHaveTextContent('Windows');
    expect(container.querySelector('mark, img')).toBeNull();
    expect(container.textContent).toContain('diagram');
    unmount();

    const next = pane({ match: snippet('[Documentation](https://example.com/Windows)') });
    expect(next.container.textContent).toContain('Documentation');
    expect(next.container.querySelector('mark, a')).toBeNull();
  });

  it('renders fallback request and reply snippets through the same Markdown path', () => {
    const { container } = pane({
      match: {
        ...STORY_SESSION_SEARCH_MATCH,
        hits: [],
        requestSnippet: '  **Windows** request  ',
        replySnippet: '_Windows_ reply',
      },
    });

    expect(container.querySelector('strong mark')).toHaveTextContent('Windows');
    expect(container.querySelector('em mark')).toHaveTextContent('Windows');
    expect(screen.getByText('You')).toBeInTheDocument();
    expect(screen.getByText('Vis')).toBeInTheDocument();
  });

  it('keeps block Markdown compact and preserves its words', () => {
    const { container } = pane({
      match: snippet('# Windows checks\n\n- **Windows** passes\n- `Windows` is enabled'),
    });

    expect(container.textContent).toContain('Windows checks');
    expect(container.textContent).toContain('Windows passes');
    expect(container.textContent).toContain('Windows is enabled');
    expect(container.querySelector('h1, ul, pre')).toBeNull();
    expect(container.querySelectorAll('mark')).toHaveLength(3);
  });

  it('says why there is no message to show', () => {
    const titleOnly: SessionMatch = {
      ...STORY_SESSION_SEARCH_MATCH,
      inTitle: true,
      hits: [],
      requestSnippet: '  ',
      replySnippet: null,
    };
    const { container, unmount } = pane({ title: 'Windows release', match: titleOnly });
    expect(screen.getByText('The title matches. No message matches.')).toBeVisible();
    expect(screen.queryByText('You')).toBeNull();
    expect(screen.queryByText('Vis')).toBeNull();
    expect(marks(container)).toEqual(['Windows']);
    unmount();

    pane({ match: null, isSearching: true });
    expect(screen.getByText('Searching messages...')).toBeVisible();
  });

  it('says so when neither the title nor a message matches', () => {
    pane({ match: null });
    expect(screen.getByText('No message matches.')).toBeVisible();
  });

  it('opens the session from the Open button or from any message', () => {
    const { onOpen } = pane();

    fireEvent.click(screen.getByRole('button', { name: 'Open' }));
    for (const item of screen.getAllByRole('listitem')) {
      fireEvent.click(within(item).getByRole('button'));
    }
    expect(onOpen).toHaveBeenCalledTimes(4);
  });
});
