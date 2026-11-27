// @vitest-environment jsdom
import { render, screen, within } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import councilWake from '../../../../packages/vis-contract/resources/vis-contract/fixtures/council-wake.json';
import { STORY_SESSION_SEARCH_MATCH } from '../dev/story-data';
import type { SessionMatch } from '../lib/gateway';
import { SearchMessages } from './SearchMessages';
import type { ForkPoint } from '../lib/types';

type SearchMessagesRecent = readonly ForkPoint[] | 'loading' | 'failed';

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
});

// User report (paraphrased): in the app and in the TUI, the pane at the right of the session
// list must always show a part of the conversation, also before a query.
describe('search messages pane without a message match', () => {
  const turns: ForkPoint[] = [
    { turn_id: 't1', request: 'First ask', answer: 'First **answer**', created_at: 0 },
    { turn_id: 't2', request: 'Second ask', created_at: 60_000 },
  ];
  const recentPane = (query: string, recent: SearchMessagesRecent, match: SessionMatch | null = null) =>
    render(
      <SearchMessages title="Release checks" match={match} query={query} isSearching={false} recent={recent} onOpen={vi.fn()} />,
    );

  it('shows the newest messages before a query, newest turn first', () => {
    const { container } = recentPane('', turns);
    const items = screen.getAllByRole('listitem');

    expect(items.map((item) => item.textContent)).toEqual([
      expect.stringContaining('Second ask'),
      expect.stringContaining('First ask'),
      expect.stringContaining('First answer'),
    ]);
    expect(within(items[2]).getByText('Vis')).toBeVisible();
    expect(container.querySelector('strong')).toHaveTextContent('answer');
    expect(screen.queryByText(/matches/)).toBeNull();
  });

  it('keeps the note above the newest messages when a query matches no message', () => {
    recentPane('zzz', turns);

    expect(screen.getByText('No message matches.')).toBeVisible();
    expect(screen.getAllByRole('listitem')).toHaveLength(3);
  });

  it('shows the matched messages, not the newest ones, when a message matches', () => {
    recentPane('windows', turns, STORY_SESSION_SEARCH_MATCH);

    expect(screen.queryByText('Second ask')).toBeNull();
  });

  it('shows Council entries from recent turns and matched requests under Council', () => {
    const councilTurns: ForkPoint[] = councilWake.turns.map((turn) => ({ ...turn, request_kind: 'council' }));
    const { unmount } = recentPane('', councilTurns);
    expect(screen.getAllByRole('listitem').map((item) => item.textContent)).toEqual(
      [...councilWake.turns].reverse().map(({ council }) => `Council${council.note ?? council.content}`),
    );
    unmount();

    const { council, request } = councilWake.turns[1];
    const { container } = recentPane('two', [], {
      ...STORY_SESSION_SEARCH_MATCH,
      hits: [{ side: 'request', requestKind: 'council', council, snippet: request, at: null }],
    });
    expect(screen.getByRole('listitem')).toHaveTextContent(`Council${council.note}`);
    expect(marks(container)).toEqual(['two']);
    expect(container.querySelector('code')).toBeNull();
  });

  it('shows why a Council batch matches when the words are only in a later entry', () => {
    const { council, request } = councilWake.turns.find((turn) => turn.turn_id === 't5')!;
    const { container } = recentPane('parser tests', [], {
      ...STORY_SESSION_SEARCH_MATCH,
      hits: [{ side: 'request', requestKind: 'council', council, snippet: 'All parser tests pass.', at: null }],
    });
    const row = screen.getByRole('listitem');
    expect(row).toHaveTextContent('Council');
    expect(row).toHaveTextContent(request.replace(/\s+/g, ' '));
    expect(marks(container)).toEqual(['parser', 'tests']);
  });

  it('keeps the six newest turns and cuts a long request', () => {
    const many: ForkPoint[] = Array.from({ length: 8 }, (_, index) => ({ turn_id: `t${index}`, request: `Ask ${index}` }));
    const { unmount } = recentPane('', many);
    expect(screen.getAllByRole('listitem').map((item) => item.textContent)).toEqual(
      [7, 6, 5, 4, 3, 2].map((index) => `YouAsk ${index}`),
    );
    unmount();

    recentPane('', [{ turn_id: 'long', request: 'a'.repeat(500) }]);
    const text = screen.getByRole('listitem').textContent ?? '';
    expect(text).toMatch(/…$/);
    expect(text.length).toBe('You'.length + 400);
  });
});
