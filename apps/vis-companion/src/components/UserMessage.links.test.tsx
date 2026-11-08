// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/message-links.json';
import { OpenPathContext } from '../lib/open-path';
import { Markdown, UserMessage } from './ChatContent';

afterEach(cleanup);

/** The links of a rendered message in order: a web link by its href, a file link by its text. */
function links(container: HTMLElement): string[] {
  return [...container.querySelectorAll('a, span[role="button"]')].map(
    (link) => link.getAttribute('href') ?? link.textContent ?? '',
  );
}

// #446: the TUI reads the same cases, so both clients link the same URLs.
it.each(fixture.cases)('links the bare URLs of a sent message: $name', ({ text, links: urls }) => {
  const view = render(
    <OpenPathContext.Provider value={vi.fn()}>
      <UserMessage>{text}</UserMessage>
    </OpenPathContext.Provider>,
  );
  expect([...new Set(links(view.container))]).toEqual(urls);
  // The link shows the URL as the user typed it, and a web link opens outside the app.
  for (const a of view.container.querySelectorAll('a')) {
    expect(a.textContent).toBe(a.getAttribute('href'));
    expect(a.getAttribute('target')).toBe('_blank');
  }
});

it('keeps the punctuation after a link as text of the bubble', () => {
  const view = render(<UserMessage>{'Open https://example.com/docs. Thanks'}</UserMessage>);
  expect(view.container.textContent).toContain('Open https://example.com/docs. Thanks');
});

// #449: a file URL in a message did nothing, so a recording on the gateway could not be played.
it('opens the file preview with the decoded path when a file URL is pressed', () => {
  const openPath = vi.fn();
  const view = render(
    <OpenPathContext.Provider value={openPath}>
      <UserMessage>{'Listen to file:///Users/me/Podcasts/my%20show.mp3.'}</UserMessage>
    </OpenPathContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'file:///Users/me/Podcasts/my%20show.mp3' }));
  expect(openPath).toHaveBeenCalledWith('/Users/me/Podcasts/my show.mp3');
  // A file URL never becomes a browser link.
  expect(view.container.querySelector('a')).toBeNull();
});

it('keeps a file URL as text when its encoding does not decode', () => {
  const view = render(
    <OpenPathContext.Provider value={vi.fn()}>
      <UserMessage>{'Broken file:///tmp/a%E0%A4%A.mp3 here'}</UserMessage>
    </OpenPathContext.Provider>,
  );
  expect(view.container.querySelector('a, [role="button"]')).toBeNull();
  expect(view.container.textContent).toContain('Broken file:///tmp/a%E0%A4%A.mp3 here');
});

it('keeps a file URL as text when nothing can open it', () => {
  const view = render(<UserMessage>{'See file:///tmp/a.mp3 now'}</UserMessage>);
  expect(view.container.querySelector('a, [role="button"]')).toBeNull();
  expect(view.container.textContent).toContain('See file:///tmp/a.mp3 now');
});

// #449: react-markdown dropped the file URL of a link that the model wrote, so it was dead text.
it('opens the file preview from a file link that the model wrote', () => {
  const openPath = vi.fn();
  const view = render(
    <OpenPathContext.Provider value={openPath}>
      <Markdown>{'The episode is [here](file:///Users/me/final/zero%20to%20demo.mp3).'}</Markdown>
    </OpenPathContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'here' }));
  expect(openPath).toHaveBeenCalledWith('/Users/me/final/zero to demo.mp3');
  expect(view.container.querySelector('a')).toBeNull();
});

// #449: an absolute path in a model link went to the gateway origin as a web link and gave 404.
it('opens the file preview from a model link that names an absolute path', () => {
  const openPath = vi.fn();
  const view = render(
    <OpenPathContext.Provider value={openPath}>
      <Markdown>{'Play [ton](/var/folders/x/my%20tone.mp3) now.'}</Markdown>
    </OpenPathContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'ton' }));
  expect(openPath).toHaveBeenCalledWith('/var/folders/x/my tone.mp3');
  expect(view.container.querySelector('a')).toBeNull();
  cleanup();
  // Without an opener the path stays text, never a link to the gateway origin.
  const plain = render(<Markdown>{'Play [ton](/var/folders/x/tone.mp3) or [root](/) now.'}</Markdown>);
  expect(plain.container.querySelector('a, [role="button"]')).toBeNull();
  expect(plain.container.textContent).toContain('Play ton or root now.');
});
