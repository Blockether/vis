// @vitest-environment jsdom
import { cleanup, render } from '@testing-library/react';
import { afterEach, expect, it } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/message-links.json';
import { UserMessage } from './ChatContent';

afterEach(cleanup);

// #446: the TUI reads the same cases, so both clients link the same URLs.
it.each(fixture.cases)('links the bare URLs of a sent message: $name', ({ text, links }) => {
  const view = render(<UserMessage>{text}</UserMessage>);
  const anchors = [...view.container.querySelectorAll('a')];
  expect([...new Set(anchors.map((a) => a.getAttribute('href')))]).toEqual(links);
  // The link shows the URL as the user typed it, and it opens outside the app.
  for (const a of anchors) {
    expect(a.textContent).toBe(a.getAttribute('href'));
    expect(a.getAttribute('target')).toBe('_blank');
  }
});

it('keeps the punctuation after a link as text of the bubble', () => {
  const view = render(<UserMessage>{'Open https://example.com/docs. Thanks'}</UserMessage>);
  expect(view.container.textContent).toContain('Open https://example.com/docs. Thanks');
});
