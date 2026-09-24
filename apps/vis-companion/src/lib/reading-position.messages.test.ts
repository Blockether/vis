// @vitest-environment jsdom
import { describe, expect, it, vi } from 'vitest';

import { messagesBelow } from './reading-position';

function rect(bottom: number): DOMRect {
  return { bottom } as DOMRect;
}

describe('remaining message bubbles', () => {
  it('counts partial user and assistant bubbles, not non-message content', () => {
    const viewport = document.createElement('div');
    const transcript = document.createElement('div');
    vi.spyOn(viewport, 'getBoundingClientRect').mockReturnValue(rect(300));
    for (const bottom of [100, 300, 301, 420]) {
      const article = document.createElement('article');
      article.dataset.transcriptMessage = '';
      vi.spyOn(article, 'getBoundingClientRect').mockReturnValue(rect(bottom));
      transcript.append(article);
    }
    transcript.append(document.createElement('div'));

    expect(messagesBelow(viewport, transcript)).toBe(2);
    vi.spyOn(viewport, 'getBoundingClientRect').mockReturnValue(rect(380));
    expect(messagesBelow(viewport, transcript)).toBe(1);

    const newMessage = document.createElement('article');
    newMessage.dataset.transcriptMessage = '';
    vi.spyOn(newMessage, 'getBoundingClientRect').mockReturnValue(rect(500));
    transcript.append(newMessage);
    expect(messagesBelow(viewport, transcript)).toBe(2);
    expect(messagesBelow(viewport, null)).toBe(0);
  });
});
