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

  it('counts the steps and answer of a long turn as the reader scrolls up through it', () => {
    const viewport = document.createElement('div');
    const transcript = document.createElement('div');
    const edge = vi.spyOn(viewport, 'getBoundingClientRect').mockReturnValue(rect(300));
    const message = (top: number, bottom: number, partBottoms: number[] = []) => {
      const article = document.createElement('article');
      article.dataset.transcriptMessage = '';
      vi.spyOn(article, 'getBoundingClientRect').mockReturnValue({ top, bottom } as DOMRect);
      const parts = partBottoms.map((partBottom) => {
        const part = document.createElement('section');
        part.dataset.transcriptPart = '';
        article.append(part);
        return vi.spyOn(part, 'getBoundingClientRect').mockReturnValue(rect(partBottom));
      });
      transcript.append(article);
      return parts;
    };
    message(0, 50);
    message(60, 1000, [300, 600, 900, 980]);
    message(1010, 1050);
    const unread = message(1060, 1500, [1200, 1480]);

    // Three parts of the long turn, the next request and the whole next turn.
    expect(messagesBelow(viewport, transcript)).toBe(6);
    edge.mockReturnValue(rect(250));
    expect(messagesBelow(viewport, transcript)).toBe(7);
    edge.mockReturnValue(rect(990));
    expect(messagesBelow(viewport, transcript)).toBe(4);
    for (const part of unread) expect(part).not.toHaveBeenCalled();
  });
});
