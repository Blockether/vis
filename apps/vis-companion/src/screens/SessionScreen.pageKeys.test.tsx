// @vitest-environment jsdom
import { fireEvent, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { renderSessionScreen } from './session-screen-harness';

// Regression, user report (paraphrased: Page Up and Page Down must work in the web
// app as they do natively): the composer holds the focus on desktop and web, and a
// text field keeps these keys for its caret, so the transcript did not move.
describe('Page Up and Page Down in the composer', () => {
  const scrollBy = Element.prototype.scrollBy;
  afterEach(() => {
    vi.restoreAllMocks();
    Element.prototype.scrollBy = scrollBy;
  });

  it('pages the transcript and keeps the focus in the composer', async () => {
    vi.spyOn(HTMLElement.prototype, 'clientHeight', 'get').mockReturnValue(500);
    const moves: Array<{ scroller: Element; top: number }> = [];
    Element.prototype.scrollBy = function scrollBy(this: Element, options?: ScrollToOptions | number) {
      moves.push({ scroller: this, top: typeof options === 'object' ? (options.top ?? 0) : 0 });
    } as Element['scrollBy'];
    renderSessionScreen();
    const composer = await screen.findByLabelText('Message Vis');
    composer.focus();

    expect(fireEvent.keyDown(composer, { key: 'PageUp' })).toBe(false);
    expect(fireEvent.keyDown(composer, { key: 'PageDown' })).toBe(false);

    expect(moves.map((move) => move.top)).toEqual([-450, 450]);
    expect(moves[0].scroller.contains(composer)).toBe(false);
    expect(document.activeElement).toBe(composer);
    // With Shift the field keeps the keys to select text.
    expect(fireEvent.keyDown(composer, { key: 'PageUp', shiftKey: true })).toBe(true);
  });
});
