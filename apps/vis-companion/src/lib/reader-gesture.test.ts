// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { readerMayBeScrolling, readerOwnsScroll, releaseReaderScroll } from './reader-gesture';

let now = 1_000_000;

/** Dispatch `type` with the given fields, as the window-capture listeners receive it. */
function send(type: string, fields: Record<string, unknown> = {}, target: EventTarget = window) {
  target.dispatchEvent(Object.assign(new Event(type, { bubbles: true }), fields));
}

function mount<K extends keyof HTMLElementTagNameMap>(tag: K): HTMLElementTagNameMap[K] {
  return document.body.appendChild(document.createElement(tag));
}

describe('reader gestures', () => {
  beforeEach(() => {
    now = 1_000_000;
    vi.spyOn(Date, 'now').mockImplementation(() => now);
    releaseReaderScroll();
  });

  afterEach(() => {
    vi.restoreAllMocks();
    document.body.replaceChildren();
  });

  it('does not count a tap as a scroll', () => {
    send('touchstart', { touches: [{}] });
    send('touchend', { touches: [] });

    expect(readerOwnsScroll()).toBe(false);
    expect(readerMayBeScrolling()).toBe(false);
  });

  it('gives a finger that moved the scroller the scroll until it lifts, then the grace', () => {
    send('touchstart', { touches: [{}] });
    send('scroll');
    now += 5_000;
    expect(readerOwnsScroll()).toBe(true);

    send('touchend', { touches: [] });
    now += 300;
    expect(readerOwnsScroll()).toBe(true);
    now += 1;
    expect(readerOwnsScroll()).toBe(false);
    expect(readerMayBeScrolling()).toBe(true);
    now += 700;
    expect(readerMayBeScrolling()).toBe(false);
  });

  it('keeps a drag the native scroller took over within reach of the reader', () => {
    send('touchstart', { touches: [{}] });
    now += 5_000;
    expect(readerMayBeScrolling()).toBe(false);

    send('touchcancel', { touches: [] });
    now += 1_000;
    expect(readerOwnsScroll()).toBe(false);
    expect(readerMayBeScrolling()).toBe(true);
    now += 1;
    expect(readerMayBeScrolling()).toBe(false);
  });

  // Stop hides itself on `pointerup`, so the `touchend` of the same tap reaches only
  // the button that left the document, never `window`.
  it('hears the lift of a finger whose element left the document', () => {
    const stop = mount('button');
    send('touchstart', { touches: [{}] }, stop);
    stop.remove();
    send('touchend', { touches: [] }, stop);
    send('scroll');

    expect(readerOwnsScroll()).toBe(false);
    expect(readerMayBeScrolling()).toBe(false);
  });

  it('keeps the other finger down when one lifts from an element that left', () => {
    const stop = mount('button');
    send('touchstart', { touches: [{}] }, stop);
    send('touchstart', { touches: [{}, {}] });
    stop.remove();
    send('touchend', { touches: [{}] }, stop);
    send('scroll');

    expect(readerOwnsScroll()).toBe(true);
  });

  // A finger resting on Stop, in the composer, cannot drag the transcript: the clamp a
  // streaming re-layout left under that press was read as a drag and dropped the follow.
  it('credits a press only with the scrollers it landed in', () => {
    const transcript = mount('div');
    const stop = mount('button');
    send('touchstart', { touches: [{}] }, stop);
    send('scroll', {}, transcript);

    expect(readerOwnsScroll()).toBe(false);
    expect(readerMayBeScrolling()).toBe(false);

    send('scroll', {}, document);
    expect(readerOwnsScroll()).toBe(true);
  });

  it('keeps crediting the scroller a finger landed in after its element left', () => {
    const transcript = mount('div');
    const line = transcript.appendChild(document.createElement('p'));
    send('touchstart', { touches: [{}] }, line);
    line.remove();
    send('scroll', {}, transcript);

    expect(readerOwnsScroll()).toBe(true);
  });

  it('credits a held mouse button only with the scrollers it pressed in', () => {
    const transcript = mount('div');
    send('pointerdown', { pointerType: 'mouse', buttons: 1 }, mount('button'));
    send('scroll', {}, transcript);

    expect(readerMayBeScrolling()).toBe(false);
  });

  it('treats a held mouse button that moves the scroller as a drag', () => {
    send('pointerdown', { pointerType: 'mouse', buttons: 1 });
    send('scroll');
    now += 5_000;
    expect(readerOwnsScroll()).toBe(true);

    send('pointerup', { pointerType: 'mouse', buttons: 0 });
    expect(readerOwnsScroll()).toBe(true);
    now += 301;
    expect(readerOwnsScroll()).toBe(false);
  });

  it('forgets a held button once the pointer moves without it', () => {
    send('pointerdown', { pointerType: 'mouse', buttons: 1 });
    send('pointermove', { pointerType: 'mouse', buttons: 0 });
    send('scroll');

    expect(readerMayBeScrolling()).toBe(false);
  });

  it('leaves touch pointers to the touch count', () => {
    send('pointerdown', { pointerType: 'touch', buttons: 1 });
    send('scroll');

    expect(readerMayBeScrolling()).toBe(false);
  });

  it('counts the keys that scroll, but not in a text field or on a button', () => {
    const field = mount('textarea');
    const button = mount('button');
    send('keydown', { key: 'PageUp' }, field);
    send('keydown', { key: ' ' }, button);
    send('keydown', { key: 'a' }, document.body);
    expect(readerMayBeScrolling()).toBe(false);

    send('keydown', { key: 'PageUp' }, document.body);
    expect(readerOwnsScroll()).toBe(true);
  });

  it('counts Tab, which scrolls the control it focuses into view', () => {
    send('keydown', { key: 'Tab' }, mount('textarea'));

    expect(readerOwnsScroll()).toBe(true);
  });

  it('starts a new scroll surface without the previous gesture', () => {
    send('pointerdown', { pointerType: 'mouse', buttons: 1 });
    send('scroll');
    releaseReaderScroll();

    expect(readerOwnsScroll()).toBe(false);
    expect(readerMayBeScrolling()).toBe(false);
  });
});
