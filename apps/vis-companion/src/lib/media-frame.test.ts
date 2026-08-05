import { describe, expect, it } from 'vitest';

import {
  mediaContentClass,
  mediaFrameClass,
  mediaSlotFrame,
  type MediaSlotState,
} from './media-frame';

const STATES: MediaSlotState[] = ['pending', 'ready', 'failed'];

// Regression, issue: scrolling an iOS transcript full of screenshots jumped.
// A produced-image tile reserved a 96 px pulse while its object URL loaded and
// then became whatever the picture measured (up to 60svh), and a user bubble's
// pasted image reserved nothing at all until it decoded. Each of those swaps
// above the fold shoved the reader's line down — and the scroll corrector
// stands down while a finger is on the glass, so nobody put it back.
describe('transcript media frame', () => {
  it('reserves the same box in every state of a slot', () => {
    const frames = new Set(STATES.map(mediaSlotFrame));

    expect(frames).toEqual(new Set([mediaFrameClass]));
  });

  it('sizes the box from the column and the viewport, never from the picture', () => {
    expect(mediaFrameClass).toContain('w-full');
    expect(mediaFrameClass).toMatch(/aspect-/u);
    // `w-auto`/`h-auto` are exactly the "ask the image how big it is" sizings
    // that made the box move once the bytes landed.
    expect(mediaFrameClass).not.toMatch(/\b[wh]-auto\b/u);
  });

  it('contains the media inside the reserved box instead of letting it push', () => {
    expect(mediaContentClass).toContain('h-full');
    expect(mediaContentClass).toContain('w-full');
    expect(mediaContentClass).toContain('object-contain');
  });
});
