import { describe, expect, it } from 'vitest';
import source from './SessionScreen.tsx?raw';

// Regression, issue 6830218b-c00d-497a-86b9-1a8966cd92ca: opening an iOS session
// repeatedly corrected the same transcript scroll while cached history was laying out,
// making the reader visibly jump three or more times before landing at the bottom.
describe('session opening scroll ownership', () => {
  it('waits for the first paint window before its one initial correction', () => {
    expect(source).toContain(
      'hydratedTurnCount >= Math.min(FIRST_PAINT_TURNS, turns.length)',
    );
    expect(source).toContain('initialScrollPendingRef.current = false;');
  });

  it('does not schedule a string of delayed opening scroll corrections', () => {
    expect(source).not.toContain('for (const delay of [60, 160, 320, 600, 1000])');
  });
});

// Regression, session 617d3b77-8522-4866-b4b4-01cc8253bf1a: an image-heavy traced
// answer stayed hidden behind "Loading session…" while the off-screen history ramped in.
describe('session opening reveal', () => {
  it('reveals after the first painted turn window instead of the full background ramp', () => {
    expect(source).toContain(
      'hydratedTurnCount >= Math.min(FIRST_PAINT_TURNS, turns.length)',
    );
    expect(source).not.toContain(
      'hydratedTurnCount >= Math.min(visibleTurnCount, turns.length)',
    );
  });
});

// Regression, iOS keyboard jump: tapping the composer while pinned to the newest
// turn shrank the shell, and the re-pin was deferred to the next animation frame.
// The reader saw one painted frame with the conversation a keyboard's height above
// the bottom (measured 274 px on an iPhone 17 Pro simulator), then a snap down.
describe('keyboard compensation while following', () => {
  it('re-pins inside the resize callback rather than a frame later', () => {
    expect(source).toContain(
      'else if (followingRef.current && !readerOwnsScroll()) {',
    );
    const pin = source.indexOf(
      'box.scrollTop = Math.max(0, box.scrollHeight - height);',
    );
    const deferred = source.indexOf(
      'resizeScrollFrameRef.current = window.requestAnimationFrame(',
    );
    expect(pin).toBeGreaterThan(-1);
    expect(deferred).toBeGreaterThan(pin);
  });
});
