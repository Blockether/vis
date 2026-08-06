import { describe, expect, it } from 'vitest';
import source from './SessionScreen.tsx?raw';

// Regression, issue 6830218b-c00d-497a-86b9-1a8966cd92ca: opening an iOS session
// repeatedly corrected the same transcript scroll while cached history was laying out,
// making the reader visibly jump three or more times before landing at the bottom.
describe('session opening scroll ownership', () => {
  it('waits for the opening window to mount before its one initial correction', () => {
    expect(source).toContain(
      'hydratedTurnCount >= Math.min(visibleTurnCount, turns.length)',
    );
    expect(source).toContain('initialScrollPendingRef.current = false;');
  });

  it('does not schedule a string of delayed opening scroll corrections', () => {
    expect(source).not.toContain('for (const delay of [60, 160, 320, 600, 1000])');
  });
});
