import { describe, expect, it } from 'vitest';
import source from './SessionsScreen.tsx?raw';

// Regression, user report ("the colour is the same as rename, and after I star it I
// don't see the star until I click on the session"): the swipe strip painted Star in
// the same neutral ink as Rename, and starring PINS the row to the top of its project
// — measured on a 390px viewport, the tapped row travelled from y=619 to y=325 — so
// the row left the spot it was tapped in and an unstarred neighbour slid under the
// thumb. Nothing changed where the user was looking until the list was rebuilt.
describe('starring a session', () => {
  const row = source.slice(source.indexOf('const SessionRow = memo('));

  it('paints the star action in the brand accent, not the neutral verb ink', () => {
    const favorite = row.slice(row.indexOf("key: 'favorite'"), row.indexOf("key: 'rename'"));
    expect(favorite).toContain("tone: 'accent'");
    // Rename stays neutral: the strip has exactly one coloured verb beside Delete.
    const rename = row.slice(row.indexOf("key: 'rename'"), row.indexOf("key: 'delete'"));
    expect(rename).not.toContain('tone:');
  });

  it('follows the row to wherever its own pin just sent it', () => {
    expect(row).toContain('const toggleStar = useCallback(');
    expect(row).toContain('pinned.current = true;');
    expect(row).toContain("onSelect: toggleStar,");
    expect(row).toContain('rowRef.current?.scrollIntoView(');
    expect(row).toContain('}, [isStarred]);');
    // The scroll is anchored on the row's own wrapper, so it is the row that comes
    // back into view and not the header above it.
    expect(row).toContain('<div ref={rowRef}');
  });
});
