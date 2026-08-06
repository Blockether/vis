import { describe, expect, it } from 'vitest';
import source from './SessionsScreen.tsx?raw';

// Regression (reported: new sessions belong to a project, not a machine): the create
// button used to live on the fleet and machine headers, where it had no project owner.
// The project header now owns the action and passes its workspace root.
describe('where "New session" lives', () => {
  it('is rendered once at project level, not on the fleet or machine headers', () => {
    expect(source.match(/<NewSessionButton/g)?.length).toBe(1);
    expect(source).toContain('onNewSession={(root)');
    expect(source).toContain('onPress={() => onNewSession(root)}');
  });

  it('does not offer machine-level project switching and names project management', () => {
    expect(source).not.toContain('Switch project');
    expect(source).toContain('Manage projects');
  });

  it('creates directly in the selected project root', () => {
    expect(source).toContain("onNewSession={(root) => void createSession({ kind: 'trunk' }, machine.conn, root)}");
    expect(source).toContain('startAt={root || null}');
  });

  it('keeps machine actions inside the full-width header', () => {
    expect(source).not.toContain('translate-x-2');
    expect(source).not.toContain('sm:translate-x-0');
  });
});
