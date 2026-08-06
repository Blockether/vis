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

  it('puts project management on the machine bar, not project actions', () => {
    expect(source).not.toContain('Switch project');
    expect(source).toContain('Manage projects');
    expect(source).toContain('aria-label={`Manage projects on ${machineLabel(machine.conn)}`}');
    expect(source).not.toContain('Create, move, or remove projects and their sessions.');
  });

  it('creates directly in the selected project root', () => {
    expect(source).toContain("onNewSession={(root) => void createSession({ kind: 'trunk' }, machine.conn, root)}");
    expect(source).toContain('startAt={root || null}');
  });

  it('keeps machine actions inside the full-width header', () => {
    expect(source).not.toContain('translate-x-2');
    expect(source).not.toContain('sm:translate-x-0');
  });

  it('keeps machine and project boundaries to one rule', () => {
    expect(source).toContain('border-b border-dialog-edge bg-panel px-3 mouse:min-h-9');
    expect(source).toContain('firstProject={groupIndex === 0}');
    expect(source).toContain("className={`${firstProject ? '' : 'border-t'} border-dialog-edge`}");
    expect(source.match(/border-t border-dialog-edge first:border-t-0/g)?.length).toBe(1);
  });
});
