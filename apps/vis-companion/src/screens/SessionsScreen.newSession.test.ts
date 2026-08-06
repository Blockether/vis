import { describe, expect, it } from 'vitest';
import source from './SessionsScreen.tsx?raw';
import appSource from '../App.tsx?raw';

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

  // Regression: machine settings must remain on the machine bar after project
  // management moves there; removing the old machine overflow control hid pairing
  // and unpair actions.
  it('keeps machine settings in the machine bar actions', () => {
    expect(source).toContain('aria-label={`Machine actions for ${machineLabel(machine.conn)}`}');
    expect(source).toContain('title="Machine settings"');
    expect(source).toContain('onMachineSettings(target)');
  });
  it('creates directly in the selected project root', () => {
    expect(source).toContain("onNewSession={(root) => void createSession({ kind: 'trunk' }, machine.conn, root)}");
    expect(source).toContain('startAt={root || null}');
  });

  it('keeps machine actions inside the full-width header', () => {
    expect(source).not.toContain('translate-x-2');
    expect(source).not.toContain('sm:translate-x-0');
  });

  // Regression (reported: adjacent bottom/top rules made the filter and session rows render as a doubled divider).
  it('uses one rule at each filter, project, and session boundary', () => {
    expect(source).toContain('className="border-b border-dialog-edge bg-panel-2 px-3 py-2 sm:px-4"');
    expect(source).toContain('className="flex min-h-10 items-center -mb-px border-b border-dialog-edge bg-panel px-3 mouse:min-h-9');
    expect(source).toContain('        ) : (\n          <div>\n            {sections.map');
    expect(source).toContain('      {rows.length > 0 && (\n        <div>');
    expect(source).toContain("className={`${firstProject ? '' : '-mt-px border-t'} border-dialog-edge`}");
    expect(source).toContain('firstProject={groupIndex === 0}');
  });

  // Regression, issue: the machine panel disappeared when only one machine was paired.
  it('keeps the machine panel and rail unconditional for every machine section', () => {
    expect(source).toContain('<MachineBanner>');
    expect(source).toContain('const hasScopeStrip = showsScopeStrip(machines);');
    expect(source).toContain('{index > 0 && <MachineGap />}');
    expect(source).not.toContain('showMachineHeaders');
  });


  // Regression: the session count used to flash from empty to the cached total on
  // every cold start while the async native connection store was loading.
  it('seeds the application connection list synchronously on startup', () => {
    expect(appSource).toContain('loadConnectionsSync');
    expect(appSource).toContain('useState<GatewayConn[]>(loadConnectionsSync)');
  });
});
