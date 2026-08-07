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

  // Regression, user report: "manage projects is not under this ⋯ but separately".
  // The machine header carried a bespoke bordered word-button beside its ⋯, so the
  // machine row had two controls where the project row one line below had one.
  it('keeps project management behind the machine ⋯, not beside it', () => {
    expect(source).not.toContain('Switch project');
    expect(source).toContain('title="Manage projects"');
    expect(source).not.toContain('aria-label={`Manage projects on ${machineLabel(machine.conn)}`}');
    expect(source).not.toContain('Create, move, or remove projects and their sessions.');
  });

  // Regression: machine settings must remain reachable after project management
  // joins it behind the same control.
  it('keeps machine settings in the machine ⋯', () => {
    expect(source).toContain('label={`Actions for ${machineLabel(machine.conn)}`}');
    expect(source).toContain('title="Machine settings"');
    expect(source).toContain('onMachineSettings(target)');
  });

  // Regression, user report: "when I open the ⋯ the view is not coherent between the
  // one ⋯ on the machine and another ⋯ on the project level" — two hand-built panels
  // of different widths, one with an accent band and one with none, opened by two
  // hand-built buttons of different heights wearing two different glyph sizes.
  it('opens both overflow menus with the same button and the same Menu', () => {
    expect(source.match(/<KebabButton/g)?.length).toBe(2);
    expect(source).not.toContain('<IconButton');
    expect(source).not.toContain('<DotsIcon');
    expect(source.match(/<Menu[\s>]/g)?.length).toBe(2);
    expect(source.match(/<MenuHeading>/g)?.length).toBe(3);
    expect(source).toContain('label={`Actions for ${project}`}');
    expect(source).not.toContain('<StartOption');
    expect(source).not.toContain('createPortal(');
  });

  // Regression, user report ("still the ⋯ between the machine and project are different
  // fix it! MARGIN RIGHT DIFFERS AND ALSO WHY THERE IS NO MARGIN BEFORE NEW SESSION"):
  // the machine banner padded its own right edge and the project header ended flush
  // against the screen, so the two identical buttons still sat at two different
  // distances from the same edge — and the yellow verb touched the words beside it.
  it('gives both headers the same trailing cluster, so both edges are one decision', () => {
    // Machine header, project header, and the skeleton that stands in for a project
    // header while the list loads — one cluster, so the loading screen cannot be a
    // different shape from the screen it becomes.
    expect(source.match(/<HeaderActions>/g)?.length).toBe(3);
    // Both headers now REPORT in the same voice too: the project's counts moved out of
    // the toggle's fixed column and into the cluster the machine header already used.
    expect(source.match(/<HeaderMeta>/g)?.length).toBe(2);
    expect(source).not.toContain('flex shrink-0 items-center justify-end gap-2 font-mono');
  });
  // Regression, user report: the project header reused a home-shortened display path as
  // both its name and the root sent back to the gateway. On a gateway that resolved `~`
  // relatively, creating in `~/vis` produced the impossible `~/vis/~/vis` project.
  it('keeps the project name, display path, and canonical create root separate', () => {
    expect(source).toContain("project={projectLabel(projectSessions[0]!)}");
    expect(source).toContain("return sessions.map(projectPath).find(Boolean) ?? '';");
    expect(source).toContain("{homeifyPath(root) || 'No workspace path'}");
    expect(source).toContain("onNewSession={(root) => void createSession({ kind: 'trunk' }, machine.conn, root)}");
    expect(source).not.toContain('return homeifyPath(sessions.map(projectPath).find(Boolean));');
  });

  it('keeps machine actions inside the full-width header', () => {
    expect(source).not.toContain('translate-x-2');
    expect(source).not.toContain('sm:translate-x-0');
  });

  // Regression, user report: every project seam was painted by the section's negative
  // margin, the header's two borders, and the toggle's two more borders. Adjacent rows
  // therefore overlapped by a pixel and the same line had as many as three DOM owners.
  it('assigns every list boundary to one outgoing edge without negative overlap', () => {
    expect(source).toContain('className="flex min-h-10 items-center border-b border-dialog-edge bg-panel px-3 mouse:min-h-9');
    expect(source).toContain('<section aria-label={`${project} sessions`}>');
    // The header's own band — its rule, its paper, its height — belongs to
    // `SectionHeader`, and is pinned once in `ui.test.tsx`.
    expect(source).toContain('<SectionHeader tone="project">');
    expect(source).toContain('      {rows.length > 0 && (\n        <div className="border-b border-dialog-edge">');
    expect(source).not.toContain('-mt-px');
    expect(source).not.toContain('-mb-px');
    expect(source).not.toContain('-my-px');
    expect(source).not.toContain('items-stretch border-y border-dialog-edge');
  });

  // Regression, user report: making New session 28px still left it visibly taller than
  // the neighboring 24px small action even after the project row itself was compacted.
  it('leaves every header metric to the component that owns it', () => {
    // The ⋯ no longer spells its own metrics: `IconButton` is `Button` at the
    // header's own compact desktop density, so it cannot drift from the yellow
    // button it stands next to — and the row it sits in is `SectionHeader`.
    expect(source).not.toContain('motion-reduce:transition-none mouse:min-h-0 sm:px-4');
    expect(source).not.toContain('mouse:min-h-0 mouse:py-0');
    expect(source).not.toContain('bg-panel-2 mouse:h-9');
  });

  // Regression, issue: the machine panel disappeared when only one machine was paired.
  it('keeps the machine panel and rail unconditional for every machine section', () => {
    expect(source).toContain('<MachineBanner>');
    expect(source).toContain('const hasScopeStrip = showsScopeStrip(machines);');
    expect(source).toContain('{index > 0 && <MachineGap />}');
    expect(source).not.toContain('showMachineHeaders');
  });


  // Regression, user report ("THEY LOOK FUCKING SHITTY ON THE IPHONE. See the machine
  // height project heights etc margin rights etc"): measured at 390px, the machine
  // banner stood 61px tall — its own `py-2` wrapped around a 44px control — while the
  // project header one row below it, with the very same controls, stood 49px. The
  // project header also hid its own name behind a fixed 160px count column, so `~/vis`
  // rendered as `~/v…` on a phone. Every header in the list is ONE component now.
  it('builds both list headers from one band, so neither spells its own box', () => {
    expect(source).toContain('<SectionHeader tone="project">');
    expect(source).toContain('<HeaderToggle');
    expect(source.match(/<HeaderTally/g)?.length).toBe(2);
    // Not one height, padding or paper spelled at a call site.
    expect(source).not.toContain('<header className=');
    expect(source).not.toContain('min-h-11 min-w-0 flex-1');
    expect(source).not.toContain('w-40 shrink-0');
    expect(source).not.toContain('mouse:h-9');
  });

  // Regression: the session count used to flash from empty to the cached total on
  // every cold start while the async native connection store was loading.
  it('seeds the application connection list synchronously on startup', () => {
    expect(appSource).toContain('loadConnectionsSync');
    expect(appSource).toContain('useState<GatewayConn[]>(loadConnectionsSync)');
  });
});
