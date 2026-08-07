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
  // Follow-up report ("these remove sessions do we really need to have it in the ⋯?"):
  // the project header's `⋯` opened a popover holding exactly ONE destructive row — a
  // menu of one, repeated on every project header, standing permanently beside the
  // verb that creates. There is ONE `⋯` in this list now, on the machine, and removal
  // moved into the portal that manages projects, per project, where it belongs.
  it('opens the one overflow menu with the same button and the same Menu', () => {
    expect(source.match(/<KebabButton/g)?.length).toBe(1);
    expect(source).not.toContain('label={`Actions for ${project}`}');
    expect(source).not.toContain("'purge'");
    // Removal is the portal's, and it is aimed by the project's canonical ROOT rather
    // than by its display name — two projects on one machine can share a name.
    expect(source).toContain('onRemove={(entry) => {');
    expect(source).toContain('projectPath(session) === entry.root');
    // The machine's verbs read at a glance now, not as three lines of prose.
    expect(source).toContain('<ProjectsIcon className="size-4" />');
    expect(source).toContain('<SettingsIcon className="size-4" />');
    // A `⋯` is never hand-assembled from the parts: no bare glyph, and no
    // `IconButton` standing in for the one component that means "the rarer half of
    // what this row can do". (The filter's own clear IS a plain icon button — it
    // opens no menu, so it must not wear the control that promises one.)
    expect(source).not.toContain('<DotsIcon');
    expect(source.match(/<IconButton/g)?.length).toBe(1);
    expect(source).toContain('label="Clear filter"');
    expect(source.match(/<Menu[\s>]/g)?.length).toBe(1);
    expect(source.match(/<MenuHeading>/g)?.length).toBe(2);
    expect(source).not.toContain('<StartOption');
    expect(source).not.toContain('createPortal(');
  });

  // Regression, user report ("still the ⋯ between the machine and project are different
  // fix it! MARGIN RIGHT DIFFERS AND ALSO WHY THERE IS NO MARGIN BEFORE NEW SESSION"):
  // the machine banner padded its own right edge and the project header ended flush
  // against the screen, so the two identical buttons still sat at two different
  // distances from the same edge — and the yellow verb touched the words beside it.
  // Follow-up report ("some things are having margin left like the ⋯ then chevrons to
  // open the session details are not having"): the same failure one row further down.
  // The headers finally agreed with each other and the SESSION ROWS then disagreed
  // with both — their disclosure ran flush to the screen edge, 12px past the `⋯`
  // directly above it, in what the eye reads as a single column of controls.
  it('gives every row the same trailing cluster, so the right edge is one decision', () => {
    // Machine header, project header, session row, the filter band, and the two
    // skeletons that stand in for a project header and a session row while the list
    // loads — one cluster, so the loading screen cannot be a different shape from the
    // screen it becomes, and the filter's own controls land in the same column as
    // every `⋯` above and below them.
    expect(source.match(/<HeaderActions>/g)?.length).toBe(6);
    // The disclosure is that cluster's own control, never a hand-built strip: a `w-8`
    // welded to the edge at 40% opacity is how it drifted out of the column.
    expect(source).toContain('<RowDisclosure');
    expect(source).not.toContain('sm:w-9 sm:pt-2');
    expect(source).not.toContain('opacity-40 hover:opacity-100');
    // Both headers now REPORT in the same voice too — and so does the filter, whose
    // match count is the same kind of fact in the same cluster: the project's counts
    // moved out of the toggle's fixed column into the one the machine header used.
    expect(source.match(/<HeaderMeta>/g)?.length).toBe(3);
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
    // The filter band IS the field: the input paper marks it at rest, its own rule
    // inks amber on focus, and nothing nests a second box inside it. It wore a
    // borrowed disclosure caret first, then a generic bordered `Input`.
    expect(source).toContain('bg-input transition-colors duration-150 focus-within:border-accent');
    expect(source).toContain('aria-label="Filter sessions"');
    expect(source).not.toContain('<ChevronIcon className="size-3.5 text-accent-ink" />');
    // The printed `/` hint is only honest because the key is actually bound.
    expect(source).toContain("if (event.key !== '/'");
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

  // Regression, user report ("the fucking individual session is bigger then project"):
  // a session row stood 48px against a 36px project band — the child taller than the
  // thing that contains it. The leaf is the SHORTEST of the three levels now, and on a
  // desktop the row is one line, so 32px holds it exactly. Touch keeps 48px, which is
  // still a real thumb target and still under the project band's 52.
  it('keeps the session row shorter than the bands that contain it', () => {
    expect(source).toContain('min-h-12 min-w-0 flex-1 items-center py-1.5');
    expect(source).toContain('mouse:min-h-8 mouse:py-1');
    expect(source).not.toContain('min-h-14 min-w-0 flex-1');
    // The skeleton stands in for that row, so it is the same height or the screen
    // jumps the moment data lands.
    expect(source).toContain('flex min-h-12 w-full items-center py-1.5');
  });

  // Regression, issue: the machine panel disappeared when only one machine was paired.
  it('keeps the machine panel unconditional for every machine section', () => {
    // The rail is back, but as the card's LEFT FRAME rather than a line inside it:
    // the card gives that side up (`LIST_FRAME` on every other child), both sides are
    // 2px, and the machine simply colours the one on the left. The banner keeps the
    // plain hairline — one machine must not wear its hue twice in the same corner.
    expect(source).toContain('<MachineRail color={machineColor(machineColors, key)}>');
    expect(source).toContain('<MachineBanner>');
    expect(source).toContain('border-b border-r-2 border-dialog-edge bg-panel sm:border-y sm:border-r-2');
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
