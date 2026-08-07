import { describe, expect, it } from "vitest";
import source from "./SessionsScreen.tsx?raw";
import appSource from "../App.tsx?raw";

// Regression (reported: new sessions belong to a project, not a machine): the create
// button used to live on the fleet and machine headers, where it had no project owner.
// The project header now owns the action and passes its workspace root.
describe('where "New session" lives', () => {
  it("is rendered once at project level, not on the fleet or machine headers", () => {
    expect(source.match(/<NewSessionButton/g)?.length).toBe(1);
    expect(source).toContain("onNewSession={(root)");
    expect(source).toContain("onPress={() => onNewSession(root)}");
  });

  // Regression, user report: "manage projects is not under this ⋯ but separately".
  // The machine header carried a bespoke bordered word-button beside its ⋯, so the
  // machine row had two controls where the project row one line below had one.
  it("keeps project management behind the machine ⋯, not beside it", () => {
    expect(source).not.toContain("Switch project");
    expect(source).toContain('title="Manage projects"');
    expect(source).not.toContain(
      "aria-label={`Manage projects on ${machineLabel(machine.conn)}`}",
    );
    expect(source).not.toContain(
      "Create, move, or remove projects and their sessions.",
    );
  });

  // Regression, user report: the machine header should not carry a `⋯` at all —
  // "add + and the gear and this will be add project and the machine settings".
  // Its menu held exactly two rows, so it charged a tap and a guess for what two
  // glyphs say on the band itself.
  it("puts the machine's two verbs on the header, not behind a ⋯", () => {
    expect(source).not.toContain("<KebabButton");
    expect(source).not.toContain(
      "label={`Actions for ${machineLabel(machine.conn)}`}",
    );
    expect(source).toContain(
      "aria-label={`Add a project on ${machineLabel(scopeChrome.conn)}`}",
    );
    expect(source).toContain(">Add project</Button>");
    expect(source).toContain(
      "aria-label={`Settings for ${machineLabel(scopeChrome.conn)}`}",
    );
    expect(source).toContain(">Machine settings</Button>");
    expect(source).toContain("onMachineSettings(scopeChrome.conn)");
    // The `+` opens the SAME sheet the menu row opened, aimed at this machine.
    expect(source).toContain("setManageProjects({ machine: scopeChrome, at })");
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
  it("opens the one overflow menu with the same button and the same Menu", () => {
    expect(source).not.toContain("label={`Actions for ${project}`}");
    expect(source).not.toContain("'purge'");
    // Removal is the portal's, and it is aimed by the project's canonical ROOT rather
    // than by its display name — two projects on one machine can share a name.
    expect(source).toContain("onRemove={(entry) => {");
    expect(source).toContain("projectPath(session) === entry.root");
    // The machine's verbs read at a glance now, not as three lines of prose.
    expect(source).toContain('<ProjectsIcon className="size-4" />');
    expect(source).toContain('<SettingsIcon className="size-4" />');
    // A `⋯` is never hand-assembled from the parts: no bare glyph, and no
    // `IconButton` standing in for the one component that means "the rarer half of
    // what this row can do". (The filter's own clear IS a plain icon button — it
    // opens no menu, so it must not wear the control that promises one.)
    expect(source).not.toContain("<DotsIcon");
    // Only the filter's own Clear is glyph-only now; every verb is a word.
    expect(source.match(/<IconButton/g)?.length).toBe(1);
    expect(source).toContain('label="Clear filter"');
    expect(source.match(/<Menu[\s>]/g)?.length).toBe(1);
    expect(source.match(/<MenuHeading>/g)?.length).toBe(2);
    expect(source).not.toContain("<StartOption");
    expect(source).not.toContain("createPortal(");
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
  it("gives every row the same trailing cluster, so the right edge is one decision", () => {
    // Project header, session row, the filter band, and the two
    // skeletons that stand in for a project header and a session row while the list
    // loads — one cluster, so the loading screen cannot be a different shape from the
    // screen it becomes, and the filter's own controls land in the same column as
    // every `⋯` above and below them.
    expect(source.match(/<HeaderActions>/g)?.length).toBe(5);
    // The disclosure is that cluster's own control, never a hand-built strip: a `w-8`
    // welded to the edge at 40% opacity is how it drifted out of the column.
    expect(source).toContain("<RowDisclosure");
    expect(source).not.toContain("sm:w-9 sm:pt-2");
    expect(source).not.toContain("opacity-40 hover:opacity-100");
    // Both headers now REPORT in the same voice too — and so does the filter, whose
    // match count is the same kind of fact in the same cluster: the project's counts
    // moved out of the toggle's fixed column into the one the machine header used.
    expect(source.match(/<HeaderMeta>/g)?.length).toBe(2);
    expect(source).not.toContain(
      "flex shrink-0 items-center justify-end gap-2 font-mono",
    );
  });
  // Regression, user report: the project header reused a home-shortened display path as
  // both its name and the root sent back to the gateway. On a gateway that resolved `~`
  // relatively, creating in `~/vis` produced the impossible `~/vis/~/vis` project.
  it("keeps the project name, display path, and canonical create root separate", () => {
    expect(source).toContain("project={projectLabel(projectSessions[0]!)}");
    expect(source).toContain(
      "return sessions.map(projectPath).find(Boolean) ?? '';",
    );
    expect(source).toContain("{homeifyPath(root) || 'No workspace path'}");
    expect(source).toContain(
      "onNewSession={(root) => void createSession({ kind: 'trunk' }, machine.conn, root)}",
    );
    expect(source).not.toContain(
      "return homeifyPath(sessions.map(projectPath).find(Boolean));",
    );
  });

  it("keeps machine actions inside the full-width header", () => {
    expect(source).not.toContain("translate-x-2");
    expect(source).not.toContain("sm:translate-x-0");
  });

  // Regression, user report: every project seam was painted by the section's negative
  // margin, the header's two borders, and the toggle's two more borders. Adjacent rows
  // therefore overlapped by a pixel and the same line had as many as three DOM owners.
  it("assigns every list boundary to one outgoing edge without negative overlap", () => {
    // The filter band IS the field: the input paper marks it at rest, its own rule
    // inks amber on focus, and nothing nests a second box inside it. It wore a
    // borrowed disclosure caret first, then a generic bordered `Input`.
    expect(source).toContain(
      "bg-input transition-colors duration-150 focus-within:border-accent",
    );
    expect(source).toContain('aria-label="Filter sessions"');
    expect(source).not.toContain(
      '<ChevronIcon className="size-3.5 text-accent-ink" />',
    );
    // The printed `/` hint is only honest because the key is actually bound.
    expect(source).toContain("if (event.key !== '/'");
    expect(source).toContain("<section aria-label={`${project} sessions`}>");
    // The header's own band — its rule, its paper, its height — belongs to
    // `SectionHeader`, and is pinned once in `ui.test.tsx`.
    expect(source).toContain("<SectionHeader>");
    expect(source).not.toContain('tone="machine"');
    expect(source).toContain(
      '      {rows.length > 0 && (\n        <div className="border-b border-dialog-edge">',
    );
    expect(source).not.toContain("-mt-px");
    expect(source).not.toContain("-mb-px");
    expect(source).not.toContain("-my-px");
    expect(source).not.toContain("items-stretch border-y border-dialog-edge");
  });

  // Regression, user report: making New session 28px still left it visibly taller than
  // the neighboring 24px small action even after the project row itself was compacted.
  it("leaves every header metric to the component that owns it", () => {
    // The ⋯ no longer spells its own metrics: `IconButton` is `Button` at the
    // header's own compact desktop density, so it cannot drift from the yellow
    // button it stands next to — and the row it sits in is `SectionHeader`.
    expect(source).not.toContain(
      "motion-reduce:transition-none mouse:min-h-0 sm:px-4",
    );
    expect(source).not.toContain("mouse:min-h-0 mouse:py-0");
    expect(source).not.toContain("bg-panel-2 mouse:h-9");
  });

  // Regression, user report ("the individual session is bigger then project"):
  // a session row stood 48px against a 36px project band — the child taller than the
  // thing that contains it. The leaf is the SHORTEST of the three levels now, and on a
  // desktop the row is one line, so 32px holds it exactly. Touch keeps 48px, which is
  // still a real thumb target and still under the project band's 52.
  it("keeps the session row shorter than the bands that contain it", () => {
    expect(source).toContain("min-h-12 min-w-0 flex-1 items-center py-1.5");
    expect(source).toContain("mouse:min-h-8 mouse:py-1");
    expect(source).not.toContain("min-h-14 min-w-0 flex-1");
    // The skeleton stands in for that row, so it is the same height or the screen
    // jumps the moment data lands.
    expect(source).toContain("flex min-h-12 w-full items-center py-1.5");
  });

  // Regression, issue: the machine panel disappeared when only one machine was paired.
  it("keeps the machine panel unconditional for every machine section", () => {
    // The rail is back, but as the card's LEFT FRAME rather than a line inside it:
    // the card gives that side up (`LIST_FRAME` on every other child), both sides are
    // 2px, and the machine simply colours the one on the left. The banner keeps the
    // plain hairline — one machine must not wear its hue twice in the same corner.
    expect(source).toContain(
      "<MachineRail color={machineColor(machineColors, key)}>",
    );
    expect(source).toContain(
      "border-b border-r-2 border-dialog-edge bg-panel sm:border-y sm:border-r-2",
    );
    expect(source).toContain("{index > 0 && <MachineGap />}");
    expect(source).not.toContain("showMachineHeaders");
  });

  // Regression, user report ("THEY LOOK BAD ON THE IPHONE. See the machine
  // height project heights etc margin rights etc"): measured at 390px, the machine
  // banner stood 61px tall — its own `py-2` wrapped around a 44px control — while the
  // project header one row below it, with the very same controls, stood 49px. The
  // project header also hid its own name behind a fixed 160px count column, so `~/vis`
  // rendered as `~/v…` on a phone. Every header in the list is ONE component now.
  it("builds both list headers from one band, so neither spells its own box", () => {
    expect(source).toContain("<SectionHeader>");
    expect(source).not.toContain('tone="machine"');
    expect(source).toContain("<HeaderTitle");
    expect(source.match(/<HeaderTally/g)?.length).toBe(1);
    // Not one height, padding or paper spelled at a call site.
    expect(source).not.toContain("<header className=");
    expect(source).not.toContain("min-h-11 min-w-0 flex-1");
    expect(source).not.toContain("w-40 shrink-0");
    expect(source).not.toContain("mouse:h-9");
  });

  // Regression: the session count used to flash from empty to the cached total on
  // every cold start while the async native connection store was loading.
  it("seeds the application connection list synchronously on startup", () => {
    expect(appSource).toContain("loadConnectionsSync");
    expect(appSource).toContain("useState<GatewayConn[]>(loadConnectionsSync)");
  });
  // Regression, user report: the NEW badge belongs in a column of its own. The
  // unread/dirty/draft/star flags used to sit INSIDE the title cell, so each row
  // started its flags wherever its title happened to end and a long title pushed
  // them off the line.
  it("gives the row flags their own grid column, next to the title and not inside it", () => {
    const grid =
      "grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 sm:grid-cols-[minmax(0,1fr)_5.5rem_5.5rem_4.5rem_5rem_6rem]";
    // The row and the skeleton that stands in for it share ONE track list, or the
    // columns jump the moment the rows land.
    expect(
      source.match(new RegExp(grid.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"), "g"))
        ?.length,
    ).toBe(2);
    const flags = source.slice(
      source.indexOf(
        "col-start-2 row-start-1 flex min-w-0 items-center justify-end gap-1.5",
      ),
    );
    // Every flag lives in that cell...
    for (const flag of [
      "? `${unread} new` : ",
      "dirty",
      "draft {draftName}",
      "<StarIcon filled",
    ]) {
      expect(
        flags.slice(0, flags.indexOf("</span>\n          {/* `sm:contents`")),
      ).toContain(flag);
    }
    // ...and the title cell holds the title alone.
    const name = source.slice(
      source.indexOf(
        '<span className="col-start-1 row-start-1 flex min-w-0 items-center sm:',
      ),
    );
    expect(
      name.slice(0, name.indexOf("{/* What the session HAS")),
    ).not.toContain("unread > 0");
    // Status and time moved one column out to make room.
    expect(source).toContain("col-start-3 row-start-1 inline-flex shrink-0");
    expect(source).toContain(
      "col-start-2 col-end-4 row-start-2 justify-self-end whitespace-nowrap",
    );
  });
  // Regression, user report: on a phone the NEW badge should sit NEXT TO `IDLE`.
  // The flags had a fixed 3.5rem track and the status a fixed 6.75rem one that
  // right-aligned four characters inside 108px, so the badge ended up 86px from the
  // word it qualifies. The flag track is `auto` (the elastic title still pins its
  // right edge on every row) and the status track beside it is fixed at the width of
  // `WAITING` with the label aligned to its start, so every row's status begins on
  // the same x one gutter after the badge.
  it("sits the phone flags directly beside the status they qualify", () => {
    expect(source).toContain("grid-cols-[minmax(0,1fr)_auto_auto]");
    expect(source).not.toContain("grid-cols-[minmax(0,1fr)_3.5rem_6.75rem]");
    expect(source).toContain("return 'WAITING'");
  });

  // Regression, user report: on a phone `NEW IDLE` should END where `7 hours ago`
  // ends. The status was aligned to the START of its track while the timestamp
  // sharing that track on the line below was aligned to its END, so the two lines
  // of the same row stopped on two different right margins. Both end on the track's
  // end now; only the wide `sm:` layout, which puts them on their own columns of a
  // single line, aligns them to the start.
  it("ends the phone status on the same edge as the timestamp under it", () => {
    expect(source).toContain(
      "col-start-3 row-start-1 inline-flex shrink-0 items-center gap-1 justify-self-end",
    );
    expect(source).toContain(
      "sm:col-start-auto sm:row-start-auto sm:justify-self-start ${statusTone(session)}",
    );
    expect(source).toContain(
      "col-start-2 col-end-4 row-start-2 justify-self-end whitespace-nowrap",
    );
  });
});

// Regression, user report: the `+` on a machine header opened the project
// INVENTORY, so adding a project meant finding "Add project…" inside it; and the
// machine's name was only editable from the settings screen.
describe("the machine header's own verbs", () => {
  it("opens the folder browser directly from `+`", () => {
    expect(source).toContain("isAdding");
  });

  it("makes the machine name itself the rename control", () => {
    expect(source).toContain("onRenameMachine(scopeChrome.conn, next)");
    expect(source).toContain("label={`Rename ${machineLabel(scopeChrome.conn)}`}");
  });

  // Regression, user report: the `+` and the gear on a machine header were bordered
  // boxes around a glyph. They are WORDS in buttons now (the later report "make them
  // nice buttons like New Session" reversed the frameless `quiet` face this test used
  // to pin), so what is left of the original complaint is that neither draws a glyph.
  it("paints the header's two verbs as words, not as glyphs", () => {
    expect(source).toMatch(
      /<Button\s+variant="solid"\s+density="compact"[\s\S]{0,120}aria-label=\{`Add a project on/,
    );
    expect(source).toMatch(
      /<Button\s+variant="ghost"\s+density="compact"[\s\S]{0,120}aria-label=\{`Settings for/,
    );
  });
});

// Regression, user report ("there is no much difference visually between the machine
// and the project"): the list carried TWO bands one hairline apart, both starting at
// the same x and both ending in the same trailing cluster, so nothing said the second
// was inside the first. The machine is SELECTION now — a chip in the strip and the
// title of the chrome — and the project header is the only header kind in the list.
describe("the machine is a chip, not a second band", () => {
  it("leaves exactly one header shape inside the list", () => {
    expect(source).not.toContain("MachineBanner");
    expect(source).not.toContain('tone="machine"');
    // The hue still says which computer owns a block; it is a rail, not a band.
    expect(source).toContain(
      "<MachineRail color={machineColor(machineColors, key)}>",
    );
  });

  it("gives the machine's verbs to the chrome that names it", () => {
    // One Add project and one Machine settings on the screen, both acting on the
    // machine in scope, and both spelled out.
    expect(source).toContain(
      "aria-label={`Add a project on ${machineLabel(scopeChrome.conn)}`}",
    );
    expect(source).toContain(
      "aria-label={`Settings for ${machineLabel(scopeChrome.conn)}`}",
    );
    expect(source).toContain("onRenameMachine(scopeChrome.conn, next)");
  });

  // Regression, user report: "Everything labeled, no icons and the MACHINES are in
  // the fucking HEADER" — paraphrased: the machine verbs were glyph-only, and the
  // machines themselves stood in a strip of their own BELOW the header band, so the
  // one question the screen answers first was not part of the header that answers it.
  it("carries the machine chips inside the header band itself", () => {
    // The chips come before the header's own report line, in the same band.
    expect(source.indexOf("aria-pressed={scope === null}")).toBeLessThan(
      source.indexOf("Reading sessions..."),
    );
    // ...and the separate strip row is gone.
    expect(source).not.toContain("overflow-x-auto bg-panel px-3 py-2");
  });

  it("spells the machine's verbs instead of drawing glyphs", () => {
    expect(source).not.toContain('<PlusIcon className="size-4" />');
    expect(source).toContain(">Add project</Button>");
    expect(source).toContain(">Machine settings</Button>");
  });

  // Regression, user report: "make them nice buttons like the fucking New Session" —
  // both verbs were `variant="quiet"`, which is deliberately frameless, so the two
  // words sat on the chrome as bare ink beside an amber `New session` slab.
  it("gives the machine's verbs a real button face", () => {
    const start = source.indexOf("The machine's two verbs");
    const verbs = source.slice(start, source.indexOf("Machine settings</Button>", start));
    expect(verbs).not.toContain('variant="quiet"');
    // ADD is the amber primary, its settings sibling the framed one.
    expect(verbs.slice(0, verbs.indexOf("Add project</Button>"))).toContain(
      'variant="solid"',
    );
    expect(verbs.slice(0, verbs.indexOf("Machine settings</Button>"))).toContain(
      'variant="ghost"',
    );
  });

  it("keeps a dead machine's Retry where its sessions would have been", () => {
    expect(source).toContain("onClick={() => void loadMachine(machine.conn)}");
  });
});

// Regression, user report ("PLEASE MAKE THE UNIFIED VIEW REGARDLESS IF WE HAVE ON
// MACHINE OR MANY MACHINES IN FLEET"): the strip appeared only above two machines, and
// carried a pairing chip that a solo user saw instead of chips — two different screens.
describe('the machine strip', () => {
  it('is one row, paired one machine or many', () => {
    expect(source).not.toContain('hasScopeStrip');
    expect(source).not.toContain('showsScopeStrip');
  });

  it('does not pair — that is app chrome now', () => {
    expect(source).not.toContain('onPairMachine');
  });
});
