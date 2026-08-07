import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";

import uiSource from "./ui.tsx?raw";
import sessionsListSource from "../screens/SessionsScreen.tsx?raw";
import settingsSource from "../screens/SettingsScreen.tsx?raw";

import { MACHINE_COLORS } from "../lib/machine-colors";
import {
  Button,
  DialogClose,
  DialogFrame,
  HeaderActions,
  HeaderMeta,
  HeaderTally,
  HeaderTitle,
  HeaderToggle,
  IconButton,
  KebabButton,
  LiveCount,
  LiveTally,
  MachineBanner,
  MachineGap,
  MachineMark,
  MachineRail,
  NewSessionButton,
  RowDisclosure,
  SectionHeader,
  UnreadBadge,
} from "./ui";

// Regression (reported: "why we still have this chevron here showing something is
// collapsible if we cannot click it — let's have just one color"): the caret half of
// the New session split control was painted in the dark title-bar ink, so an amber
// primary carried a charcoal slab that read as switched-off chrome — the one half
// that DOES open a menu looked like the one half nobody may press.
describe("split button", () => {
  const split = () =>
    renderToStaticMarkup(
      <span className="flex items-stretch">
        <Button pressEffect="none" className="border-r-0">
          New session
        </Button>
        <Button
          pressEffect="none"
          aria-haspopup="menu"
          className="border-l-accent-foreground/30"
        >
          <span aria-hidden>▾</span>
        </Button>
      </span>,
    );

  it("paints both halves in the one accent, split by a hairline of its own ink", () => {
    const html = split();

    expect(html.match(/(?<!:)bg-accent(?![/-])/g)).toHaveLength(2);
    expect(html).toContain("border-l-accent-foreground/30");
    expect(html).not.toContain("bg-dialog-title");
    expect(html).not.toContain("text-dialog-title-foreground");
  });

  it("leaves the caret pressable, so the chevron is a promise the control keeps", () => {
    const html = split();

    expect(html.match(/<button/g)).toHaveLength(2);
    expect(html).toContain('aria-haspopup="menu"');
    expect(html).not.toContain('disabled=""');
  });
});

// The live count wears the SAME filled block as the unread badge, in green:
// `macbook \u25ae3\u25ae\u25ae4\u25ae` — one shape, two colours, running then
// waiting. It replaced a bracketed `[3]`, which read lighter than the badge
// beside it, and before that a `\u25cf` whose metrics sat below the digits.
describe("LiveTally", () => {
  it("is a filled green block, not bracketed text or a glyph", () => {
    const html = renderToStaticMarkup(<LiveTally count={5} />);

    expect(html).toContain("bg-ok-surface");
    expect(html).toContain("text-ok-foreground");
    expect(html).toContain(">5<");
    expect(html).not.toContain("[");
    expect(html).not.toContain("\u25cf");
  });

  // `--ok` is the app's green INK (LIVE text, the 6px machine dot). Poured into
  // a badge it is a slab twice as dark as the amber block beside it and carries
  // its digit at 5:1; the fill has to be the lightened `ok-surface` peer.
  it("fills with the green surface, never with the green ink", () => {
    const html = renderToStaticMarkup(<LiveTally count={5} />);

    expect(html).not.toMatch(/bg-ok(?!-surface)/);
  });

  it("says what the number counts, for a reader that cannot see green", () => {
    const html = renderToStaticMarkup(<LiveTally count={1} />);

    expect(html).toContain('<span class="sr-only"> live</span>');
  });

  it("renders nothing when nothing is running", () => {
    expect(renderToStaticMarkup(<LiveTally count={0} />)).toBe("");
  });
});

// Unread is a notification, not a second tally: beside the bracketed live
// count it has to be told apart from it WITHOUT the reader remembering a colour
// code, so it wears the same filled amber block the session row uses for "new".
describe("UnreadBadge", () => {
  it("is a filled block, not a bare number beside the live count", () => {
    const html = renderToStaticMarkup(<UnreadBadge count={3} />);

    expect(html).toContain("bg-accent");
    expect(html).toContain("text-accent-foreground");
    expect(html).toContain(">3<");
    expect(html).not.toContain("[");
  });

  it("says what the number counts, for a reader that cannot see amber", () => {
    const html = renderToStaticMarkup(<UnreadBadge count={1} />);

    expect(html).toContain('<span class="sr-only"> unread</span>');
  });

  it("renders nothing when there is nothing new", () => {
    expect(renderToStaticMarkup(<UnreadBadge count={0} />)).toBe("");
  });
});

// A machine gap separates computers with space; the banner supplies the promoted name.
describe("MachineGap", () => {
  it("is a band of page ink, not another pair of overlapping rules", () => {
    const html = renderToStaticMarkup(<MachineGap />);

    expect(html).toContain("h-3");
    expect(html).toContain("bg-ink");
    expect(html).not.toContain("border-");
    expect(html).not.toContain("-mb-px");
  });

  it("is decoration, so a screen reader never stops on it", () => {
    expect(renderToStaticMarkup(<MachineGap />)).toContain(
      'aria-hidden="true"',
    );
  });
});

describe("MachineRail", () => {
  // A machine's hue separates two computers before a word is read. It ran as a 2px
  // border INSIDE the card, one pixel from the card's own border — a grey hairline
  // immediately followed by a coloured one, doing one job twice — and, being a
  // border, it also stole 2px of layout the trailing edge had no match for (left ink
  // 19px against right ink 17px). It is the card's LEFT FRAME now: the card gives
  // that side up, both sides are 2px, and the rail simply colours one of them.
  it("is the frame, in the machine colour", () => {
    const html = renderToStaticMarkup(
      <MachineRail color={MACHINE_COLORS[3]!}>rows</MachineRail>,
    );
    expect(html).toContain("border-l-2");
    expect(html).toContain(MACHINE_COLORS[3]!.rail);
    expect(html).toContain("rows");
  });

  it("gives two machines two different rails", () => {
    const first = renderToStaticMarkup(
      <MachineRail color={MACHINE_COLORS[0]!}>a</MachineRail>,
    );
    const second = renderToStaticMarkup(
      <MachineRail color={MACHINE_COLORS[1]!}>a</MachineRail>,
    );
    expect(first).not.toBe(second);
  });

  // Without a hue it still has to PAINT: this is the card's edge, and a frame that
  // disappears where a colour is missing is a hole in the panel, not a subtlety.
  it("falls back to the list frame rather than vanishing", () => {
    const html = renderToStaticMarkup(<MachineRail>rows</MachineRail>);
    expect(html).toContain("border-l-2");
    expect(html).toContain("border-dialog-edge");
  });
});

describe("MachineBanner", () => {
  // The hue lives on the RAIL. A banner that also inked its outgoing rule wore the
  // machine's colour twice in one corner.
  it("keeps the list hairline, leaving the colour to the rail", () => {
    const html = renderToStaticMarkup(<MachineBanner>rows</MachineBanner>);
    expect(html).toContain("border-b border-dialog-edge");
    expect(html).not.toContain("border-b-2");
  });

  it("makes the machine band one deliberate step taller than the project band", () => {
    const machine = renderToStaticMarkup(
      <MachineBanner>studio-mbp</MachineBanner>,
    );
    const project = renderToStaticMarkup(
      <SectionHeader tone="project">vis</SectionHeader>,
    );

    expect(machine).toContain("min-h-14");
    expect(machine).toContain("mouse:min-h-10");
    expect(project).toContain("min-h-13");
    expect(project).toContain("mouse:min-h-9");

    for (const html of [machine, project]) {
      expect(html).toContain("items-stretch");
      expect(html).not.toContain("py-2");
    }
  });

  it("gives each level its own step on the type scale", () => {
    expect(
      renderToStaticMarkup(
        <MachineBanner>
          <HeaderTitle name="studio-mbp" />
        </MachineBanner>,
      ),
    ).toContain("text-subhead");
    expect(
      renderToStaticMarkup(
        <SectionHeader tone="project">
          <HeaderTitle name="vis" />
        </SectionHeader>,
      ),
    ).toContain("text-title");
    expect(renderToStaticMarkup(<HeaderTitle name="orphan" />)).toContain(
      "text-title",
    );
  });

  it("stands each level on its own paper", () => {
    const machine = renderToStaticMarkup(<MachineBanner>x</MachineBanner>);
    const project = renderToStaticMarkup(
      <SectionHeader tone="project">x</SectionHeader>,
    );
    expect(machine).toContain("bg-level-machine");
    expect(project).toContain("bg-level-project");
    for (const html of [machine, project]) {
      expect(html).not.toContain("bg-panel-2");
    }
  });

  it("marks a machine with a block bigger than a session status dot", () => {
    expect(
      renderToStaticMarkup(
        <MachineMark size="banner" color={MACHINE_COLORS[2]!} />,
      ),
    ).toContain("size-2.5");
    expect(
      renderToStaticMarkup(<MachineMark color={MACHINE_COLORS[2]!} />),
    ).toContain("size-1.5");
  });
});

describe("MachineMark", () => {
  it("is the rail hue as a solid block, and decoration only", () => {
    const html = renderToStaticMarkup(
      <MachineMark color={MACHINE_COLORS[7]} />,
    );

    expect(html).toContain(MACHINE_COLORS[7].dot);
    expect(html).toContain('aria-hidden="true"');
    expect(html).not.toContain("bg-ok");
  });
});

// Regression (reported: "the new session is button so frequently used that we should
// take it from the ⋯ and put on every machine header before the ⋯, as a yellow
// button"): the verb this whole screen exists for was the first row of a menu, so the
// thing people do all day cost a tap, a popover and a read before it could be pressed.
describe("NewSessionButton", () => {
  const html = (props: Partial<Parameters<typeof NewSessionButton>[0]> = {}) =>
    renderToStaticMarkup(
      <NewSessionButton machine="tower" onPress={() => {}} {...props} />,
    );

  it("is the yellow one: the verb of the screen, not a row of a menu", () => {
    expect(html()).toContain("bg-accent");
    expect(html()).toContain("New session");
  });

  it("names the machine it will start on, because every header carries one", () => {
    expect(html({ machine: "nuc" })).toContain(
      'aria-label="New session on nuc"',
    );
  });

  it("puts the project on the tooltip, where the header has no room for a path", () => {
    expect(html({ where: "vis" })).toContain(
      'title="New session on tower, in vis"',
    );
    expect(html()).toContain('title="New session on tower"');
  });

  it("does not move under the press: it anchors the folder browser", () => {
    expect(html()).not.toContain("active:scale");
  });

  // Regression, user report: the 28px desktop box still read as a tall slab beside the
  // 24px machine action. The shared 16px line box fits safely inside a 24px control.
  it("uses the same compact mouse height as the neighboring small action", () => {
    expect(html()).toContain("mouse:h-6");
    expect(html()).toContain("mouse:min-h-6");
    expect(html()).toContain("self-center");
    expect(html()).toContain(" h-8 ");
    expect(html()).toContain("mouse:text-meta");
    expect(html()).not.toContain("mouse:h-7");
  });

  it("is refused while the machine is busy or not answering", () => {
    expect(html({ disabled: true })).toContain('disabled=""');
  });
});

// Regression, user report ("this new session button should be the same as other
// buttons"): every icon-only control was written by hand at its call site, so the
// machine header's `⋯` was a 32px bordered box while the project header's, one row
// below it, was a 44px borderless slab with a bigger glyph — and neither of them
// looked like the yellow button they stood beside.
describe("IconButton", () => {
  const html = (props: Partial<Parameters<typeof IconButton>[0]> = {}) =>
    renderToStaticMarkup(
      <IconButton label="Actions for tower" {...props}>
        <span aria-hidden>⋯</span>
      </IconButton>,
    );

  it("is the app’s button with a glyph where its word would be", () => {
    expect(html()).toContain("border-edge-strong");
    expect(html()).toContain("min-h-7");
    expect(html()).toContain("focus-visible:ring-accent/60");
  });

  // Reported: "the height of the new session button on iOS is too big". The touch
  // box WAS the paint (`h-11`), so the amber slab filled the whole header band.
  it("wears the same compact box as the yellow button beside it", () => {
    const primary = renderToStaticMarkup(
      <NewSessionButton machine="tower" onPress={() => {}} />,
    );

    for (const rhythm of [
      " h-8 ",
      "self-center",
      "mouse:h-6",
      "mouse:min-h-6",
    ]) {
      expect(html()).toContain(rhythm);
      expect(primary).toContain(rhythm);
    }
  });

  it("keeps the 44px finger target the 32px face gave up", () => {
    const primary = renderToStaticMarkup(
      <NewSessionButton machine="tower" onPress={() => {}} />,
    );

    // 32px of ink + 6px above + 6px below = 44px of touchable button.
    for (const reach of [
      "after:absolute",
      "after:-top-1.5",
      "after:-bottom-1.5",
    ]) {
      expect(html()).toContain(reach);
      expect(primary).toContain(reach);
    }
    // A cursor needs no invisible reach, and the desktop box is 24px anyway.
    expect(primary).toContain("mouse:after:content-none");
  });

  it("is named, because it carries no word", () => {
    expect(html()).toContain('aria-label="Actions for tower"');
  });

  it("does not move under the press: it anchors a menu", () => {
    expect(html()).not.toContain("active:scale");
  });
});

// Regression, user report ("there is this exit button in the artifacts and it also
// looks awful"): the artifacts sheet, an opened artifact and every dialog each spelled
// their own close out again, so the sheet ended up wearing a bordered chip in a strip
// of bordered chips where every other surface wears chrome.
describe("DialogClose", () => {
  const html = (props: Partial<Parameters<typeof DialogClose>[0]> = {}) =>
    renderToStaticMarkup(
      <DialogClose label="Close artifacts" onClose={() => {}} {...props} />,
    );

  it("is welded to the band it closes, by that band’s own hairline", () => {
    expect(html()).toContain("border-l");
    expect(html()).not.toMatch(/class="[^"]*\bborder\s/);
  });

  // Closing is not a destructive act until you mean it.
  it("goes red only under the pointer", () => {
    expect(html()).toContain("hover:bg-err/15");
    expect(html()).toContain("hover:text-err");
    expect(html()).not.toContain('text-err"');
  });

  it("changes nothing but the paper it sits on", () => {
    expect(html()).toContain("border-dialog-title-foreground/20");
    expect(html({ tone: "panel" })).toContain("border-dialog-edge");
  });

  it("is named for what it closes", () => {
    expect(html()).toContain('aria-label="Close artifacts"');
  });
});

// Regression, user report ("still the ⋯ between the machine and project are different
// fix it! MARGIN RIGHT DIFFERS AND ALSO WHY THERE ARE BORDERS"): the two
// kebabs had become the same Button, but each call site still spelled out its own
// popup semantics and glyph, and the app's default bordered box turned a header
// glyph into a second rival to the yellow verb standing beside it.
describe("KebabButton", () => {
  const html = (props: Partial<Parameters<typeof KebabButton>[0]> = {}) =>
    renderToStaticMarkup(<KebabButton label="Actions for tower" {...props} />);

  it("is one control: the machine’s and the project’s render the same box", () => {
    expect(
      html({ label: "Actions for tower" }).replace("Actions for tower", "X"),
    ).toBe(html({ label: "Actions for vis" }).replace("Actions for vis", "X"));
  });

  // Reported: the `⋯` and the row chevrons "have borders". They did — the frame
  // arrived on hover, and on a touch screen a tap counts as hover, so the glyph
  // boxed itself and stayed boxed with no pointer to leave.
  it("wears no border in ANY state: a header glyph is ink, not a rival box", () => {
    expect(html()).toContain("border-transparent");
    expect(html()).not.toContain("border-edge-strong");
    expect(html()).not.toContain("hover:border");
  });

  it("carries the popup semantics itself, so no call site can forget them", () => {
    expect(html()).toContain('aria-haspopup="menu"');
    expect(html({ isOpen: true })).toContain('aria-expanded="true"');
    expect(html()).toContain('aria-label="Actions for tower"');
  });

  it("keeps the header’s compact rhythm and one glyph size", () => {
    expect(html()).toContain(" h-8 ");
    expect(html()).toContain("mouse:h-6");
    expect(html()).not.toContain("active:scale");
  });

  // Over a thumbnail the app's paper is not underneath it, so the same control brings
  // its own ink instead of a call-site `bg-*` that Tailwind's emission order decides.
  it("has an overlay face for the artifact tile, with no height of its own", () => {
    const over = html({ variant: "overlay", density: "default" });
    expect(over).toContain("bg-ink/80");
    expect(over).not.toContain("self-center");
  });
});

// Regression, user report ("MARGIN RIGHT DIFFERS AND ALSO WHY THERE IS NO MARGIN
// BEFORE NEW SESSION"): the machine header padded its own right edge while the project
// header one row below ended flush against the screen, and the yellow verb was welded
// to the words beside it. The trailing cluster is one component now, so all three gaps
// are decided once.
describe("HeaderActions", () => {
  const html = renderToStaticMarkup(
    <HeaderActions>
      <HeaderMeta>2 projects</HeaderMeta>
      <KebabButton label="Actions for tower" />
    </HeaderActions>,
  );

  it("owns the right edge of every header in the list", () => {
    expect(html).toContain("pr-3");
    expect(html).toContain("sm:pr-4");
  });

  it("puts a gap in front of the cluster and between its controls", () => {
    expect(html).toContain("pl-2");
    expect(html).toContain("gap-2");
  });

  it("never stretches: a header control is centred in whatever row it landed in", () => {
    expect(html).toContain("shrink-0");
    expect(html).toContain("self-center");
  });

  it("is the only one padding that side, so the header stops doing it", () => {
    const banner = renderToStaticMarkup(<MachineBanner>machine</MachineBanner>);
    expect(banner).not.toContain("pr-");
    expect(banner).not.toContain("px-");
    // The leading edge belongs to whichever half starts the header, so a pressable
    // one can reach the screen edge with its hover.
    expect(renderToStaticMarkup(<HeaderTitle name="tower" />)).toContain(
      "pl-3",
    );
    expect(renderToStaticMarkup(<HeaderTitle name="tower" />)).toContain(
      "sm:pl-4",
    );
  });
});

describe("LiveCount", () => {
  it("says nothing when nothing is running", () => {
    expect(renderToStaticMarkup(<LiveCount count={0} />)).toBe("");
  });

  it("wears the same pulse a live session row does", () => {
    const html = renderToStaticMarkup(<LiveCount count={3} />);
    expect(html).toContain("animate-pulse bg-ok motion-reduce:animate-none");
    expect(html).toContain("3 live");
  });
});

// Regression, same report: the project header carried a FIXED 160px count column inside
// its own toggle, so on a 390px iPhone the name it exists to show was truncated to
// `~/v…` while "699 sessions" kept every pixel it asked for.
describe("HeaderToggle", () => {
  const html = renderToStaticMarkup(
    <HeaderToggle
      isOpen
      onToggle={() => {}}
      name="vis"
      path="~/vis"
      pathTitle="/Users/dev/vis"
    />,
  );

  it("gives the name every pixel the trailing cluster leaves", () => {
    expect(html).toContain("min-w-0");
    expect(html).toContain("flex-1");
    expect(html).not.toContain("w-40");
    expect(html).not.toContain("session");
  });

  it("is a button that fills the band, so its hover reaches the screen edge", () => {
    expect(html).toContain("<button");
    expect(html).toContain('aria-expanded="true"');
    expect(html).toContain("pl-3");
    expect(html).toContain("hover:bg-hover");
    expect(html).not.toContain("min-h-");
  });

  it("shows the path it is checked out at, with the full one on its title", () => {
    expect(html).toContain("~/vis");
    expect(html).toContain('title="/Users/dev/vis"');
  });
});

describe("HeaderTally", () => {
  it("says the whole phrase in a header that has the room for it", () => {
    const html = renderToStaticMarkup(<HeaderTally count={2} unit="project" />);
    expect(html).not.toContain("hidden sm:inline");
    expect(html).toContain("2");
  });

  it("keeps the number alone on a phone where a yellow verb shares the row", () => {
    const html = renderToStaticMarkup(
      <HeaderTally count={699} unit="session" isCrowded />,
    );
    expect(html).toContain("hidden sm:inline");
    expect(html).toContain("699");
  });

  it("always says the whole phrase to a screen reader", () => {
    const html = renderToStaticMarkup(
      <HeaderTally count={699} unit="session" />,
    );
    expect(html).toContain('<span class="sr-only">699 sessions</span>');
    expect(html).toContain('aria-hidden="true"');
  });

  it("counts one of a thing in the singular", () => {
    expect(
      renderToStaticMarkup(<HeaderTally count={1} unit="project" />),
    ).toContain('<span class="sr-only">1 project</span>');
  });
});

// Regression, user report ("some things are having margin left like the ⋯ then
// chevrons to open the session details are not having — i dnt want these margins"):
// measured on a 390px iPhone, the machine's mark began at x=14 but its NAME at 28,
// the project's name at 36, and a session's title at 10 — the deepest thing on the
// screen starting furthest left, so depth read backwards. On the other side the two
// header `⋯` stopped at x=378 while the session row's disclosure ran flush to 390.
describe("the list grid", () => {
  const leading = (html: string) =>
    html.includes("pl-3") && html.includes("sm:pl-4");

  it("starts every header on one leading edge", () => {
    expect(
      leading(
        renderToStaticMarkup(
          <HeaderTitle
            mark={<MachineMark color={MACHINE_COLORS[0]!} />}
            name="tower"
          />,
        ),
      ),
    ).toBe(true);
    expect(
      leading(
        renderToStaticMarkup(
          <HeaderToggle
            isOpen={false}
            onToggle={() => {}}
            name="vis"
            path="~/vis"
          />,
        ),
      ),
    ).toBe(true);
  });

  // The last 8px of the same misalignment: a 6px machine mark and a 14px project
  // chevron each sized to its own ink put the two header NAMES 8px apart.
  it("gives a machine mark and a project chevron one glyph column", () => {
    const title = renderToStaticMarkup(
      <HeaderTitle
        mark={<MachineMark color={MACHINE_COLORS[0]!} />}
        name="tower"
      />,
    );
    const toggle = renderToStaticMarkup(
      <HeaderToggle
        isOpen={false}
        onToggle={() => {}}
        name="vis"
        path="~/vis"
      />,
    );
    for (const html of [title, toggle]) {
      expect(html).toContain("grid size-3.5 shrink-0 place-items-center");
    }
  });

  // The trailing gutter lives INSIDE the last control, not on the cluster: a box
  // that respects the gutter and then centres a 12px glyph in 28px of its own put
  // the right-hand INK 30px from the paper while the left-hand ink sat at 19px.
  it("ends every row on one trailing edge, carried by the control itself", () => {
    // The cluster owns the gutter unconditionally — a project header drops its `⋯`
    // while a filter is live, and the amber verb must not then run to the paper.
    const cluster = renderToStaticMarkup(<HeaderActions>x</HeaderActions>);
    expect(cluster).toContain("pr-3");
    expect(cluster).toContain("sm:pr-4");

    for (const html of [
      renderToStaticMarkup(<KebabButton label="Actions for vis" />),
      renderToStaticMarkup(
        <RowDisclosure isOpen={false} label="Show details" />,
      ),
    ]) {
      expect(html).toContain("pr-3");
      expect(html).toContain("sm:pr-4");
      expect(html).toContain("justify-items-end");
      // ...and reclaims the cluster's gutter, so the BOX reaches the paper while the
      // GLYPH stops where the leading glyph starts.
      expect(html).toContain("-mr-3");
      expect(html).toContain("sm:-mr-4");
      // It ends ON the paper's edge, which already draws that line.
      expect(html).toContain("border-r-0");
    }
  });
});

// The disclosure is the `⋯`'s sibling — the rarer FACTS of a row where the kebab
// holds its rarer VERBS — so it is the same box in the same column, not a hand-built
// strip welded to the screen edge at 40% opacity.
describe("RowDisclosure", () => {
  const html = (isOpen: boolean) =>
    renderToStaticMarkup(
      <RowDisclosure isOpen={isOpen} label="Show details for Untitled" />,
    );

  it("is the same button as the kebab beside it", () => {
    const kebab = renderToStaticMarkup(<KebabButton label="Actions for vis" />);
    for (const token of [
      "min-w-10",
      "sm:min-w-12",
      "mouse:min-w-10",
      " h-8 ",
      "mouse:h-6",
    ]) {
      expect(html(false)).toContain(token);
      expect(kebab).toContain(token);
    }
  });

  it("names what it opens and reports whether it is open", () => {
    expect(html(false)).toContain('aria-expanded="false"');
    expect(html(true)).toContain('aria-expanded="true"');
    expect(html(false)).toContain('aria-label="Show details for Untitled"');
  });

  it("never rests on an opacity that would fail contrast while it does", () => {
    expect(html(false)).not.toContain("opacity-40");
  });
});

// The dialog that "Manage projects" opens arrived by dropping into the middle of
// the glass while the `⋯` menu beside it slid up from the bottom edge: two layers
// with the same job and two physics. Below `sm:` a dialog is a SHEET.
// Regression, user report ("dialogs should occupy full height on the iPhone, and on
// desktop they should have similar heights and widths"): the sheet stopped at its
// content's height and `size` gave two different desktop widths.
describe("Modal and DialogFrame as a phone sheet", () => {
  const source = uiSource;

  it("lets the sheet take the whole glass on a phone and centres it from sm: up", () => {
    expect(source).toContain("flex items-stretch justify-center bg-ink/85");
    expect(source).toContain("sm:items-center");
    // No padding at all on the phone: a sheet touches all four edges.
    expect(source).toContain("sm:pb-[max(1rem,env(safe-area-inset-bottom))]");
  });

  it("gives every dialog ONE desktop box and a full-height phone sheet", () => {
    expect(source).toContain(
      "DIALOG_DESKTOP_HEIGHT = 'sm:h-[min(38rem,100%)]'",
    );
    expect(source).toContain(
      "flex w-full flex-col sm:max-w-xl ${DIALOG_DESKTOP_HEIGHT}",
    );
    // One width, so a question and a file browser are the same rectangle.
    expect(source).not.toContain("sm:max-w-md");
    expect(source).not.toContain("sm:max-w-lg");
  });

  it("slides the frame in from below by its own height, and only tips in on desktop", () => {
    const html = renderToStaticMarkup(
      <DialogFrame title="Manage projects">body</DialogFrame>,
    );
    expect(html).toContain("starting:translate-y-full");
    expect(html).toContain("sm:starting:translate-y-2");
    // The menu sheet's own top edge, so the two layers read as one family.
    expect(html).toContain("border-t-2 border-accent");
    expect(html).toContain("sm:border sm:border-dialog-edge");
    // Full-bleed on the phone, it owns BOTH safe areas itself.
    expect(html).toContain("pt-[env(safe-area-inset-top)]");
    expect(html).toContain("pb-[env(safe-area-inset-bottom)]");
    expect(html).toContain("sm:pb-0");
    // A column that fills its parent, so the body scrolls and the footer docks.
    expect(html).toContain("flex min-h-0 flex-1 flex-col");
  });
});

// Regression, user report ("the cog dialog looks slightly better than manage
// projects — make manage projects use the same outer component, canonicalize"):
// application settings hand-rolled its own scrim and its own `<section>` beside
// `Modal`/`DialogFrame`, so the app had two dialogs with two heights, two widths
// and two entrances. There is one now, and it wears the better glass of the two.
describe("one outer dialog component", () => {
  const settings = settingsSource;

  it("opens application settings in Modal + DialogFrame like every other dialog", () => {
    expect(settings).toContain("<Modal onDismiss={onClose}>");
    expect(settings).toContain("<DialogFrame");
    // No second scrim, no second dialog box, no second close button.
    expect(settings).not.toContain("fixed inset-0 z-50");
    expect(settings).not.toContain('aria-modal="true"');
    expect(settings).not.toContain(
      "max-h-[calc(100%-env(safe-area-inset-top))]",
    );
  });

  it("keeps the glass that dialog had, in the one Modal", () => {
    expect(uiSource).toContain("bg-ink/85 backdrop-blur-[2px]");
    expect(uiSource).toContain("starting:opacity-0");
  });
});

// Regression, user report ("why doesn't the delete button on a project have the
// hover of the full height of its parent?"): an `edge` IconButton wore the compact
// scale's fixed 32px face and centred it, so the trash ending a "Manage projects"
// row painted a floating hover band inside a taller row, with a dead strip above
// and below it. A control that ENDS a row hovers the row.
describe("a row-ending icon button fills its row", () => {
  it("stretches instead of centring a fixed face", () => {
    const edge = uiSource.slice(uiSource.indexOf("const box = edge"));
    const line = edge.slice(0, edge.indexOf("\n", edge.indexOf("?")));
    expect(line).toContain("h-auto");
    expect(line).toContain("self-stretch");
    // A stretched box needs no invisible reach for its target.
    expect(line).toContain("after:content-none");
  });
});

// Regression, user report ("session element right side before the > has no enough
// padding right because hovering over the element looks awful"): the pressable half of
// a session row painted its hover slab to x=340 and its own facts ended at 340 too, so
// the ink sat on the boundary of its own highlight, a hair from the `›`.
describe("a row's pressable slab", () => {
  it("pads its trailing inside edge exactly as LIST_EDGE pads the leading one", () => {
    expect(uiSource).toContain("export const LIST_EDGE = 'pl-3 sm:pl-4';");
    expect(uiSource).toContain("export const LIST_EDGE_END = 'pr-3 sm:pr-4';");
  });

  it("never lets a row spell that padding itself", () => {
    expect(sessionsListSource).not.toContain("pr-3 sm:pr-4");
    expect(sessionsListSource).toContain("${LIST_EDGE} ${LIST_EDGE_END}");
  });
});

// Regression, user report ("the offer drafts is jumping ... left right",
// then "we need to normalize this setting because it's so hard to read"): the
// setting was a lone on/off button whose title, sub-line and a trailing state word
// all changed at once — and the auto-width `on`/`off` slid the row 6px sideways on
// every tap. It is a CHOICE now, spelled with the dialog's one choice control.
describe("where a new session starts", () => {
  it("is a pair of named choices, not a toggle with a changing state word", () => {
    expect(settingsSource).not.toContain('offerDrafts ? "on" : "off"');
    expect(settingsSource).toContain('title="Where a new session starts"');
    expect(settingsSource).toContain('title="In the project"');
    expect(settingsSource).toContain('title="Ask me first"');
    expect(settingsSource).toContain("selected={!offerDrafts}");
  });

  it("picks every setting with the one ChoiceCell, spelled once", () => {
    expect(settingsSource.match(/<ChoiceCell/g)?.length).toBe(4);
    expect(settingsSource.match(/function ChoiceCell\(/g)?.length).toBe(1);
    // No hand-rolled copy of it left behind.
    expect(settingsSource).not.toContain(
      '{selected ? "\u25cf" : "\u25cb"}\n                    </span>',
    );
  });
});

// Regression, user report ("New session in a draft should be under the project line,
// not the machine one — and there is already a New session button there"): the draft
// verb was a row in the machine's kebab menu, two headers above the project it forks,
// while the project header carried the plain verb alone.
describe("NewSessionButton, split", () => {
  it("carries the draft as a joined second half, not a second button", () => {
    const html = renderToStaticMarkup(
      <NewSessionButton machine="visgw" where="vis" onPress={() => {}} onDraft={() => {}} />,
    );
    // One cluster, two halves of the same amber box: the verb drops its trailing
    // border and the draft half wears the seam.
    expect(html).toContain("border-r-0");
    expect(html).toContain("border-l-accent-foreground/30");
    // It NAMES the project it forks and the machine it forks on: several headers are
    // on screen at once and "New session" alone says nothing to a screen reader.
    expect(html).toContain('aria-label="New session in a draft of vis on visgw"');
  });

  it("is a plain button when drafts are not offered", () => {
    const html = renderToStaticMarkup(
      <NewSessionButton machine="visgw" where="vis" onPress={() => {}} />,
    );
    expect(html).not.toContain("border-r-0");
    expect(html).not.toContain("in a draft");
  });

  it("hangs the draft question off the project header, never the machine menu", () => {
    // The project header's split button is the only way in...
    expect(sessionsListSource).toContain("onDraft={onNewDraft ?");
    expect(sessionsListSource).toContain("const openDraftsAt = useCallback(");
    // ...and the machine's own menu keeps only what is genuinely a machine verb.
    expect(sessionsListSource).not.toContain('title="New session in a draft…"');
    // The parked drafts read belongs to the project that was tapped, not to whatever
    // the machine happened to touch last.
    expect(sessionsListSource).toContain("projectPath(session) === draftRoot");
  });
});
