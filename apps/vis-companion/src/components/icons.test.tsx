import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";
import {
  AlertIcon,
  ArrowDownIcon,
  ArrowOutIcon,
  CameraIcon,
  ChevronIcon,
  ClipIcon,
  CloseIcon,
  ImageIcon,
  MicIcon,
  PencilIcon,
  PlayIcon,
  PlusIcon,
  StarIcon,
  TrashIcon,
} from "./icons";

const ICONS = {
  AlertIcon: <AlertIcon />,
  ArrowDownIcon: <ArrowDownIcon />,
  ArrowOutIcon: <ArrowOutIcon />,
  CameraIcon: <CameraIcon />,
  ChevronIcon: <ChevronIcon />,
  ClipIcon: <ClipIcon />,
  CloseIcon: <CloseIcon />,
  ImageIcon: <ImageIcon />,
  MicIcon: <MicIcon />,
  PencilIcon: <PencilIcon />,
  PlayIcon: <PlayIcon />,
  PlusIcon: <PlusIcon />,
  StarIcon: <StarIcon />,
  TrashIcon: <TrashIcon />,
};

describe("the icon set", () => {
  it("draws every icon in ONE grammar", () => {
    for (const [name, icon] of Object.entries(ICONS)) {
      const html = renderToStaticMarkup(icon);
      // The composer's own grid, size and stroke: a strip of these has to look
      // like a set, not like five fonts.
      expect(html, name).toContain('viewBox="0 0 24 24"');
      expect(html, name).toContain('stroke="currentColor"');
      expect(html, name).toContain('stroke-width="1.8"');
      expect(html, name).toMatch(/class="[^"]*\bsize-(3\.5|4)\b/);
      // Decoration inside an already-labelled control.
      expect(html, name).toContain('aria-hidden="true"');
      // It is a drawing, not a character.
      expect(html, name).toMatch(/<(path|circle|rect)/);
    }
  });

  it("turns the disclosure instead of swapping one character for another", () => {
    const shut = renderToStaticMarkup(<ChevronIcon />);
    const open = renderToStaticMarkup(<ChevronIcon open />);
    const shape = (html: string) => /d="([^"]*)"/.exec(html)?.[1];

    expect(shape(shut)).toBe(shape(open));
    expect(open).toContain("rotate-90");
    expect(shut).not.toContain("rotate-90");
    expect(open).toContain("motion-reduce:transition-none");
  });

  // Regression: the star was drawn in the legible amber INK a text glyph needs,
  // so "starred" never showed the brand yellow.
  it("fills a starred star with the brand accent and nothing else", () => {
    expect(renderToStaticMarkup(<StarIcon filled />)).toContain(
      "fill-accent stroke-accent",
    );
    expect(renderToStaticMarkup(<StarIcon />)).toContain(
      "fill-none stroke-current",
    );
  });
});

/**
 * Characters the app used to paint where an icon belongs. Each one is a mark
 * for a CONTROL — close, disclose, play, load more, open elsewhere — drawn in
 * whatever face the platform picked, at whatever weight that face ships.
 *
 * `●`/`○`/`[✓]` are NOT here on purpose: they are the cross-channel choice
 * marks the TUI paints too, and the spinner's Braille cadence is deliberate as
 * well. Only marks that stand in for an icon are refused.
 */
const GLYPHS_AS_ICONS = ["✕", "✖", "✗", "▾", "▶", "◀", "▲", "↓", "↗", "▣", "≡"];

/**
 * Two of them have an honest job in TEXT: `×` multiplies (`retried 3×`, `3 rows
 * × 3 cols`) and `▸` names a menu path (`Settings ▸ Vis`). They are refused
 * only when one stands ALONE — a lone `×` in a button is a close icon nobody
 * drew.
 */
const GLYPHS_ALONE = ["×", "▸"];

/** Comments may NAME the glyph they replaced — that is how a regression is documented. */
const withoutComments = (source: string) =>
  source.replace(/\/\*[\s\S]*?\*\//g, "").replace(/(^|[^:])\/\/.*$/gm, "$1");

const STRING = /(['"`])(?:\\.|(?!\1)[^\\])*\1/g;

/**
 * A glyph inside a SENTENCE is prose a human reads — "enable it in Settings ▸
 * Vis" names a menu path, and no icon belongs in the middle of a message. A
 * glyph that is a whole string, or that sits in the markup rather than in a
 * message, is standing in for a control mark and is refused.
 */
const isProse = (literal: string) =>
  /[\p{L}\p{N}]/u.test(literal.replace(/\$\{[^}]*\}/g, "").slice(1, -1));

const glyphsAsIcons = (source: string) => {
  const marks = new Set<string>();
  const collect = (run: string) => {
    for (const glyph of GLYPHS_AS_ICONS) {
      if (run.includes(glyph)) marks.add(glyph);
    }
    for (const glyph of GLYPHS_ALONE) {
      if (run.trim() === glyph) marks.add(glyph);
    }
  };
  const markup = withoutComments(source).replace(STRING, (literal) => {
    if (!isProse(literal)) collect(literal.slice(1, -1));
    return " ";
  });
  // What is left is markup: every run between the tags and the braces is either
  // the text a reader sees or code, and neither may carry a control mark.
  for (const run of markup.split(/[<>{}]/)) collect(run);
  return [...marks];
};

// Regression, reported as "INSTEAD OF THOSE GLYPHS PLEASE USE REAL ICONS!": the
// artifacts sheet shipped a `▶` for video, `✕` for close and `↓` for load-more,
// and the same characters stood in for icons across the dialogs, the session
// list and the transcript's disclosures.
describe("the shipped screens", () => {
  const sources = import.meta.glob(["../**/*.ts", "../**/*.tsx"], {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;

  it("tells a control mark from a sentence", () => {
    expect(glyphsAsIcons("<span>✕</span>")).toEqual(["✕"]);
    expect(glyphsAsIcons("<button>↓ Latest</button>")).toEqual(["↓"]);
    expect(glyphsAsIcons("const label = '✕';")).toEqual(["✕"]);
    expect(glyphsAsIcons("const label = `▶ ${name}`;")).toEqual(["▶"]);
    expect(glyphsAsIcons("const help = 'open Settings ▸ Vis';")).toEqual([]);
    expect(glyphsAsIcons("<button>×</button>")).toEqual(["×"]);
    expect(glyphsAsIcons("const tail = ` ×${count}`;")).toEqual([]);
    expect(glyphsAsIcons("// this used to paint a ✕\n")).toEqual([]);
  });

  it("never paint a glyph where an icon belongs", () => {
    const offenders: string[] = [];
    for (const [path, source] of Object.entries(sources)) {
      // `src/dev/**` is the design gallery: proposals, never shipped (see
      // `main.tsx`, which reaches it behind `import.meta.env.DEV`).
      if (path.includes("/dev/") || path.includes(".test.")) continue;
      for (const glyph of glyphsAsIcons(source)) {
        offenders.push(`${path}: ${glyph}`);
      }
    }
    expect(offenders).toEqual([]);
  });

  it("import those icons from the one module that draws them", () => {
    const drawn = Object.entries(sources).filter(
      ([path, source]) =>
        !path.includes("/icons.tsx") &&
        !path.includes("/dev/") &&
        !path.includes(".test.") &&
        /<svg/.test(source),
    );
    expect(drawn.map(([path]) => path)).toEqual([]);
  });
});
