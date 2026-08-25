import iconsSource from "./icons.tsx?raw";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";
import {
  AlertIcon,
  ArrowDownIcon,
  ArrowOutIcon,
  CameraIcon,
  CheckIcon,
  ChevronIcon,
  ClipIcon,
  CloseIcon,
  CopyIcon,
  DownloadIcon,
  DotsIcon,
  ImageIcon,
  MicIcon,
  PencilIcon,
  PauseIcon,
  PlayIcon,
  PlusIcon,
  SettingsIcon,
  SortIcon,
  StarIcon,
  StopIcon,
  TrashIcon,
  VoiceLoopIcon,
} from "./icons";

const ICONS = {
  AlertIcon: <AlertIcon />,
  ArrowDownIcon: <ArrowDownIcon />,
  ArrowOutIcon: <ArrowOutIcon />,
  CameraIcon: <CameraIcon />,
  CheckIcon: <CheckIcon />,
  ChevronIcon: <ChevronIcon />,
  ClipIcon: <ClipIcon />,
  CloseIcon: <CloseIcon />,
  CopyIcon: <CopyIcon />,
  DotsIcon: <DotsIcon />,
  DownloadIcon: <DownloadIcon />,
  ImageIcon: <ImageIcon />,
  MicIcon: <MicIcon />,
  PencilIcon: <PencilIcon />,
  PauseIcon: <PauseIcon />,
  PlayIcon: <PlayIcon />,
  PlusIcon: <PlusIcon />,
  SettingsIcon: <SettingsIcon />,
  SortIcon: <SortIcon />,
  "SortIcon asc": <SortIcon dir="asc" />,
  "SortIcon desc": <SortIcon dir="desc" />,
  StarIcon: <StarIcon />,
  StopIcon: <StopIcon />,
  TrashIcon: <TrashIcon />,
  VoiceLoopIcon: <VoiceLoopIcon />,
};

/**
 * WHAT THE MARK ACTUALLY MEASURES.
 *
 * `viewBox="0 0 24 24"` and `size-3.5` pin the CANVAS, not the drawing on it: a
 * paperclip drawn corner to corner and an `✕` drawn across the middle third are
 * the same box and two different sizes on screen. So the box is walked — every
 * command of every `d`, absolute and relative, plus circles and rects — and the
 * INK is measured.
 */
const ARGS: Record<string, number> = {
  M: 2,
  L: 2,
  H: 1,
  V: 1,
  C: 6,
  S: 4,
  Q: 4,
  T: 2,
  A: 7,
  Z: 0,
};

function markBox(html: string) {
  const xs: number[] = [];
  const ys: number[] = [];
  const add = (x: number, y: number) => {
    xs.push(x);
    ys.push(y);
  };

  for (const m of html.matchAll(/\sd="([^"]+)"/g)) {
    const tokens = m[1].match(/[A-Za-z]|-?\d*\.?\d+/g) ?? [];
    let cmd = "M";
    let x = 0;
    let y = 0;
    let i = 0;
    while (i < tokens.length) {
      const token = tokens[i];
      if (/[A-Za-z]/.test(token)) {
        cmd = token;
        i += 1;
        continue;
      }
      const up = cmd.toUpperCase();
      const count = ARGS[up] ?? 0;
      if (count === 0) {
        i += 1;
        continue;
      }
      const values = tokens.slice(i, i + count).map(Number);
      i += count;
      const rel = cmd === cmd.toLowerCase();
      if (up === "H") x = rel ? x + values[0] : values[0];
      else if (up === "V") y = rel ? y + values[0] : values[0];
      else {
        const [ex, ey] = values.slice(-2);
        x = rel ? x + ex : ex;
        y = rel ? y + ey : ey;
      }
      add(x, y);
      // An implicit repeat of `M` is a line, and it keeps the same case.
      if (up === "M") cmd = rel ? "l" : "L";
    }
  }

  for (const tag of html.match(/<(circle|rect)[^>]*>/g) ?? []) {
    const at = (key: string) =>
      Number(new RegExp(`${key}="(-?[\\d.]+)"`).exec(tag)?.[1] ?? Number.NaN);
    if (tag.startsWith("<circle")) {
      const [cx, cy, r] = [at("cx"), at("cy"), at("r")];
      add(cx - r, cy - r);
      add(cx + r, cy + r);
    } else {
      const [x, y, w, h] = [at("x"), at("y"), at("width"), at("height")];
      add(x, y);
      add(x + w, y + h);
    }
  }

  const x0 = Math.min(...xs);
  const y0 = Math.min(...ys);
  const x1 = Math.max(...xs);
  const y1 = Math.max(...ys);
  const width = x1 - x0;
  const height = y1 - y0;
  const aspect = Math.max(width, height) / Math.min(width, height);
  return {
    x0,
    y0,
    x1,
    y1,
    width,
    height,
    // A chevron is a thin mark: it can only be as big as it is TALL, so
    // matching it by area would tower over the square icons beside it.
    // Everything else is measured by area, so a wide camera and a tall
    // microphone still weigh the same.
    size: aspect >= 1.8 ? Math.max(width, height) : Math.sqrt(width * height),
  };
}

const round = (n: number) => Math.round(n * 100) / 100;

describe("the icon set", () => {
  it("draws every icon in ONE grammar", () => {
    for (const [name, icon] of Object.entries(ICONS)) {
      const html = renderToStaticMarkup(icon);
      // The composer's own grid, size and stroke: a strip of these has to look
      // like a set, not like five fonts.
      expect(html, name).toContain('viewBox="0 0 24 24"');
      expect(html, name).toContain('stroke="currentColor"');
      expect(html, name).toContain('stroke-width="1.8"');
      expect(html, name).toMatch(/class="[^"]*\bsize-3\.5\b/);
      // Decoration inside an already-labelled control.
      expect(html, name).toContain('aria-hidden="true"');
      // It is a drawing, not a character.
      expect(html, name).toMatch(/<(path|circle|rect)/);
    }
  });

  // Regression, reported as a chevron as tall as the whole paste block: an icon
  // takes `className` for colour and spacing, but `className = "size-3.5"` was a
  // DEFAULT PARAMETER, so the caller's string REPLACED the size instead of
  // adding to it. The transcript's collapsed paste passes `mr-1.5 inline-block
  // text-dialog-hint` and shipped an SVG with no width at all — which an `<svg>`
  // answers by growing to fill its container.
  it("keeps its own size when a caller only styles it", () => {
    const styled = renderToStaticMarkup(
      <ChevronIcon className="mr-1.5 inline-block text-dialog-hint" />,
    );
    expect(styled).toContain("size-3.5");
    expect(styled).toContain("mr-1.5");
  });

  it("leaves the size to a caller that names one", () => {
    const chip = renderToStaticMarkup(
      <CloseIcon className="size-3 opacity-70" />,
    );
    expect(chip).toContain("size-3 ");
    expect(chip).not.toContain("size-3.5");
    const boxed = renderToStaticMarkup(<CloseIcon className="h-4 w-4" />);
    expect(boxed).not.toContain("size-3.5");
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
  //
  // Regression, user report ("the star is not showing on the session row"): the
  // whole mark was then painted in that fill and nothing else — #ffc420 on the
  // light theme's #faf3eb paper is 1.45:1, so the starred row carried a glyph
  // that could not be seen. The FILL stays the brand yellow; the OUTLINE is the
  // amber ink, which is what makes the shape visible on paper.
  it("fills a starred star with the brand accent and outlines it in the ink", () => {
    const filled = renderToStaticMarkup(<StarIcon filled />);
    expect(filled).toContain("fill-accent stroke-accent-ink");
    // Not the ink as a FILL: that was the brown star an earlier report rejected.
    expect(filled).not.toContain("fill-accent-ink");
    expect(filled).not.toContain("fill-current");
    expect(renderToStaticMarkup(<StarIcon />)).toContain(
      "fill-none stroke-current",
    );
  });

  // Regression: every icon was drawn to its own extent inside the shared
  // viewBox, so the paperclip (corner to corner) came out half again as big as
  // the close cross (the middle third) at the very same `size-3.5`, and the
  // gear and the star were bigger still.
  it("draws every mark at the same optical size", () => {
    const measured = Object.entries(ICONS).map(([name, icon]) => {
      const box = markBox(renderToStaticMarkup(icon));
      return { name, size: round(box.size) };
    });
    const off = measured.filter(({ size }) => size < 12.5 || size > 14.5);

    expect(off).toEqual([]);
  });

  it("keeps every mark inside one live area", () => {
    // 4–20 of the 24 grid. The padding is what lets a control put an icon
    // against a label without the icon deciding the row's height.
    const off = Object.entries(ICONS)
      .map(([name, icon]) => ({
        name,
        box: markBox(renderToStaticMarkup(icon)),
      }))
      .filter(
        ({ box }) => box.x0 < 4 || box.y0 < 4 || box.x1 > 20 || box.y1 > 20,
      )
      .map(({ name, box }) => ({
        name,
        box: [round(box.x0), round(box.y0), round(box.x1), round(box.y1)],
      }));

    expect(off).toEqual([]);
  });

  it("centres every mark on the grid", () => {
    const off = Object.entries(ICONS)
      .map(([name, icon]) => ({
        name,
        box: markBox(renderToStaticMarkup(icon)),
      }))
      .map(({ name, box }) => ({
        name,
        centre: [round((box.x0 + box.x1) / 2), round((box.y0 + box.y1) / 2)],
      }))
      .filter(
        ({ centre }) =>
          Math.abs(centre[0] - 12) > 0.5 || Math.abs(centre[1] - 12) > 0.5,
      );

    expect(off).toEqual([]);
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
const GLYPHS_AS_ICONS = [
  "✕",
  "✖",
  "✗",
  "▾",
  "▶",
  "◀",
  "▲",
  "↓",
  "↗",
  "▣",
  "≡",
  "⋯",
  "›",
  "‹",
];

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

/**
 * `{'›'}` paints the very same chevron as `›` — it is only spelled so that a
 * scanner reading the source misses it. Every escape inside a STRING is folded
 * back to its character before the string is judged, so a glyph cannot hide
 * behind its own code point. Only strings: a regex that MATCHES a character in
 * text the app receives (the TUI's `⋯` gutter divider) paints nothing and
 * is none of this test's business.
 */
const UNICODE_ESCAPE = /\\u\{([0-9a-fA-F]{1,6})\}|\\u([0-9a-fA-F]{4})/g;
const unescaped = (source: string) =>
  source.replace(
    UNICODE_ESCAPE,
    (_match, braced: string | undefined, plain: string) =>
      String.fromCodePoint(Number.parseInt(braced ?? plain, 16)),
  );

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
  const markup = withoutComments(source).replace(STRING, (raw) => {
    const literal = unescaped(raw);
    if (!isProse(literal)) collect(literal.slice(1, -1));
    return " ";
  });
  // What is left is markup: every run between the tags and the braces is either
  // the text a reader sees or code, and neither may carry a control mark.
  for (const run of markup.split(/[<>{}]/)) collect(run);
  return [...marks];
};

/**
 * An icon rides a LINE OF TYPE, and that line says how big it is: `text-chip` is
 * 9px on a 12px box (`index.css`), so the 14px default stands a head taller than
 * every word beside it. A chip line takes `size-3`; `text-meta` and up keep the
 * default. The type is read off the element the icon SITS IN — the nearest one
 * above it that names a size — and an icon nobody put on a line of type is left
 * alone.
 */
const TYPE_SCALE = /\btext-(chip|meta|ui|body|title|subhead|head|display)\b/g;

const oversizedOnChipLines = (source: string) => {
  const lines = source.split("\n");
  const offenders: string[] = [];
  for (let i = 0; i < lines.length; i += 1) {
    const icon = /<([A-Z]\w*Icon)\b/.exec(lines[i]);
    if (!icon) continue;
    const above = lines.slice(Math.max(0, i - 12), i + 1).join("\n");
    const type = [...above.matchAll(TYPE_SCALE)].pop();
    if (type?.[1] !== "chip") continue;
    const props = lines
      .slice(i, i + 6)
      .join("\n")
      .split("/>")[0];
    if (!/size-3(?![\d.])/.test(props)) offenders.push(icon[1]);
  }
  return offenders;
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
    // Regression, reported as "these chevrons look bad — different heights,
    // fonts etc" in the session list: the disclosure in front of a session row was
    // the CHARACTER › set in the row's mono face, one line above a real
    // `ChevronIcon`, so the same mark came out at two sizes in the same column. It
    // survived this scan twice over — › was not on the list, and the session row
    // spelled it `{'\\u203a'}`, which reads as prose to a scanner that never
    // decodes an escape.
    expect(glyphsAsIcons("<span>›</span>")).toEqual(["›"]);
    expect(glyphsAsIcons("<span>{'\\u203a'}</span>")).toEqual(["›"]);
    expect(glyphsAsIcons("<span aria-hidden>‹</span>")).toEqual(["‹"]);
    expect(glyphsAsIcons("<span>Settings ›</span>")).toEqual(["›"]);
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

  // A PLOT IS NOT AN ICON. `ui.tsx`'s `Waveform` draws one `<rect>` per measured
  // bucket, as many as the width a `ResizeObserver` reports; an icon is a fixed glyph
  // on the 24-unit grid, and keeping those in one module is the whole point of this
  // rule. So the plot is named here rather than exempted by folder, and it still may
  // never draw a glyph's `<path>`.
  it("import those icons from the one module that draws them", () => {
    const drawn = Object.entries(sources).filter(
      ([path, source]) =>
        !path.includes("/icons.tsx") &&
        !path.includes("/dev/") &&
        !path.includes(".test.") &&
        /<svg/.test(source),
    );

    expect(drawn.map(([path]) => path)).toEqual(["./ui.tsx"]);
    const [[, wave]] = drawn;
    expect(wave.match(/<svg/g)).toHaveLength(1);
    expect(wave).not.toContain("<path");
  });

  // Regression: the app's two-item tab bar was deleted, and its marks — a
  // document and a stack of machines — stayed behind in `icons.tsx` with no call
  // site, kept alive only by this file. A geometry test over a mark nobody draws
  // proves nothing, so the set is checked against the screens that use it.
  it("draw every mark the icon module exports", () => {
    const exported = [...iconsSource.matchAll(/export function (\w+)/g)].map(
      ([, name]) => name,
    );
    const dead = exported.filter((name) =>
      Object.entries(sources).every(
        ([path, source]) =>
          path.includes("/icons.tsx") ||
          path.includes("/dev/") ||
          path.includes(".test.") ||
          !new RegExp(`\\b${name}\\b`).test(source),
      ),
    );

    expect(dead).toEqual([]);
  });

  // Regression, reported as "the paperclip is too big now": the artifacts chip
  // is a 9px `text-chip` line and the icon beside those words was the 14px
  // default, so the clip read as a sticker pasted onto the label.
  it("size an icon to the line of type it rides", () => {
    const offenders = Object.entries(sources).flatMap(([path, source]) =>
      path.includes("/dev/") || path.includes(".test.")
        ? []
        : oversizedOnChipLines(source).map((mark) => `${path}: ${mark}`),
    );

    expect(offenders).toEqual([]);
  });

  it("reads the type off the element the icon sits in", () => {
    const chip =
      '<span className="font-mono text-chip">\n  <CloseIcon />\n</span>';
    expect(oversizedOnChipLines(chip)).toEqual(["CloseIcon"]);
    expect(
      oversizedOnChipLines(
        chip.replace("<CloseIcon />", '<CloseIcon className="size-3" />'),
      ),
    ).toEqual([]);
    // `size-3.5` is the default, not a chip size.
    expect(
      oversizedOnChipLines(
        chip.replace("<CloseIcon />", '<CloseIcon className="size-3.5" />'),
      ),
    ).toEqual(["CloseIcon"]);
    // A `text-meta` row keeps the default.
    expect(
      oversizedOnChipLines(chip.replace("text-chip", "text-meta")),
    ).toEqual([]);
  });
});
