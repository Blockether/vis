import { readFileSync, readdirSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// An iPad is a WIDE TOUCH device, so density may never be keyed to width alone.
// `sm:` means "at least 640px" and on a tablet that is true while the finger is
// unchanged: every `sm:`-gated shrink of a hit box or of the type scale made the
// iPad the second-class citizen of the two touch devices — session rows 56px ->
// 48px, composer buttons 32x28 -> 28x24, a settings toggle 32px -> 24px, and the
// composer's own text a step smaller. The `mouse:` variant (width >= 40rem AND
// `pointer: fine`) is the only place a control may get smaller, so this test
// reads the component tree and refuses the mistake at its source.

const src = join(dirname(fileURLToPath(import.meta.url)), '..', 'src');

/** Hit-box utilities. Padding is deliberately NOT policed: a wide screen may
 *  breathe differently as long as the tappable rectangle never shrinks. */
const boxPrefixes = ['min-h', 'max-h', 'min-w', 'size', 'h', 'w'];

/** The app's whole type scale, smallest first (AGENTS.md: no ad-hoc sizing). */
const textScale = [
  'text-chip',
  'text-meta',
  'text-ui',
  'text-body',
  'text-title',
  'text-subhead',
  'text-head',
  'text-display',
];

/** A Tailwind magnitude in quarter-rem steps, or null when there is none. */
const spacing = (raw) => {
  // An arbitrary value counts too: `w-[calc(2.75rem+env(safe-area-inset-left))]`
  // shrinking to `2.5rem` is exactly how the back button lost 4px on the iPad.
  const arbitrary = /^\[(?:calc\()?(\d+(?:\.\d+)?)(px|rem)/.exec(raw);
  if (arbitrary) return Number(arbitrary[1]) / (arbitrary[2] === 'px' ? 4 : 0.25);
  return /^\d+(?:\.\d+)?$/.test(raw) ? Number(raw) : null;
};

/** Measure one utility, or null when it carries no comparable magnitude
 *  (`w-full`, `h-px`, `text-left`, a colour, ...). */
export function measure(token) {
  const textIndex = textScale.indexOf(token);
  if (textIndex >= 0) return { kind: 'text', value: textIndex };
  for (const prefix of boxPrefixes) {
    if (!token.startsWith(`${prefix}-`)) continue;
    const value = spacing(token.slice(prefix.length + 1));
    return value === null ? null : { kind: prefix, value };
  }
  return null;
}

const sourceFiles = (dir) =>
  readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) return sourceFiles(path);
    return /\.tsx?$/.test(entry.name) && !/\.test\.tsx?$/.test(entry.name) ? [path] : [];
  });

/** Every quoted/backticked literal in the file — a base/`sm:` pair is always
 *  written inside one class list, so that is the scope of the comparison. */
const literals = (source) =>
  (source.match(/(["'`])(?:\\.|(?!\1)[^\\])*\1/g) ?? []).map((raw) => raw.slice(1, -1));

/** Every `sm:` utility in the source that is smaller than the base it overrides.
 *
 *  A class list is written on ONE line, and that line is the comparison's real
 *  scope: this scanner pairs quotes without a parser, so a lone apostrophe inside
 *  a comment ("the frame's own border") pairs with the next quote in the file and
 *  hands the rule a "literal" spanning two unrelated components — measured here, a
 *  section's `sm:min-h-0` was reported as shrinking the `min-h-12` of a different
 *  component nine lines below it. Line by line cannot pair those, and no real class
 *  list loses a pair it actually declares. */
export function shrinksAtSm(source) {
  const found = [];
  for (const line of literals(source).flatMap((literal) => literal.split('\n'))) {
    const tokens = line.split(/\s+/).filter(Boolean);
    const baseline = new Map();
    for (const token of tokens) {
      const m = measure(token);
      if (m && !baseline.has(m.kind)) baseline.set(m.kind, { token, value: m.value });
    }
    for (const token of tokens) {
      if (!token.startsWith('sm:')) continue;
      const m = measure(token.slice(3));
      const base = m && baseline.get(m.kind);
      if (m && base && m.value < base.value) found.push({ base: base.token, small: token });
    }
  }
  return found;
}

describe('touch density', () => {
  it('never shrinks a hit box or the type scale at a width-only breakpoint', () => {
    const offenders = sourceFiles(src).flatMap((file) =>
      shrinksAtSm(readFileSync(file, 'utf8')).map(
        ({ base, small }) =>
          `${file.slice(src.length + 1)}: ${base} -> ${small} (use mouse: instead)`,
      ),
    );
    expect(offenders).toEqual([]);
  });

  it('declares the mouse variant as width AND a fine pointer', () => {
    const css = readFileSync(join(src, 'index.css'), 'utf8');
    expect(css).toContain('@custom-variant mouse (@media (width >= 40rem) and (pointer: fine));');
  });

  it('recognises the utilities it is meant to police', () => {
    expect(measure('min-h-14')).toEqual({ kind: 'min-h', value: 14 });
    expect(measure('size-8')).toEqual({ kind: 'size', value: 8 });
    expect(measure('h-[36px]')).toEqual({ kind: 'h', value: 9 });
    expect(measure('w-[calc(2.75rem+env(safe-area-inset-left))]')).toEqual({
      kind: 'w',
      value: 11,
    });
    expect(measure('w-[calc(100%-2rem)]')).toBeNull();
    expect(measure('text-meta').value).toBeLessThan(measure('text-ui').value);
    expect(measure('w-full')).toBeNull();
    expect(measure('px-4')).toBeNull();
  });

  it('catches a shrink and ignores a grow or a padding change', () => {
    expect(shrinksAtSm('<i className="min-h-10 sm:min-h-9" />')).toEqual([
      { base: 'min-h-10', small: 'sm:min-h-9' },
    ]);
    expect(shrinksAtSm('<i className="min-h-10 mouse:min-h-9 sm:px-2 sm:min-h-12" />')).toEqual([]);
  });
});

// The type step owns its line-height (`index.css`): a `leading-*` utility or an
// ad-hoc `text-[Npx]` is exactly how the rhythm drifts — a 9px chip wearing
// `leading-snug` is a 12.375px line box, off the whole-pixel 2px grid. And the
// scale itself has to loosen as the text SHRINKS: 10px prose on a 14px line box
// reads as a block, which is what a 9px/12px chip and a 10px/14px meta step did
// to every wrapped description in the app.

/** Every hardcoded line-height or ad-hoc text size in one source file. */
export function overridesLeading(source) {
  return literals(source)
    .flatMap((literal) => literal.split(/\s+/).filter(Boolean))
    .filter((token) => /^(?:[a-z-]+:)*(?:leading-|text-\[)/.test(token));
}

/** `{size, lineHeight}` in px for every step of the scale, smallest first. */
export function typeSteps(css) {
  return textScale.map((name) => {
    const px = (suffix) =>
      Number(new RegExp(`--${name}${suffix}:\\s*(\\d+)px`).exec(css)?.[1] ?? NaN);
    return { name, size: px(''), lineHeight: px('--line-height') };
  });
}

describe('type scale', () => {
  it('never overrides a step line-height in the component tree', () => {
    const offenders = sourceFiles(src).flatMap((file) =>
      overridesLeading(readFileSync(file, 'utf8')).map(
        (token) => `${file.slice(src.length + 1)}: ${token} (the type step owns the line-height)`,
      ),
    );
    expect(offenders).toEqual([]);
  });

  it('keeps every line box on whole even pixels', () => {
    const offenders = typeSteps(readFileSync(join(src, 'index.css'), 'utf8')).filter(
      ({ size, lineHeight }) =>
        !Number.isInteger(size) || !Number.isInteger(lineHeight) || lineHeight % 2 !== 0,
    );
    expect(offenders).toEqual([]);
  });

  it('loosens the leading as the text gets smaller', () => {
    const steps = typeSteps(readFileSync(join(src, 'index.css'), 'utf8'));
    const tight = steps.filter(({ size, lineHeight }) => lineHeight / size < 1.4);
    expect(tight.map(({ name }) => name)).toEqual(['text-display']);
    // The two smallest steps carry every wrapped description in the app.
    expect(steps.slice(0, 2).map(({ size, lineHeight }) => lineHeight / size >= 1.5)).toEqual([
      true,
      true,
    ]);
  });

  it('keeps the smallest type step compact and refuses overrides', () => {
    expect(typeSteps(readFileSync(join(src, 'index.css'), 'utf8'))[0]).toEqual({
      name: 'text-chip',
      size: 8,
      lineHeight: 14,
    });
    expect(overridesLeading('<p className="text-chip leading-snug" />')).toEqual(['leading-snug']);
    expect(overridesLeading('<p className="sm:leading-5 text-[13px]" />')).toEqual([
      'sm:leading-5',
      'text-[13px]',
    ]);
    expect(overridesLeading('<p className="text-chip tracking-wider" />')).toEqual([]);
  });
});

// A control that pins its own HEIGHT has to fit its own type step: the step owns
// the line-height, so `h-7` (28px) with `py-[0.4375rem]` (14px) leaves 14px of
// content box for a 16px line box and the single line the composer holds is
// scrolled 2px out of its own field — and the autosizer, seeing scrollHeight 30
// against clientHeight 28, then jumped the box on the first keystroke. Padding
// is still not policed for hit boxes; it is policed HERE, because height minus
// padding is what the text actually gets.

/** Every `className` list in the file. Deliberately narrower than `literals`:
 *  this check resolves a control's box, so it must not read prose or a `class`
 *  fragment out of an unrelated string. */
export function classLists(source) {
  return [
    ...source.matchAll(/className=(?:"([^"]*)"|\{`([^`]*)`\}|\{'([^']*)'\})/g),
  ].map((match) => match[1] ?? match[2] ?? match[3] ?? '');
}

/** A vertical Tailwind magnitude in px, or null when it carries none. */
const pixels = (raw) => {
  const arbitrary = /^\[(\d+(?:\.\d+)?)(px|rem)\]$/.exec(raw);
  if (arbitrary) return Number(arbitrary[1]) * (arbitrary[2] === 'px' ? 1 : 16);
  return /^\d+(?:\.\d+)?$/.test(raw) ? Number(raw) * 4 : null;
};

/** The box a class list declares at one breakpoint: base utilities, then the
 *  variant's own overrides on top (`null` reads the base alone). */
function box(tokens, variant) {
  const seen = {};
  for (const token of tokens) {
    const colon = token.lastIndexOf(':');
    if (colon >= 0 && token.slice(0, colon) !== variant) continue;
    const utility = token.slice(colon + 1);
    if (textScale.includes(utility)) seen.step = utility;
    const box = /^(h|py|pt|pb|p)-(.+)$/.exec(utility);
    if (box) seen[box[1]] = box[2];
  }
  return seen;
}

/** Every fixed-height control whose own line box does not fit inside it. */
export function clipsItsLineBox(source, steps) {
  const lineBox = new Map(steps.map(({ name, lineHeight }) => [name, lineHeight]));
  const found = [];
  for (const list of classLists(source)) {
    const tokens = list.split(/\s+|\$\{[^}]*\}/).filter(Boolean);
    const variants = new Set([null]);
    for (const token of tokens) {
      const colon = token.lastIndexOf(':');
      if (colon >= 0) variants.add(token.slice(0, colon));
    }
    for (const variant of variants) {
      const at = { ...box(tokens, null), ...(variant ? box(tokens, variant) : {}) };
      const height = at.h === undefined ? null : pixels(at.h);
      const line = lineBox.get(at.step);
      if (height === null || !line) continue;
      const pad =
        (at.py !== undefined ? (pixels(at.py) ?? 0) * 2 : (at.p !== undefined ? (pixels(at.p) ?? 0) * 2 : 0)) +
        (pixels(at.pt ?? '') ?? 0) +
        (pixels(at.pb ?? '') ?? 0);
      if (height - pad < line) {
        found.push(
          `${variant ? `${variant}:` : ''}h-${at.h} with ${pad}px padding leaves ${height - pad}px for a ${line}px ${at.step} line`,
        );
      }
    }
  }
  return found;
}

describe('fixed-height controls', () => {
  it('never pins a height that clips its own line box', () => {
    const steps = typeSteps(readFileSync(join(src, 'index.css'), 'utf8'));
    const offenders = sourceFiles(src).flatMap((file) =>
      clipsItsLineBox(readFileSync(file, 'utf8'), steps).map(
        (detail) => `${file.slice(src.length + 1)}: ${detail}`,
      ),
    );
    expect(offenders).toEqual([]);
  });

  it('measures the composer the browser reported clipped', () => {
    const steps = typeSteps(readFileSync(join(src, 'index.css'), 'utf8'));
    // Measured in Chromium at 1280x900 with a fine pointer: clientHeight 28,
    // scrollHeight 30.
    expect(
      clipsItsLineBox(
        '<textarea className="h-8 py-2 text-ui mouse:h-7 mouse:py-[0.4375rem] mouse:text-meta" />',
        steps,
      ),
    ).toEqual(['mouse:h-7 with 14px padding leaves 14px for a 16px text-meta line']);
    expect(
      clipsItsLineBox(
        '<textarea className="h-8 py-2 text-ui mouse:h-7 mouse:py-1.5 mouse:text-meta" />',
        steps,
      ),
    ).toEqual([]);
    // A box that sets no height of its own, or none of ours, is not this test's
    // business.
    expect(clipsItsLineBox('<p className="min-h-7 py-2 text-ui" />', steps)).toEqual([]);
    expect(clipsItsLineBox('<p className="h-4 text-[9px]" />', steps)).toEqual([]);
  });
});
