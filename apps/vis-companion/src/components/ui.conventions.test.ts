// The vocabulary's rules about the SOURCE, not about behaviour. These scan files
// instead of rendering them: they are a lint for the design system, kept here
// because a failing rule must block the suite like any other regression.
//
// What a control DOES is tested next door in `ui.test.tsx` by rendering it. What
// it LOOKS like is drawn by Storybook (`ui.stories.tsx`) and looked at. Only
// rules that are genuinely about the source belong in this file: the closed
// vocabulary, the corner and shadow rungs, named ways out, and gallery coverage.
import { describe, expect, it } from 'vitest';

import uiSource from './ui.tsx?raw';

// Every production component module, as text. Test fixtures, stories and dev-only
// files are excluded: they are allowed to spell things the shipped app may not.
const production = Object.entries(
  import.meta.glob(['../**/*.tsx', '!../**/*.test.tsx', '!../**/*.stories.tsx'], {
    query: '?raw',
    import: 'default',
    eager: true,
  }) as Record<string, string>,
).filter(([path]) => !path.includes('/dev/') && !path.includes('harness'));

const stories = Object.entries(
  import.meta.glob(['../**/*.stories.tsx'], {
    query: '?raw',
    import: 'default',
    eager: true,
  }) as Record<string, string>,
);

const productionSource = production.map(([, source]) => source).join('\n');

/** Every control `ui.tsx` exports, by name. SCREAMING_CASE class strings are not controls. */
const controls = [...uiSource.matchAll(/^export (?:function|const) ([A-Z]\w+)/gm)]
  .map(([, name]) => name)
  .filter((name) => name !== name.toUpperCase());

// The design constitution, as executable rules.
describe('the vocabulary stays closed', () => {
  // One radius rung: the square corner. A `rounded-*` utility anywhere else is a
  // second corner being born.
  it('uses only square-corner utilities throughout the app', () => {
    for (const [path, source] of production) {
      const corners = [...source.matchAll(/\brounded(?:-[\w-]+|-\[[^\]]+\])?/g)];
      expect(
        corners.map(([corner]) => corner).filter((corner) => corner !== 'rounded-none'),
        path,
      ).toEqual([]);
    }
  });

  // One shadow: the floating-layer offset `shadow-float` that index.css defines
  // from the TUI's dialog-shadow token. The QR viewfinder spreads a box-shadow as
  // a camera scrim, which is a mask rather than elevation.
  it('casts only the one floating-layer offset', () => {
    for (const [path, source] of production) {
      if (path.endsWith('/QrScanner.tsx')) continue;
      const shadows = [
        ...source.matchAll(/(?<![\w-])(?:[\w-]+:)*shadow(?:-[\w-]+|-\[[^\]]+\])(?![\w-])/g),
      ];
      expect(
        shadows.map(([shadow]) => shadow).filter((shadow) => !shadow.endsWith('shadow-float')),
        path,
      ).toEqual([]);
    }
  });

  // Hover feedback stays in the foreground: a hovered background, border, shadow,
  // ring or transform is a surface the call site invented.
  it('keeps hover feedback in the foreground throughout production', () => {
    const hoverSurfaces = (source: string) =>
      source.match(
        /[^\s"'`]*hover[^\s"'`]*:(?:bg-|border-|shadow\b|ring-|outline-|scale-|translate-|rotate-)[^\s"'`]*/g,
      ) ?? [];

    for (const [path, source] of production) {
      expect(hoverSurfaces(source), path).toEqual([]);
    }
  });

  // A `className` at a call site may only POSITION the control it is given. Ink,
  // paper, frame, type, padding and height belong to the component that owns the
  // face — otherwise the last call site Tailwind emitted wins the design.
  it('hands no paint to a control ui.tsx owns', () => {
    const owned = new Set(controls);
    const paint =
      /^(?:sm:|md:|lg:|mouse:|hover:|focus:|focus-visible:|active:|disabled:|motion-reduce:|dark:)*(?:text-|font-|bg-|border|rounded|shadow|opacity-|italic|uppercase|tracking-|leading-|p-|px-|py-|pt-|pb-|pl-|pr-|min-h-|h-\d)/;

    const offenders: string[] = [];
    for (const [path, source] of production) {
      if (path.endsWith('/ui.tsx')) continue;
      for (const [, name, between, classes] of source.matchAll(
        /<([A-Z]\w+)([^>]*?)className=\{?["'`]([^"'`]*)["'`]/g,
      )) {
        if (!owned.has(name) || between.includes('>')) continue;
        const seen = classes.split(/\s+/).filter((one) => paint.test(one));
        if (seen.length > 0) offenders.push(`${path}: ${name}: ${seen.join(', ')}`);
      }
    }
    expect(offenders).toEqual([]);
  });

  // A control exported but used in fewer than two production places is dead, or
  // private to a single call site it should live inside.
  it('keeps the shared vocabulary shared and deletes dead controls', () => {
    const privateOrDead = controls.filter(
      (name) => [...productionSource.matchAll(new RegExp(`<${name}(?=[\\s/>])`, 'g'))].length < 2,
    );
    expect(privateOrDead).toEqual([]);
  });

  // The surfaces that once hand-rolled their buttons may not grow one back; the
  // transcript keeps exactly one — the `attachment://` link the desktop window
  // claims before React is reached.
  it('spells no button the vocabulary does not own', () => {
    const mustNotSpell = [
      '../screens/SessionScreen.tsx',
      '../screens/SettingsScreen.tsx',
      './Machines.tsx',
      '../screens/ConnectScreen.tsx',
      './DocArtifact.tsx',
      './ErrorBoundary.tsx',
      './HumanInputPrompt.tsx',
      './ProviderAuth.tsx',
      '../screens/RouterScreen.tsx',
    ];
    for (const [path, source] of production) {
      if (mustNotSpell.includes(path) || path.startsWith('../screens/settings/')) {
        expect(source.match(/<button\b/g) ?? [], path).toEqual([]);
      }
      if (path === './ChatContent.tsx') {
        expect(source.match(/<button\b/g) ?? []).toHaveLength(1);
      }
    }
  });

  // One ✕: the mark is `CloseButton`'s alone, so a `<CloseIcon` at a call site is
  // a second close button growing back.
  it('draws the ✕ only inside the one component', () => {
    const drawn: string[] = [];
    for (const [path, source] of production) {
      if (path.endsWith('/icons.tsx')) continue;
      const marks = [...source.matchAll(/<CloseIcon\b/g)].length;
      if (path.endsWith('/ui.tsx')) {
        expect(marks).toBe(1);
        continue;
      }
      if (marks > 0) drawn.push(path);
    }
    expect(drawn).toEqual([]);
  });
});

// One gesture, named for the thing it leaves. A screen reader on a stack of three
// open bands must be able to tell the human which one it is about to close.
describe('every way out is named', () => {
  /** Every opening `<Tag …>` in a file, as text; braces are counted so a `>` inside an arrow function or a `footer={<div>…}` never ends the element early. */
  const elementsOf = (source: string, tag: string) => {
    const found: string[] = [];
    const open = new RegExp(`<${tag}\\b`, 'g');
    for (let match = open.exec(source); match; match = open.exec(source)) {
      let depth = 0;
      for (let i = match.index; i < source.length; i += 1) {
        const ch = source[i];
        if (ch === '{') depth += 1;
        else if (ch === '}') depth -= 1;
        else if (ch === '>' && depth === 0) {
          found.push(source.slice(match.index, i + 1));
          break;
        }
      }
    }
    return found;
  };

  it('never leaves an icon-only way out unnamed', () => {
    const unnamed: string[] = [];
    for (const [path, source] of production) {
      for (const tag of ['DialogHeader', 'MenuHeading']) {
        for (const element of elementsOf(source, tag)) {
          const named = element.includes('closeLabel') || element.includes('closeWith(');
          if (element.includes('onClose') && !named) unnamed.push(`${path} <${tag}>`);
        }
      }
      for (const element of elementsOf(source, 'CloseButton')) {
        if (!element.includes('label')) unnamed.push(`${path} <CloseButton>`);
      }
    }
    expect(unnamed).toEqual([]);
  });

  it('never names a way out just "Close"', () => {
    const generic = production.filter(([, source]) => /\blabel="Close"/.test(source)).map(([path]) => path);
    expect(generic).toEqual([]);
  });
});

// The gallery is the ONE place a design is looked at, so a control no story draws
// is a control nobody has seen since the commit that added it.
describe('every control is drawn in the gallery', () => {
  // Pieces of a larger surface rather than gallery entries of their own. Naming
  // the owner keeps the exception closed: a new component module must either get
  // its own story or say which existing frame actually exercises it.
  const indirectStoryOwner: Record<string, string> = {
    SessionHealth: './SessionStats.stories.tsx',
    AnnotationLayer: './MarkdownArtifact.stories.tsx',
    ChatContent: './LiveView.stories.tsx',
    HumanInputPrompt: '../dev/humanInput.stories.tsx',
    LiveArtifact: './ChatContent.live.stories.tsx',
    PdfArtifact: './DocArtifact.stories.tsx',
    SessionNavigator: './ui.stories.tsx',
  };

  it('draws every component ui.tsx exports', () => {
    const drawn = stories.map(([, source]) => source).join('\n');
    const undrawn = controls.filter((name) => !new RegExp(`<${name}[\\s/>]`).test(drawn));
    expect(undrawn).toEqual([]);
  });

  it('gives every component module a direct story or a named parent frame', () => {
    const componentModules = Object.keys(
      import.meta.glob(['./*.tsx', '!./*.test.tsx', '!./*.stories.tsx'], {
        query: '?raw',
        import: 'default',
        eager: true,
      }) as Record<string, string>,
    );
    const infrastructure = new Set(['icons', 'ui']);
    const direct = new Set(
      stories
        .map(([path]) => path)
        .filter((path) => path.startsWith('./'))
        .map((path) => path.replace(/^\.\//, '').replace('.stories.tsx', '')),
    );
    const missing = componentModules
      .map((path) => path.replace(/^\.\//, '').replace('.tsx', ''))
      .filter((name) => !infrastructure.has(name))
      .filter((name) => !direct.has(name) && !(name in indirectStoryOwner));

    expect(missing).toEqual([]);
    for (const owner of Object.values(indirectStoryOwner)) {
      expect(
        stories.map(([path]) => path),
        `${owner} is named as a component's gallery owner`,
      ).toContain(owner);
    }
  });

  // A story is a FIXTURE. Anything that fetches, ticks or rolls a die draws a
  // different picture every time it is opened, and two frames stop comparing.
  // Interaction tests may wait for a fixed grace period, but the callback may
  // only resolve that awaited promise: it must not generate or mutate fixtures.
  const withoutInteractionDelays = (source: string) =>
    source.replace(
      /\bawait\s+new\s+Promise\(\s*\(([A-Za-z_$][\w$]*)\)\s*=>\s*setTimeout\(\s*\1\s*,\s*\d+\s*\)\s*\)/g,
      '',
    );

  it('allows only side-effect-free awaited literal interaction delays', () => {
    expect(
      withoutInteractionDelays('await new Promise((resolve) => setTimeout(resolve, 350));'),
    ).not.toContain('setTimeout');
    for (const source of [
      'setTimeout(updateFixture, 350);',
      'new Promise((resolve) => setTimeout(resolve, 350));',
      'await new Promise((resolve) => setTimeout(resolve, delay));',
      'await new Promise((resolve) => setTimeout(updateFixture, 350));',
      'await new Promise((resolve) => setTimeout(() => { updateFixture(); resolve(); }, 350));',
      'await new Promise((resolve) => { setTimeout(resolve, 350); updateFixture(); });',
      'await new Promise((resolve) => setTimeout(resolve, 350, updateFixture()));',
      'setInterval(updateFixture, 350);',
      'fetch("/fixture");',
      'Math.random();',
      'new Date();',
      'new GatewayClient();',
    ]) {
      expect(withoutInteractionDelays(source)).toBe(source);
    }
  });

  it('draws from fixtures, never from a clock, a die or a gateway', () => {
    for (const [path, source] of stories) {
      for (const forbidden of [
        'fetch(',
        'Math.random',
        'setInterval',
        'setTimeout',
        'new Date(',
        'GatewayClient',
      ]) {
        expect(withoutInteractionDelays(source), `${path} ${forbidden}`).not.toContain(forbidden);
      }
    }
  });
});

// A DIALOG STANDS OVER THE APPLICATION WITHOUT PAINTING ON IT
//
// Reported: with settings open over a live session, every arriving message made the
// picker inside the dialog flicker. A `backdrop-filter` re-rasterises everything it
// covers and everything stacked above it whenever the page beneath changes, and a
// streaming transcript changes on nearly every frame. The ink wash went with the blur
// by the same call: no dialog dims or blurs what it covers. Each box carries its own
// paper, so nothing below it has to be painted for the dialog to be legible.
describe('a dialog neither blurs nor dims what it covers', () => {
  /** Every class list in a file, written either as a string or as a template. */
  const classLists = (source: string) =>
    [...source.matchAll(/className=(?:"([^"]*)"|\{`([^`]*)`)/g)].map(
      ([, quoted, templated]) => quoted ?? templated ?? '',
    );

  it('asks for no backdrop filter on any layer the app ships', () => {
    for (const [path, source] of production) {
      for (const classes of classLists(source)) {
        expect(classes.match(/\bbackdrop-[\w[\]-]+/)?.[0], `${path} class`).toBeUndefined();
      }
    }
  });

  it('leaves the full-layer scrim of every overlay unpainted', () => {
    for (const [path, source] of production) {
      for (const classes of classLists(source)) {
        // A layer that covers its whole host and stands in the stack: a scrim. An
        // opaque surface (`bg-ink`, `bg-black`) is a PAGE of its own and may paint;
        // a translucent wash is the application showing through, dimmed.
        if (!/\binset-0\b/.test(classes) || !/\bz-\[?\d/.test(classes)) continue;
        expect(classes.match(/\bbg-[\w-]+\/\d+/)?.[0], `${path} scrim`).toBeUndefined();
      }
    }
  });
});
