import { createElement } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import gallerySource from './DesignGallery.tsx?raw';
import { FLOW_STEPS, PROJECT_STATES, SessionFlowVariant } from './projectVariants';
import variantSource from './projectVariants.tsx?raw';

/**
 * The board is the deliverable, and a flow is only a flow when every card renders
 * the state it names: a card pointing at a state nobody wrote falls through to the
 * default and two "different" steps become the same photograph — which is exactly
 * how three identical PNGs shipped once before.
 */
describe('session flow design board', () => {
  it('registers every photographed proposal in the gallery', () => {
    // Importing the gallery module drags the whole app in, so the registry is
    // read as text: an unregistered variant never reaches a screenshot.
    for (const id of Object.keys(PROJECT_STATES)) {
      expect(gallerySource, id).toContain(`id: '${id}'`);
      expect(gallerySource, id).toContain(`states: PROJECT_STATES['${id}']`);
    }
    for (const [id, states] of Object.entries(PROJECT_STATES)) {
      expect(new Set(states).size, id).toBe(states.length);
    }
  });

  it('shoots the board at its own viewport, because 390px would clip it', () => {
    // The page owns the matrix, so it owns the size: `design-shots.mjs` reads
    // this back and skips the phone/desktop sweep for that one proposal.
    expect(gallerySource).toMatch(/id: 'session-ux-board',[\s\S]*?viewport: '\d+x\d+',/);
  });

  it('gives every step a state its variant actually has', () => {
    for (const step of FLOW_STEPS) {
      expect(PROJECT_STATES[step.variant], step.step).toBeDefined();
      expect(PROJECT_STATES[step.variant], `${step.step} ${step.state}`).toContain(step.state);
    }
  });

  it('numbers the steps 1.. once each, in walking order', () => {
    const steps = FLOW_STEPS.map((step) => step.step);
    expect(steps.length).toBe(new Set(steps).size);
    // The flow is read as an order; a gap or a repeat makes a reference ambiguous.
    expect(steps).toEqual(steps.map((_, index) => String(index + 1)));
  });

  it('photographs every state of the flow somewhere on the board', () => {
    // A state nothing on the board shows is a screen nobody will look at.
    const shown = new Set(FLOW_STEPS.map((step) => `${step.variant}:${step.state}`));
    for (const [id, states] of Object.entries(PROJECT_STATES)) {
      if (id === 'session-ux-board') continue;
      for (const state of states) expect(shown, `${id}:${state}`).toContain(`${id}:${state}`);
    }
  });
});

/**
 * A proposal photographed in chrome the app does not own is a proposal about a
 * different app. The source is read as text because the point is the CLASSES and
 * the primitives, not the render: these are the four ways this mock had already
 * drifted from `SessionsScreen`, `ui.tsx` and `icons.tsx`.
 */
describe('the design board wears the shipped chrome', () => {
  it('draws with the app’s own primitives, never a look-alike', () => {
    // Hand-rolled spans for a button, a switch, a machine mark or a pencil are how
    // a design system drifts one screenshot at a time.
    expect(variantSource).toContain("from '../components/ui'");
    expect(variantSource).toContain('<Button');
    expect(variantSource).toContain('<MachineMark');
    expect(variantSource).toContain('<PencilIcon');
    // `✎` renders in whatever fallback face the machine happens to have.
    expect(variantSource).not.toContain('{PENCIL}');
  });

  it('docks a menu to the bottom edge, because a phone never gets a popover', () => {
    // Both shipped `role="menu"` portals are sheets under `sm:`; every card here
    // is a 390px column, so a floating popover would photograph a screen that
    // does not exist.
    expect(variantSource).toContain('absolute inset-x-0 bottom-0');
    expect(variantSource).toContain('border-t-2 border-accent');
    expect(variantSource).not.toMatch(/absolute right-2[^`"]*top-full/);
  });

  it('keeps every tap target at the shipped 44px', () => {
    // A 32px kebab or pencil is exactly the shrink `scripts/touch-density.test.mjs`
    // refuses in shipped components.
    expect(variantSource).not.toContain('size-8');
    expect(variantSource).toMatch(/function KebabButton[\s\S]*?min-h-11/);
    expect(variantSource).toMatch(/function PencilButton[\s\S]*?size-11/);
  });

  it('spends the accent once per surface', () => {
    // The switch sheet already spends its amber on `Switch here`, so its title
    // band goes quiet; a filled band above a filled primary says it twice.
    expect(variantSource).toMatch(/<Sheet\s+tone="quiet"/);
  });

  it('renders a machine with the components the app renders it with', () => {
    // `MachineGap`/`MachineRail`/`MachineBanner` ARE the fleet list's machine block.
    // Re-drawing that block in look-alike classes is how the board and the screen
    // start disagreeing about what a machine looks like.
    for (const primitive of ['<MachineGap', '<MachineRail', '<MachineBanner', '<ChevronIcon']) {
      expect(variantSource, primitive).toContain(primitive);
    }
    // Identity, not liveness: the mark wears this machine's own hue, and `offline`
    // is a word — a green/grey lamp in that slot means something else entirely.
    expect(variantSource).toMatch(/<MachineMark color=\{machineHue\(/);
    expect(variantSource).toContain('offline');
  });

  it('draws every glyph as an icon, never as a dingbat', () => {
    // A literal `▸` or `✎` renders in whatever fallback face the machine happens to
    // have, so it is the one thing on the board that is not the app's own type. The
    // prose above may name the glyph it bans, so only the CODE is scanned.
    const rendered = variantSource.replace(/\/\*[\s\S]*?\*\//g, '').replace(/\/\/.*/g, '');
    for (const dingbat of ['▸', '▾', '✎', '✓']) {
      expect(rendered, dingbat).not.toContain(dingbat);
    }
  });

  it('has exactly one field skin, lifted from `ui.tsx`’s Input', () => {
    // Two hand-rolled copies of `bg-input …` had already drifted apart by a
    // `rounded-none`; the const is the only place that skin may be spelled.
    expect(variantSource.match(/bg-input/g)).toHaveLength(1);
    expect(variantSource).toMatch(/const FIELD =[\s\S]*?ring-1 ring-accent\/30/);
    // And one badge look, `StartOption`'s, so a chip never competes with a primary.
    expect(variantSource).toContain('{entry.badge && <span className={CHIP}>');
    expect(variantSource).not.toContain('border-accent text-accent-ink');
  });

  it('titles a settings section the way `SettingsPanel` does', () => {
    // The shipped panel header is a 2px accent tick beside the name, not the menu's
    // filled band: the settings screen has never worn a menu heading.
    expect(variantSource).toMatch(/function SettingsBand[\s\S]*?border-l-2 border-accent/);
    expect(variantSource).toMatch(/<SettingsBand title="Sessions"/);
    expect(variantSource).not.toMatch(/\$\{QUIET_BAND\}`}>(settings|appearance)/);
  });
});
/**
 * The one test here that RENDERS instead of reading: the solo promise is about what
 * is absent, and absence is the thing a source grep is worst at proving. The board
 * shipped a card captioned "no machine header, no chips, no machine question" whose
 * fleet bar counted three machines.
 */
describe('solo pays nothing', () => {
  const html = (state: string) =>
    renderToStaticMarkup(createElement(SessionFlowVariant, { state }));

  it('never mentions the fleet when one machine is paired', () => {
    const solo = html('solo');
    expect(solo).not.toMatch(/machines?</);
    expect(solo).toContain('3 projects');
    expect(solo).toContain('214 sessions');
  });

  it('still counts machines when there is a fleet to count', () => {
    // The negative above only means something next to the state it contrasts with.
    expect(html('menu')).toContain('3 machines');
  });
});
