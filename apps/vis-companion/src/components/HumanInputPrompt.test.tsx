// @vitest-environment jsdom
// The sheet opens in `Modal`, which PORTALS into the document — there is no
// document in the `node` environment, and a portal cannot be rendered to a
// string. Every case here renders and then reads the body it landed in.
import { cleanup, render, screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { HumanInputSheet } from './HumanInputPrompt';
import promptSource from './HumanInputPrompt.tsx?raw';
import { HUMAN_INPUT_REQUESTS } from '../dev/humanInputVariants';
import fixture from '../lib/human-input.fixture.json';
import {
  humanInputIsDecoration,
  humanInputRequestFromWire,
  initialHumanInputValues,
  HUMAN_INPUT_CHOICE_MARKS,
  HUMAN_INPUT_NODE_TYPES,
  type HumanInputField,
  type HumanInputValues,
} from '../lib/human-input';

const noop = () => {};

function markup(state: string, extra: Record<string, unknown> = {}, values?: HumanInputValues) {
  const request = HUMAN_INPUT_REQUESTS[state];
  // One sheet in the document at a time: two mounts would leave the previous
  // request's markup in the string this returns.
  cleanup();
  render(
    <HumanInputSheet
      request={request}
      values={values ?? initialHumanInputValues(request)}
      onChange={noop}
      onSubmit={noop}
      onCancel={noop}
      {...extra}
    />,
  );
  return document.body.innerHTML;
}

const otpValues = initialHumanInputValues(HUMAN_INPUT_REQUESTS.otp);

/**
 * The outer HTML of the first element whose attributes contain `marker`, found
 * by walking tag depth — enough of a parser to ask "is this inside that?".
 */
function element(html: string, marker: string): string {
  const open = html.lastIndexOf('<', html.indexOf(marker));
  let depth = 0;
  const tag = /<(\/?)div\b[^>]*?(\/?)>/g;
  tag.lastIndex = open;
  for (let hit = tag.exec(html); hit; hit = tag.exec(html)) {
    if (hit[2] === '/') continue;
    depth += hit[1] === '/' ? -1 : 1;
    if (depth === 0) return html.slice(open, hit.index + hit[0].length);
  }
  throw new Error(`unterminated element for ${marker}`);
}

describe('human input sheet', () => {
  // The question can be longer than the phone. When the whole dialog scrolled,
  // the two buttons that END the pause scrolled away with it and a long form
  // could not be answered at all without scrolling back.
  it('keeps the answer buttons out of the scrolling question', () => {
    const html = markup('long');
    const scroller = element(html, 'overflow-y-auto');
    expect(html).toContain('Cut the release');
    expect(html).toContain('Abort');
    expect(scroller).toContain('Release notes');
    expect(scroller).not.toContain('Cut the release');
    expect(scroller).not.toContain('Abort');
  });

  it('offers no way out of a pause that has none', () => {
    const html = markup('uncancellable');
    expect(html).not.toContain('Cancel this request');
    expect(html).not.toContain('Esc cancel');
    expect(html).toContain('Unlock');
    const cancellable = markup('approve');
    // The way out of a question CANCELS it — the one dialog whose leaving is not
    // simply "Close <title>", and it says so.
    expect(cancellable).toContain('Cancel this request');
    expect(cancellable).toContain('Not now');
  });

  it('never invents a verdict: pristine says nothing, and submit still fires', () => {
    // A disabled button with no reason next to it is the worst answer to "why
    // can I not send this" — and the app cannot even know: the validators are
    // functions in the extension. So the sheet stays pressable and stays quiet
    // until the engine refuses something.
    const off = (html: string) => /<button[^>]*\sdisabled=""/.test(html);
    const buttons = 'flex gap-2 sm:justify-end';
    const pristine = markup('minimal');
    expect(off(element(pristine, buttons))).toBe(false);
    expect(pristine).not.toContain('is required');
    // The engine's refusal needs no touch, and it does not deaden the button:
    // the operator fixes the field and confirms again.
    const refused = markup('minimal', { errors: { branch: 'is required' } });
    expect(refused).toContain('is required');
    expect(off(element(refused, buttons))).toBe(false);
  });

  it('says that the run is blocked, and how many more are behind it', () => {
    expect(markup('approve')).toContain('This run is waiting for you');
    expect(markup('approve', { waiting: 1 })).toContain('1 more request waiting');
    expect(markup('approve', { waiting: 3 })).toContain('3 more requests waiting');
  });

  it('names whoever stopped the run', () => {
    expect(markup('approve')).toContain('deploy.sh');
  });

  it('shows the engine’s refusal next to the field it refused', () => {
    const html = markup('rejected', {
      error: 'The engine refused this answer.',
      errors: { ticket: 'Must look like OPS-1234.' },
    });
    expect(html).toContain('The engine refused this answer.');
    expect(element(html, 'overflow-y-auto')).toContain('Must look like OPS-1234.');
  });

  it('connects every written-field label and refusal to its native control', () => {
    markup('grouped', { errors: { user: 'Use a service account.' } });

    expect(screen.getByRole('textbox', { name: 'User, required' })).toBeTruthy();
    const password = screen.getByLabelText(/Password/);
    expect(screen.getByRole('textbox', { name: 'Notes' })).toBeTruthy();
    const user = screen.getByRole('textbox', { name: 'User, required' });
    const describedBy = user.getAttribute('aria-describedby');
    expect(describedBy).toBeTruthy();
    expect(document.getElementById(describedBy ?? '')?.textContent).toBe('Use a service account.');
    expect(user.getAttribute('aria-invalid')).toBe('true');
    expect(user.getAttribute('aria-required')).toBe('true');
    expect(password.getAttribute('aria-required')).toBe('true');
  });

  it('gives a range field a real slider, its bounds, and a readable value', () => {
    const html = markup('slider');
    const slider = /<input[^>]*type="range"[^>]*>/.exec(html)?.[0] ?? '';
    expect(slider).toContain('min="0"');
    expect(slider).toContain('max="10"');
    expect(slider).toContain('step="0.5"');
    expect(slider).toContain('value="2.5"');
    // A slider with no number beside it is a guess, not an answer.
    expect(html).toContain('2.5');
    expect(html).toContain('Error budget');
  });

  it('masks a password and shows a checkbox as a pressed toggle', () => {
    const html = markup('slider');
    expect(html).toContain('type="password"');
    // The checkbox is a TUI-style `[✓]` toggle, so the state has to reach
    // assistive tech through aria-pressed rather than a checked input.
    expect(html).toContain('aria-pressed="true"');
    expect(html).toContain(HUMAN_INPUT_CHOICE_MARKS.inclusiveOn);
    expect(html).toContain('Halt on the first regression');
  });

  // The question IS the dialog's title, and the shared frame clipped it to one
  // line: on a phone the header read "How much of the error budget may …",
  // which is no longer a question anyone can answer.
  it('shows the whole question in the header', () => {
    const html = markup('slider');
    const heading = /<h2[^>]*>[\s\S]*?<\/h2>/.exec(html)?.[0] ?? '';
    expect(heading).toContain(HUMAN_INPUT_REQUESTS.slider.title);
    expect(heading).not.toContain('truncate');
    expect(heading).toContain('line-clamp-3');
  });

  it('enters a one-time code as one box per digit, not as a text field', () => {
    const html = markup('otp', {}, { ...otpValues, code: '408' });
    const boxes = html.match(/<input[^>]*aria-label="One-time code digit \d"[^>]*>/g) ?? [];
    expect(boxes).toHaveLength(6);
    // The keypad, not the alphabet: a code field that opens a QWERTY keyboard on
    // a phone costs the operator every tap it takes to find the numbers.
    expect(boxes[0]).toContain('inputmode="numeric"');
    expect(boxes[0]).toContain('pattern="[0-9]*"');
    expect(boxes[0]).toContain('maxlength="1"');
    // Only the FIRST box may claim the SMS autofill, or one code arrives six times.
    expect(boxes[0]).toContain('autocomplete="one-time-code"');
    expect(boxes[1]).toContain('autocomplete="off"');
    expect(boxes.slice(0, 4).map((box) => /value="(\d?)"/.exec(box)?.[1])).toEqual([
      '4',
      '0',
      '8',
      '',
    ]);
    expect(html).toContain('6 digits');
  });

  // Formik's lesson, both halves of it: a form that shouts before anyone has
  // typed is noise, and one that stays quiet after a refusal is a trap. Since
  // every validator lives in the extension that asked the question, the ONLY
  // thing that can speak here is a confirmation the engine turned down — and it
  // speaks for every field it named, touched or not.
  it('names a broken field only once a confirmation came back refused', () => {
    const values = { ...otpValues, code: '408', notify: 'ops@' };
    const pristine = markup('otp', {}, values);
    expect(pristine).not.toContain('must be an email address');
    expect(pristine).not.toContain('must be 6 digits');

    const refused = markup(
      'otp',
      { errors: { code: 'must be 6 digits', notify: 'must be an email address' } },
      values,
    );
    expect(refused).toContain('must be 6 digits');
    expect(refused).toContain('must be an email address');
  });

  // Grouping is LAYOUT, not an answer: a group holds no value, it only says which
  // fields stand side by side. One field per line made a host and its port read
  // as two unrelated questions.
  it('lays a group out as a row or a column, and nests the two directions', () => {
    const html = markup('grouped');
    const server = element(html, 'data-group-id="group:host+port"');
    expect(server).toContain('data-direction="row"');
    expect(server).toContain('flex-row');
    expect(server).toContain('placeholder="db.internal"');
    expect(server).toContain('placeholder="5432"');
    // The legend names the group; what it does not own stays outside it.
    expect(html).toContain('Server');
    expect(server).not.toContain('Notes');

    // A column INSIDE the form, holding a row: the two directions nest.
    const pool = element(html, 'data-group-id="group:pool"');
    expect(pool).toContain('flex-col');
    const inner = element(pool, 'data-group-id="group:size+tls"');
    expect(inner).toContain('data-direction="row"');
    expect(inner).toContain('placeholder="8"');
    expect(inner).toContain('placeholder="30"');
    // The checkbox is the column's second child, not part of that row.
    expect(inner).not.toContain('Require TLS');
    expect(pool).toContain('Require TLS');
  });
});

// A PAUSE IS ONLY AS TALL AS THE QUESTION

// Regression, user report ("when we are showing OTP cannot we make it less height,
// like it goes from bottom only and occupies only the height its required?"): the
// sheet hand-rolled its own scrim beside `Modal` and took the WHOLE glass, so six
// digit boxes opened a full-screen page — 844px tall on a 390x844 phone, with a
// phone's length of empty panel between the code and the verbs that end the pause.
describe('a pause is only as tall as the question it asks', () => {
  it('opens in the sheet that stops at its content, arriving from the bottom edge', () => {
    const html = markup('otp');
    // `Modal size="fit"`: welded to the bottom edge on a phone, and no fixed
    // 38rem box above `sm:` either.
    const scrim = /<div class="fixed inset-0 z-50[^"]*"/.exec(html)?.[0] ?? '';
    expect(scrim).not.toContain('items-stretch');
    expect(html).not.toContain('sm:h-[min(38rem,100%)]');
  });

  // The scrim was a third copy of the same forty characters, and it had already
  // drifted — `bg-black/60` against the one glass every other layer wears.
  it('brings no second scrim of its own', () => {
    expect(promptSource).toContain('<Modal');
    expect(promptSource).toContain('size="fit"');
    expect(promptSource).not.toContain('fixed inset-0 z-50');
    expect(promptSource).not.toContain('bg-black/60');
    expect(promptSource).not.toContain('DIALOG_DESKTOP_HEIGHT');
  });

  // A sheet that starts halfway down the glass has no notch above it: the inset
  // hung 47px of dead paper over the title of the one dialog whose whole point
  // is to take no more height than it needs.
  it('does not clear a notch that is not above it', () => {
    expect(markup('otp')).not.toContain('pt-[env(safe-area-inset-top)]');
  });

  // Regression, user report ("in general you need to ALSO DO BEST EFFORT JUSTIFY
  // because now its too much"): each box took a sixth of the glass — 55px wide on
  // a 390px phone — so a six-digit code read as six empty fields.
  it('stretches a digit box only as far as a digit box goes', () => {
    const html = markup('otp');
    const boxes = html.match(/<input[^>]*aria-label="One-time code digit \d"[^>]*>/g) ?? [];
    // One shape everywhere — the desktop no longer shrinks it to `w-9`.
    expect(boxes[0]).not.toContain('sm:flex-none');
    expect(boxes[0]).not.toContain('w-9');
  });

  // Regression, user report (a 440pt iPhone photograph of the shipped sheet): the
  // bounded row was capped on the phone too, so the code stopped 24px short of the
  // field under it — justified against nothing.
  it('runs the code row to the edges of the field column on a phone', () => {
    const row = element(markup('otp'), 'aria-label="One-time code"');
    const classes = (/class="([^"]*)"/.exec(row)?.[1] ?? '').split(/\s+/);
    // Below `sm:` the cap is wider than the widest phone's field column (408px on
    // a 440pt iPhone), so it never bites there; it only tightens in the box.
    expect(classes).not.toContain('max-w-sm');
  });
});

// FULL support, proved against the engine's own bytes
//
// `human-input.fixture.json` is `request->view` verbatim, and the Clojure suite
// that re-derives it also pins that it holds ONE NODE OF EVERY KIND the engine
// can send. Rendering that very request is therefore this app's proof of
// complete human-input support: a node type nobody wired up is a hole in a
// dialog that has already stopped somebody's run, and it fails here instead of
// in front of the operator.
describe('the engine’s whole node vocabulary', () => {
  const request = humanInputRequestFromWire(fixture);
  if (!request) throw new Error('the engine fixture must parse');
  cleanup();
  render(
    <HumanInputSheet
      request={request}
      values={initialHumanInputValues(request)}
      onChange={noop}
      onSubmit={noop}
      onCancel={noop}
    />,
  );
  const html = document.body.innerHTML;
  cleanup();

  /** Every node of the tree, groups and their children alike. */
  function nodes(fields: readonly HumanInputField[]): HumanInputField[] {
    return fields.flatMap((field) => [field, ...nodes(field.fields ?? [])]);
  }

  it('is what the engine sends — nothing skipped, nothing invented', () => {
    expect([...new Set(nodes(request.fields).map((field) => field.type))].sort()).toEqual(
      [...HUMAN_INPUT_NODE_TYPES].sort(),
    );
  });

  it('gives every answerable type a control of its own', () => {
    // plaintext / password / multiline: a line, a masked line, and a box.
    expect(html).toContain('placeholder="Anything the on-call should know"');
    expect(html).toContain('<textarea');
    expect(html).toContain('type="password"');
    expect(html).toContain('Notify');
    // select vs multiselect: exclusive dots, inclusive boxes.
    expect(html).toContain('role="radiogroup"');
    expect(html).toContain(HUMAN_INPUT_CHOICE_MARKS.exclusiveOn);
    expect(html).toContain(HUMAN_INPUT_CHOICE_MARKS.exclusiveOff);
    expect(html).toContain('Staging');
    expect(html).toContain(HUMAN_INPUT_CHOICE_MARKS.inclusiveOff);
    // checkbox: the fixture's is defaulted ON, so it renders as pressed.
    expect(html).toContain(HUMAN_INPUT_CHOICE_MARKS.inclusiveOn);
    expect(html).toContain('aria-pressed="true"');
    // range: the field's own track, not the engine's percentage default.
    const slider = /<input[^>]*type="range"[^>]*>/.exec(html)?.[0] ?? '';
    expect(slider).toContain('min="0"');
    expect(slider).toContain('max="10"');
    expect(slider).toContain('step="0.5"');
    expect(slider).toContain('value="2.5"');
    // otp: one box per digit it accepts, and it says the shorter code is enough.
    expect(html.match(/aria-label="One-time code digit \d"/g) ?? []).toHaveLength(6);
    expect(html).toContain('4–6 digits');
    // group: the row, and the column nested inside it.
    expect(html).toContain('data-direction="row"');
    expect(html).toContain('data-direction="column"');
    expect(html).toContain('Server');
  });

  it('paints ink as ink: read, never answered, never keyed', () => {
    expect(html).toContain('<h3');
    expect(html).toContain('Target');
    expect(html).toContain('Staging pages nobody.');
    // A decoration has no name and a group holds no answer, so the values map
    // is exactly the answerable leaves.
    expect(Object.keys(initialHumanInputValues(request)).sort()).toEqual(
      nodes(request.fields)
        .filter((field) => field.type !== 'group' && !humanInputIsDecoration(field))
        .map((field) => field.id)
        .sort(),
    );
  });

  it('marks what the engine requires, for the eye and for a screen reader', () => {
    const required = nodes(request.fields).filter((field) => field.is_required);
    expect(required.map((field) => field.id).sort()).toEqual(['code', 'host', 'key']);
    expect(html.match(/<span class="sr-only">, required<\/span>/g) ?? []).toHaveLength(
      required.length,
    );
  });
});
