import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import { HumanInputSheet } from './HumanInputPrompt';
import { HUMAN_INPUT_REQUESTS } from '../dev/humanInputVariants';
import { initialHumanInputValues, type HumanInputValues } from '../lib/human-input';

const noop = () => {};

function markup(state: string, extra: Record<string, unknown> = {}, values?: HumanInputValues) {
  const request = HUMAN_INPUT_REQUESTS[state];
  return renderToStaticMarkup(
    <HumanInputSheet
      request={request}
      values={values ?? initialHumanInputValues(request)}
      onChange={noop}
      onSubmit={noop}
      onCancel={noop}
      {...extra}
    />,
  );
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
    expect(html).not.toContain('Close dialog');
    expect(html).not.toContain('Esc cancel');
    expect(html).toContain('Unlock');
    const cancellable = markup('approve');
    expect(cancellable).toContain('Close dialog');
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
    // The checkbox is a TUI-style `[x]` toggle, so the state has to reach
    // assistive tech through aria-pressed rather than a checked input.
    expect(html).toContain('aria-pressed="true"');
    expect(html).toContain('[x]');
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
    expect(boxes[0]).toContain('inputMode="numeric"');
    expect(boxes[0]).toContain('pattern="[0-9]*"');
    expect(boxes[0]).toContain('maxLength="1"');
    // Only the FIRST box may claim the SMS autofill, or one code arrives six times.
    expect(boxes[0]).toContain('autoComplete="one-time-code"');
    expect(boxes[1]).toContain('autoComplete="off"');
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
