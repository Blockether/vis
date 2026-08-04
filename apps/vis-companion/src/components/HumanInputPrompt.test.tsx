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

  it('refuses to submit an answer the engine would reject anyway', () => {
    // `disabled` is also a Tailwind variant on every button, so this asks for
    // the ATTRIBUTE, not for the word.
    const off = (html: string) => /<button[^>]*\sdisabled=""/.test(html);
    const buttons = 'flex gap-2 sm:justify-end';
    expect(off(element(markup('minimal'), buttons))).toBe(true);
    expect(off(element(markup('minimal', {}, { branch: 'fix/108' }), buttons))).toBe(false);
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
      fieldErrors: { ticket: 'Must look like OPS-1234.' },
    });
    expect(html).toContain('The engine refused this answer.');
    expect(element(html, 'overflow-y-auto')).toContain('Must look like OPS-1234.');
  });
});
