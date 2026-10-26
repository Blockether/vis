// @vitest-environment jsdom
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { afterEach, describe, expect, it } from 'vitest';

import { type DocumentChrome, MarkdownArtifact } from './MarkdownArtifact';

/**
 * A document is always read inside somebody else's chrome, and this stands in for
 * it with the two parts a band has: the verbs it offers, and what it reports.
 */
const chrome: DocumentChrome = ({ actions, note, body }) => (
  <div>
    <header>
      {actions}
      {note}
    </header>
    {body}
  </div>
);

const mounted: (() => void)[] = [];

afterEach(() => {
  for (const done of mounted.splice(0)) done();
});

/** Mount the reader and wait until it has read its source. */
async function read(text: string, plain = false): Promise<HTMLElement> {
  globalThis.IS_REACT_ACT_ENVIRONMENT = true;
  const host = document.createElement('div');
  document.body.append(host);
  const root = createRoot(host);
  mounted.push(() => {
    act(() => root.unmount());
    host.remove();
  });
  await act(async () => {
    root.render(
      <MarkdownArtifact source={new Blob([text], { type: 'text/markdown' })} plain={plain} chrome={chrome} />,
    );
  });
  for (let attempt = 0; attempt < 20 && !host.textContent?.includes(text.split('\n')[0].replace(/^# /, '')); attempt++) {
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 5));
    });
  }
  return host;
}

describe('an opened markdown note', () => {
  it('renders the note as prose and offers no comment controls', async () => {
    const host = await read('# Ship it\n\n- one\n- two\n');
    expect(host.querySelector('h1')?.textContent).toBe('Ship it');
    expect(host.textContent).not.toContain('# Ship it');
    expect(host.querySelector('textarea')).toBeNull();
    expect(
      [...host.querySelectorAll('button')].some((button) =>
        /comment|save|annotate/i.test(button.getAttribute('aria-label') ?? button.textContent ?? ''),
      ),
    ).toBe(false);
  });
});

describe('an opened plain-text artifact', () => {
  it('reads the file verbatim', async () => {
    const host = await read('# not a heading\nsecond line\n', true);
    expect(host.querySelector('h1')).toBeNull();
    expect(host.textContent).toContain('# not a heading');
    expect(host.textContent).toContain('second line');
  });
});
