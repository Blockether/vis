// @vitest-environment jsdom
import { act, fireEvent, render, waitFor } from '@testing-library/react';
import { renderToStaticMarkup } from 'react-dom/server';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import {
  AssistantMessage,
  AttachmentRail,
  IterationTrace,
  Markdown,
  SpeechBlock,
  ThinkingBand,
  UserMessage,
} from './ChatContent';
import { openStepsMarkup, renderOpenSteps } from './trace-harness';
import type { ActivityProjection } from '../lib/activity';
import { ArtifactLinkContext } from '../lib/artifact-links';
import type { GatewayClient } from '../lib/gateway';
import { speechOutput } from '../lib/speech';
import { setStepsSummarized } from '../lib/transcript-display';
import type { IterationAttachment, TranscriptTurn } from '../lib/types';

/**
 * Visible text of a rendered chunk: tags out, entities back. A tag ends outside its
 * quoted attributes, because DOM markup keeps the `>` of a class such as `[&>*]:mt-0`.
 */
const text = (html: string) =>
  html
    .replace(/<(?:[^>"']|"[^"]*"|'[^']*')*>/g, '')
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, '>')
    .replace(/&lt;/g, '<')
    .replace(/&amp;/g, '&');

/** One entry per PAINTED code row — the code block gives every line its own div. */
const codeRows = (html: string) =>
  (html.match(/<div class="flex whitespace-pre-wrap[^"]*">.*?<\/div>/g) ?? []).map(text);

/**
 * What a step's row says to a screen reader and not to the eye: the ring on the
 * thread carries the state visually, so the word for it lives here alone.
 */
const announcedStates = (root: HTMLElement) =>
  [...root.querySelectorAll('.sr-only')].map((node) =>
    (node.textContent ?? '').replace(/:\s*$/, ''),
  );
const count = (html: string, pattern: RegExp) => (html.match(pattern) ?? []).length;

// Regression, user screenshot: code blocks used a wide Copy label instead of an icon.
describe('code block copy controls', () => {
  it.each([
    ['bash', 'vis-agent python uv sync --project ./einmal --locked'],
    ['diff', '--- a/config.txt\n+++ b/config.txt\n@@ -1 +1 @@\n-before\n+after'],
  ])('copies a %s block with an icon and accessible feedback', async (language, source) => {
    const writeText = vi.fn().mockResolvedValue(undefined);
    vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
    const view = render(<Markdown>{`\`\`\`${language}\n${source}\n\`\`\``}</Markdown>);
    try {
      const button = view.getByRole('button', { name: 'Copy code' });
      fireEvent.click(button);
      await waitFor(() => expect(writeText).toHaveBeenCalledWith(source));
      await waitFor(() => expect(button.getAttribute('aria-label')).toBe('Copied'));
      expect(button.querySelector('svg.lucide-check')).toBeInTheDocument();
    } finally {
      view.unmount();
      vi.unstubAllGlobals();
    }
  });
});

describe('spoken transcript', () => {
  it('restarts speech at the position pressed on the wave', async () => {
    const calls: string[] = [];
    let finish: () => void = () => undefined;
    const pending = new Promise<void>((resolve) => {
      finish = resolve;
    });
    const speak = vi.spyOn(speechOutput, 'speak').mockImplementation((value) => {
      calls.push(value);
      return pending;
    });
    const stop = vi.spyOn(speechOutput, 'stop').mockImplementation(() => undefined);
    const view = render(<SpeechBlock text="one two three four five six" />);

    try {
      fireEvent.click(view.getByRole('button', { name: 'Play' }));
      expect(calls).toEqual(['one two three four five six']);

      const wave = view.getByRole('slider', { name: 'Speech position' });
      wave.getBoundingClientRect = () => ({ left: 0, width: 100 }) as unknown as DOMRect;
      fireEvent.pointerDown(wave, { clientX: 50 });

      await waitFor(() => expect(calls.at(-1)).toBe('four five six'));
      expect(stop).toHaveBeenCalled();
    } finally {
      finish();
      view.unmount();
      speak.mockRestore();
      stop.mockRestore();
    }
  });

  // Regression, user report: the live speech block unmounted during the transcript
  // handoff and its cleanup cancelled the separate voice-mode reading.
  it('does not stop voice-mode speech when a transcript block unmounts', () => {
    let voiceModeStopped = false;
    const stop = vi.spyOn(speechOutput, 'stop').mockImplementation((owner?: object) => {
      if (owner === undefined) voiceModeStopped = true;
    });
    const view = render(<SpeechBlock text="The same answer appears in the transcript." />);

    view.unmount();

    expect(stop).toHaveBeenCalledWith(expect.any(Object));
    expect(voiceModeStopped).toBe(false);
    stop.mockRestore();
  });
});

describe('Markdown thinking breaks', () => {
  // The engine's `reasoning->ast` turns a single authored newline into `[:br]`, and the
  // TUI paints it as its own row. `hardBreaks` is how the web card honours that contract.
  it('keeps every authored newline as its own line', () => {
    const html = renderToStaticMarkup(
      <Markdown compact hardBreaks>
        {'**Plan**\nfirst line\nsecond line\n\nnext para'}
      </Markdown>,
    );
    expect(count(html, /<br\s*\/?>/g)).toBe(2);
    // Still real paragraphs — a blank line is a break BETWEEN blocks, not a third `<br>`.
    expect(count(html, /<p class=/g)).toBe(2);
    expect(text(html)).toContain('Plan\nfirst line\nsecond line');
    expect(text(html)).not.toContain('Planfirst');
  });
});

// User report (screenshot): a numbered list in an answer started further left than the
// bulleted list right above it. Native markers are sized by the engine, so both lists now
// draw their own marker in a fixed column instead of leaning on `list-style`.
describe('Markdown list columns', () => {
  it('widens the column for a list that reaches two digits', () => {
    const content = Array.from({ length: 10 }, (_, index) => `${index + 1}. step`).join('\n');
    const ordered = render(<Markdown>{content}</Markdown>).container.querySelector('ol');
    expect(ordered?.style.getPropertyValue('--marker-column')).toBe('4ch');
  });

  // Each list sizes its own column: a nested list starts its own numbering and must not
  // inherit the width of the list it sits in.
  it('sizes a nested list on its own numbers', () => {
    const view = render(<Markdown>{'10. outer\n\n    1. inner\n    2. inner'}</Markdown>);
    const outer = view.container.querySelector('ol');
    expect(outer?.style.getPropertyValue('--marker-column')).toBe('4ch');
    expect(outer?.querySelector('ol')?.style.getPropertyValue('--marker-column')).toBe('3ch');
  });

  it('counts from the number the list starts on', () => {
    const view = render(<Markdown>{'8. eight\n9. nine\n10. ten'}</Markdown>);
    const ordered = view.container.querySelector('ol');
    expect(ordered).toHaveAttribute('start', '8');
    expect(ordered?.style.getPropertyValue('--marker-column')).toBe('4ch');
  });

  // User report (screenshot): numbered steps split by fenced code blocks restarted at 1 each
  // time, because every fence closes one list and the next step opens another.
  it('keeps counting through steps split by a code fence', () => {
    const content = '1. First.\n\n```\none\n```\n\n2. Second.\n\n```\ntwo\n```\n\n3. Third.';
    const view = render(<Markdown>{content}</Markdown>);
    const starts = Array.from(view.container.querySelectorAll('ol'), (list) =>
      list.getAttribute('start'),
    );
    expect(starts).toEqual([null, '2', '3']);
  });
});

// User report: a Markdown attachment preview looked like a link but tapping it
// invoked the unsupported `attachment:` browser scheme and opened nothing.
describe('Markdown attachment links', () => {
  it('hands a safe attachment link to the artifact opener', () => {
    const opened = vi.fn();
    const view = render(
      <AssistantMessage
        turn={{
          turn_id: 't1',
          status: 'completed',
          content: [
            {
              id: 'answer',
              type: 'prose',
              markdown:
                '[Open preview](attachment://8e3a587d-232c-497d-a290-7d16cfcf0e02) and [open docs](https://example.com/docs).',
            },
          ],
        }}
        onOpenAttachment={opened}
      />,
    );

    // A BUTTON: as an anchor, the desktop window's click capture took the click first.
    const preview = view.getByRole('button', { name: 'Open preview' });
    expect(view.queryByRole('link', { name: 'Open preview' })).toBeNull();
    fireEvent.click(preview);
    expect(opened).toHaveBeenCalledOnce();
    expect(opened).toHaveBeenCalledWith('8e3a587d-232c-497d-a290-7d16cfcf0e02');

    const external = view.getByRole('link', { name: 'open docs' });
    expect(external.getAttribute('target')).toBe('_blank');
  });

  it('does not turn an unsafe attachment target into an internal action', () => {
    const opened = vi.fn();
    const view = render(
      <Markdown onOpenAttachment={opened}>
        {'[Not an artifact](attachment://../../settings)'}
      </Markdown>,
    );

    expect(view.queryByRole('link', { name: 'Not an artifact' })).toBeNull();
    expect(opened).not.toHaveBeenCalled();
  });

  // User report (screenshot): an answer showed `![…](/tmp/zip-results-2.png)` as a broken
  // image. That path is on the gateway host, never a URL this app can load.
  it('opens an image the session attached instead of loading its host path', () => {
    const open = vi.fn();
    const id = '8e3a587d-232c-497d-a290-7d16cfcf0e02';
    const view = render(
      <ArtifactLinkContext.Provider value={{ byName: new Map([['zip-results-2.png', id]]), open }}>
        <Markdown>{'![ZIP archive in Companion](/tmp/zip-results-2.png)'}</Markdown>
      </ArtifactLinkContext.Provider>,
    );

    expect(view.container.querySelector('img')).toBeNull();
    fireEvent.click(view.getByRole('button', { name: 'ZIP archive in Companion' }));
    expect(open).toHaveBeenCalledExactlyOnceWith(id);
  });

  // User report (screenshots): a picture fence stood against the prose, and the app
  // painted its host path and metadata as a code block.
  it('shows a picture fence as its own block, apart from the prose', () => {
    const open = vi.fn();
    const id = '0b1c2d3e-4f50-4617-8899-aabbccddeeff';
    const fence = [
      '````vis-image',
      '[Image: tui-card.png 1100×400, 27.9 KB]',
      '/Users/me/.vis/cache/display/att-5c8e9329e774d421.png',
      'image/png',
      '1100x400',
      '27.9 KB',
      '````',
    ].join('\n');
    const view = render(
      <ArtifactLinkContext.Provider value={{ byName: new Map([['tui-card.png', id]]), open }}>
        <Markdown>{`The card:\n${fence}\n\nAfter the card.`}</Markdown>
      </ArtifactLinkContext.Provider>,
    );

    const card = view.getByRole('group', { name: 'tui-card.png' });
    expect(card.previousElementSibling?.tagName).toBe('P');
    expect(card.nextElementSibling?.tagName).toBe('P');
    expect(view.container).not.toHaveTextContent('/Users/me');
    fireEvent.click(view.getByRole('button', { name: 'tui-card.png' }));
    expect(open).toHaveBeenCalledExactlyOnceWith(id);
  });

  it('names an unknown local image instead of painting a broken frame', () => {
    const view = render(
      <Markdown>{'![Chart](/tmp/missing.png) ![](./plots/loss%20curve.png) ![Logo](https://example.com/logo.png)'}</Markdown>,
    );

    expect(Array.from(view.container.querySelectorAll('img')).map((image) => image.getAttribute('src'))).toEqual([
      'https://example.com/logo.png',
    ]);
    expect(view.container.textContent).toContain('Chart');
    expect(view.container.textContent).toContain('loss curve.png');
  });

  // Regression: hotlink-protected hosts such as Fandom answer 404 when the app origin is the Referer.
  it('loads a web image without a Referer', () => {
    const view = render(<Markdown>{'![Sparkion](https://static.wikia.nocookie.net/tibia/images/4/48/Sparkion.gif)'}</Markdown>);

    expect(view.container.querySelector('img')?.getAttribute('referrerpolicy')).toBe('no-referrer');
  });

});

describe('Markdown tool card body', () => {

  it('keeps blank lines and indentation inside a COMMAND block', () => {
    const html = renderToStaticMarkup(
      <Markdown compact>
        {'**COMMAND**\n\n```bash\nset -e\n\nif [ -f x ]; then\n  npm test\nfi\n```\n'}
      </Markdown>,
    );
    expect(codeRows(html)).toEqual(['set -e', ' ', 'if [ -f x ]; then', '  npm test', 'fi']);
  });

  it('splits a quoted commit MESSAGE into subject and body', () => {
    const html = renderToStaticMarkup(
      <Markdown compact>{'**MESSAGE**\n\n> feat: thing\n>\n> body line\n'}</Markdown>,
    );
    const quote = html.slice(html.indexOf('<blockquote'), html.indexOf('</blockquote>'));
    expect(quote).not.toBe('');
    expect(count(quote, /<p class=/g)).toBe(2);
    expect(text(quote).replace(/\n+/g, '\n').trim()).toBe('feat: thing\nbody line');
  });
});

// Regression, companion transcript report: PATCH diffs used a two-column
// desktop layout, leaving each side unreadably narrow in the web and native apps.
describe('compact diff blocks', () => {
  it('renders one unified column with explicit removed and added lines', () => {
    const html = renderToStaticMarkup(
      <Markdown compact>
        {'```diff\n--- a/file.ts\n+++ b/file.ts\n@@ -1,2 +1,2 @@\n keep\n-old\n+new\n```'}
      </Markdown>,
    );

    expect(html).toContain('aria-label="Unified diff"');
    expect(html).toContain('aria-label="Removed line 2"');
    expect(html).toContain('aria-label="Added line 2"');
  });
});

describe('turn header identity', () => {
  const createdAt = new Date(2026, 8, 16, 14, 35, 27).getTime();

  it.each([
    [{ position: 42 }, 'T42 / Fork from this turn'],
    [{ created_at: createdAt }, '16/09/2026, 14:35:27 / Fork from this turn'],
    [{ position: 0, created_at: NaN }, 'VisFork from this turn'],
  ])('omits separators for absent or invalid fields: %j', (metadata, expected) => {
    const view = render(
      <AssistantMessage turn={{ turn_id: 'persisted-id', ...metadata }} onFork={vi.fn()} />,
    );
    expect(view.container.querySelector('article')!.firstElementChild).toHaveTextContent(expected);
  });
});

describe('request speaker labels', () => {

  it('collapses after four rendered lines and expands without losing the original content', () => {
    const height = vi.spyOn(HTMLElement.prototype, 'scrollHeight', 'get').mockReturnValue(200);
    const request = 'Actual request\nLine two\nLine three\nLine four\nLast line';
    try {
      const view = render(<UserMessage requestKind="council">{request}</UserMessage>);
      const body = view.container.querySelector('.line-clamp-4')!;
      const button = view.getByRole('button', { name: 'Show full message' });
      expect([...body.querySelectorAll('p')].map((line) => line.textContent)).toEqual(
        request.split('\n'),
      );
      expect(button).toHaveAttribute('aria-expanded', 'false');
      expect(button).toHaveAttribute('aria-controls', body.id);
      fireEvent.click(button);
      expect(button).toHaveAttribute('aria-expanded', 'true');
      fireEvent.click(button);
    } finally {
      height.mockRestore();
    }
  });

  // User report 2026-10-31, Council thread #8805: the request showed raw backticks and
  // ran a plain line after a list into the last list item.
  it('renders Council Markdown and keeps each authored line as its own block', () => {
    const view = render(
      <UserMessage requestKind="council">
        {'Context: run `npm test`.\nGoals:\n1. First goal\n2. Second goal\nDone: tests pass.'}
      </UserMessage>,
    );
    const article = view.container.querySelector('article')!;
    expect(article.querySelector('code')).toHaveTextContent('npm test');
    expect([...article.querySelectorAll('li')].map((item) => item.textContent)).toEqual([
      'First goal',
      'Second goal',
    ]);
    expect([...article.querySelectorAll(':scope p')].map((line) => line.textContent)).toContain(
      'Done: tests pass.',
    );
  });
});

// Regression, iOS scroll jump: a pasted picture used to be laid out at whatever
// its own decoded pixels measured (`max-h-[min(28rem,60dvh)] w-auto`), so the
// bubble reserved NOTHING for it until the decode landed — which, with
// `loading="lazy"` on iOS, happens as the bubble nears the viewport, i.e. while
// the reader is scrolling. Everything below it then jumped down by the height
// of the picture, and this scroller (`overflow-anchor:none`, no WebKit
// anchoring, corrector standing down mid-gesture) never put it back.
describe('user bubble pictures', () => {
  const html = () =>
    renderToStaticMarkup(
      <UserMessage
        attachments={[
          {
            filename: 'shot.png',
            media_type: 'image/png',
            base64: 'iVBORw0KGgo=',
            size: 8,
          },
        ]}
      >
        {'look at this'}
      </UserMessage>,
    );

  // Regression, user report with a screenshot: a wide picture the human sent stood
  // under a band of empty mat in its 4:3 plate. Its bytes are inline, so the plate
  // takes the picture's own ratio from the header before anything decodes.
  it("gives a wide picture's plate the picture's own ratio", () => {
    const wide = renderToStaticMarkup(
      <UserMessage
        attachments={[
          {
            filename: 'wide.png',
            media_type: 'image/png',
            // A PNG header: the signature and an IHDR chunk for 851×332.
            base64: 'iVBORw0KGgoAAAANSUhEUgAAA1MAAAFMCAYAAAAup+b7',
            size: 33,
          },
        ]}
      >
        {'look at this'}
      </UserMessage>,
    );
    expect(wide).toContain('aspect-ratio:851 / 332');
    expect(html()).not.toContain('aspect-ratio');
  });
});

// A voice memo the human sent used to reach this bubble and paint NOTHING: the
// rail admitted `image/*` and `video/*` only, so the recording the gateway had
// stored was invisible in the very message that carried it.
describe('user bubble recordings', () => {
  it('keeps the same player when gateway metadata catches up', () => {
    const bytes = 'AAAAIGZ0eXBNNEEg';
    const { container, rerender } = render(
      <UserMessage attachments={[{ filename: 'memo.m4a', media_type: 'audio/mp4', base64: bytes }]}>
        listen
      </UserMessage>,
    );
    const player = container.querySelector('audio');
    expect(player).toBeInTheDocument();
    if (player) player.currentTime = 7;

    rerender(
      <UserMessage
        attachments={[
          {
            id: 'stored-audio',
            filename: 'memo.m4a',
            media_type: 'audio/mp4',
            base64: bytes,
            transcription: 'ready words',
          },
        ]}
      >
        listen
      </UserMessage>,
    );

    expect(container.querySelector('audio')).toBe(player);
    expect(container.querySelector('audio')?.currentTime).toBe(7);
  });
});

// Regression, TestFlight crash feedback: build 2875 rendered every collapsed tool result body,
// so a large transcript left WebKit with hundreds of thousands of DOM nodes until iOS killed it
// at the 2 GiB per-process limit.
describe('collapsed tool results', () => {
  it('does not mount result bodies before a card is opened', () => {
    const bodySentinel = 'UNMOUNTED_TOOL_RESULT_BODY';
    const turn: TranscriptTurn = {
      turn_id: 'large-trace',
      status: 'completed',
      iterations: [
        {
          id: 'iteration-1',
          forms: Array.from({ length: 400 }, (_, index) => ({
            op: 'shell',
            stdout: `${bodySentinel} ${index}\n`,
          })),
        },
      ],
    };

    const html = openStepsMarkup(<AssistantMessage turn={turn} />);

    expect(count(html, /<details/g)).toBe(400);
    expect(count(html, /RESULT/g)).toBe(400);
    expect(html).not.toContain(bodySentinel);
  });
});

// Regression, reported white artefacts while scrolling a long session upward:
// measured in headless WebKit on this very transcript (77 000 px, 401 code
// blocks), a fling up left as much as 376 px of the 900 px viewport painting as
// bare paper, card frames already there and their bodies still empty. That was
// `contain:layout style` on every turn — a paint island WebKit rasterizes only
// when it enters the viewport, which a fast scroll always outruns.
//
// A finished turn IS skipped again (`useMeasuredPaintSkip`), and this is the
// difference that made it safe: the skip is armed by a MEASUREMENT and never
// declared in the markup, so no turn can be skipped at a height nobody measured
// — the guessed size is what used to correct itself above the reader.
// `ChatContent.paintSkip.test.tsx` pins the armed half.
describe('a turn declares no size it has not measured', () => {

  it('marks each trace step and the answer as parts the jump chip counts', () => {
    const shell = { op: 'shell', stdout: 'ok\n' };
    const view = render(
      <AssistantMessage
        turn={{
          turn_id: 'parts',
          status: 'completed',
          content: [{ id: 'answer', type: 'prose', markdown: 'Done.' }],
          iterations: [
            { id: 'iteration-1', assistant_prose: 'Look first.', forms: [shell] },
            { id: 'iteration-2', forms: [shell] },
            { id: 'iteration-3', assistant_prose: 'Then check.', forms: [shell] },
          ],
        }}
        whole
      />,
    );

    const parts = view.container.querySelectorAll('[data-transcript-message] [data-transcript-part]');
    expect(Array.from(parts, (part) => part.textContent)).toEqual([
      expect.stringContaining('Look first.'),
      expect.stringContaining('Then check.'),
      'Done.',
    ]);
  });
});

describe('a card gives stdout one stable band and no op badge', () => {
  const card = (form: Record<string, unknown>) =>
    text(
      openStepsMarkup(
        <AssistantMessage
          turn={{
            turn_id: 'titles',
            status: 'completed',
            iterations: [{ id: 'iteration-1', forms: [form] }],
          }}
        />,
      ),
    );

  it('keeps a non-Python failure message collapsed without its structured payload', () => {
    const view = render(
      <AssistantMessage
        turn={{
          turn_id: 'failed-tool',
          status: 'completed',
          iterations: [
            {
              id: 'iteration-1',
              forms: [
                {
                  op: 'shell',
                  error: {
                    message: 'Permission denied',
                    trace: 'internal detail',
                    data: { source: 'large input' },
                  },
                },
              ],
            },
          ],
        }}
      />,
    );
    expect(view.queryByRole('button', { name: 'Expand error details' })).toBeNull();
    fireEvent.click(view.getByRole('button', { name: 'Expand steps: 1 failed' }));
    const toggle = view.getByRole('button', { name: 'Expand error details' });
    expect(view.queryByText('Permission denied')).toBeNull();
    fireEvent.click(toggle);
    expect(view.getByText('Permission denied')).toBeVisible();
    expect(view.container.textContent).not.toContain('internal detail');
    expect(view.container.textContent).not.toContain('large input');
    fireEvent.click(view.getByRole('button', { name: 'Collapse error details' }));
    expect(view.queryByText('Permission denied')).toBeNull();
  });

  it('never paints a private transport op a handle method answered', () => {
    const rendered = card({ op: '_shell_wait', stdout: 'exit 0\n' });
    expect(rendered).toContain('RESULT');
    expect(rendered).not.toContain('_SHELL_WAIT');
  });
});

// A slash envelope is hidden behind its answer; a bang turn has no duplicate answer,
// so its persisted `user-shell` form is the one visible owner of command output.
describe('command turns expose one canonical result', () => {
  const trace = (form: Record<string, unknown>, answer = 'Reloaded — configuration') =>
    renderToStaticMarkup(
      <AssistantMessage
        turn={{
          turn_id: 'command',
          status: 'completed',
          request: '/reload',
          iterations: [{ id: 'iteration-1', forms: [form] }],
          content: answer ? [{ id: 'block-1', type: 'prose', markdown: answer }] : [],
        }}
      />,
    );

  it('hides the slash envelope a slash turn persists', () => {
    const html = trace({
      scope: 't1/i1/f1',
      tag: 'user-slash',
      src: '/reload',
    });

    expect(html).not.toContain('PYTHON');
    expect(text(html)).not.toContain('/reload');
    expect(text(html)).not.toContain('slash/status');
    expect(text(html)).toContain('Reloaded — configuration');
  });

  it('shows a bang result from its one stdout fact', () => {
    const painted = renderOpenSteps(
      <AssistantMessage
        turn={{
          turn_id: 'bang',
          status: 'completed',
          request: '!ls',
          iterations: [
            {
              id: 'iteration-1',
              forms: [
                {
                  scope: 't1/i1/f1',
                  tag: 'user-shell',
                  op: 'shell',
                  src: '!ls',
                  stdout: 'README.md\nexit 0',
                },
              ],
            },
          ],
          content: [],
        }}
      />,
    );

    expect(painted.container.textContent).not.toContain('PYTHON');
    fireEvent.click(painted.getByRole('button', { name: 'Expand code' }));
    expect(painted.container.textContent).toContain('!ls');
    // The RESULT under the code starts folded, its tally saying how much it holds.
    expect(painted.container.textContent).toContain('RESULT +2 more');
    expect(painted.container.textContent).not.toContain('exit 0');
    fireEvent.click(painted.getByRole('button', { name: 'Expand result' }));
    expect(painted.container.textContent).toContain('exit 0');
    expect(painted.container.textContent).toContain('README.md');
    expect(painted.container.textContent).not.toContain('Reloaded — configuration');
  });
});

// Regression, issue td-546817: Python evaluations without detected Activity
// retained the old always-expanded frame instead of the canonical execution receipt.
describe('a Python evaluation without detected Activity', () => {
  const turnWith = (form: Record<string, unknown>, status = 'completed') =>
    ({
      id: 'python-only',
      status,
      iterations: [{ id: 'iteration-1', position: 41, forms: [form] }],
      content: [],
    }) as unknown as TranscriptTurn;

  // Regression, user screenshot: an open digest showed Failed without the CODE that failed.
  it('keeps a failed Python source under CODE and its message under Failed, both collapsed', () => {
    const painted = renderOpenSteps(
      <AssistantMessage
        turn={turnWith({
          source: 'raise Error()',
          error: 'failed',
          duration_ms: 29,
        })}
      />,
    );

    // Regression, T153: the row shouted FAILED beside a ring that already drew the cross.
    expect(painted.container.textContent).not.toContain('FAILED · PYTHON');
    expect(announcedStates(painted.container)).toContain('Failed');
    expect(painted.queryByRole('button', { name: 'Expand execution trace' })).toBeNull();
    const code = painted.getByRole('button', { name: 'Expand code' });
    const toggle = painted.getByRole('button', { name: 'Expand error details' });
    expect(code.compareDocumentPosition(toggle) & Node.DOCUMENT_POSITION_FOLLOWING).toBe(
      Node.DOCUMENT_POSITION_FOLLOWING,
    );
    // The CODE head carries the time, so the Failed row does not paint it twice.
    expect(code.closest('[data-execution-code]')).toHaveTextContent('29ms');
    expect(toggle.closest('[data-code-result]')).not.toHaveTextContent('29ms');
    expect(painted.queryByText('failed')).toBeNull();
    expect(painted.container.textContent).not.toContain('raise Error()');
    fireEvent.click(code);
    expect(painted.container.textContent).toContain('raise Error()');
    expect(painted.queryByText('failed')).toBeNull();
    fireEvent.click(toggle);
    expect(painted.getByText('failed')).toBeVisible();
    expect(painted.container.querySelector('[data-code-result] .text-err')).toBeInTheDocument();
    fireEvent.click(painted.getByRole('button', { name: 'Collapse error details' }));
    expect(painted.queryByText('failed')).toBeNull();
  });

  // Regression, user screenshot: a stopped execution showed a second Interrupted band
  // instead of a quiet state beside CODE in the same header.
  it('places an interrupted Python execution beside CODE without a result band', () => {
    const painted = renderOpenSteps(
      <AssistantMessage
        turn={turnWith({
          source: 'walk_the_tree()',
          error: {
            message: 'java.lang.InterruptedException',
            trace: 'java.lang.InterruptedException: FutureTask/awaitDone',
          },
          duration_ms: 29,
        })}
      />,
    );

    expect(painted.container.textContent).toContain('29ms');
    expect(announcedStates(painted.container)).toContain('Interrupted');
    const code = painted.getByRole('button', { name: 'Expand code' });
    expect(painted.container.querySelector('[data-code-result]')).toBeNull();
    expect(painted.container.textContent).not.toContain('FutureTask/awaitDone');
    fireEvent.click(code);
    expect(painted.getByRole('button', { name: 'Collapse code' })).toHaveTextContent(
      'CODE (interrupted)',
    );
    expect(painted.getByText('walk_the_tree()')).toBeVisible();
    expect(painted.container.querySelector('[data-code-result]')).toBeNull();
  });

  // Regression, user report: a stop is a failure too. The row of a stopped step counts its
  // call and turns red, but the Interrupted card stays inside the steps.
  it.each(['failed', 'cancelled'] as const)(
    'counts a step stopped in a %s call as a failure, inside its closed digest',
    (state) => {
      const painted = render(
        <AssistantMessage
          turn={turnWith({
            source: 'run_suite()',
            error: { type: 'vis/interrupted', message: 'Python execution was interrupted' },
            duration_ms: 1237,
            activity: {
              state,
              counts: {
                running: 0,
                succeeded: 0,
                failed: state === 'failed' ? 1 : 0,
                cancelled: state === 'cancelled' ? 1 : 0,
              },
              rows: [
                {
                  id: 'shell-1',
                  sequence: 1,
                  operation: 'shell',
                  state,
                  summary: 'python3 suite.py',
                  resources: [],
                  evidence: [],
                },
              ],
            },
          })}
        />,
      );
      const digest = painted.container.querySelector<HTMLElement>('[data-step-digest]')!;
      expect(digest).toHaveAttribute('aria-label', `Expand steps: 1 ${state}`);
      expect(digest.querySelector('.text-err-ink')).toHaveTextContent(`1 ${state}`);
      expect(digest.parentElement).toHaveTextContent('1.2s');
      expect(painted.container.textContent).not.toContain('Interrupted');
      fireEvent.click(digest);
      expect(announcedStates(painted.container)).toContain('Interrupted');
    },
  );

  it('enhances the same receipt when semantic activity arrives', () => {
    const runningTurn = turnWith({ source: 'answer = search()' }, 'running');
    const painted = renderOpenSteps(<AssistantMessage turn={runningTurn} streaming />);
    const receipt = painted.container.querySelector('[aria-label="Execution trace"]')!;
    expect(receipt.getAttribute('role')).toBe('status');

    // Protocol 7: the snapshot arrives ON the form, from `block.activity`.
    painted.rerender(
      <AssistantMessage
        turn={turnWith(
          {
            source: 'answer = search()',
            activity: {
              state: 'running',
              counts: { running: 1, succeeded: 0, failed: 0, cancelled: 0 },
              rows: [
                {
                  id: 'grep-1',
                  sequence: 1,
                  operation: 'grep',
                  state: 'running',
                  summary: 'searching',
                  resources: [],
                  evidence: [],
                },
              ],
            },
          },
          'running',
        )}
        streaming
      />,
    );

    expect(painted.container.querySelector('[aria-label="Execution trace"]')).toBe(receipt);
    expect(receipt.textContent).toContain('searching');
    expect(receipt.querySelectorAll('[data-activity-row]')).toHaveLength(1);
  });

  it('does not invent Activity when an empty projection settles', () => {
    const painted = renderOpenSteps(
      <AssistantMessage
        turn={turnWith({
          source: 'print(42)',
          stdout: '42\n',
          duration_ms: 29,
          activity: {
            state: 'succeeded',
            counts: { running: 0, succeeded: 0, failed: 0, cancelled: 0 },
            rows: [],
          },
        })}
      />,
    );

    expect(painted.container.textContent).toContain('29ms');
    expect(painted.container.textContent).not.toContain('0 activities');
    expect(painted.container.textContent).not.toContain('ACTIVITY');
  });
});

// Regression, issue td-cc41a1: Activity was appended after the entire assistant
// row, so a multi-form iteration could not show which complete Python form owned it.
// Protocol 7 answers that structurally: the snapshot IS a field of the form, so
// there is no anchor to resolve and no way for it to land under the wrong one.
describe('Activity follows the combined Python source', () => {
  const rows = [
    {
      id: 'call-1',
      sequence: 1,
      operation: 'grep',
      presenter: 'observation',
      signal: 'observation',
      state: 'succeeded',
      summary: '18 matches',
      resources: [],
      evidence: [],
    },
    {
      id: 'call-2',
      sequence: 2,
      operation: 'suite',
      presenter: 'tests',
      signal: 'verification',
      state: 'running',
      summary: 'companion suite',
      resources: [],
      evidence: [],
    },
  ];
  const runningActivity = {
    state: 'running',
    counts: { running: 1, succeeded: 1, failed: 0, cancelled: 0 },
    rows,
  };
  const turnOf = (forms: Record<string, unknown>[]): TranscriptTurn =>
    ({
      turn_id: 'activity-turn',
      status: 'completed',
      iterations: [{ id: 'iteration-1', position: 41, forms }],
    }) as unknown as TranscriptTurn;

  it('groups adjacent calls without dropping either result', () => {
    const painted = renderOpenSteps(
      <AssistantMessage
        turn={turnOf([
          { source: 'first_form()', stdout: 'first result\n' },
          { source: 'second_form()', stdout: 'second result\n' },
        ])}
      />,
    );
    expect(painted.container.querySelectorAll('[data-execution-code]')).toHaveLength(1);
    expect(painted.container.textContent).toContain('CODE');
    expect(painted.getByRole('button', { name: 'Expand code' })).not.toHaveTextContent(
      '(interrupted)',
    );
    fireEvent.click(painted.getByRole('button', { name: 'Expand code' }));
    expect(painted.container.textContent).toContain('second_form()');
    for (const details of painted.container.querySelectorAll('details')) {
      details.open = true;
      fireEvent(details, new Event('toggle', { bubbles: true }));
    }
    for (const toggle of painted.getAllByRole('button', {
      name: 'Expand result',
    })) {
      fireEvent.click(toggle);
    }
    expect(painted.container.textContent).toContain('first result');
    expect(painted.container.textContent).toContain('second result');
    expect(painted.getAllByRole('button', { name: 'Copy code' })).toHaveLength(1);
    // Nothing is fetched for a receipt any more, so there is no loading state.
    expect(painted.container.textContent).not.toContain('Loading Activity');
  });

  it('keeps grouped failures and output while placing an interruption beside CODE', () => {
    const painted = renderOpenSteps(
      <AssistantMessage
        turn={turnOf([
          { source: 'first_form()', error: 'java.lang.InterruptedException', duration_ms: 29 },
          { source: 'second_form()', stdout: 'second result\n' },
          { source: 'third_form()', error: { message: 'actual failure' }, duration_ms: 12 },
        ])}
      />,
    );

    const code = painted.getByRole('button', { name: 'Expand code' });
    expect(painted.container.querySelectorAll('[data-code-result]')).toHaveLength(1);
    const failure = painted.getByRole('button', { name: 'Expand error details' });
    fireEvent.click(failure);
    expect(painted.getByText('actual failure')).toBeVisible();
    fireEvent.click(code);
    expect(painted.getByRole('button', { name: 'Collapse code' })).toHaveTextContent(
      'CODE (interrupted)',
    );
    expect(painted.getByText('first_form()')).toBeVisible();
    fireEvent.click(painted.getByRole('button', { name: 'Expand result' }));
    expect(painted.getByText('second result')).toBeVisible();
  });

  // Regression, issue td-65cdf6: a 1-based iteration anchor was compared with the
  // iteration's array index, so the running panel vanished — and the same anchor
  // could place a second copy. One field on one form cannot do either.
  it('paints exactly one Activity for the form that owns it', () => {
    const painted = renderOpenSteps(
      <AssistantMessage
        turn={turnOf([
          { source: 'first_form()', stdout: 'first result\n' },
          { source: 'second_form()', activity: runningActivity },
        ])}
      />,
    );
    expect(painted.container.querySelectorAll('[data-execution-code]')).toHaveLength(1);
    // Grouping neither invents nor duplicates the second form's operations.
    expect(painted.container.querySelectorAll('[data-activity-row]')).toHaveLength(2);
    expect(painted.container.querySelectorAll('[data-activity-axis]')).toHaveLength(1);
    expect(painted.container.textContent).not.toContain('Loading Activity');
  });

  // Regression #181: hiding Python details still hides diagnostics; expanding
  // a visible failure shows only its message, not source or structured internals.
  it.each([true, false])('hides or folds grouped failure messages with showCode=%s', (showCode) => {
    const message = 'ToolError: ' + 'Diagnostic details for the agent. '.repeat(40);
    const trace = 'Traceback: diagnostic evidence';
    const turn = turnOf([
      {
        source: 'print(private_result)',
        duration_ms: 29,
        error: { message, trace },
      },
      {
        source: 'next_call()',
        duration_ms: 0,
        error: { message: 'SECOND_FAILURE' },
      },
    ]);
    const original = JSON.stringify(turn);
    const painted = renderOpenSteps(
      <IterationTrace iterations={turn.iterations ?? []} showCode={showCode} whole />,
    );
    // A visible failure keeps its source under a collapsed CODE band (#181).
    expect(painted.container.querySelector('[data-execution-code]') !== null).toBe(showCode);
    expect(painted.container.textContent).not.toContain(trace);
    expect(painted.container.textContent).not.toContain('print(private_result)');
    if (!showCode) {
      expect(painted.container.querySelector('[data-code-result]')).toBeNull();
      expect(painted.container.textContent).not.toContain(message);
      expect(painted.container.textContent).not.toContain('SECOND_FAILURE');
    } else {
      const toggle = painted.getByRole('button', { name: 'Expand all error details' });
      expect(toggle).toHaveTextContent('Failed ×2');
      expect(painted.container.textContent).toContain('29ms');
      expect(painted.container.textContent).not.toContain(message);
      expect(painted.container.textContent).not.toContain('SECOND_FAILURE');
      fireEvent.click(toggle);
      expect(painted.container.textContent).toContain(message);
      expect(painted.container.textContent).toContain('SECOND_FAILURE');
      fireEvent.click(painted.getByRole('button', { name: 'Collapse all error details' }));
      expect(painted.container.textContent).not.toContain(message);
      expect(painted.container.textContent).not.toContain('SECOND_FAILURE');
    }
    expect(JSON.stringify(turn)).toBe(original);
  });

  it.each([true, false])('shows only the failed tool message with showCode=%s', (showCode) => {
    const source = 'print(' + 'large_source'.repeat(200) + ')';
    const painted = renderOpenSteps(
      <IterationTrace
        iterations={turnOf([
          {
            source,
            error: {
              message: 'patch refused — nothing was written',
              data: { source, phase: 'python_host' },
              trace: 'internal trace details',
            },
          },
        ]).iterations ?? []}
        showCode={showCode}
        whole
      />,
    );
    if (showCode) {
      expect(painted.queryByText('patch refused — nothing was written')).toBeNull();
      fireEvent.click(painted.getByRole('button', { name: 'Expand error details' }));
      expect(painted.getByText('patch refused — nothing was written')).toBeVisible();
    } else {
      expect(painted.queryByRole('button', { name: 'Expand error details' })).toBeNull();
      expect(painted.queryByText('patch refused — nothing was written')).toBeNull();
    }
    expect(painted.container.textContent).not.toContain(source);
    expect(painted.container.textContent).not.toContain('internal trace details');
    // The source stays under a collapsed CODE band, never in the failure message.
    expect(painted.queryByRole('button', { name: 'Expand code' }) !== null).toBe(showCode);
  });

  // Adjacent failures share one collapsed label and reveal their messages together.
  it('counts adjacent failures under a single head', () => {
    const painted = render(
      <AssistantMessage
        turn={turnOf([
          { source: 'first()', error: { message: 'FIRST_FAILURE' } },
          { source: 'second()', error: { message: 'SECOND_FAILURE' } },
          { source: 'third()', error: { message: 'THIRD_FAILURE' } },
        ])}
      />,
    );
    expect(painted.queryByRole('button', { name: 'Expand all error details' })).toBeNull();
    fireEvent.click(painted.getByRole('button', { name: 'Expand steps: 3 failed' }));
    const toggle = painted.getByRole('button', { name: 'Expand all error details' });
    expect(toggle).toHaveTextContent('Failed ×3');
    expect(painted.queryByText(/FIRST_FAILURE/)).toBeNull();
    fireEvent.click(toggle);
    expect(painted.getByText(/FIRST_FAILURE/)).toBeVisible();
    expect(painted.getByText(/SECOND_FAILURE/)).toBeVisible();
    expect(painted.getByText(/THIRD_FAILURE/)).toBeVisible();
    fireEvent.click(painted.getByRole('button', { name: 'Collapse all error details' }));
    expect(painted.queryByText(/FIRST_FAILURE/)).toBeNull();
  });

  it('keeps mixed outcomes independently collapsible', () => {
    const painted = renderOpenSteps(
      <AssistantMessage
        turn={turnOf([
          { source: 'fail()', error: { message: 'Operation failed' } },
          { source: 'print_summary()', stdout: 'Successful output' },
        ])}
      />,
    );
    expect(painted.queryByText('Operation failed')).toBeNull();
    expect(painted.container.textContent).not.toContain('fail()');
    expect(painted.queryByRole('button', { name: 'Expand result' })).toBeNull();
    fireEvent.click(painted.getByRole('button', { name: 'Expand code' }));
    expect(painted.container.textContent).toContain('print_summary()');
    fireEvent.click(painted.getByRole('button', { name: 'Expand result' }));
    expect(painted.getByText('Successful output')).toBeVisible();
    expect(painted.queryByText('Operation failed')).toBeNull();
    fireEvent.click(painted.getByRole('button', { name: 'Expand error details' }));
    expect(painted.getByText('Operation failed')).toBeVisible();
    expect(painted.getByText('Successful output')).toBeVisible();
  });

  // Regression, issue td-f9035e: Python, Result, and Activity each painted an
  // independent receipt, while the live Activity headline claimed an unknowable total.
  it.each([320, 390, 768, 1440])(
    'keeps Activity visible while source independently expands at %ipx',
    (width) => {
      const painted = renderOpenSteps(
        <div style={{ width }}>
          <AssistantMessage
            turn={turnOf([
              {
                source: 'line_1()\nline_2()\nline_3()\nline_4()\nline_5()\nline_6()',
                stdout: 'result body\n',
                activity: runningActivity,
              },
            ])}
          />
        </div>,
      );
      expect(painted.container.firstElementChild).toHaveStyle({
        width: `${width}px`,
      });

      expect(painted.queryByRole('button', { name: 'Expand execution trace' })).toBeNull();
      expect(painted.container.textContent).toContain('CODE');
      expect(painted.container.textContent).not.toContain('line_2()');
      expect(painted.container.textContent).not.toContain('RESULT');
      expect(painted.container.textContent).toContain('18 matches');
      fireEvent.click(painted.getByRole('button', { name: 'Expand code' }));
      expect(painted.container.textContent).not.toContain('result body');
      for (const band of painted.container.querySelectorAll(
        '[data-execution-code], [data-code-result], [data-execution-activity]',
      )) {
        expect(band.className).not.toMatch(/\bborder-l(?:-|\b)/);
      }
      fireEvent.click(painted.getByRole('button', { name: 'Expand result' }));
      expect(painted.container.textContent).toContain('result body');
      expect(painted.container.textContent).toContain('line_6()');
      expect(painted.getAllByRole('button', { name: 'Copy code' })).toHaveLength(1);
      expect(painted.container.textContent).toContain('RESULT');
      expect(painted.container.textContent).toContain('18 matches');
      // Source, result and chronology retain their order after either fold changes.
      const opened = painted.container.innerHTML;
      expect(opened.indexOf('data-execution-code')).toBeLessThan(
        opened.indexOf('data-activity-chronology'),
      );
      fireEvent.click(painted.getByRole('button', { name: 'Collapse code' }));
      expect(painted.container.textContent).not.toContain('RESULT');
      expect(painted.container.textContent).not.toContain('result body');
      expect(painted.container.textContent).not.toContain('line_6()');
      expect(painted.container.textContent).toContain('18 matches');
      fireEvent.click(painted.getByRole('button', { name: 'Expand code' }));
      expect(painted.getByRole('button', { name: 'Expand result' })).toBeVisible();
    },
  );

  // Regression, issue td-5b6b08: the filed Companion receipt omitted its primary
  // operation and elapsed time. The elapsed time is the FORM's duration now — the
  // very number the terminal frame measured.
  it('keeps settled Activity and elapsed time at the transcript boundary', () => {
    const rendered = text(
      openStepsMarkup(
        <AssistantMessage
          turn={turnOf([
            {
              source: 'work()',
              stdout: 'done\n',
              duration_ms: 12_600,
              activity: {
                ...runningActivity,
                state: 'succeeded',
                counts: { running: 0, succeeded: 2, failed: 0, cancelled: 0 },
                rows: rows.map((row) => ({ ...row, state: 'succeeded' })),
              },
            },
          ])}
        />,
      ),
    );

    expect(rendered).toContain('12.6s');
    expect(rendered).toContain('18 matches');
    expect(rendered).toContain('companion suite');
    expect(rendered).not.toContain('1 read');
    expect(rendered).not.toContain('finished 2/2');
  });

  // Regression, T137: one duration must not appear on two nested bands.
  it('prints elapsed once on the result rather than a redundant execution band', () => {
    const painted = renderOpenSteps(
      <AssistantMessage
        turn={turnOf([
          {
            source: 'work()',
            stdout: 'done\n',
            duration_ms: 12_600,
            activity: {
              ...runningActivity,
              state: 'succeeded',
              counts: { running: 0, succeeded: 2, failed: 0, cancelled: 0 },
              rows: rows.map((row) => ({ ...row, state: 'succeeded' })),
            },
          },
        ])}
      />,
    );
    // The digest row totals its steps; within the execution, one band prints the time.
    const digest = painted.container.querySelector('[data-step-digest]')!.parentElement!;
    const elapsed = () => (painted.container.textContent?.match(/12\.6s/g) ?? []).length;

    expect(digest).toHaveTextContent('12.6s');
    expect(elapsed()).toBe(2);
    fireEvent.click(painted.getByRole('button', { name: 'Expand code' }));
    expect(painted.container.textContent).toContain('RESULT');
    expect(elapsed()).toBe(2);
  });
  it('uses only the actual terminal Activity count after settlement', () => {
    const rendered = text(
      openStepsMarkup(
        <AssistantMessage
          turn={turnOf([
            {
              source: 'work()',
              stdout: 'done\n',
              activity: {
                ...runningActivity,
                state: 'failed',
                counts: { running: 0, succeeded: 5, failed: 1, cancelled: 0 },
                rows: [],
              },
            },
          ])}
        />,
      ),
    );

    // Activity has no step limit, so no tail invents an omission count.
    expect(rendered).not.toContain('FAILED');
    expect(rendered).not.toContain('omitted');
    expect(rendered).not.toContain('finished 6/');
  });

  it('does not attach Activity to a print-only form', () => {
    const rendered = text(
      renderToStaticMarkup(
        <AssistantMessage turn={turnOf([{ stdout: 'printed\n', activity: runningActivity }])} />,
      ),
    );
    expect(rendered).not.toContain('ACTIVITY');
  });
});
// Every tile in this rail fetches its own bytes on first paint, so an iteration
// that produced forty artifacts fired forty requests in one tick — on whatever
// connection the phone had. A page at a time now, by count AND by weight.
describe('the attachment rail', () => {
  // These media tests have no layout. The history suite controls viewport entry.
  beforeEach(() => vi.stubGlobal('IntersectionObserver', undefined));
  afterEach(() => vi.unstubAllGlobals());
  const client = {
    attachmentUrl: async () => 'blob:none',
    retainAttachment: () => () => {},
  } as unknown as GatewayClient;
  const rail = (count: number, size: number) =>
    renderToStaticMarkup(
      <AttachmentRail
        client={client}
        sid="s1"
        attachments={Array.from({ length: count }, (_, at) => ({
          filename: `report-${at}.pdf`,
          media_type: 'application/pdf',
          size,
          iteration_id: 'i1',
          index: at,
        }))}
      />,
    );

  it('paints one page of artifacts and offers the rest', () => {
    const html = rail(20, 64 * 1024);
    expect(html).toContain('report-5.pdf');
    expect(html).not.toContain('report-6.pdf');
    expect(text(html)).toContain('Load 14 more');
  });

  it('pages on WEIGHT before it ever reaches the count', () => {
    const html = rail(6, 3 * 1024 * 1024);
    expect(html).toContain('report-1.pdf');
    expect(html).not.toContain('report-2.pdf');
    expect(text(html)).toContain('4 more');
  });

  // Regression, issue vis_session_id#83d1d828-d2a1-45b2-bbdc-4a5fea1ec354: iOS left a
  // produced clip grey until playback began, and its plate had no way to share it.
  it('opens a produced clip on its first frame and shares its original bytes', async () => {
    const blob = new Blob(['clip'], { type: 'video/mp4' });
    const attachmentBlob = vi.fn(async () => blob);
    const share = vi.fn(async (_data: ShareData) => undefined);
    Object.defineProperty(navigator, 'share', { value: share, configurable: true });
    const view = render(
      <AttachmentRail
        client={{ ...client, attachmentBlob } as unknown as GatewayClient}
        sid="s1"
        attachments={[
          {
            filename: 'brag.mp4',
            media_type: 'video/mp4',
            size: 8_600_000,
            iteration_id: 'i1',
            index: 0,
          },
        ]}
      />,
    );

    try {
      await waitFor(() =>
        expect(view.container.querySelector('video')).toHaveAttribute('src', 'blob:none#t=0.001'),
      );
      fireEvent.click(view.getByRole('button', { name: 'Share brag.mp4' }));
      expect(await view.findByText('Video shared.')).toBeVisible();
      expect(attachmentBlob).toHaveBeenCalledWith('s1', 'i1', 0);
      expect(share.mock.calls[0]?.[0].files?.[0]).toMatchObject({
        name: 'brag.mp4',
        type: 'video/mp4',
      });
    } finally {
      view.unmount();
      Reflect.deleteProperty(navigator, 'share');
    }
  });
  // Shares a recording and saves a recorded archive from the session, not only from Artifacts.
  it('shares a produced recording and downloads a recorded archive from the session', async () => {
    const attachmentBlob = vi.fn(async () => new Blob(['bytes']));
    const share = vi.fn(async (_data: ShareData) => undefined);
    Object.defineProperty(navigator, 'share', { value: share, configurable: true });
    const createUrl = vi.spyOn(URL, 'createObjectURL').mockReturnValue('blob:bundle');
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => {});
    const view = render(
      <AttachmentRail
        client={{ ...client, attachmentBlob } as unknown as GatewayClient}
        sid="s1"
        attachments={[
          { filename: 'memo.m4a', media_type: 'audio/mp4', size: 4_000, iteration_id: 'i1', index: 0 },
          { filename: 'bundle.zip', media_type: 'application/zip', size: 9_000, iteration_id: 'i1', index: 1 },
        ]}
      />,
    );

    try {
      fireEvent.click(view.getByRole('button', { name: 'Share memo.m4a' }));
      expect(await view.findByText('Recording shared.')).toBeVisible();
      expect(attachmentBlob).toHaveBeenCalledWith('s1', 'i1', 0);
      fireEvent.click(view.getByText('bundle.zip'));
      fireEvent.click(view.getByRole('button', { name: 'Download bundle.zip' }));
      expect(await view.findByText('Archive downloaded.')).toBeVisible();
      expect(attachmentBlob).toHaveBeenCalledWith('s1', 'i1', 1);
      expect((click.mock.instances[0] as HTMLAnchorElement).download).toBe('bundle.zip');
      expect(share.mock.calls.map((call) => call[0].files?.[0]?.name)).toEqual(['memo.m4a']);
    } finally {
      view.unmount();
      createUrl.mockRestore();
      click.mockRestore();
      Reflect.deleteProperty(navigator, 'share');
    }
  });
  it('opens a recorded ZIP archive in the artifact sheet', () => {
    const open = vi.fn();
    const view = render(
      <ArtifactLinkContext.Provider value={{ byName: new Map(), open }}>
        <AttachmentRail
          client={client as unknown as GatewayClient}
          sid="s1"
          attachments={[
            {
              filename: 'bundle.zip',
              media_type: 'application/zip',
              size: 9_000,
              iteration_id: 'i1',
              index: 0,
              attachment_id: 'att-zip',
            },
            { filename: 'notes.bin', media_type: 'application/octet-stream', iteration_id: 'i1', index: 1, attachment_id: 'att-bin' },
          ]}
        />
      </ArtifactLinkContext.Provider>,
    );

    try {
      fireEvent.click(view.getByText('bundle.zip'));
      fireEvent.click(view.getByRole('button', { name: 'Open bundle.zip' }));
      expect(open).toHaveBeenCalledExactlyOnceWith('att-zip');
      expect(view.queryByRole('button', { name: 'Open notes.bin' })).toBeNull();
    } finally {
      view.unmount();
    }
  });
  it("saves an HTML page's note for the model on the cut that is on screen", async () => {
    const setPageAgentNote = vi.fn(async () => undefined);
    const pageClient = {
      attachmentUrl: async (_sid: string, iid: string, index: number) => `blob:${iid}/${index}`,
      retainAttachment: () => () => {},
      setPageAgentNote,
    } as unknown as GatewayClient;
    vi.stubGlobal(
      'fetch',
      vi.fn(async () => ({ blob: async () => new Blob(['<head></head>'], { type: 'text/html' }) })),
    );
    const create = URL.createObjectURL;
    const revoke = URL.revokeObjectURL;
    URL.createObjectURL = () => 'blob:bridged';
    URL.revokeObjectURL = () => {};
    const page = (index: number, version: number) => ({
      filename: 'board.html',
      media_type: 'text/html',
      iteration_id: 'i1',
      index,
      version,
    });
    const view = render(<AttachmentRail client={pageClient} sid="s1" attachments={[page(0, 1), page(1, 2)]} />);
    try {
      fireEvent.click(view.getByRole('button', { name: 'Open board.html' }));
      const frame = await waitFor(() => {
        const shown = document.querySelector<HTMLIFrameElement>('iframe[title="board.html"]');
        expect(shown?.getAttribute('src')).toBe('blob:bridged');
        return shown as HTMLIFrameElement;
      });
      act(() => {
        window.dispatchEvent(
          new MessageEvent('message', {
            data: { type: 'vis-doc-agent-note', text: 'Card 3 is done' },
            source: frame.contentWindow,
          }),
        );
      });
      await waitFor(() => expect(setPageAgentNote).toHaveBeenCalledExactlyOnceWith('s1', 'i1', 1, 'Card 3 is done'));
    } finally {
      view.unmount();
      URL.createObjectURL = create;
      URL.revokeObjectURL = revoke;
    }
  });
  // Regression, user report: "the model attached a document, I commented on it and
  // saved, and now instead of one there are two". Saving files the same filename
  // as the NEXT VERSION of the same artifact; this rail painted a row per
  // descriptor, so the revision arrived as a second row under the same name and
  // the stack grew a header summing both cuts.
  const note = (size: number, index: number, version: number) => ({
    filename: 'README.md',
    media_type: 'text/markdown',
    size,
    iteration_id: 'i1',
    index,
    version,
  });
  const revised = (extra: IterationAttachment[] = []) =>
    renderToStaticMarkup(
      <AttachmentRail
        client={client}
        sid="s1"
        attachments={[note(13_000, 0, 1), note(12_700, 1, 2), ...extra]}
      />,
    );

  it('answers a saved revision with the SAME one row', () => {
    const html = revised();
    expect(html.split('aria-label="Open README.md"').length - 1).toBe(1);
    // One artifact is one row and no header at all — nothing to report a group of.
    expect(text(html)).not.toContain('documents');
    expect(text(html)).not.toContain('25.1KB');
  });

  it('makes the row say the version moved, and weighs the newest cut', () => {
    const html = revised();
    expect(text(html)).toContain('v2');
    expect(text(html)).toContain('12.4KB');
    expect(text(html)).not.toContain('12.7KB');
  });

  it('still reports a GROUP, counting artifacts and not cuts', () => {
    const html = revised([
      {
        filename: 'NOTES.md',
        media_type: 'text/markdown',
        size: 1_000,
        iteration_id: 'i1',
        index: 2,
        version: 1,
      },
    ]);
    expect(text(html)).toContain('2 documents');
    expect(text(html)).not.toContain('3 documents');
  });

  // Regression, user report with a screenshot: a `gh` watch that had FINISHED
  // read as "1 file · release.live.ndjson" — a nameless line under the recorded
  // files disclosure, while the record behind it holds the picture the run
  // ended on and its whole log. A settled run is an artifact this app opens,
  // so it stands in the step as one.
  it('collapses repeated cuts of one settled run into one row', () => {
    const run = {
      filename: 'release.live.ndjson',
      media_type: 'application/vnd.vis.live+ndjson',
      iteration_id: 'i1',
    };
    const html = renderToStaticMarkup(
      <AttachmentRail
        client={client}
        sid="s1"
        attachments={Array.from({ length: 11 }, (_, index) => ({
          ...run,
          index,
          version: index + 1,
          size: 40_000 + index,
        }))}
      />,
    );

    // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: every saved cut of
    // one CI run rendered as another identical RUN row in the same tool result.
    expect(html.match(/aria-label="Open run release"/g)).toHaveLength(1);
    expect(text(html).match(/RUN/g)).toHaveLength(1);
    expect(text(html)).not.toContain('release.live.ndjson');
  });
});

// Regression, live reasoning scroll jump: the trace ramp used to pin its own
// scrollTop while SessionScreen's content observer pinned the same scroller again,
// making streamed thinking visibly jump.
describe('transcript scroll ownership', () => {
  it('leaves scroll correction to the session screen owner', async () => {
    const view = render(
      <div data-testid="scroller" className="overflow-y-auto">
        <ThinkingBand>{'one\ntwo\nthree\nfour\nfive\nsix'}</ThinkingBand>
      </div>,
    );
    const scroller = view.getByTestId('scroller');
    scroller.scrollTop = 120;
    // The band grows the way a live trace grows: same component, more rows.
    view.rerender(
      <div data-testid="scroller" className="overflow-y-auto">
        <ThinkingBand>
          {Array.from({ length: 40 }, (_, row) => `line ${row}`).join('\n')}
        </ThinkingBand>
      </div>,
    );
    await waitFor(() => expect(scroller.textContent).toContain('line 39'));
    expect(scroller.scrollTop).toBe(120);
    view.unmount();
  });
});

// Regression: a document artifact was on screen twice — the `vis-doc` fence
// painted its own card in the tool result while the attachment rail below it
// painted the openable tile for the very same file.
describe('a vis-doc fence', () => {
  const fence = [
    '````vis-doc',
    '[Document: report.pdf PDF, 1.2 MB]',
    '/tmp/vis-python/doc-1/report.pdf',
    'application/pdf',
    'report.pdf',
    '1.2 MB',
    '````',
  ].join('\n');

  it("paints nothing: the attachment tile is the document's one appearance", () => {
    const markup = renderToStaticMarkup(<Markdown compact>{fence}</Markdown>);
    expect(text(markup)).not.toContain('report.pdf');
    expect(text(markup)).not.toContain('/tmp/vis-python/doc-1/report.pdf');
    expect(markup).not.toContain('<iframe');
  });
});

// Regression, session 0ec1e9f3-23d5-4070-a17e-46f8e7f514e8: the moment a long
// streamed answer became the finished one, the transcript flickered — it jumped
// up, went blank, and dropped back to the bottom a few frames later. The live
// bubble's trace was unmounted and the persisted row mounted a BRAND NEW trace,
// which started the opening ramp over: only the last `SEGMENT_FIRST_PAINT`
// segments painted, and the rest came back a chunk per frame. Measured on the
// handover of a short turn, the scroller lost 102 nodes and 378 px and got them
// back 8 ms later; on a long answer that is most of the transcript.
describe('the trace a settled row inherits from the running-turn bubble', () => {
  const iterations = Array.from({ length: 20 }, (_, index) => ({
    id: `i${index}`,
    assistant_prose: `step ${index}`,
  })) as unknown as TranscriptTurn['iterations'];
  const finished = {
    turn_id: 't1',
    request: 'do the long thing',
    status: 'completed',
    iterations,
    content: [],
  } as unknown as TranscriptTurn;

  it('mounts every segment in the first paint', () => {
    const html = renderToStaticMarkup(<AssistantMessage turn={finished} whole />);

    for (let step = 0; step < 20; step += 1) expect(text(html)).toContain(`step ${step}`);
  });

  it('still ramps a trace nobody has seen yet', () => {
    const html = renderToStaticMarkup(<AssistantMessage turn={finished} />);

    // The tail is what the reader can see when a session OPENS pinned to the
    // bottom, and mounting only that is what keeps the opening frame short.
    expect(text(html)).toContain('step 19');
    expect(text(html)).not.toContain('step 0');
  });

  // Regression, the same flicker one tick later: the row can MOUNT before the
  // bubble is retired. Its settled transcript row lands while the registry still
  // reports the turn as running, so the handover verdict only comes on the NEXT
  // reconcile tick — and `whole`, read once as the ramp's initial state, arrived
  // after that mount and changed nothing. The transcript collapsed to
  // `SEGMENT_FIRST_PAINT` exactly as before for every handover that took more
  // than one tick.
  it('takes the whole trace even when it mounted before the handover', () => {
    const { container, rerender } = render(<AssistantMessage turn={finished} />);

    // Mounted cold: the ramp is holding everything but the tail.
    expect(container.textContent).not.toContain('step 0');

    rerender(<AssistantMessage turn={finished} whole />);

    for (let step = 0; step < 20; step += 1)
      expect(container.textContent).toContain(`step ${step}`);
  });

  // Regression, the same collapse deferred by one turn: `handedOverRowId` holds
  // ONE row, so the next turn's handover takes the flag away from this one. A
  // trace that showed everything only while the flag was up dropped back to the
  // tail the moment it moved on.
  it("keeps the whole trace after the flag moves to the next turn's row", () => {
    const { container, rerender } = render(<AssistantMessage turn={finished} />);
    rerender(<AssistantMessage turn={finished} whole />);
    rerender(<AssistantMessage turn={finished} />);

    expect(container.textContent).toContain('step 0');
    expect(container.textContent).toContain('step 19');
  });
});

describe('compact execution groups', () => {
  it('does not leave a CODE receipt when Python is hidden and no Activity was emitted', () => {
    const painted = render(
      <IterationTrace
        whole
        showCode={false}
        iterations={[1, 2, 3].map((position) => ({
          position,
          forms: [{ source: 'value = 42', success: true, duration_ms: 10, stdout: '' }],
        }))}
      />,
    );
    expect(painted.container.querySelector('[data-execution-code]')).toBeNull();
    expect(painted.container.textContent).not.toContain('CODE');
    // Only the digest row totals the time of the hidden steps.
    expect(painted.container.textContent?.match(/30ms/g)).toHaveLength(1);
    expect(painted.container.querySelector('[data-step-digest]')!.parentElement).toHaveTextContent(
      '30ms',
    );
    expect(painted.container.textContent).not.toContain('value = 42');
    expect(painted.queryByRole('button', { name: 'Expand code' })).toBeNull();
    expect(painted.container.querySelector('[data-execution-activity]')).toBeNull();
  });

  // Regression #216: the engine's empty-response explanation must survive trace filtering.
  it('shows an empty-response diagnosis without inventing a tool execution', () => {
    const diagnosis = 'Provider returned no executable tool call or answer text; nothing was executed.';
    const painted = render(
      <IterationTrace
        whole
        iterations={[{ position: 1, assistant_prose: diagnosis, forms: [] }]}
      />,
    );
    expect(painted.getByText(diagnosis)).toBeVisible();
    expect(painted.container.querySelector('[data-execution-code]')).toBeNull();
    expect(painted.container.querySelector('[data-execution-activity]')).toBeNull();
  });

  it('shows one code line before one activity for adjacent Python calls', () => {
    const painted = renderOpenSteps(
      <IterationTrace
        whole
        iterations={[
          {
            id: 'batch-1',
            position: 1,
            forms: [
              { source: 'first_call()\nfirst_detail()', duration_ms: 10 },
              { source: 'second_call()', duration_ms: 20 },
            ],
          },
        ]}
      />,
    );
    expect(painted.container.querySelectorAll('[aria-label="Execution trace"]')).toHaveLength(1);
    expect(painted.getAllByRole('button', { name: 'Copy code' })).toHaveLength(1);
    expect(painted.container.textContent).not.toContain('PYTHON');
    expect(painted.container.textContent).toContain('CODE');
    expect(painted.container.textContent).not.toContain('first_detail()');
    // Regression: compact groups discarded every member's measured duration.
    expect(painted.container.querySelector('[data-execution-code]')?.textContent).toContain('30ms');
    fireEvent.click(painted.getByRole('button', { name: 'Expand code' }));
    expect(painted.container.textContent).toContain('first_detail()');
    expect(painted.container.textContent).toContain('second_call()');
  });
  it.each([0, 0.5, 0.999])(
    'shows <1ms for a measured sub-millisecond execution (%s)',
    (duration) => {
      const painted = renderOpenSteps(
        <IterationTrace
          whole
          iterations={[{ position: 1, forms: [{ source: 'pass', duration_ms: duration }] }]}
        />,
      );
      expect(painted.container.querySelector('[data-execution-code]')?.textContent).toContain(
        '<1ms',
      );
    },
  );
});

// Progress notes are the only visible separators of a turn's work: the steps between two
// notes fold into one digest row, live and after the turn finishes. Opening the row shows
// their one reasoning band, their code and their Activity.
describe('steps between progress notes', () => {
  const note = 'Sources read; now the last batch.';
  const iterations = [
    {
      id: 'step-1',
      position: 1,
      thinking: 'Read the sources first.',
      forms: [{ source: 'read_first()', duration_ms: 10 }],
    },
    {
      id: 'step-2',
      position: 2,
      thinking: 'One more source.',
      forms: [{ source: 'read_second()', duration_ms: 10 }],
    },
    {
      id: 'step-3',
      position: 3,
      assistant_prose: note,
      forms: [
        { source: 'check()', duration_ms: 10 },
        { source: 'report()', duration_ms: 10 },
      ],
    },
  ];
  const traces = (root: HTMLElement) => [
    ...root.querySelectorAll('[aria-label="Execution trace"]'),
  ];
  // Each reasoning band, read as its paragraphs.
  const bands = (root: HTMLElement) =>
    [...root.querySelectorAll('.bg-thinking-surface')].map((band) =>
      [...band.querySelectorAll('p')].map((paragraph) => paragraph.textContent),
    );
  const follows = (before: Node, after: Node) =>
    Boolean(before.compareDocumentPosition(after) & Node.DOCUMENT_POSITION_FOLLOWING);
  const digests = (root: HTMLElement) => [
    ...root.querySelectorAll<HTMLButtonElement>('button[data-step-digest]'),
  ];
  const client = {
    attachmentUrl: async () => 'blob:none',
    retainAttachment: () => () => {},
  } as unknown as GatewayClient;
  const report: IterationAttachment = {
    index: 0,
    iteration_id: 'step-2',
    filename: 'report.pdf',
    media_type: 'application/pdf',
    size: 2048,
  };
  const release: IterationAttachment = {
    index: 1,
    iteration_id: 'step-2',
    filename: 'Release.live.ndjson',
    media_type: 'application/vnd.vis.live+ndjson',
    size: 2048,
  };
  // One settled read: its open steps show it as Activity, and the digest row does not count it.
  const read: ActivityProjection = {
    state: 'succeeded',
    counts: { running: 0, succeeded: 1, failed: 0, cancelled: 0 },
    rows: [
      {
        id: 'read-call',
        sequence: 1,
        operation: 'cat',
        presenter: 'observation',
        signal: 'observation',
        state: 'succeeded',
        summary: 'Read sources.md',
        duration_ms: 12,
        resources: [],
        evidence: [],
      },
    ],
  };

  it('combines the steps between notes into one Activity and one reasoning band', () => {
    const painted = renderOpenSteps(<IterationTrace whole iterations={iterations} />);
    const [first, second] = traces(painted.container);
    const noted = painted.getByText(note);
    expect(traces(painted.container)).toHaveLength(2);
    expect(bands(painted.container)).toEqual([['Read the sources first.', 'One more source.']]);
    expect(painted.container.textContent).toContain('One more source.');
    expect(follows(first, noted)).toBe(true);
    expect(follows(noted, second)).toBe(true);
  });

  // Regression: a step that produced an artifact opened a second Activity between the
  // same two notes, so one run of work read as two.
  it('keeps an artifact inside the Activity of its notes', () => {
    const produced = iterations.map((step) =>
      step.id === 'step-2' ? { ...step, attachments: [report] } : step,
    );
    const painted = renderOpenSteps(<IterationTrace whole iterations={produced} client={client} sid="s1" />);
    const [first, second] = traces(painted.container);
    const artifact = painted.getByText('report.pdf');
    const noted = painted.getByText(note);
    expect(traces(painted.container)).toHaveLength(2);
    expect(follows(first, artifact)).toBe(true);
    expect(follows(artifact, noted)).toBe(true);
    expect(follows(noted, second)).toBe(true);
  });

  it('shows each note above one closed digest of its steps', () => {
    const painted = render(<IterationTrace whole showCode iterations={iterations} />);
    const [first, second] = digests(painted.container);
    const noted = painted.getByText(note);
    expect(digests(painted.container).map((row) => row.getAttribute('aria-label'))).toEqual([
      'Expand steps: RAW EXECUTION',
      'Expand steps: RAW EXECUTION',
    ]);
    // Regression, user report: the row names what the calls did, not how many steps ran.
    const counted = digests(painted.container).map((row) => row.textContent);
    expect(counted).toEqual(['RAW EXECUTION', 'RAW EXECUTION']);
    // The measured time of the steps stands on the right of each row.
    expect(digests(painted.container).map((row) => row.nextElementSibling?.textContent)).toEqual([
      '20ms',
      '20ms',
    ]);
    expect(follows(first, noted)).toBe(true);
    expect(follows(noted, second)).toBe(true);
    expect(traces(painted.container)).toHaveLength(0);
    expect(bands(painted.container)).toEqual([]);
    expect(painted.container.textContent).not.toContain('read_first()');
  });

  // Regression, user report: each running step flashed its code below the closed digest
  // until it folded in. The digest row alone counts the running step now.
  it('keeps the running step inside a closed live digest', () => {
    const done = { source: 'print(1)', duration_ms: 45 };
    const painted = render(
      <IterationTrace
        whole
        showCode
        live
        iterations={[{ id: 'run', forms: [done, { source: 'wait_for_seal()' }] }]}
      />,
    );
    const digest = painted.getByRole('button', { name: /^Expand steps/ });
    expect(digest).toHaveAttribute('aria-expanded', 'false');
    expect(digest.textContent).toContain('running');
    expect(painted.container.textContent).not.toContain('wait_for_seal()');
    expect(painted.queryByRole('button', { name: /code$/ })).toBeNull();
    painted.rerender(
      <IterationTrace
        whole
        showCode
        live
        iterations={[
          { id: 'run', forms: [done, { source: 'wait_for_seal()', duration_ms: 300_000 }] },
        ]}
      />,
    );
    expect(painted.container.textContent).not.toContain('wait_for_seal()');
  });

  // Regression: thinking flashed outside the digest until the step's code arrived.
  it.each([false, true])('folds thinking without code into a THINKING row (live: %s)', (live) => {
    const thought = 'Plan the queue change.';
    const painted = render(
      <IterationTrace whole showCode live={live} iterations={[{ id: 'think', thinking: thought, forms: [] }]} />,
    );
    const digest = painted.getByRole('button', { name: 'Expand steps: THINKING' });
    expect(painted.container.textContent).not.toContain(thought);
    fireEvent.click(digest);
    expect(painted.container.textContent).toContain(thought);
    fireEvent.click(digest);
    painted.rerender(
      <IterationTrace
        whole
        showCode
        live={live}
        iterations={[{ id: 'think', thinking: thought, forms: [{ source: 'print(1)', duration_ms: 12 }] }]}
      />,
    );
    expect(painted.getByRole('button', { name: 'Expand steps: RAW EXECUTION' })).toBeInTheDocument();
    expect(painted.container.querySelectorAll('[data-step-digest]')).toHaveLength(1);
    expect(painted.container.textContent).not.toContain(thought);
  });

  // Regression, user report: a provider 401 showed an empty RAW EXECUTION step above its error.
  it('paints no execution step for a failed model call', () => {
    const failed = { id: 'failed', position: 1, status: 'error', code: '', duration_ms: 0 };
    const painted = render(<IterationTrace whole showCode iterations={[failed]} />);
    expect(painted.container.querySelectorAll('[data-step-digest]')).toHaveLength(0);
    painted.rerender(
      <IterationTrace whole showCode iterations={[{ ...failed, thinking: 'Plan the call.' }]} />,
    );
    expect(painted.getByRole('button', { name: 'Expand steps: THINKING' })).toBeInTheDocument();
    expect(painted.container.textContent).not.toContain('RAW EXECUTION');
  });

  it('opens a digest to show the thinking, code and Activity of its steps', () => {
    const observed = iterations.map((step) =>
      step.id === 'step-2'
        ? { ...step, forms: [{ source: 'read_second()', duration_ms: 10, activity: read }] }
        : step,
    );
    const painted = render(<IterationTrace whole showCode iterations={observed} />);
    const [first, second] = digests(painted.container);
    expect(first).toHaveAttribute(
      'aria-label',
      'Expand steps: 1 observation',
    );
    fireEvent.click(first);
    const [band] = painted.container.querySelectorAll('.bg-thinking-surface');
    const [opened] = traces(painted.container);
    expect(first).toHaveAttribute('aria-expanded', 'true');
    expect(first).toHaveAttribute(
      'aria-label',
      'Collapse steps: 1 observation',
    );
    expect(second).toHaveAttribute('aria-expanded', 'false');
    expect(bands(painted.container)).toEqual([['Read the sources first.', 'One more source.']]);
    expect(traces(painted.container)).toHaveLength(1);
    fireEvent.click(painted.getByRole('button', { name: 'Expand code' }));
    const code = painted.container.querySelector('[data-execution-code]')?.textContent;
    expect(code).toContain('read_first()');
    expect(code).toContain('read_second()');
    expect(painted.getByRole('button', { name: 'Expand Activity' })).toBeInTheDocument();
    expect(follows(first, band)).toBe(true);
    expect(follows(band, opened)).toBe(true);
    expect(painted.container.textContent).not.toContain('check()');
    expect(painted.getAllByText(note)).toHaveLength(1);
    fireEvent.click(first);
    expect(traces(painted.container)).toHaveLength(0);
    expect(bands(painted.container)).toEqual([]);
  });

  // Regression, user screenshot: a closed digest leaked a separate Failed card.
  it.each([false, true])('keeps failures inside the digest and files visible (live: %s)', (live) => {
    const failed = iterations.map((step) =>
      step.id === 'step-2'
        ? {
            ...step,
            attachments: [report],
            forms: [{ source: 'read_second()', duration_ms: 10, error: 'missing source' }],
          }
        : step,
    );
    const painted = render(
      <IterationTrace whole showCode live={live} iterations={failed} client={client} sid="s1" />,
    );
    const [first] = digests(painted.container);
    expect(painted.queryByRole('button', { name: 'Expand error details' })).toBeNull();
    expect(first).toHaveAttribute('aria-label', 'Expand steps: 1 failed');
    expect(painted.getByText('report.pdf')).toBeVisible();
    expect(traces(painted.container)).toHaveLength(0);
    expect(painted.container.textContent).not.toContain('read_second()');
    fireEvent.click(first);
    const failure = painted.getByRole('button', { name: 'Expand error details' });
    expect(failure).toHaveTextContent('Failed');
    fireEvent.click(failure);
    expect(painted.getByText('missing source')).toBeVisible();
    fireEvent.click(first);
    expect(painted.queryByRole('button', { name: /error details/ })).toBeNull();
    expect(painted.queryByText('missing source')).toBeNull();
    expect(painted.getByText('report.pdf')).toBeVisible();
  });

  it.each(
    [false, true].flatMap((live) =>
      (['missing', 'empty', 'succeeded', 'failed', 'cancelled'] as const).map((state) => ({ live, state })),
    ),
  )('counts an execution failure once with $state Activity (live: $live)', ({ live, state }) => {
    const activity: ActivityProjection | undefined =
      state === 'missing'
        ? undefined
        : {
            state: state === 'empty' ? 'succeeded' : state,
            counts: {
              running: 0,
              succeeded: state === 'succeeded' ? 1 : 0,
              failed: state === 'failed' ? 1 : 0,
              cancelled: state === 'cancelled' ? 1 : 0,
            },
            rows: state === 'empty' ? [] : [{ ...read.rows[0], signal: 'generic', state }],
          };
    const completed: ActivityProjection = {
      ...read,
      counts: { running: 0, succeeded: 3, failed: 0, cancelled: 0 },
      rows: (['mutation', 'observation', 'observation'] as const).map((signal, index) => ({
        ...read.rows[0],
        id: `completed-${index}`,
        sequence: index + 1,
        signal,
      })),
    };
    const painted = render(
      <IterationTrace
        whole
        showCode={false}
        live={live}
        iterations={[
          {
            id: 'step-1',
            position: 1,
            forms: [
              { source: 'read_sources()', duration_ms: 559, activity: completed },
              { source: 'check()', duration_ms: 163, error: 'check failed', activity },
            ],
          },
        ]}
      />,
    );
    const [digest] = digests(painted.container);
    const count = state === 'cancelled' ? '1 cancelled' : '1 failed';
    expect(digest).toHaveAttribute(
      'aria-label',
      `Expand steps: 1 mutation · 2 observations · ${count}`,
    );
    expect(digest.querySelector('.text-err-ink')).toHaveTextContent(count);
    expect(digest.parentElement).toHaveTextContent('722ms');
    expect(painted.queryByRole('button', { name: 'Expand error details' })).toBeNull();
  });

  // Regression, user report: opening the steps must keep their live control and count.
  it.each([1, 2])('keeps the control for %i recorded runs when the steps open', (count) => {
    const records = Array.from({ length: count }, (_, index) => ({
      ...release,
      index,
      filename: `Release ${index + 1}.live.ndjson`,
    }));
    const recorded = iterations.map((step) =>
      step.id === 'step-2' ? { ...step, attachments: records } : step,
    );
    const painted = render(
      <IterationTrace whole showCode iterations={recorded} client={client} sid="s1" />,
    );
    const name = `Release ${count}`;
    const label = count > 1 ? `${count} live` : 'live';
    const control = painted.getByRole('button', { name: `Open live recording: ${name}` });
    const digest = digests(painted.container)[0];
    expect(control).toHaveTextContent(new RegExp(`^${label}$`));
    expect(digest.parentElement).toContainElement(control);
    expect(painted.queryByRole('button', { name: `Open run ${name}` })).toBeNull();
    fireEvent.click(control);
    fireEvent.click(painted.getByRole('button', { name: `Close ${name}` }));
    fireEvent.click(digest);
    expect(control).toBeVisible();
    expect(control).toHaveTextContent(new RegExp(`^${label}$`));
    expect(painted.getByRole('button', { name: `Open run ${name}` })).toBeInTheDocument();
    fireEvent.click(control);
    expect(painted.getAllByRole('button', { name: `Close ${name}` })).toHaveLength(1);
    fireEvent.click(painted.getByRole('button', { name: `Close ${name}` }));
    fireEvent.click(digest);
    expect(control).toBeVisible();
    expect(control).toHaveTextContent(new RegExp(`^${label}$`));
  });

  it('shows Activity and reasoning for each step when summarizing is off', () => {
    setStepsSummarized(false);
    try {
      const painted = render(<IterationTrace whole iterations={iterations} />);
      expect(traces(painted.container)).toHaveLength(4);
      expect(digests(painted.container)).toEqual([]);
      expect(bands(painted.container)).toEqual([['Read the sources first.'], ['One more source.']]);
      expect(localStorage.getItem('vis.summarize_steps')).toBe('separate');
    } finally {
      setStepsSummarized(true);
    }
  });
});

// Regression: the user transcript discarded every non-media attachment.
describe('user log attachments', () => {
  it('shows log files without prose, and never treats them as images', () => {
    const view = render(
      <UserMessage
        attachments={[
          {
            source: 'user',
            filename: 'vis-diagnostics.jsonl.gz',
            media_type: 'application/gzip',
            base64: 'H4sIAAAAAAAA/w==',
            size: 10,
          },
          {
            source: 'user',
            filename: 'vis-diagnostics.jsonl',
            media_type: 'application/x-ndjson',
            base64: 'e30K',
            size: 3,
          },
          {
            source: 'tool',
            filename: 'not-user.gz',
            media_type: 'application/gzip',
            base64: 'H4sI',
          },
        ]}
      >
        {''}
      </UserMessage>,
    );
    expect(view.getByText('vis-diagnostics.jsonl.gz')).toBeVisible();
    expect(view.getByText('vis-diagnostics.jsonl')).toBeVisible();
    expect(view.queryByText('not-user.gz')).toBeNull();
    expect(view.container.querySelector('img, audio, video')).toBeNull();
  });
});

// #212: adjacent forms must not flatten away durable source addresses.
it('keeps independently paged Activity sources addressable in a combined execution band', async () => {
  const { ActivityHistoryContext } = await import('./ActivityPanel');
  const { activityHistoryPage } = await import('../dev/activity-history');
  const first = activityHistoryPage();
  const second = activityHistoryPage();
  second.history!.id = '87654321-1234-1234-1234-123456789012';
  const load = vi.fn(async (id: string, after: number) => ({
    ...activityHistoryPage(after),
    history: { ...activityHistoryPage(after).history!, id },
  }));
  const turn = {
    turn_id: 'history-turn',
    status: 'completed',
    iterations: [
      {
        id: 'history-iteration',
        position: 1,
        forms: [
          { source: 'first()', activity: first },
          { source: 'second()', activity: second },
        ],
      },
    ],
  } as TranscriptTurn;
  const view = renderOpenSteps(
    <ActivityHistoryContext.Provider value={{ load }}>
      <AssistantMessage turn={turn} />
    </ActivityHistoryContext.Provider>,
  );
  const expands = view.getAllByRole('button', { name: 'Expand Activity' });
  expect(expands).toHaveLength(1);
  expect(expands[0]).toHaveTextContent('320 operations');
  fireEvent.click(expands[0]);
  await waitFor(() => {
    expect(load).toHaveBeenCalledWith(first.history!.id, 32, '', expect.any(AbortSignal));
    expect(load).toHaveBeenCalledWith(second.history!.id, 32, '', expect.any(AbortSignal));
  });
  expect(view.getAllByRole('list', { name: 'Operation groups' })).toHaveLength(1);
  view.unmount();
});
