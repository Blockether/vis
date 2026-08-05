import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import { AssistantMessage, Markdown, UserMessage } from './ChatContent';
import { mediaFrameClass } from '../lib/media-frame';
import type { TranscriptTurn } from '../lib/types';

/** Visible text of a rendered chunk: tags out, entities back. */
const text = (html: string) =>
  html
    .replace(/<[^>]+>/g, '')
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, '>')
    .replace(/&lt;/g, '<')
    .replace(/&amp;/g, '&');

/** One entry per PAINTED code row — the code block gives every line its own div. */
const codeRows = (html: string) =>
  (html.match(/<div class="flex w-fit[^"]*">.*?<\/div>/g) ?? []).map(text);

const count = (html: string, pattern: RegExp) => (html.match(pattern) ?? []).length;

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

  it('flows soft newlines when hard breaks are off', () => {
    const html = renderToStaticMarkup(<Markdown compact>{'first line\nsecond line'}</Markdown>);
    expect(html).not.toContain('<br');
    expect(count(html, /<p class=/g)).toBe(1);
  });
});

describe('Markdown tool card body', () => {
  it('keeps blank lines and indentation inside a COMMAND block', () => {
    const html = renderToStaticMarkup(
      <Markdown compact>{'**COMMAND**\n\n```bash\nset -e\n\nif [ -f x ]; then\n  npm test\nfi\n```\n'}</Markdown>,
    );
    expect(codeRows(html)).toEqual(['set -e', ' ', 'if [ -f x ]; then', '  npm test', 'fi']);
  });

  it('keeps a blank line between two phases of STDOUT', () => {
    const html = renderToStaticMarkup(
      <Markdown compact>{'**STDOUT**\n\n```\nphase one ok\n\nphase two ok\n```\n'}</Markdown>,
    );
    expect(codeRows(html)).toEqual(['phase one ok', ' ', 'phase two ok']);
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
          { filename: 'shot.png', media_type: 'image/png', base64: 'iVBORw0KGgo=', size: 8 },
        ]}
      >
        {'look at this'}
      </UserMessage>,
    );

  it('reserves the picture box before a single byte has decoded', () => {
    expect(html()).toContain(mediaFrameClass);
  });

  it('never lets the picture size its own slot', () => {
    expect(html()).not.toMatch(/<img[^>]*\bw-auto\b/u);
    expect(html()).not.toMatch(/<img[^>]*\bh-auto\b/u);
  });
});

// Regression, TestFlight crash feedback: build 2875 rendered every collapsed tool result body,
// so a large transcript left WebKit with hundreds of thousands of DOM nodes until iOS killed it
// at the 2 GiB per-process limit.
describe('collapsed tool results', () => {
  it('does not mount result bodies before a card is opened', () => {
    const bodySentinel = 'UNMOUNTED_TOOL_RESULT_BODY';
    const turn: TranscriptTurn = {
      id: 'large-trace',
      status: 'completed',
      iterations: [
        {
          id: 'iteration-1',
          forms: Array.from({ length: 400 }, (_, index) => ({
            tool_name: 'shell',
            result: 'ok',
            result_summary: `summary ${index}`,
            result_render: `**STDOUT**\n\n\`\`\`\n${bodySentinel} ${index}\n\`\`\``,
          })),
        },
      ],
    };

    const html = renderToStaticMarkup(<AssistantMessage turn={turn} />);

    expect(count(html, /<details/g)).toBe(400);
    expect(html).toContain('summary 0');
    expect(html).toContain('summary 399');
    expect(html).not.toContain(bodySentinel);
  });
});
