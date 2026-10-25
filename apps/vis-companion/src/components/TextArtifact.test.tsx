import { renderToStaticMarkup } from 'react-dom/server';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { clampArtifactText, readArtifactText, TEXT_ARTIFACT_LIMIT, TextBody } from './TextArtifact';

const html = (node: Parameters<typeof renderToStaticMarkup>[0]) => renderToStaticMarkup(node);

afterEach(() => {
  vi.unstubAllGlobals();
});

// Regression: a markdown artifact was handed to the document iframe, which
// painted `# Heading` as `# Heading` — the source of the note instead of the
// note — and the app has exactly one markdown renderer that already knows
// better.
describe('a written artifact', () => {
  it('renders subtitles as a timed transcript with one name per turn', () => {
    const markup = html(
      <TextBody
        text={'WEBVTT\n\n00:00:00.300 --> 00:00:02.000\n<v Maya>Welcome.\n\n00:00:02.000 --> 00:00:03.000\n<v Maya>Again.\n\n00:01:05.000 --> 00:01:06.000\n<v Daniel>Hi.\n'}
        mime="text/vtt"
        name="episode.vtt"
      />,
    );
    expect(markup).toContain('<ol');
    expect(markup).toContain('1:05');
    expect(markup.match(/>Maya</g)).toHaveLength(1);
    expect(markup).toContain('>Daniel<');
    expect(markup).not.toContain('--&gt;');
  });

  it('renders markdown as prose, not as its own source', () => {
    const markup = html(
      <TextBody
        text={'# Ship it\n\n- one\n- two\n'}
        mime="text/markdown"
        name="vis-issue-115-comment.md"
      />,
    );
    expect(markup).toContain('<h1');
    expect(markup).toContain('Ship it');
    expect(markup).toContain('<li');
    expect(markup).not.toContain('# Ship it');
  });

  it('trusts the name when the gateway only guessed a generic type', () => {
    const markup = html(
      <TextBody text={'## Heading'} mime="application/octet-stream" name="notes.md" />,
    );
    expect(markup).toContain('<h2');
  });

  it('paints anything else verbatim, wrapped and monospaced', () => {
    const markup = html(
      <TextBody text={'# not markdown\n  spaced'} mime="text/plain" name="a.log" />,
    );
    expect(markup).toContain('<pre');
    expect(markup).toContain('tabindex="0"');
    expect(markup).toContain('aria-label="a.log"');
    expect(markup).toContain('whitespace-pre-wrap');
    expect(markup).toContain('# not markdown');
  });
});

describe('reading the bytes', () => {
  it('never asks a phone to lay out a whole log', () => {
    const clamped = clampArtifactText('x'.repeat(TEXT_ARTIFACT_LIMIT + 10));
    expect(clamped.length).toBeLessThan(TEXT_ARTIFACT_LIMIT + 40);
    expect(clamped).toContain('… truncated');
    expect(clampArtifactText('short')).toBe('short');
  });

  it('reads the artifact url as text', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(async () => new Response('# hi', { status: 200 })),
    );
    expect(await readArtifactText('blob:one')).toBe('# hi');
  });

  it('refuses a failed read instead of painting an empty note', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(async () => new Response('nope', { status: 404 })),
    );
    await expect(readArtifactText('blob:two')).rejects.toThrow('404');
  });
});

// A plan sitting INLINE in the session is source, not a rendered document: the
// turn that produced it was being swallowed by a wall of headings.
describe('a markdown artifact inline in the session', () => {
  it('shows the source verbatim when asked for the raw form', () => {
    const markup = html(
      <TextBody text={'# Ship it\n\n- one\n'} mime="text/markdown" name="PLAN.md" raw />,
    );
    expect(markup).toContain('<pre');
    expect(markup).toContain('# Ship it');
    expect(markup).not.toContain('<h1');
  });
});
