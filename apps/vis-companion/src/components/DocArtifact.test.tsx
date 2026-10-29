// @vitest-environment jsdom
import { act, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { renderToStaticMarkup } from 'react-dom/server';
import { afterEach, describe, expect, it, vi } from 'vitest';
import {
  DocFrame,
  DocOverlay,
  DocPreview,
  DOC_LINK_BRIDGE,
  OverlayScreen,
  docAgentNote,
  docErrorText,
  docLinkHref,
  docSandbox,
  docStackSummary,
  withDocLinkBridge,
} from './DocArtifact';
import { dismissTopLayer } from '../lib/edge-back';
import { VIEW_AGENT_NOTE_MAX } from '../lib/view';

/** Visible text of a rendered chunk: tags out, entities back. */
const text = (html: string) =>
  html
    .replace(/<[^>]+>/g, ' ')
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, '>')
    .replace(/&lt;/g, '<')
    .replace(/&amp;/g, '&');

afterEach(() => vi.unstubAllGlobals());

// An attached page is UNTRUSTED markup. It renders in an iframe, which is its
// own document with its own CSS scope, and the sandbox is what makes that a
// security boundary: a blob: URL inherits the app's origin, so `allow-same-origin`
// would hand the artifact the app's storage and the gateway's bearer token.
describe('sandboxing', () => {
  it('never grants an artifact the app origin', () => {
    expect(docSandbox('text/html')).not.toContain('allow-same-origin');
    expect(docSandbox('application/pdf')).not.toContain('allow-same-origin');
  });

  it('never lets an artifact act on the app around it', () => {
    for (const mime of ['text/html', 'application/xhtml+xml', 'application/pdf']) {
      expect(docSandbox(mime)).not.toContain('allow-top-navigation');
      expect(docSandbox(mime)).not.toContain('allow-popups');
    }
  });

  // Regression: an attached design page rendered as dead paper — the frame was
  // mounted without `allow-scripts` until someone pressed a strip above it, so
  // a document whose layout its own script builds arrived half-painted, and the
  // reader had to know that a press existed. The sandbox is the boundary; there
  // is nothing left to grant.
  it("runs an attached page's own script", () => {
    expect(docSandbox('text/html')).toContain('allow-scripts');
    expect(docSandbox('application/xhtml+xml')).toContain('allow-scripts');
  });

  // Chromium's built-in viewer refuses to paint without it, and what runs there
  // is the viewer, never the artifact.
  it('allows the browser PDF viewer', () => {
    expect(docSandbox('application/pdf')).toBe('allow-scripts');
  });

  it('paints the artifact inside a sandboxed frame', () => {
    const html = renderToStaticMarkup(<DocFrame url="blob:x" mime="text/html" name="page.html" />);
    expect(html).toContain('<iframe');
    expect(html).toContain('sandbox="allow-scripts allow-forms');
    expect(html).toContain('title="page.html"');
    expect(html).not.toContain('allow-same-origin');
  });
});

// Regression for #382: a report's links to Jira, Gerrit or Jenkins did nothing. The
// sandbox refuses popups and framing sites paint blank, so links leave through the app.
describe('links in a framed page', () => {
  afterEach(() => vi.restoreAllMocks());

  const latin1 = (bytes: Uint8Array) => new TextDecoder('latin1').decode(bytes);
  const page = (source: string) => new TextEncoder().encode(source);

  it('runs the link bridge first inside the head and keeps every other byte', () => {
    const bytes = Uint8Array.from([...page('<!DOCTYPE html><html><head><title>caf'), 0xe9, ...page('</title>')]);
    const out = latin1(withDocLinkBridge(bytes));
    expect(out).toBe(
      `<!DOCTYPE html><html><head><script>${DOC_LINK_BRIDGE}</script><title>caf\u00e9</title>`,
    );
  });

  it('falls back to the html tag, then the doctype, then the start', () => {
    expect(latin1(withDocLinkBridge(page('<html lang="en"><p>x')))).toMatch(/^<html lang="en"><script>/);
    expect(latin1(withDocLinkBridge(page('<!doctype html><p>x')))).toMatch(/^<!doctype html><script>/);
    expect(latin1(withDocLinkBridge(page('<p>x')))).toMatch(/^<script>/);
    const bom = withDocLinkBridge(Uint8Array.from([0xef, 0xbb, 0xbf, ...page('<p>x')]));
    expect([...bom.subarray(0, 3)]).toEqual([0xef, 0xbb, 0xbf]);
    expect(latin1(bom.subarray(3))).toMatch(/^<script>/);
  });

  it('leaves a UTF-16 page as it is', () => {
    const bytes = Uint8Array.from([0xff, 0xfe, 0x3c, 0x00]);
    expect(withDocLinkBridge(bytes)).toBe(bytes);
  });

  it('stays well-formed inside an XHTML page', () => {
    expect(DOC_LINK_BRIDGE).not.toMatch(/[<&]/);
  });

  it('accepts only web and mail links', () => {
    const open = (href: unknown) => docLinkHref({ type: 'vis-doc-open', href });
    expect(open('https://example.org/a b')).toBe('https://example.org/a%20b');
    expect(open('http://jenkins.example.com/job/1')).toBe('http://jenkins.example.com/job/1');
    expect(open('mailto:team@example.com')).toBe('mailto:team@example.com');
    for (const href of ['javascript:alert(1)', 'blob:https://app/x', 'data:text/html,x', 'file:///etc', 'x', 1]) {
      expect(open(href)).toBeNull();
    }
    expect(docLinkHref({ type: 'other', href: 'https://example.org' })).toBeNull();
    expect(docLinkHref('https://example.org')).toBeNull();
    expect(docLinkHref(null)).toBeNull();
  });

  /** A frame of the test document with the bridge running in it, as in the artifact. */
  function bridgedFrame() {
    const frame = document.createElement('iframe');
    document.body.append(frame);
    const win = frame.contentWindow as Window & typeof globalThis;
    new Function('window', DOC_LINK_BRIDGE)(win);
    const posted = vi.spyOn(window, 'postMessage').mockImplementation(() => {});
    const link = (href: string, target?: string) => {
      const anchor = win.document.createElement('a');
      anchor.setAttribute('href', href);
      if (target) anchor.setAttribute('target', target);
      anchor.textContent = href;
      win.document.body.append(anchor);
      return anchor;
    };
    const click = (anchor: HTMLAnchorElement) => {
      const event = new win.MouseEvent('click', { bubbles: true, cancelable: true, button: 0 });
      anchor.dispatchEvent(event);
      return event;
    };
    return { frame, win, posted, link, click };
  }

  it('sends a followed link to the app instead of loading it in the frame', () => {
    const { frame, posted, link, click } = bridgedFrame();
    for (const target of [undefined, '_blank']) {
      posted.mockClear();
      const event = click(link('https://example.org/', target));
      expect(event.defaultPrevented).toBe(true);
      expect(posted).toHaveBeenCalledWith({ type: 'vis-doc-open', href: 'https://example.org/' }, '*');
    }
    frame.remove();
  });

  it('keeps in-page anchors, script links and clicks the page handles itself', () => {
    const { frame, posted, link, click } = bridgedFrame();
    expect(click(link('#results')).defaultPrevented).toBe(false);
    expect(click(link('javascript:void(0)')).defaultPrevented).toBe(false);
    const handled = link('https://example.org/');
    handled.addEventListener('click', (event) => event.preventDefault());
    click(handled);
    expect(posted).not.toHaveBeenCalled();
    frame.remove();
  });

  it('sends window.open to the app and gives the page no window', () => {
    const { frame, win, posted } = bridgedFrame();
    expect(win.open('https://github.com/', '_blank')).toBeNull();
    expect(posted).toHaveBeenCalledWith({ type: 'vis-doc-open', href: 'https://github.com/' }, '*');
    frame.remove();
  });

  // Regression #418: a single-file app read localStorage in the opaque frame, got a
  // SecurityError and painted nothing under its fixed banner.
  it('stands in for each store that the sandbox refuses and keeps one that works', () => {
    const frame = document.createElement('iframe');
    document.body.append(frame);
    const win = frame.contentWindow as Window & typeof globalThis;
    const refuse = () => {
      throw new DOMException('The document is sandboxed', 'SecurityError');
    };
    Object.defineProperty(win, 'localStorage', { get: refuse, configurable: true });
    Object.defineProperty(win.document, 'cookie', { get: refuse, set: refuse, configurable: true });
    const session = win.sessionStorage;
    new Function('window', DOC_LINK_BRIDGE)(win);
    win.localStorage.setItem('theme', 'dark');
    expect(win.localStorage.getItem('theme')).toBe('dark');
    expect(win.localStorage.getItem('other')).toBeNull();
    expect(win.localStorage.length).toBe(1);
    expect(win.localStorage.key(0)).toBe('theme');
    win.localStorage.clear();
    expect(win.localStorage.length).toBe(0);
    expect(win.sessionStorage).toBe(session);
    win.document.cookie = 'a=1; path=/';
    win.document.cookie = 'b=2';
    expect(win.document.cookie).toBe('a=1; b=2');
    win.document.cookie = 'a=; max-age=0';
    expect(win.document.cookie).toBe('b=2');
    frame.remove();
  });

  it("sends the page's uncaught errors and rejections to the app", () => {
    const { frame, win, posted } = bridgedFrame();
    win.dispatchEvent(new win.ErrorEvent('error', { message: 'Uncaught TypeError: Invalid URL' }));
    expect(posted).toHaveBeenCalledWith({ type: 'vis-doc-error', message: 'Uncaught TypeError: Invalid URL' }, '*');
    const rejection = Object.assign(new win.Event('unhandledrejection'), { reason: new win.Error('no data') });
    win.dispatchEvent(rejection);
    expect(posted).toHaveBeenCalledWith({ type: 'vis-doc-error', message: 'Error: no data' }, '*');
    frame.remove();
  });

  it('accepts only a non-empty error text', () => {
    expect(docErrorText({ type: 'vis-doc-error', message: ' boom ' })).toBe('boom');
    expect(docErrorText({ type: 'vis-doc-error', message: 'x'.repeat(400) })).toHaveLength(300);
    expect(docErrorText({ type: 'vis-doc-error', message: ' ' })).toBeNull();
    expect(docErrorText({ type: 'vis-doc-error', message: 1 })).toBeNull();
    expect(docErrorText({ type: 'vis-doc-open', message: 'boom' })).toBeNull();
    expect(docErrorText(null)).toBeNull();
  });

  it('sends the note for the model to the app and refuses a note that is not text or too long', () => {
    const { frame, win, posted } = bridgedFrame();
    const { vis } = win as unknown as { vis: { setAgentNote: (text: unknown) => void } };
    vis.setAgentNote('Row 3 is selected');
    expect(posted).toHaveBeenCalledWith({ type: 'vis-doc-agent-note', text: 'Row 3 is selected' }, '*');
    posted.mockClear();
    expect(() => vis.setAgentNote(3)).toThrow(win.TypeError);
    expect(() => vis.setAgentNote('x'.repeat(VIEW_AGENT_NOTE_MAX + 1))).toThrow(win.RangeError);
    expect(posted).not.toHaveBeenCalled();
    vis.setAgentNote('x'.repeat(VIEW_AGENT_NOTE_MAX));
    expect(posted).toHaveBeenCalledTimes(1);
    frame.remove();
  });

  it('accepts only a note of text within the bound', () => {
    expect(docAgentNote({ type: 'vis-doc-agent-note', text: ' Row 3 ' })).toBe(' Row 3 ');
    expect(docAgentNote({ type: 'vis-doc-agent-note', text: '' })).toBe('');
    expect(docAgentNote({ type: 'vis-doc-agent-note', text: 'x'.repeat(VIEW_AGENT_NOTE_MAX + 1) })).toBeNull();
    expect(docAgentNote({ type: 'vis-doc-agent-note', text: 1 })).toBeNull();
    expect(docAgentNote({ type: 'vis-doc-error', text: 'Row 3' })).toBeNull();
    expect(docAgentNote(null)).toBeNull();
  });

  async function framedDoc(onAgentNote?: (text: string) => void) {
    const made: Blob[] = [];
    const create = URL.createObjectURL;
    const revoke = URL.revokeObjectURL;
    URL.createObjectURL = (blob: Blob | MediaSource) => `blob:bridged/${made.push(blob as Blob)}`;
    const revoked = vi.fn();
    URL.revokeObjectURL = revoked;
    const source = '<!DOCTYPE html><head></head><a href="https://example.org">x</a>';
    vi.stubGlobal(
      'fetch',
      vi.fn().mockResolvedValue({ blob: async () => new Blob([source], { type: 'text/html; charset=utf-8' }) }),
    );
    const view = render(
      <DocFrame url="blob:shared" mime="text/html" name="links.html" onAgentNote={onAgentNote} />,
    );
    const frame = view.container.querySelector('iframe') as HTMLIFrameElement;
    await vi.waitFor(() => expect(frame.getAttribute('src')).toBe('blob:bridged/1'));
    const opened = vi.spyOn(window, 'open').mockImplementation(() => null);
    const send = (data: unknown, source: Window | null = frame.contentWindow) =>
      act(() => {
        window.dispatchEvent(new MessageEvent('message', { data, source }));
      });
    const restore = () => {
      view.unmount();
      URL.createObjectURL = create;
      URL.revokeObjectURL = revoke;
    };
    return { made, frame, opened, send, restore, revoked, source };
  }

  it('frames its own copy of the page with the bridge in it', async () => {
    const { made, restore, revoked, source } = await framedDoc();
    expect(made[0].type).toBe('text/html; charset=utf-8');
    expect(made[0].size).toBe(new TextEncoder().encode(source).length + `<script>${DOC_LINK_BRIDGE}</script>`.length);
    restore();
    expect(revoked).toHaveBeenCalledWith('blob:bridged/1');
  });

  it('opens a link from its own frame as an external link', async () => {
    const { opened, send, restore } = await framedDoc();
    send({ type: 'vis-doc-open', href: 'https://example.org/' });
    expect(opened).toHaveBeenCalledWith('https://example.org/', '_blank', 'noopener,noreferrer');
    restore();
  });

  it('ignores other windows, other schemes and a page without a click', async () => {
    const { opened, send, restore } = await framedDoc();
    send({ type: 'vis-doc-open', href: 'https://example.org/' }, window);
    send({ type: 'vis-doc-open', href: 'javascript:alert(1)' });
    Object.defineProperty(navigator, 'userActivation', { value: { isActive: false }, configurable: true });
    try {
      send({ type: 'vis-doc-open', href: 'https://example.org/' });
    } finally {
      delete (navigator as { userActivation?: unknown }).userActivation;
    }
    expect(opened).not.toHaveBeenCalled();
    restore();
  });

  // Regression #418: the page stopped on an error and the viewer showed only its banner.
  it("shows the frame's first error over the page", async () => {
    const { send, restore } = await framedDoc();
    expect(screen.queryByRole('alert')).toBeNull();
    send({ type: 'vis-doc-error', message: 'boom' }, window);
    expect(screen.queryByRole('alert')).toBeNull();
    send({ type: 'vis-doc-error', message: "Uncaught SecurityError: Failed to read 'localStorage'" });
    send({ type: 'vis-doc-error', message: 'Uncaught TypeError: later' });
    expect(screen.getByRole('alert').textContent).toBe(
      "This page reported an error: Uncaught SecurityError: Failed to read 'localStorage'",
    );
    restore();
  });

  it('saves the last note after a pause, and a waiting note when the page closes', async () => {
    const saved = vi.fn();
    const { send, restore } = await framedDoc(saved);
    vi.useFakeTimers();
    try {
      send({ type: 'vis-doc-agent-note', text: 'Row 1' });
      send({ type: 'vis-doc-agent-note', text: 'Row 2' });
      send({ type: 'vis-doc-agent-note', text: 'Row 9' }, window);
      vi.advanceTimersByTime(399);
      expect(saved).not.toHaveBeenCalled();
      vi.advanceTimersByTime(1);
      expect(saved).toHaveBeenCalledTimes(1);
      expect(saved).toHaveBeenCalledWith('Row 2');
      send({ type: 'vis-doc-agent-note', text: '' });
      restore();
      expect(saved).toHaveBeenCalledTimes(2);
      expect(saved).toHaveBeenLastCalledWith('');
    } finally {
      vi.useRealTimers();
    }
  });
});

// Regression: a document artifact stood in the transcript twice — the `vis-doc`
// fence painted a card and the attachment rail painted a tile for the very same
// file — and the tile embedded the bytes, so a note taller than the turn that
// produced it had to be scrolled past to reach the next line of the
// conversation.
describe('DocPreview', () => {
  const preview = (mime = 'application/pdf', name = 'report.pdf') =>
    renderToStaticMarkup(
      <DocPreview
        name={name}
        mime={mime}
        sizeLabel="1.2 MB"
        url="blob:x"
        failed={false}
        onNeeded={() => undefined}
      />,
    );

  it('names the artifact without painting a byte of it', () => {
    const html = preview();
    expect(text(html)).toContain('report.pdf');
    expect(text(html)).toContain('1.2 MB');
    expect(html).not.toContain('<iframe');
    expect(html).not.toContain('60vh');
  });

});

// Reported ("I love 4"): every document artifact was its own framed card with its own
// kind chip and its own Open, so a step that wrote four files read as four settings
// panels stacked down the transcript instead of as one thing the step produced.
describe('DocStack', () => {
  it('claims a weight only when every document reported one', () => {
    expect(docStackSummary([{ size: 1024 }, { size: 2048 }])).toBe('2 documents · 3.0KB');
    expect(docStackSummary([{ size: 1024 }, {}])).toBe('2 documents');
    expect(docStackSummary([{ size: 1024 }])).toBe('1 document · 1.0KB');
  });
});

// Regression: the opened document was read in a letterbox — the frame sized
// itself to 60vh inside a padded scroller — and the session's composer strip
// still stood at the bottom of the screen over it.
describe('an opened document', () => {

  // Shares from a document opened in the session, like the artifacts sheet does.
  // The outcome must not become a second header line: the band grew after Share.
  it('shares the original document and keeps the outcome out of the band', async () => {
    const blob = new Blob(['%PDF'], { type: 'application/pdf' });
    const share = vi.fn(async (_data: ShareData) => undefined);
    // Node's Response cannot read a jsdom Blob: Node 22 throws, and later versions send "[object Blob]".
    vi.stubGlobal('fetch', vi.fn(async () => ({ blob: async () => blob }) as Response));
    Object.defineProperty(navigator, 'share', { value: share, configurable: true });
    try {
      render(
        <DocOverlay
          name="shared.pdf"
          mime="application/pdf"
          sizeLabel="1.2 MB"
          url="blob:shared"
          failed={false}
          onClose={() => undefined}
        />,
      );
      await userEvent.click(screen.getByRole('button', { name: 'Share' }));
      // The status region exists before the share ends, so wait for its text, not for the region.
      await waitFor(() =>
        expect(screen.getAllByRole('status').some((status) => status.textContent === 'Artifact shared.')).toBe(
          true,
        ),
      );
      expect(share.mock.calls[0]?.[0]).toEqual({ files: [expect.any(File)] });
      expect(share.mock.calls[0]?.[0].files?.[0]).toMatchObject({
        name: 'shared.pdf',
        type: 'application/pdf',
        size: blob.size,
      });
      const band = screen.getByRole('heading', { name: 'shared.pdf' }).closest('header');
      expect(band).toHaveTextContent('shared.pdf');
      expect(band).not.toHaveTextContent('Artifact shared.');
    } finally {
      Reflect.deleteProperty(navigator, 'share');
    }
  });

  it('reports a failed share over the body, not in the band', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => Promise.reject(new Error('offline'))));
    Object.defineProperty(navigator, 'share', { value: vi.fn(), configurable: true });
    try {
      render(
        <DocOverlay
          name="offline.pdf"
          mime="application/pdf"
          url="blob:offline"
          failed={false}
          onClose={() => undefined}
        />,
      );
      await userEvent.click(screen.getByRole('button', { name: 'Share' }));
      expect(await screen.findByRole('alert')).toHaveTextContent('Could not share artifact.');
      const band = screen.getByRole('heading', { name: 'offline.pdf' }).closest('header');
      expect(band).not.toHaveTextContent('Could not share artifact.');
    } finally {
      Reflect.deleteProperty(navigator, 'share');
    }
  });

  // One document per test: `useStickyOverlay` remembers an opened document BY NAME
  // across mounts, so a second test reusing the name opens on arrival.
  const openPreview = async (name: string) => {
    const view = render(
      <DocPreview
        name={name}
        mime="application/pdf"
        sizeLabel="1.2 MB"
        url="blob:x"
        failed={false}
        onNeeded={() => undefined}
      />,
    );
    await userEvent.click(screen.getByText(name));
    const close = document.querySelector(`[aria-label="Close ${name}"]`)!;
    expect(close).toBeInTheDocument();
    return { view, screenEl: close.closest('div[class*=inset-0]') as HTMLElement };
  };

  // No shell yet — the layer still leaves the transcript, so the composer strip the
  // session screen pins to the bottom cannot paint on top of it.
  it('falls back to the body, fixed, when there is no shell', async () => {
    const { view, screenEl } = await openPreview('bodied.pdf');
    expect(view.container.contains(screenEl)).toBe(false);
    expect(screenEl.closest('body')).toBe(document.body);
    view.unmount();
  });

});

// Regression, user report: "the model attached me a document, then I commented on
// it and saved, and suddenly instead of the original one I see two". The rail now
// paints ONE row per name (`collapseAttachmentVersions`), so the row itself has to
// say that the version moved — and the thread it stands for opens from the band of
// the document it opens, never from a second row in the transcript.
describe('a revised document', () => {
  const cuts = [
    {
      filename: 'PLAN.md',
      index: 1,
      iteration_id: 'i1',
      version: 2,
      size: 12_700,
    },
    {
      filename: 'PLAN.md',
      index: 0,
      iteration_id: 'i1',
      version: 1,
      size: 13_000,
    },
  ];

  it('says the version moved in the row the document already had', () => {
    const html = renderToStaticMarkup(
      <DocPreview
        name="PLAN.md"
        mime="text/markdown"
        sizeLabel="12.4KB"
        url="blob:x"
        failed={false}
        versions={cuts}
        onNeeded={() => undefined}
      />,
    );
    expect(text(html)).toContain('v2');
    // A revision is a LINE in the row, never a second row: one name, one chevron.
    expect(html.split('aria-label="Open PLAN.md"').length - 1).toBe(1);
  });

  it('opens the thread from the BAND and reads the cut that is picked', async () => {
    const picked: number[] = [];
    render(
      <DocOverlay
        name="THREAD.md"
        mime="text/markdown"
        sizeLabel="12.4KB"
        url={null}
        failed={false}
        versions={cuts}
        shownAt={0}
        onPick={(at) => picked.push(at)}
        onClose={() => undefined}
      />,
    );
    const cell = screen.getByLabelText('Versions of THREAD.md');
    expect(cell.textContent).toContain('v2 of 2');

    await userEvent.click(cell);
    expect(screen.getByText('2 versions')).toBeVisible();

    await userEvent.click(screen.getByLabelText('Read v1 of THREAD.md'));
    expect(picked).toEqual([1]);
    // Picking closes the thread again: the document is what the screen is for.
    expect(screen.queryByText('2 versions')).toBeNull();
  });
});

// Reported with the back stroke: Android's back is ONE event for the whole shell, so an
// opened document used to carry the reader out of the session standing under it. The
// document is the layer on top, and back is the door Escape is for a keyboard.
describe('an opened document answering the phone', () => {
  it('closes itself rather than the session under it', () => {
    const onClose = vi.fn();
    const view = render(
      <OverlayScreen title="q3-report.pdf" onClose={onClose}>
        <p>the document</p>
      </OverlayScreen>,
    );

    let spent = false;
    act(() => {
      spent = dismissTopLayer();
    });

    expect(spent).toBe(true);
    expect(onClose).toHaveBeenCalledTimes(1);

    view.unmount();
    expect(dismissTopLayer()).toBe(false);
  });
});
