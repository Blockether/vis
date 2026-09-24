// @vitest-environment jsdom
import { act, type ReactElement } from 'react';
import { createRoot } from 'react-dom/client';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { GatewayClient } from '../lib/gateway';
import { ArtifactLinkContext } from '../lib/artifact-links';
import { MarkdownArtifact, type DocumentChrome } from './MarkdownArtifact';
import { DocOverlay } from './DocArtifact';
import { DiffArtifact } from './DiffArtifact';
import { ImageViewer } from './ImageViewer';
import { diffReviewRequest } from '../lib/diff';
import { DIFF_MEDIA } from '../lib/artifacts';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/diff.json';

vi.mock('./TextArtifact', () => ({
  readArtifactText: async (source: string) => source,
}));
const text =
  '# Session search\n\n**Feature:** session-search\n**Status:** ready\n\n## Spec\nFind sessions.';
const chrome: DocumentChrome = ({ actions, note, body }) => (
  <div>
    <header>
      {actions}
      {note}
    </header>
    {body}
  </div>
);
const cleanups: (() => void)[] = [];
afterEach(() => cleanups.splice(0).forEach((cleanup) => cleanup()));
async function mount(node: ReactElement) {
  globalThis.IS_REACT_ACT_ENVIRONMENT = true;
  const host = document.createElement('div');
  document.body.append(host);
  const tree = createRoot(host);
  await act(async () => tree.render(node));
  cleanups.push(() => {
    act(() => tree.unmount());
    host.remove();
  });
  return { host, tree };
}
function client() {
  return {
    base: 'http://127.0.0.1',
    setting: vi.fn(async () => ({ enabled: true })),
    saveArtifactText: vi.fn(async () => ({ version: 4 })),
    submitTurn: vi.fn(async () => {}),
  } as unknown as GatewayClient;
}

describe('attachment review capability', () => {
  it.each([undefined, false])(
    'keeps a specification read-only when commentable is %s',
    async (commentable) => {
      const gateway = client();
      const { host } = await mount(
        <MarkdownArtifact
          client={gateway}
          sid="s"
          iterationId="i"
          name="PLAN-session-search.md"
          mediaType="text/markdown"
          source={text}
          version={3}
          commentable={commentable}
          chrome={chrome}
        />,
      );
      expect(host.textContent).toContain('Session search');
      expect(host.querySelectorAll('button')).toHaveLength(0);
      expect(host.querySelector('[aria-label="Specification workflow"]')).toBeNull();
      expect(gateway.setting).not.toHaveBeenCalled();
    },
  );
  it('only enables specification review for an explicitly commentable attachment', async () => {
    const { host } = await mount(
      <MarkdownArtifact
        client={client()}
        sid="s"
        iterationId="i"
        name="PLAN-session-search.md"
        mediaType="text/markdown"
        source={text}
        version={3}
        commentable
        chrome={chrome}
      />,
    );
    expect(host.querySelector('[aria-label="Comment on the whole document"]')).toBeInTheDocument();
    expect(host.textContent).toContain('Approve and start');
  });
  it('shows IMPLEMENTATION as a report without review controls', async () => {
    const { host } = await mount(
      <MarkdownArtifact
        client={client()}
        sid="s"
        iterationId="i"
        name="IMPLEMENTATION-session-search.md"
        mediaType="text/markdown"
        source={text}
        version={3}
        commentable={false}
        chrome={chrome}
      />,
    );
    expect(host.querySelectorAll('button')).toHaveLength(0);
    expect(host.textContent).toContain('Session search');
  });
  // Reported from an IMPLEMENTATION record: the relative diff link opened an app URL.
  it('opens a linked diff without navigating the application', async () => {
    const opened = vi.fn();
    const closed = vi.fn();
    const diffId = '8e3a587d-232c-497d-a290-7d16cfcf0e02';
    const { host } = await mount(
      <ArtifactLinkContext.Provider
        value={{ byName: new Map([['DIFF-ungroup-hover-hint.json', diffId]]), open: opened }}
      >
        <DocOverlay
          name="IMPLEMENTATION-ungroup-hover-hint.md"
          mime="text/markdown"
          url={`# Report

[Review diff](DIFF-ungroup-hover-hint.json) · [Missing](missing.json) · [Website](https://example.com)`}
          failed={false}
          annotate={{ client: client(), sid: 's', iterationId: 'i' }}
          onClose={closed}
        />
      </ArtifactLinkContext.Provider>,
    );
    expect(host.querySelector('a[href="DIFF-ungroup-hover-hint.json"]')).toBeNull();
    expect(host.querySelector('a[href="missing.json"]')).toBeNull();
    expect(host.querySelector('a[href="https://example.com"]')?.getAttribute('target')).toBe(
      '_blank',
    );
    const diff = [...host.querySelectorAll('button')].find(
      (button) => button.textContent === 'Review diff',
    );
    expect(diff).toBeInTheDocument();
    await act(async () => diff!.click());
    expect(closed).toHaveBeenCalledOnce();
    expect(opened).toHaveBeenCalledExactlyOnceWith(diffId);
  });
  it('uses the selected cut capability rather than the latest document capability', async () => {
    const gateway = client();
    const versions = [
      { index: 1, version: 2, commentable: true },
      { index: 0, version: 1 },
    ];
    const props = {
      name: 'note.md',
      mime: 'text/markdown',
      url: text,
      failed: false,
      annotate: { client: gateway, sid: 's', iterationId: 'i' },
      commentable: true,
      versions,
      onClose: () => {},
    };
    const { host, tree } = await mount(<DocOverlay {...props} shownAt={0} />);
    expect(host.querySelector('[aria-label="Comment on the whole document"]')).toBeInTheDocument();
    await act(async () => tree.render(<DocOverlay {...props} shownAt={1} />));
    expect(host.querySelector('[aria-label="Comment on the whole document"]')).toBeNull();
    expect(host.querySelector('[aria-label="Save changes"]')).toBeNull();
    expect(host.textContent).toContain('Session search');
  });
  it('keeps an image readable without drawing or trimming when review was not enabled', async () => {
    const { host } = await mount(
      <ImageViewer src="blob:picture" name="chart.png" onClose={() => {}} />,
    );
    // ImageViewer portals its screen outside the local host.
    expect(document.querySelector('[aria-label="Draw on image"]')).toBeNull();
    expect(document.querySelector('[aria-label="Trim to view"]')).toBeNull();
    expect(document.querySelector('[aria-label="Copy image"]')).toBeInTheDocument();
    expect(host).toBeEmptyDOMElement();
  });
});

async function press(host: HTMLElement, label: string) {
  const button = [...host.querySelectorAll('button')].find(
    (entry) => entry.getAttribute('aria-label') === label || entry.textContent?.trim() === label,
  );
  expect(button, label).toBeInTheDocument();
  await act(async () => button!.click());
}
async function addComment(host: HTMLElement, body: string) {
  await press(host, 'Comment on the whole document');
  const input = host.querySelector('textarea')!;
  await act(async () => {
    Object.getOwnPropertyDescriptor(HTMLTextAreaElement.prototype, 'value')!.set!.call(input, body);
    input.dispatchEvent(new Event('input', { bubbles: true }));
  });
  await press(host, 'Add comment');
}
let diffSession = 0;
function diffProps(gateway = client(), source = JSON.stringify({ ...fixture, comments: [] })) {
  return {
    client: gateway,
    sid: `diff-${++diffSession}`,
    iterationId: 'i',
    name: 'DIFF-search.json',
    source,
    version: 3,
    chrome,
  };
}

describe('diff review', () => {
  it('is read-only by default and renders the patch instead of its JSON container', async () => {
    const { host } = await mount(<DiffArtifact {...diffProps()} />);
    expect(host.textContent).toContain('+(search title {:archived true})');
    expect(host.textContent).not.toContain('schema_version');
    expect(host.querySelectorAll('button')).toHaveLength(0);
  });
  it('explains empty and invalid snapshots without offering review controls for invalid bytes', async () => {
    const props = diffProps(client(), JSON.stringify({ ...fixture, patch: '', comments: [] }));
    const { host, tree } = await mount(<DiffArtifact {...props} />);
    expect(host.textContent).toContain('No changes in this snapshot.');
    await act(async () => tree.render(<DiffArtifact {...props} source="not JSON" commentable />));
    expect(host.querySelector('[role="alert"]')?.textContent).toContain('Ask for a new snapshot');
    expect(host.querySelectorAll('button')).toHaveLength(0);
  });
  it('saves only comments and sends the whole round for the confirmed version', async () => {
    const gateway = client();
    const patch = fixture.patch + ' \r\n\n';
    const props = diffProps(gateway, JSON.stringify({ ...fixture, patch, comments: [] }));
    const { host } = await mount(<DiffArtifact {...props} commentable />);
    expect(host.textContent).not.toContain('Approve and start');
    expect(host.querySelector('[aria-label="Save changes"]')).toBeNull();
    await addComment(host, 'Keep archived optional.');
    expect(gateway.saveArtifactText).not.toHaveBeenCalled();
    expect(gateway.submitTurn).not.toHaveBeenCalled();
    await press(host, 'Send for revision');
    const call = vi.mocked(gateway.saveArtifactText).mock.calls[0];
    expect(call.slice(0, 4)).toEqual([props.sid, 'i', 'DIFF-search.json', DIFF_MEDIA]);
    const saved = JSON.parse(call[4]);
    expect(saved.patch).toBe(patch);
    expect(saved.source).toEqual(fixture.source);
    expect(saved.comments).toEqual([{ quote: '', body: 'Keep archived optional.' }]);
    expect(gateway.submitTurn).toHaveBeenCalledExactlyOnceWith(
      props.sid,
      diffReviewRequest(props.name, 4),
    );
    expect(host.textContent).toContain('Revision requested for v4');
  });
  it('retries a failed send without saving the same comment round twice', async () => {
    const gateway = client();
    vi.mocked(gateway.submitTurn).mockRejectedValueOnce(new Error('Disconnected. Try again.'));
    const { host } = await mount(<DiffArtifact {...diffProps(gateway)} commentable />);
    await addComment(host, 'Retry this review.');
    await press(host, 'Send for revision');
    expect(host.textContent).toContain('Disconnected. Try again.');
    await press(host, 'Send for revision');
    expect(gateway.saveArtifactText).toHaveBeenCalledTimes(1);
    expect(gateway.submitTurn).toHaveBeenCalledTimes(2);
  });
  it('sends existing comments without rewriting the snapshot', async () => {
    const gateway = client();
    const props = diffProps(gateway, JSON.stringify(fixture));
    const { host } = await mount(<DiffArtifact {...props} commentable />);
    const highlighted = [...host.querySelectorAll('p')].find(
      (line) => line.textContent === fixture.comments[0].quote,
    );
    expect(highlighted?.style.color).toBe('var(--foreground)');
    await press(host, 'Send for revision');
    expect(gateway.saveArtifactText).not.toHaveBeenCalled();
    expect(gateway.submitTurn).toHaveBeenCalledExactlyOnceWith(
      props.sid,
      diffReviewRequest(props.name, 3),
    );
  });
  it('retains comments after a save failure and does not send an unconfirmed version', async () => {
    const gateway = client();
    vi.mocked(gateway.saveArtifactText).mockRejectedValueOnce(new Error('Save unavailable.'));
    const { host } = await mount(<DiffArtifact {...diffProps(gateway)} commentable />);
    await addComment(host, 'Do not lose this comment.');
    await press(host, 'Send for revision');
    expect(host.textContent).toContain('Save unavailable.');
    expect(host.textContent).toContain('Do not lose this comment.');
    expect(gateway.submitTurn).not.toHaveBeenCalled();
    await press(host, 'Send for revision');
    expect(gateway.saveArtifactText).toHaveBeenCalledTimes(2);
    expect(gateway.submitTurn).toHaveBeenCalledTimes(1);
  });
});
