// @vitest-environment jsdom
import { fireEvent, render, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { UserMessage } from './ChatContent';
import { shareArtifact } from '../lib/artifact-share';

vi.mock('../lib/artifact-share', () => ({
  artifactShareVerb: () => 'Save',
  shareArtifact: vi.fn(async () => 'Artifact saved.'),
}));
const log = {
  filename: 'vis-diagnostics.jsonl.gz',
  media_type: 'application/gzip',
  base64:
    'H4sIAAAAAAAC/w3KMQ6AIAxG4Z1j/LMMrtyGQNVGLMQ2LoS72/V7b+JmqUj4WGMeI1bOp3Q1LooNWi56MtK+wgR9JOarWn6Nqufm1FxYjo4VfvjpmRxNAAAA',
  size: 90,
};
afterEach(() => {
  vi.unstubAllGlobals();
  vi.clearAllMocks();
});

it('hands original log bytes to the existing save/share boundary', async () => {
  const blob = new Blob([Uint8Array.from(atob(log.base64), (c) => c.charCodeAt(0))], {
    type: log.media_type,
  });
  vi.stubGlobal(
    'fetch',
    vi.fn(async () => ({ blob: async () => blob })),
  );
  const view = render(<UserMessage attachments={[log]}>{''}</UserMessage>);
  fireEvent.click(view.getByRole('button', { name: `Save ${log.filename}` }));
  await waitFor(() =>
    expect(shareArtifact).toHaveBeenCalledWith(blob, log.filename, log.media_type),
  );
  expect(view.getByRole('status')).toHaveTextContent('Artifact saved.');
});

it('keeps a failed save retryable and missing bytes visible', async () => {
  vi.stubGlobal(
    'fetch',
    vi.fn(async () => {
      throw new Error('Read failed');
    }),
  );
  const view = render(<UserMessage attachments={[log]}>{''}</UserMessage>);
  fireEvent.click(view.getByRole('button', { name: `Save ${log.filename}` }));
  expect(await view.findByRole('status')).toHaveTextContent('Could not share file. Try again.');
  expect(view.getByRole('button', { name: `Save ${log.filename}` })).toBeEnabled();
  view.rerender(<UserMessage attachments={[{ ...log, base64: '' }]}>{''}</UserMessage>);
  expect(view.getByRole('button', { name: `Save ${log.filename}` })).toBeDisabled();
  expect(view.getByText(log.filename)).toBeVisible();
});

// Regression, issue vis_session_id#83d1d828-d2a1-45b2-bbdc-4a5fea1ec354: a sent clip
// sat as a grey plate on iOS until it played, and its plate could not share it.
const clip = {
  filename: 'brag.mp4',
  media_type: 'video/mp4',
  base64: 'AAAAIGZ0eXBpc29t',
  size: 12,
};
const clipDataUrl = `data:${clip.media_type};base64,${clip.base64}`;

it('opens a sent clip on its first frame and shares its original bytes', async () => {
  const blob = new Blob(['clip'], { type: clip.media_type });
  const fetchClip = vi.fn(async () => ({ blob: async () => blob }));
  vi.stubGlobal('fetch', fetchClip);
  const revoked: string[] = [];
  class StubURL extends URL {
    static createObjectURL(): string {
      return 'blob:clip';
    }
    static revokeObjectURL(url: string): void {
      revoked.push(url);
    }
  }
  vi.stubGlobal('URL', StubURL);
  const view = render(<UserMessage attachments={[clip]}>{''}</UserMessage>);

  // A `data:` clip cannot carry the seek fragment, so it plays from its own object URL.
  await waitFor(() =>
    expect(view.container.querySelector('video')).toHaveAttribute('src', 'blob:clip#t=0.001'),
  );
  fireEvent.click(view.getByRole('button', { name: `Save ${clip.filename}` }));
  await waitFor(() =>
    expect(shareArtifact).toHaveBeenCalledWith(blob, clip.filename, clip.media_type, {
      dialogTitle: 'Share video',
      noun: 'Video',
    }),
  );
  expect(fetchClip).toHaveBeenLastCalledWith(clipDataUrl);
  expect(view.getByRole('status')).toHaveTextContent('Artifact saved.');
  view.unmount();
  expect(revoked).toEqual(['blob:clip']);
});

it('plays a sent clip from its data address when its bytes cannot be read', async () => {
  vi.stubGlobal(
    'fetch',
    vi.fn(async () => {
      throw new Error('Read failed');
    }),
  );
  const view = render(<UserMessage attachments={[clip]}>{''}</UserMessage>);

  await waitFor(() =>
    expect(view.container.querySelector('video')).toHaveAttribute('src', clipDataUrl),
  );
  fireEvent.click(view.getByRole('button', { name: `Save ${clip.filename}` }));
  expect(await view.findByText('Could not share video. Try again.')).toBeVisible();
  expect(view.getByRole('button', { name: `Save ${clip.filename}` })).toBeEnabled();
  expect(shareArtifact).not.toHaveBeenCalled();
});
