// @vitest-environment jsdom
import { act, fireEvent, render, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { UserMessage } from './ChatContent';
import { shareArtifact } from '../lib/artifact-share';
import type { GatewayClient } from '../lib/gateway';

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

// Session 475b983a-62be-400c-aaf7-ff7bb4822c88: a 32 MiB recording came with each read
// of the transcript. A transcript row now leaves large bytes in the gateway's store.
function stubObjectUrls() {
  let count = 0;
  const create = vi.fn((_blob: Blob) => `blob:attachment-${++count}`);
  const revoke = vi.fn();
  vi.stubGlobal('URL', class extends URL {
    static createObjectURL = create;
    static revokeObjectURL = revoke;
  });
  return { create, revoke };
}

function storeOf(blobs: Record<string, Blob>) {
  const fetchAttachmentBlob = vi.fn(async (_sid: string, _tid: string, id: string) => blobs[id]);
  return { fetchAttachmentBlob, client: { fetchAttachmentBlob } as unknown as GatewayClient };
}

it('loads a picture whose bytes its transcript row left in the store', async () => {
  const { create, revoke } = stubObjectUrls();
  const first = new Blob(['first image'], { type: 'image/png' });
  const second = new Blob(['second image'], { type: 'image/png' });
  const { client, fetchAttachmentBlob } = storeOf({ a1: first, a2: second });
  const view = render(
    <UserMessage
      attachments={[{ id: 'a1', filename: 'shot.png', media_type: 'image/png', size: 900_000 }]}
      client={client}
      sid="s1"
      turnId="t1"
    >
      {''}
    </UserMessage>,
  );

  await waitFor(() =>
    expect(view.container.querySelector('img')).toHaveAttribute('src', 'blob:attachment-1'),
  );
  expect(create).toHaveBeenCalledWith(first);
  expect(fetchAttachmentBlob).toHaveBeenCalledWith('s1', 't1', 'a1', expect.any(AbortSignal), 900_000);

  view.rerender(
    <UserMessage
      attachments={[{ id: 'a2', filename: 'next.png', media_type: 'image/png', size: 800_000 }]}
      client={client}
      sid="s1"
      turnId="t1"
    >
      {''}
    </UserMessage>,
  );
  await waitFor(() => expect(view.container.querySelector('img')).toHaveAttribute('src', 'blob:attachment-2'));
  expect(create).toHaveBeenCalledWith(second);
  expect(revoke.mock.calls).toEqual([['blob:attachment-1']]);
  view.unmount();
  expect(revoke.mock.calls).toEqual([['blob:attachment-1'], ['blob:attachment-2']]);
});

it('loads a large recording only when the reader asks for it', async () => {
  stubObjectUrls();
  const blob = new Blob(['recording'], { type: 'audio/mp4' });
  const { client, fetchAttachmentBlob } = storeOf({ a2: blob });
  const fetchMock = vi.fn();
  vi.stubGlobal('fetch', fetchMock);
  const view = render(
    <UserMessage
      attachments={[
        { id: 'a2', filename: 'talk.m4a', media_type: 'audio/mp4', size: 33_208_749, transcription: 'the words' },
      ]}
      client={client}
      sid="s1"
      turnId="t1"
    >
      {''}
    </UserMessage>,
  );

  expect(view.getByText('talk.m4a')).toBeVisible();
  expect(fetchAttachmentBlob).not.toHaveBeenCalled();
  fireEvent.click(view.getByRole('button', { name: 'Load recording' }));
  await waitFor(() => expect(view.container.querySelector('audio')).toHaveAttribute('src', 'blob:attachment-1'));
  expect(fetchAttachmentBlob).toHaveBeenCalledTimes(1);
  fireEvent.click(view.getByRole('button', { name: 'Save talk.m4a' }));
  await waitFor(() => expect(shareArtifact).toHaveBeenCalledWith(blob, 'talk.m4a', 'audio/mp4', {
    dialogTitle: 'Share recording',
    noun: 'Recording',
  }));
  expect(fetchMock).not.toHaveBeenCalled();
});

it('keeps the control of a recording whose load failed', async () => {
  stubObjectUrls();
  const fetchAttachmentBlob = vi.fn()
    .mockRejectedValueOnce(new Error('offline'))
    .mockResolvedValue(new Blob(['recording'], { type: 'audio/mp4' }));
  const view = render(
    <UserMessage
      attachments={[{ id: 'a2', filename: 'talk.m4a', media_type: 'audio/mp4', size: 33_208_749 }]}
      client={{ fetchAttachmentBlob } as unknown as GatewayClient}
      sid="s1"
      turnId="t1"
    >
      {''}
    </UserMessage>,
  );

  fireEvent.click(view.getByRole('button', { name: 'Load recording' }));
  fireEvent.click(await view.findByRole('button', { name: 'Could not load the recording. Try again' }));
  await waitFor(() => expect(view.container.querySelector('audio')).not.toBeNull());
});


it('aborts an outstanding download and ignores its result after unmount', async () => {
  const { create } = stubObjectUrls();
  let complete!: (blob: Blob) => void;
  const download = new Promise<Blob>((resolve) => { complete = resolve; });
  const fetchAttachmentBlob = vi.fn((_sid: string, _tid: string, _id: string, _signal: AbortSignal) => download);
  const view = render(
    <UserMessage
      attachments={[{ id: 'a1', filename: 'shot.png', media_type: 'image/png' }]}
      client={{ fetchAttachmentBlob } as unknown as GatewayClient}
      sid="s1"
      turnId="t1"
    >
      {''}
    </UserMessage>,
  );

  const signal = fetchAttachmentBlob.mock.calls[0]?.[3];
  expect(signal?.aborted).toBe(false);
  view.unmount();
  expect(signal?.aborted).toBe(true);
  await act(async () => complete(new Blob(['late'], { type: 'image/png' })));
  expect(create).not.toHaveBeenCalled();
});
