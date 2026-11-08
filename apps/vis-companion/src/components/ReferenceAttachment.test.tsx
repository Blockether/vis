// @vitest-environment jsdom
import { cleanup, fireEvent, render, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/attachment-reference.json';
import { artifactsFromIndex, attachmentIsReference, referenceShape } from '../lib/artifacts';
import type { GatewayClient } from '../lib/gateway';
import { OpenPathContext } from '../lib/open-path';
import type { IterationAttachment, WorkspaceFile } from '../lib/types';
import { AttachmentRail } from './ChatContent';

const descriptors = fixture.cases.map((entry) => entry.descriptor as IterationAttachment);
const [recording, document, picture] = descriptors;
const CONTENT = '/v1/sessions/s1/fs/file/content?path=%2Fx&expires=9&ticket=t';

beforeEach(() => vi.stubGlobal('IntersectionObserver', undefined));
afterEach(() => {
  cleanup();
  vi.unstubAllGlobals();
});

/** A client that counts each whole download and answers the file preview of a path. */
function gateway() {
  const attachmentUrl = vi.fn(async () => 'blob:inline');
  const attachmentBlob = vi.fn(async () => new Blob(['x']));
  const readPath = vi.fn(
    async (_sid: string, path: string): Promise<WorkspaceFile> =>
      path.endsWith('.mp3')
        ? {
            path,
            kind: 'media',
            media_type: 'audio/mpeg',
            size_bytes: 155671105,
            content_url: CONTENT,
            expires_at: Date.now() + 60_000,
          }
        : { path, kind: 'binary', size_bytes: 52428800 },
  );
  const client = {
    attachmentUrl,
    attachmentBlob,
    readPath,
    retainAttachment: () => () => {},
    gatewayUrl: (path: string) => `http://gateway.test${path}`,
  } as unknown as GatewayClient;
  return { client, attachmentUrl, attachmentBlob, readPath };
}

// #450: the gateway and this app read the same descriptors, so both agree on a reference.
it.each(fixture.cases)('reads the storage of a descriptor: $name', ({ descriptor }) => {
  const entry = descriptor as IterationAttachment;
  expect(attachmentIsReference(entry)).toBe('storage' in descriptor);
});

it('streams a recording and shows a card for a document, and never downloads either', async () => {
  const { client, attachmentUrl, attachmentBlob, readPath } = gateway();
  const openPath = vi.fn();
  const view = render(
    <OpenPathContext.Provider value={openPath}>
      <AttachmentRail client={client} sid="s1" attachments={descriptors} />
    </OpenPathContext.Provider>,
  );

  // The recording plays from the signed stream of the file preview, with seek.
  await waitFor(() =>
    expect(view.container.querySelector('audio')?.getAttribute('src')).toBe(
      `http://gateway.test${CONTENT}`,
    ),
  );
  expect(readPath).toHaveBeenCalledWith('s1', recording.path, undefined, expect.anything());
  expect(referenceShape(recording)).toBe('stream');

  // The document is a card with its size and path. A press opens the file preview.
  expect(referenceShape(document)).toBe('card');
  expect(view.container.textContent).toContain('50.0MB');
  expect(view.container.textContent).toContain('/Users/me/Documents/atlas.pdf');
  fireEvent.click(view.getByRole('button', { name: 'Open atlas.pdf' }));
  expect(openPath).toHaveBeenCalledWith('/Users/me/Documents/atlas.pdf');

  // Only the attachment that the database keeps is downloaded, as before.
  await waitFor(() => expect(attachmentUrl).toHaveBeenCalledTimes(1));
  expect(attachmentUrl).toHaveBeenCalledWith('s1', picture.iteration_id, picture.index);
  expect(attachmentBlob).not.toHaveBeenCalled();
});

it('shows the card when the gateway cannot play the file of a reference', async () => {
  const { client } = gateway();
  const clip: IterationAttachment = {
    ...recording,
    media_type: 'video/mp4',
    filename: 'talk.mp4',
    path: '/Users/me/talk.mp4',
  };
  const view = render(<AttachmentRail client={client} sid="s1" attachments={[clip]} />);
  await waitFor(() => expect(view.container.textContent).toContain('/Users/me/talk.mp4'));
  expect(view.container.querySelector('video')).toBeNull();
});

it('keeps the path of a reference in the artifacts sheet, and none for an inline one', () => {
  const rows = artifactsFromIndex(descriptors).reverse();
  expect(rows.map((row) => row.path)).toEqual([recording.path, document.path, undefined]);
});
