// @vitest-environment jsdom
import { cleanup, fireEvent, render } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';

import type { PendingAttachment } from '../lib/attachments';
import { ComposerPayloadShelf } from './ComposerPayloadShelf';

const image: PendingAttachment = {
  id: 'diagram',
  filename: 'release-map.png',
  media_type: 'image/png',
  base64: 'data:image/png;base64,AA==',
  previewUrl: 'data:image/png;base64,AA==',
  size: 1,
};

afterEach(cleanup);

// Regression: pasted log files were sent to the image decoder and looked broken.
it('shows pasted diagnostics as a file, not an image editor', () => {
  const commands = {
    editPaste: vi.fn(),
    removePaste: vi.fn(),
    editAttachment: vi.fn(),
    removeAttachment: vi.fn(),
  };
  const log = {
    ...image,
    id: 'logs',
    filename: 'vis-diagnostics.jsonl.gz',
    media_type: 'application/gzip',
  };
  const view = render(<ComposerPayloadShelf pastes={[]} attachments={[log]} commands={commands} />);
  expect(view.getByText(log.filename)).toHaveAttribute('title', log.filename);
  expect(view.container.querySelector('img, video, audio')).toBeNull();
  expect(view.queryByRole('button', { name: /expand|annotate|edit image/i })).toBeNull();
  fireEvent.click(view.getByRole('button', { name: `Remove ${log.filename}` }));
  expect(commands.removeAttachment).toHaveBeenCalledWith('logs');
});
