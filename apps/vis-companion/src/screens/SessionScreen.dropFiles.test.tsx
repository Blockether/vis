// @vitest-environment jsdom
// Regression #261: on the desktop the only way into the composer was the
// paperclip, and a file that did not fit came back as one glued-together line.
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { beforeEach, expect, it, vi } from 'vitest';
import * as attachments from '../lib/attachments';
import { renderSessionScreen } from './session-screen-harness';

const table = () => new File(['name,total\ncafé,2\n'], 'rows.csv', { type: 'text/csv' });

const carrying = (files: File[]) => ({ types: ['Files'], files });

const editor = () => screen.getByLabelText('Message Vis') as HTMLTextAreaElement;

let fromFiles: ReturnType<typeof vi.spyOn>;

beforeEach(() => {
  vi.restoreAllMocks();
  fromFiles = vi.spyOn(attachments, 'attachmentsFromFiles').mockResolvedValue({
    attachments: [
      {
        id: 'rows',
        filename: 'rows.csv',
        media_type: 'text/csv',
        base64: 'bmFtZQo=',
        previewUrl: 'data:text/csv;base64,bmFtZQo=',
        size: 18,
      },
    ],
    rejected: [],
  });
});

it('attaches a spreadsheet dropped onto the composer', async () => {
  renderSessionScreen();
  const dropped = table();
  fireEvent.drop(editor(), { dataTransfer: carrying([dropped]) });
  await waitFor(() =>
    expect(screen.getByRole('button', { name: 'Remove rows.csv' })).toBeInTheDocument(),
  );
  expect(fromFiles).toHaveBeenCalledWith([dropped], expect.anything());
});

it('claims a drag carrying files so the browser never opens the file instead', () => {
  renderSessionScreen();
  const openedByBrowser = fireEvent.dragOver(editor(), { dataTransfer: carrying([table()]) });
  expect(openedByBrowser).toBe(false);
});

it('shows the composer taking the drag, and stops when the pointer leaves', () => {
  renderSessionScreen();
  fireEvent.dragEnter(editor(), { dataTransfer: carrying([table()]) });
  expect(document.querySelector('.ring-accent')).not.toBeNull();
  fireEvent.dragLeave(editor(), { dataTransfer: carrying([table()]) });
  expect(document.querySelector('.ring-accent')).toBeNull();
});

it('leaves a dragged sentence to the text box', () => {
  renderSessionScreen();
  const handledByTextBox = fireEvent.drop(editor(), {
    dataTransfer: { types: ['text/plain'], files: [] },
  });
  expect(handledByTextBox).toBe(true);
  expect(fromFiles).not.toHaveBeenCalled();
});

it('gives every refused file its own line in the notice', async () => {
  fromFiles.mockResolvedValue({
    attachments: [],
    rejected: [
      'ledger.key: application/x-iwork-keynote-sffkey is not an accepted attachment format',
      'mystery: its file type could not be recognised',
    ],
  });
  renderSessionScreen();
  fireEvent.drop(editor(), { dataTransfer: carrying([table()]) });
  const notice = await screen.findByText(/could not be recognised/);
  expect(notice.textContent).toBe(
    '- ledger.key: application/x-iwork-keynote-sffkey is not an accepted attachment format\n' +
      '- mystery: its file type could not be recognised',
  );
  expect(notice.className).toContain('whitespace-pre-line');
});
