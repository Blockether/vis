// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { expect, it, vi } from 'vitest';

import { OpenPathContext } from '../lib/open-path';
import { useNotices } from '../lib/use-notices';
import { NoticeToast } from './NoticeToast';

/** One screen with a control that sends a notice, and the toast. */
function Toasts({ text }: { text: string }) {
  const notices = useNotices();
  return (
    <>
      <button type="button" onClick={() => notices.push({ id: 'n1', kind: 'ok', text })}>
        Notify
      </button>
      <NoticeToast notices={notices} />
    </>
  );
}

// User report: a link in a notification must be one that a person can press.
it('makes each bare URL of a notice a link, and a file URL opens the file', () => {
  const openPath = vi.fn();
  render(
    <OpenPathContext.Provider value={openPath}>
      <Toasts text="Deployed: https://example.com/runs/7. Log: file:///tmp/run%207.log" />
    </OpenPathContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Notify' }));
  expect(screen.getByRole('link', { name: 'https://example.com/runs/7' })).toHaveAttribute(
    'href',
    'https://example.com/runs/7',
  );
  fireEvent.click(screen.getByRole('button', { name: 'file:///tmp/run%207.log' }));
  expect(openPath).toHaveBeenCalledExactlyOnceWith('/tmp/run 7.log');
});
