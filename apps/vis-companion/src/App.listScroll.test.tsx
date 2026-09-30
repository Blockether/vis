// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { describe, expect, it } from 'vitest';

import { renderApp } from './app-harness';
import { listSession } from './screens/sessions-screen-harness';

// Regression, user report: returning from a session on a phone put the list back at
// the top. The list stays mounted, but a hidden webview may reset its scroll box.
describe('the sessions list after visiting a session on a phone', () => {
  it('returns to the same place after the hidden list loses its browser scroll offset', async () => {
    window.location.hash = '';
    const view = renderApp({
      machines: [
        {
          label: 'laptop',
          sessions: [
            listSession({ id: 'one', title: 'First session' }),
            listSession({ id: 'two', title: 'Second session' }),
          ],
        },
      ],
    });
    try {
      await screen.findByText('Second session');
      const region = view.baseElement.querySelector('section[aria-label="Sessions"]') as HTMLElement;
      const list = region.querySelector('.overflow-y-auto') as HTMLElement;
      const pane = region.parentElement as HTMLElement;
      Object.defineProperties(list, {
        scrollHeight: { configurable: true, value: 5000 },
        clientHeight: { configurable: true, value: 800 },
      });
      list.scrollTop = 1200;
      fireEvent.scroll(list);

      fireEvent.click(screen.getByText('Second session'));
      await screen.findByLabelText('Message Vis');
      expect(pane).toHaveAttribute('aria-hidden', 'true');
      // Model a webview dropping the scroll offset while the session covers the list.
      list.scrollTop = 0;
      fireEvent.scroll(list);
      fireEvent.click(screen.getByRole('button', { name: 'Back to sessions' }));
      await waitFor(() => expect(pane).not.toHaveAttribute('aria-hidden'));
      await waitFor(() => expect(list.scrollTop).toBe(1200));
    } finally {
      view.unmount();
      view.restore();
    }
  });

  // Regression, user report: going back from a session to a long list lagged. With
  // `display: none` the browser discarded the list's styles and layout, and the way
  // back rebuilt every row before the first frame. An inherited `visibility` toggle
  // would restyle every row instead, and taking the list out of the page's flow made
  // Chromium rebuild every row's compositor layers on the way back.
  it('keeps the list laid out while a session covers it', async () => {
    window.location.hash = '';
    const view = renderApp({
      machines: [{ label: 'laptop', sessions: [listSession({ id: 'one', title: 'First session' })] }],
    });
    try {
      await screen.findByText('First session');
      const region = view.baseElement.querySelector('section[aria-label="Sessions"]') as HTMLElement;
      const pane = region.parentElement as HTMLElement;

      fireEvent.click(screen.getByText('First session'));
      const composer = await screen.findByLabelText('Message Vis');
      // The list keeps its box and its drawing; the session's opaque pane lies over it.
      expect(pane.className).toBe('isolate h-full');
      expect(pane).toHaveAttribute('aria-hidden', 'true');
      const sessionPane = pane.parentElement?.lastElementChild as HTMLElement;
      expect(sessionPane).toContainElement(composer);
      expect(sessionPane).toHaveClass('absolute', 'inset-0', 'bg-ink');

      fireEvent.click(screen.getByRole('button', { name: 'Back to sessions' }));
      await waitFor(() => expect(pane).not.toHaveAttribute('aria-hidden'));
      expect(pane.className).toBe('isolate h-full');
    } finally {
      view.unmount();
      view.restore();
    }
  });

  // Regression, user report: on a phone the list's pinned project headers and set bands
  // stood over an open session. They carry a `z-index` and the pane over the list has
  // none, so the list must be its own stacking context for the pane to hide them.
  it('keeps the pinned headers of the list under the session that covers it', async () => {
    window.location.hash = '';
    const view = renderApp({
      machines: [{ label: 'laptop', sessions: [listSession({ id: 'one', title: 'First session' })] }],
    });
    try {
      await screen.findByText('First session');
      const region = view.baseElement.querySelector('section[aria-label="Sessions"]') as HTMLElement;
      const list = region.parentElement as HTMLElement;
      const header = region.querySelector('header.sticky') as HTMLElement;
      expect(header).toHaveClass('z-10');

      fireEvent.click(screen.getByText('First session'));
      const composer = await screen.findByLabelText('Message Vis');
      const sessionPane = list.parentElement?.lastElementChild as HTMLElement;
      expect(sessionPane).toContainElement(composer);
      expect(list).toHaveClass('isolate');
      expect(list).toContainElement(header);
      expect(list.compareDocumentPosition(sessionPane) & Node.DOCUMENT_POSITION_FOLLOWING).toBe(
        Node.DOCUMENT_POSITION_FOLLOWING,
      );
    } finally {
      view.unmount();
      view.restore();
    }
  });

  // Regression, user report: going back to a long list lagged while the transcript was
  // torn down in the same task. The pane stays for one more frame, moved off to the right
  // where the edge swipe leaves it, and goes once the list is on screen.
  it('takes the transcript down after the list is back', async () => {
    window.location.hash = '';
    const view = renderApp({
      machines: [{ label: 'laptop', sessions: [listSession({ id: 'one', title: 'First session' })] }],
    });
    try {
      await screen.findByText('First session');
      const region = view.baseElement.querySelector('section[aria-label="Sessions"]') as HTMLElement;
      const pane = region.parentElement as HTMLElement;
      fireEvent.click(screen.getByText('First session'));
      const composer = await screen.findByLabelText('Message Vis');
      const sessionPane = pane.parentElement?.lastElementChild as HTMLElement;

      fireEvent.click(screen.getByRole('button', { name: 'Back to sessions' }));
      await waitFor(() => expect(sessionPane).toHaveClass('translate-x-full'));
      expect(sessionPane).toHaveAttribute('aria-hidden', 'true');
      expect(sessionPane).toContainElement(composer);
      expect(pane).not.toHaveAttribute('aria-hidden');

      await waitFor(() => expect(sessionPane).not.toBeInTheDocument());
      expect(composer).not.toBeInTheDocument();
    } finally {
      view.unmount();
      view.restore();
    }
  });
});
