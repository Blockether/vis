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
      expect(pane.classList.contains('hidden')).toBe(true);
      // Model the browser dropping the scroll offset while the pane has display: none.
      list.scrollTop = 0;
      fireEvent.scroll(list);
      fireEvent.click(screen.getByRole('button', { name: 'Back to sessions' }));
      await waitFor(() => expect(pane.classList.contains('hidden')).toBe(false));
      expect(list.scrollTop).toBe(1200);
    } finally {
      view.unmount();
      view.restore();
    }
  });
});
