// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

const nativeCheckVisibility = Object.getOwnPropertyDescriptor(Element.prototype, 'checkVisibility');

/** The engine's answer for a row that `content-visibility: auto` may be skipping. */
function engineDraws(answer: boolean | 'unknown') {
  const check = vi.fn(() => answer === true);
  Object.defineProperty(Element.prototype, 'checkVisibility', {
    configurable: true,
    value: answer === 'unknown' ? undefined : check,
  });
  return check;
}

async function frames(count: number) {
  for (let frame = 0; frame < count; frame += 1) {
    await new Promise((resolve) => requestAnimationFrame(resolve));
  }
}

const sessions = (count: number) =>
  Array.from({ length: count }, (_, index) =>
    listSession({ id: `session-${index}`, title: `Session ${index}` }),
  );

const listViewport = () =>
  document.querySelector('section[aria-label="Sessions"] .overflow-y-auto') as HTMLElement;

afterEach(() => {
  if (nativeCheckVisibility) {
    Object.defineProperty(Element.prototype, 'checkVisibility', nativeCheckVisibility);
  } else {
    delete (Element.prototype as Partial<Element>).checkVisibility;
  }
  vi.restoreAllMocks();
});

// Regression, user report: going back from a session to a long list lagged while Chromium
// sorted a compositor layer for every row. Rows out of sight now skip their layout and
// paint, but only where the engine keeps rows drawn below the fold: in WebKit, fast flings
// showed rows that were not drawn yet.
describe('rows out of sight on the sessions list', () => {
  it('skip their drawing where the engine draws rows below the fold', async () => {
    const check = engineDraws(true);
    const view = renderSessionsScreen({ machines: [{ label: 'laptop', sessions: sessions(3) }] });
    try {
      await screen.findByText('Session 2');
      const list = listViewport();
      await waitFor(() => expect(list).toHaveAttribute('data-rows-settled'));
      await frames(4);
      expect(list).toHaveAttribute('data-rows-settled');
      expect(check).toHaveBeenCalledWith({ contentVisibilityAuto: true });
    } finally {
      view.restore();
      view.unmount();
    }
  });

  it('stay drawn where the engine skips rows below the fold', async () => {
    engineDraws(false);
    const view = renderSessionsScreen({ machines: [{ label: 'laptop', sessions: sessions(3) }] });
    try {
      await screen.findByText('Session 2');
      const list = listViewport();
      await waitFor(() => expect(list).toHaveAttribute('data-rows-settled'));
      await waitFor(() => expect(list).not.toHaveAttribute('data-rows-settled'));
      await frames(4);
      expect(list).not.toHaveAttribute('data-rows-settled');
    } finally {
      view.restore();
      view.unmount();
    }
  });

  it('stay drawn where the engine cannot tell', async () => {
    engineDraws('unknown');
    const view = renderSessionsScreen({ machines: [{ label: 'laptop', sessions: sessions(3) }] });
    try {
      await screen.findByText('Session 2');
      await frames(6);
      expect(listViewport()).not.toHaveAttribute('data-rows-settled');
    } finally {
      view.restore();
      view.unmount();
    }
  });

  it('wait for a list long enough to try, such as a project opened later', async () => {
    const check = engineDraws(true);
    let rowTop = -1;
    vi.spyOn(Element.prototype, 'getBoundingClientRect').mockImplementation(function (
      this: Element,
    ) {
      const top = this.hasAttribute('data-session-row') ? rowTop : 0;
      return { top, bottom: top, left: 0, right: 0, width: 0, height: 0, x: 0, y: top } as DOMRect;
    });
    const view = renderSessionsScreen({ machines: [{ label: 'laptop', sessions: sessions(3) }] });
    try {
      await screen.findByText('Session 2');
      const list = listViewport();
      await frames(6);
      expect(list).not.toHaveAttribute('data-rows-settled');
      expect(check).not.toHaveBeenCalled();

      // Every row now lies below the fold, the way rows arrive when a project opens.
      rowTop = 900;
      fireEvent.click(screen.getAllByRole('button', { name: /^Collapse / })[0]);
      fireEvent.click(await screen.findByRole('button', { name: /^Expand / }));
      await waitFor(() => expect(list).toHaveAttribute('data-rows-settled'));
      await frames(4);
      expect(list).toHaveAttribute('data-rows-settled');
    } finally {
      view.restore();
      view.unmount();
    }
  });
});
