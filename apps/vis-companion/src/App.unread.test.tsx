// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { renderApp } from './app-harness';
import { listSession, renderSessionsScreen } from './screens/sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
});

// Regression, user report: on a phone, quickly opening a NEW session and returning
// to the list left NEW in place. The session detail request can take longer than the
// visit, but the gateway already knows the settled answers behind the list badge.
describe('reading a session on a phone', () => {
  it('retires NEW even when the reader leaves before session detail loads', async () => {
    window.location.hash = '';
    const row = listSession({
      id: 'unread',
      title: 'Unread session',
      turn_count: 3,
      answer_count: 3,
      is_unread: true,
      unread_answers: 2,
    });
    const machine = { sessions: [row] };
    const view = renderApp({ machines: [machine] });
    const baseFetch = globalThis.fetch;
    restore = () => {
      view.unmount();
      view.restore();
    };
    let seenAnswers = 1;
    const marks: (number | undefined)[] = [];
    globalThis.fetch = ((input: RequestInfo | URL, init?: RequestInit) => {
      const url = new URL(
        typeof input === 'string' ? input : input instanceof URL ? input.href : input.url,
      );
      if (url.pathname === '/v1/sessions/unread' && (init?.method ?? 'GET') === 'GET') {
        return new Promise<Response>((_resolve, reject) => {
          init?.signal?.addEventListener(
            'abort',
            () => reject(new DOMException('Aborted', 'AbortError')),
            { once: true },
          );
        });
      }
      if (url.pathname === '/v1/sessions/unread/read' && init?.method === 'PUT') {
        const body = JSON.parse(String(init.body)) as { seen_answers?: number };
        marks.push(body.seen_answers);
        seenAnswers = Math.max(seenAnswers, body.seen_answers ?? 3);
        row.is_unread = seenAnswers < 3;
        row.unread_answers = 3 - seenAnswers;
        return Promise.resolve(
          new Response(JSON.stringify({ seen_answers: seenAnswers, is_unread: row.is_unread }), {
            status: 200,
            headers: { 'Content-Type': 'application/json' },
          }),
        );
      }
      return baseFetch(input, init);
    }) as typeof fetch;

    expect(await screen.findByText('NEW ×2')).toBeInTheDocument();
    fireEvent.click(screen.getByText('Unread session'));
    const back = await screen.findByRole('button', { name: 'Back to sessions' });
    await waitFor(() => expect(marks.length).toBeGreaterThan(0));
    fireEvent.click(back);
    await waitFor(() =>
      expect(screen.queryByRole('button', { name: 'Back to sessions' })).not.toBeInTheDocument(),
    );
    expect(marks).toContain(undefined);
    expect(row.is_unread).toBe(false);
    expect(screen.getByText('Unread session')).toBeInTheDocument();
    await waitFor(() =>
      expect(screen.queryByText(/^NEW(?: ×\d+)?$/)).not.toBeInTheDocument(),
    );
  });
});

// A read changes only the gateway's unread fields, not the session's title or
// transcript time. The visible row must adopt that otherwise unchanged list answer.
it('updates a row whose only change is its read position', async () => {
  const row = listSession({ answer_count: 3, is_unread: true, unread_answers: 2 });
  const view = renderSessionsScreen({ machines: [{ sessions: [row] }] });
  restore = () => {
    view.unmount();
    view.restore();
  };
  expect(await screen.findByText('NEW ×2')).toBeInTheDocument();

  view.setVisible(false);
  view.setRows(0, [{ ...row, is_unread: false, unread_answers: 0 }]);
  view.setVisible(true);
  await waitFor(() => expect(screen.queryByText('NEW ×2')).not.toBeInTheDocument());
});
