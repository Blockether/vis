// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { describe, expect, it } from 'vitest';

import { renderSessionScreen } from './session-screen-harness';

const row = (turn_id: string, request: string, request_kind?: string) => ({
  turn_id,
  request,
  request_kind,
  status: 'completed',
  content: [{ id: `${turn_id}-answer`, type: 'prose', markdown: `Answer to ${request}` }],
});

// Issue #372: the up arrow in the composer recalls the previous prompt, like a
// shell, and the down arrow steps forward again to an empty composer.
describe('prompt recall in the composer', () => {
  function mount(submitTurn?: () => Promise<unknown>) {
    const rows = [
      row('t1', 'first prompt'),
      row('t2', 'a peer asked this', 'council'),
      row('t3', 'second prompt'),
    ];
    renderSessionScreen({
      client: {
        cachedTranscript: () => rows,
        transcript: () => Promise.resolve(rows),
        ...(submitTurn ? { submitTurn } : {}),
      },
    });
  }

  function press(composer: HTMLTextAreaElement, key: 'ArrowUp' | 'ArrowDown') {
    return fireEvent.keyDown(composer, { key });
  }

  it('steps back through the earlier prompts and forward to an empty composer', async () => {
    mount();
    const composer = (await screen.findByLabelText('Message Vis')) as HTMLTextAreaElement;
    await screen.findByText('second prompt');

    expect(press(composer, 'ArrowUp')).toBe(false);
    expect(composer.value).toBe('second prompt');
    press(composer, 'ArrowUp');
    expect(composer.value).toBe('first prompt');
    // The oldest prompt stays; the arrow moves nothing.
    expect(press(composer, 'ArrowUp')).toBe(true);
    expect(composer.value).toBe('first prompt');

    press(composer, 'ArrowDown');
    expect(composer.value).toBe('second prompt');
    press(composer, 'ArrowDown');
    expect(composer.value).toBe('');
    expect(press(composer, 'ArrowDown')).toBe(true);
  });

  it('puts a typed draft aside and gives it back', async () => {
    mount();
    const composer = (await screen.findByLabelText('Message Vis')) as HTMLTextAreaElement;
    await screen.findByText('second prompt');
    fireEvent.change(composer, { target: { value: 'half typed' } });

    press(composer, 'ArrowUp');
    expect(composer.value).toBe('second prompt');
    press(composer, 'ArrowDown');
    await waitFor(() => expect(composer.value).toBe('half typed'));
  });

  it('moves the caret between lines before it recalls', async () => {
    mount();
    const composer = (await screen.findByLabelText('Message Vis')) as HTMLTextAreaElement;
    await screen.findByText('second prompt');
    fireEvent.change(composer, { target: { value: 'line one\nline two' } });
    composer.setSelectionRange(12, 12);

    expect(press(composer, 'ArrowUp')).toBe(true);
    expect(composer.value).toBe('line one\nline two');
  });

  it('recalls a prompt sent in this visit while its turn runs', async () => {
    mount(() => new Promise(() => {}));
    const composer = (await screen.findByLabelText('Message Vis')) as HTMLTextAreaElement;
    await screen.findByText('second prompt');
    fireEvent.change(composer, { target: { value: 'third prompt' } });
    fireEvent.click(screen.getByRole('button', { name: 'Send message' }));
    await waitFor(() => expect(composer.value).toBe(''));

    press(composer, 'ArrowUp');
    expect(composer.value).toBe('third prompt');
    press(composer, 'ArrowUp');
    expect(composer.value).toBe('second prompt');
  });
});
