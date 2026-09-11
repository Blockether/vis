// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { renderSessionScreen } from './session-screen-harness';

const query = "query = 'parse @message /(?<value>.+)/ | limit 1'";

function setup(names: string[] = []) {
  const submitTurn = vi.fn(
    (_sid: string, _request: string, _options: unknown) => new Promise(() => {}),
  );
  const suggestFiles = vi.fn(async () =>
    names.map((name) => ({ name, size: '1KB', age: '1d', status: '' })),
  );
  renderSessionScreen({ client: { submitTurn, suggestFiles } });
  const composer = screen.getByLabelText('Message Vis') as HTMLTextAreaElement;
  return { composer, submitTurn, suggestFiles };
}

// Issue #191: assert the model-facing request, not just the visible transcript.
describe('file references in submitted requests', () => {
  it.each([
    query,
    'SELECT @message FROM logs',
    '"parse @message"',
    'Read `parse @message` literally.',
    '```python\n' + query + '\n```',
    '@message',
    'Inspect @missing-file and @"missing file.txt"',
    'Keep @@message and person@example.com literal.',
  ])('preserves literal text: %s', (text) => {
    const { composer, submitTurn } = setup();
    fireEvent.input(composer, { target: { value: text } });

    fireEvent.click(screen.getByRole('button', { name: 'Send message' }));

    expect(submitTurn).toHaveBeenCalledTimes(1);
    expect(submitTurn.mock.calls[0]?.[1]).toBe(text);
  });

  it.each([false, true])('preserves expanded paste contents when queued=%s', (queued) => {
    const { composer, submitTurn } = setup();
    if (queued) {
      fireEvent.input(composer, { target: { value: 'First request' } });
      fireEvent.click(screen.getByRole('button', { name: 'Send message' }));
    }
    const content = `${query}\nSELECT @message FROM logs\nKeep @@message literal.`;
    fireEvent.paste(composer, {
      clipboardData: { files: [], getData: () => content },
    });
    expect(composer.value).toMatch(/^\[Pasted #\d+:/);

    fireEvent.click(
      screen.getByRole('button', {
        name: queued ? 'Queue message' : 'Send message',
      }),
    );

    expect(submitTurn).toHaveBeenCalledTimes(queued ? 2 : 1);
    expect(submitTurn.mock.calls.at(-1)?.[1]).toBe(content);
  });

  it('does not treat a suggestion as an attachment without selection', async () => {
    const { composer, submitTurn } = setup(['message']);
    fireEvent.input(composer, { target: { value: '@message' } });
    await screen.findByRole('option');

    fireEvent.click(screen.getByRole('button', { name: 'Send message' }));

    expect(submitTurn.mock.calls[0]?.[1]).toBe('@message');
  });

  it.each([query, ''])('keeps uploaded attachments with request %s', async (text) => {
    const { composer, submitTurn } = setup();
    fireEvent.change(screen.getByLabelText('Choose attachment files'), {
      target: {
        files: [
          new File([`<pre>${query}</pre>`], 'query.html', {
            type: 'text/html',
          }),
        ],
      },
    });
    await screen.findByRole('button', { name: 'Remove query.html' });
    fireEvent.input(composer, { target: { value: text } });

    fireEvent.click(screen.getByRole('button', { name: 'Send message' }));

    expect(submitTurn).toHaveBeenCalledWith(
      's1',
      text || 'Please inspect the attached file(s).',
      expect.objectContaining({
        attachments: [
          expect.objectContaining({
            filename: 'query.html',
            media_type: 'text/html',
          }),
        ],
      }),
    );
  });

  it.each([
    ['README.md', 'click'],
    ['message', 'click'],
    ['docs/query examples.md', 'Tab'],
    ['/workspace/żółć.md', 'click'],
  ])('inserts a visible file reference for %s selected via %s', async (path, method) => {
    const { composer, submitTurn } = setup([path]);
    const before = 'Inspect ';
    const after = ` then analyze ${query}`;
    const caret = before.length + '@read'.length;
    fireEvent.input(composer, {
      target: {
        value: `${before}@read${after}`,
        selectionStart: caret,
        selectionEnd: caret,
      },
    });
    const option = await screen.findByRole('option');
    if (method === 'click') fireEvent.click(option);
    else fireEvent.keyDown(composer, { key: method });

    const reference = `[Attached File: ${path}]\nThe user attached this file. Read it (via the file tools) before answering.\n`;
    const expected = before + reference + after;
    await waitFor(() => expect(composer.value).toBe(expected));
    await waitFor(() => expect(composer.selectionStart).toBe(before.length + reference.length));
    expect(composer.selectionEnd).toBe(composer.selectionStart);
    expect(submitTurn).not.toHaveBeenCalled();

    fireEvent.click(screen.getByRole('button', { name: 'Send message' }));

    expect(submitTurn.mock.calls[0]?.[1]).toBe(expected);
  });
});
