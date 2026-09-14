// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { CopyChip } from './ui';

afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

it('starts async clipboard writing inside the press and waits for the complete value', async () => {
  let finish!: (value: string) => void;
  const value = vi.fn(
    () =>
      new Promise<string>((resolve) => {
        finish = resolve;
      }),
  );
  const write = vi.fn(async (items: Array<{ data: Record<string, Promise<Blob>> }>) => {
    await items[0].data['text/plain'];
  });
  vi.stubGlobal(
    'ClipboardItem',
    class {
      readonly data: Record<string, Promise<Blob>>;
      constructor(data: Record<string, Promise<Blob>>) {
        this.data = data;
      }
    },
  );
  vi.stubGlobal('navigator', { ...navigator, clipboard: { write } });
  const toggle = vi.fn();
  render(
    <div onClick={toggle}>
      <CopyChip value={value} label="Copy activity" edge />
    </div>,
  );
  expect(value).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
  expect(write).toHaveBeenCalledTimes(1);
  expect(screen.getByRole('button', { name: 'Copying…' })).toBeDisabled();
  expect(toggle).not.toHaveBeenCalled();
  await waitFor(() => expect(value).toHaveBeenCalledTimes(1));
  finish('Complete activity');
  await screen.findByRole('button', { name: 'Copied' });
  const blob = await write.mock.calls[0][0][0].data['text/plain'];
  expect(blob.type).toBe('text/plain');
  const reader = new FileReader();
  const text = new Promise((resolve) => {
    reader.onload = () => resolve(reader.result);
  });
  reader.readAsText(blob);
  expect(await text).toBe('Complete activity');
});

it('reports a failed load and retries without copying partial text', async () => {
  const writeText = vi.fn().mockResolvedValue(undefined);
  const value = vi
    .fn()
    .mockRejectedValueOnce(new Error('Reconnect to copy activity.'))
    .mockResolvedValueOnce('Complete activity');
  vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
  render(<CopyChip value={value} label="Copy activity" />);
  fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
  expect(await screen.findByRole('alert')).toHaveTextContent('Reconnect to copy activity.');
  expect(writeText).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Copy failed. Try again.' }));
  await screen.findByRole('button', { name: 'Copied' });
  expect(writeText).toHaveBeenCalledExactlyOnceWith('Complete activity');
  expect(screen.queryByRole('alert')).toBeNull();
});

it('aborts pending copy on unmount and never writes stale data', async () => {
  let finish!: (value: string) => void;
  const value = vi.fn(
    (_signal: AbortSignal) =>
      new Promise<string>((resolve) => {
        finish = resolve;
      }),
  );
  const writeText = vi.fn().mockResolvedValue(undefined);
  vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
  const view = render(<CopyChip value={value} label="Copy activity" />);
  fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
  await waitFor(() => expect(value).toHaveBeenCalledTimes(1));
  view.unmount();
  expect(value.mock.calls[0][0].aborted).toBe(true);
  finish('Stale activity');
  await Promise.resolve();
  expect(writeText).not.toHaveBeenCalled();
});

it.each(['write', 'item'])(
  'cancels history retrieval when clipboard %s fails synchronously',
  async (failure) => {
    const value = vi.fn(
      (signal: AbortSignal) =>
        new Promise<string>((_resolve, reject) => {
          signal.addEventListener('abort', () => reject(signal.reason));
        }),
    );
    vi.stubGlobal(
      'ClipboardItem',
      class {
        constructor() {
          if (failure === 'item') throw new Error('Clipboard access denied.');
        }
      },
    );
    const write = vi.fn(() => {
      throw new Error('Clipboard access denied.');
    });
    vi.stubGlobal('navigator', { ...navigator, clipboard: { write } });
    render(<CopyChip value={value} label="Copy activity" />);
    fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
    expect(await screen.findByRole('alert')).toHaveTextContent('Clipboard access denied.');
    expect(value.mock.calls[0][0].aborted).toBe(true);
    expect(screen.getByRole('button', { name: 'Copy failed. Try again.' })).toBeEnabled();
  },
);
