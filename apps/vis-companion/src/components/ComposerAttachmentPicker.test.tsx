// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { ComposerAttachmentPicker } from './ComposerAttachmentPicker';

afterEach(cleanup);

describe('composer attachment picker', () => {
  it('owns the browser file input and clears it after delivery', () => {
    const addBrowserFiles = vi.fn();
    const commands = { addBrowserFiles, pickNative: vi.fn() };
    const { container } = render(
      <ComposerAttachmentPicker
        accept="image/*,video/*"
        disabled={false}
        commands={commands}
        isNative={false}
      />,
    );
    const input = container.querySelector('input[type="file"]') as HTMLInputElement;
    const file = new File(['bytes'], 'release.txt', { type: 'text/plain' });

    fireEvent.change(input, { target: { files: [file] } });

    expect(addBrowserFiles).toHaveBeenCalledWith([file]);
    expect(input.value).toBe('');
    expect(screen.queryByRole('dialog', { name: 'Attach' })).toBeNull();
  });
});
