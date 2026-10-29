// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { JumpToLatestButton } from './JumpToLatestButton';

describe('jump to the remaining messages', () => {
  it('shows a plural message count and keeps the jump action', () => {
    const onClick = vi.fn();
    render(<JumpToLatestButton remaining={3} onClick={onClick} />);

    const button = screen.getByRole('button', { name: /3 messages/ });
    fireEvent.click(button);
    expect(onClick).toHaveBeenCalledOnce();
  });

});
