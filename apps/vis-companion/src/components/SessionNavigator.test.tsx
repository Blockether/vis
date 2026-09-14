// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { Pager } from './SessionNavigator';

// Regression: the chosen header layout is `previous · current / total · next` on every device.
describe('project pages', () => {
  it.each([1, 9, 10, 99])('shows only a counter and two step controls on page %s', (page) => {
    render(<Pager page={page} pageCount={104} label="vis sessions" onPage={vi.fn()} />);
    expect(screen.getAllByRole('button')).toHaveLength(2);
    expect(screen.queryByRole('button', { name: /^Page \d+$/ })).not.toBeInTheDocument();
    expect(screen.queryByText('…')).not.toBeInTheDocument();
    expect(screen.getByText(`${page} / 104`)).toBeInTheDocument();
    expect(screen.getByText(`Page ${page} of 104`)).toHaveAttribute('aria-live', 'polite');
  });

  it.each([0, 1])('hides navigation when there are %s pages', (pageCount) => {
    render(<Pager page={1} pageCount={pageCount} label="vis sessions" onPage={vi.fn()} />);
    expect(screen.queryByRole('navigation')).not.toBeInTheDocument();
  });

  it.each([1, 40, 80])('keeps shared previous/next steps in range on page %s', (page) => {
    const onPage = vi.fn();
    render(<Pager page={page} pageCount={80} label="vis sessions" onPage={onPage} />);
    for (const [label, target] of [
      ['Previous page', page - 1],
      ['Next page', page + 1],
    ] as const) {
      const button = screen.getByLabelText(label);
      // Regression: desktop must not hide its steps inside the phone-only strip.
      expect(button.parentElement).toBe(screen.getByRole('navigation'));
      expect(button).toHaveClass('border-0');
      expect(button).not.toHaveClass('rounded-full');
      expect(button).toHaveClass('after:-inset-1.5');
      if (target < 1 || target > 80) {
        expect(button).toBeDisabled();
        expect(button).not.toHaveAttribute('aria-hidden');
        expect(button).not.toHaveClass('invisible');
        fireEvent.click(button);
        expect(onPage).not.toHaveBeenCalled();
      } else {
        fireEvent.click(button);
        expect(onPage).toHaveBeenCalledExactlyOnceWith(target);
        onPage.mockClear();
      }
    }
    expect(screen.getByText(`Page ${page} of 80`)).toHaveAttribute('aria-live', 'polite');
  });

  it('keeps the same step controls while traversing a long history', () => {
    const onPage = vi.fn();
    const { rerender } = render(
      <Pager page={1} pageCount={80} label="vis sessions" onPage={onPage} />,
    );
    const previous = screen.getByRole('button', { name: 'Previous page' });
    const next = screen.getByRole('button', { name: 'Next page' });
    for (let page = 1; page <= 80; page += 1) {
      rerender(<Pager page={page} pageCount={80} label="vis sessions" onPage={onPage} />);
      expect(screen.getByRole('button', { name: 'Previous page' })).toBe(previous);
      expect(screen.getByRole('button', { name: 'Next page' })).toBe(next);
      for (const [button, target] of [
        [previous, page - 1],
        [next, page + 1],
      ] as const) {
        fireEvent.click(button);
        if (target >= 1 && target <= 80) {
          expect(onPage).toHaveBeenCalledExactlyOnceWith(target);
        } else {
          expect(onPage).not.toHaveBeenCalled();
        }
        onPage.mockClear();
      }
    }
  });
});
