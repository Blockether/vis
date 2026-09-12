// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { Pager, pageWindow } from './SessionNavigator';

// Regression: desktop rails need page numbers, but phones keep compact previous/next steps.
describe('project pages', () => {
  it('offers the first five pages as direct jumps on desktop', () => {
    const onPage = vi.fn();
    render(<Pager page={1} pageCount={80} label="vis sessions" onPage={onPage} />);
    for (const page of [1, 2, 3, 4, 5, 80]) {
      expect(screen.getByRole('button', { name: `Page ${page}` })).toBeInTheDocument();
    }
    expect(screen.getByRole('button', { name: 'Page 1' })).toHaveAttribute('aria-current', 'page');
    fireEvent.click(screen.getByRole('button', { name: 'Page 5' }));
    expect(onPage).toHaveBeenCalledExactlyOnceWith(5);
  });

  it.each([1, 40, 80])('keeps mobile steps in range on page %s', (page) => {
    const onPage = vi.fn();
    render(<Pager page={page} pageCount={80} label="vis sessions" onPage={onPage} />);
    for (const [label, target] of [
      ['Previous page', page - 1],
      ['Next page', page + 1],
    ] as const) {
      const button = screen.getByLabelText(label);
      if (target < 1 || target > 80) {
        expect(screen.queryByRole('button', { name: label })).not.toBeInTheDocument();
        expect(button).toBeDisabled();
        expect(button).toHaveAttribute('aria-hidden', 'true');
        expect(button).toHaveAttribute('tabindex', '-1');
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

  it('keeps a bounded window with both ends and no gap hiding just one page', () => {
    expect(pageWindow(1, 1)).toEqual([1]);
    expect(pageWindow(1, 5)).toEqual([1, 2, 3, 4, 5]);
    expect(pageWindow(4, 7)).toEqual([1, 2, 3, 4, 5, 6, 7]);
    expect(pageWindow(1, 80)).toEqual([1, 2, 3, 4, 5, null, 80]);
    expect(pageWindow(40, 80)).toEqual([1, null, 39, 40, 41, null, 80]);
    expect(pageWindow(80, 80)).toEqual([1, null, 76, 77, 78, 79, 80]);
    for (let page = 1; page <= 80; page += 1) {
      expect(pageWindow(page, 80)).toHaveLength(7);
      expect(pageWindow(page, 80)).toContain(page);
    }
  });
});
