// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { Pager, pageWindow } from './SessionNavigator';

// Desktop rails offer direct jumps; phones keep previous/next steps.
describe('project pages', () => {
  it('offers at most three numbered jumps on desktop', () => {
    const onPage = vi.fn();
    render(<Pager page={1} pageCount={102} label="vis sessions" onPage={onPage} />);
    for (const page of [1, 2, 102]) {
      const button = screen.getByRole('button', { name: `Page ${page}` });
      expect(button).toBeInTheDocument();
      // Regression: page targets must match the adjacent header controls, not 24px chips.
      expect(button).toHaveClass('mouse:min-h-7', 'mouse:min-w-7', 'mouse:px-1.5');
      expect(button).toHaveClass('border-transparent', 'hover:bg-hover');
    }
    expect(screen.getAllByRole('button', { name: /^Page \d+$/ })).toHaveLength(3);
    const numbers = screen.getByRole('button', { name: 'Page 1' }).parentElement;
    expect(numbers).toHaveClass('gap-2');
    expect(numbers).not.toHaveClass('mouse:gap-0');
    expect(screen.getByRole('button', { name: 'Page 1' })).toHaveAttribute('aria-current', 'page');
    fireEvent.click(screen.getByRole('button', { name: 'Page 2' }));
    expect(onPage).toHaveBeenCalledExactlyOnceWith(2);
  });

  it.each([1, 40, 80])('keeps mobile steps in range on page %s', (page) => {
    const onPage = vi.fn();
    render(<Pager page={page} pageCount={80} label="vis sessions" onPage={onPage} />);
    for (const [label, target] of [
      ['Previous page', page - 1],
      ['Next page', page + 1],
    ] as const) {
      const button = screen.getByLabelText(label);
      expect(button).toHaveClass('border-0');
      expect(button).not.toHaveClass('rounded-full');
      expect(button).toHaveClass('after:-inset-1.5');
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

  // Regression: the desktop pager should read `1 2 … 102`, not five numbers.
  it('limits the window to three page numbers while keeping both ends reachable', () => {
    expect(pageWindow(1, 1)).toEqual([1]);
    expect(pageWindow(1, 2)).toEqual([1, 2]);
    expect(pageWindow(2, 3)).toEqual([1, 2, 3]);
    expect(pageWindow(1, 4)).toEqual([1, 2, null, 4]);
    expect(pageWindow(4, 7)).toEqual([1, null, 4, null, 7]);
    expect(pageWindow(1, 102)).toEqual([1, 2, null, 102]);
    expect(pageWindow(2, 102)).toEqual([1, 2, null, 102]);
    expect(pageWindow(40, 80)).toEqual([1, null, 40, null, 80]);
    expect(pageWindow(80, 80)).toEqual([1, null, 79, 80]);
    for (let count = 1; count <= 102; count += 1) {
      for (let page = 1; page <= count; page += 1) {
        const numbers = pageWindow(page, count).filter((entry) => entry !== null);
        expect(numbers).toHaveLength(Math.min(3, count));
        expect(numbers).toContain(page);
        expect(numbers).toContain(1);
        expect(numbers).toContain(count);
        expect(new Set(numbers).size).toBe(numbers.length);
      }
    }
  });

  it.each([
    [1, 3],
    [2, 3],
    [40, 39],
    [40, 41],
    [80, 78],
  ])('lets the gap on page %s open the nearest omitted page %s', (page, target) => {
    const onPage = vi.fn();
    render(<Pager page={page} pageCount={80} label="vis sessions" onPage={onPage} />);
    const gap = screen.getByRole('button', { name: `Go to page ${target}` });
    expect(gap).toHaveTextContent('…');
    fireEvent.click(gap);
    expect(onPage).toHaveBeenCalledExactlyOnceWith(target);
  });

  it('keeps both adjacent pages reachable through numbers or gaps', () => {
    const onPage = vi.fn();
    const { rerender } = render(
      <Pager page={1} pageCount={80} label="vis sessions" onPage={onPage} />,
    );
    for (let page = 1; page <= 80; page += 1) {
      rerender(<Pager page={page} pageCount={80} label="vis sessions" onPage={onPage} />);
      for (const target of [page - 1, page + 1]) {
        if (target < 1 || target > 80) continue;
        fireEvent.click(
          screen.getByRole('button', { name: new RegExp(`^(?:Page|Go to page) ${target}$`) }),
        );
        expect(onPage).toHaveBeenCalledExactlyOnceWith(target);
        onPage.mockClear();
      }
    }
  });
});
