// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { Pager, pageWindow } from './SessionNavigator';

// Regression: narrow desktop rails hid every page number and offered only arrows.
describe('numbered project pages', () => {
  it('offers the first five pages directly instead of previous/next arrows', () => {
    const onPage = vi.fn();
    render(<Pager page={1} pageCount={80} label="vis sessions" onPage={onPage} />);
    for (const page of [1, 2, 3, 4, 5, 80]) {
      expect(screen.getByRole('button', { name: `Page ${page}` })).toBeInTheDocument();
    }
    expect(
      screen.queryByRole('button', { name: /Previous page|Next page/ }),
    ).not.toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Page 1' })).toHaveAttribute('aria-current', 'page');
    fireEvent.click(screen.getByRole('button', { name: 'Page 5' }));
    expect(onPage).toHaveBeenCalledExactlyOnceWith(5);
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
