// @vitest-environment jsdom
import { act, fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import {
  MachineTab,
  Pager,
  SectionHeader,
  sessionStatusTone,
} from './SessionNavigator';

describe('session status colors', () => {
  it.each([
    ['LIVE', 'text-ok'], ['Live', 'text-ok'], ['NEW ×2', 'text-accent-ink'],
    ['HITL ×3', 'text-warn'], ['WAITING', 'text-warn'], ['STOPPED', 'text-err'],
    ['DIRTY', 'text-dirty'], ['ARCHIVED', 'text-muted'], ['IDLE', 'text-dialog-hint'],
  ])('uses the shared color for %s', (label, tone) => {
    expect(sessionStatusTone(label)).toBe(tone);
  });
});

describe('project heading height', () => {
  it('updates the sticky offset when counts wrap and releases the observer on unmount', () => {
    const height = vi.spyOn(HTMLElement.prototype, 'offsetHeight', 'get').mockReturnValue(62);
    const observe = vi.fn();
    const disconnect = vi.fn();
    let resize = () => {};
    vi.stubGlobal('ResizeObserver', class {
      constructor(callback: () => void) { resize = callback; }
      observe = observe;
      disconnect = disconnect;
    });
    try {
      const { container, unmount } = render(
        <section data-project-root="/project"><SectionHeader>Project</SectionHeader></section>,
      );
      const section = container.querySelector('section')!;
      expect(section.style.getPropertyValue('--project-header-height')).toBe('62px');
      expect(observe).toHaveBeenCalledWith(section.querySelector('header'));
      height.mockReturnValue(80);
      resize();
      expect(section.style.getPropertyValue('--project-header-height')).toBe('80px');
      unmount();
      expect(disconnect).toHaveBeenCalledOnce();
      expect(section.style.getPropertyValue('--project-header-height')).toBe('');
    } finally {
      height.mockRestore();
      vi.unstubAllGlobals();
    }
  });
});

describe('machine selection', () => {
  // Regression, user review: the machine look mixed a warm selection fill, a cool unread
  // tint and a white ring. Now selection is a raised segment and only news is yellow.
  it('raises the chosen segment without colour and keeps yellow for news', () => {
    const { rerender } = render(
      <MachineTab isOn={false} unread={3} onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const unread = screen.getByRole('button', { name: /tower\s*3 unread/ });
    expect(unread).toHaveAttribute('aria-pressed', 'false');

    rerender(
      <MachineTab isOn unread={3} onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const selectedUnread = screen.getByRole('button', { name: /tower\s*3 unread/ });
    expect(selectedUnread).toHaveAttribute('aria-pressed', 'true');
    // Inset, because the scrolling track clips an outline drawn outside the segment.

    rerender(
      <MachineTab isOn onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const selectedRead = screen.getByRole('button', { name: 'tower' });
    expect(selectedRead.querySelector('.bg-accent')).toBeNull();
    expect(screen.queryByText(/unread/)).toBeNull();

    rerender(
      <MachineTab isOn unread={3} isDown label="Reconnect to tower" onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const retry = screen.getByRole('button', { name: 'Reconnect to tower' });
    expect(retry).not.toHaveAttribute('aria-pressed');
    expect(screen.queryByText(/unread/)).toBeNull();
  });

  // The tab counts the news and says how long ago the machine last moved (option C).
  it('counts unread conversations in a chip and shows the age of the newest activity', () => {
    vi.useFakeTimers();
    try {
      const now = Date.parse('2024-05-02T12:00:00Z');
      vi.setSystemTime(now);
      const { rerender } = render(
        <MachineTab isOn={false} unread={4} lastActivityAt={now - 2 * 60_000} onClick={() => {}}>
          tower
        </MachineTab>,
      );
      const tab = screen.getByRole('button', { name: /^tower\s*4 unread$/ });
      expect(screen.getByText('4')).toHaveAttribute('aria-hidden', 'true');
      expect(screen.getByText('2 min')).toHaveAttribute('aria-hidden', 'true');
      expect(tab).toHaveAttribute('aria-description', 'Last activity 2 min ago');
      expect(tab).not.toHaveAttribute('title');

      act(() => {
        vi.advanceTimersByTime(60 * 60_000);
      });
      expect(screen.getByText('1 h')).toBeInTheDocument();

      rerender(
        <MachineTab isOn={false} lastActivityAt={Date.now()} onClick={() => {}}>
          tower
        </MachineTab>,
      );
      const idle = screen.getByRole('button', { name: 'tower' });
      expect(screen.getByText('now')).toBeInTheDocument();
      expect(idle).toHaveAttribute('aria-description', 'Active now');

      rerender(
        <MachineTab isOn={false} unread={4} lastActivityAt={Date.now()} isDown onClick={() => {}}>
          tower
        </MachineTab>,
      );
      expect(screen.queryByText(/now|min/)).toBeNull();
      expect(screen.queryByText('4')).toBeNull();
    } finally {
      vi.useRealTimers();
    }
  });
});

// Regression: the chosen header layout is `previous · current / total · next` on every device.
describe('project pages', () => {
  it.each([1, 9, 10, 99])('shows an editable counter and two step controls on page %s', (page) => {
    render(<Pager page={page} pageCount={104} label="vis sessions" onPage={vi.fn()} />);
    expect(screen.getAllByRole('button')).toHaveLength(2);
    expect(screen.queryByRole('button', { name: /^Page \d+$/ })).not.toBeInTheDocument();
    expect(screen.queryByText('…')).not.toBeInTheDocument();
    const current = screen.getByRole('textbox', { name: 'Current page' });
    expect(current).toHaveValue(String(page));
    // Keep hover ink off the focused field on the focused project band's background.
    expect(screen.getByText('/104')).toBeInTheDocument();
    expect(screen.getByText(`Page ${page} of 104`)).toHaveAttribute('aria-live', 'polite');
  });

  it('disables both steps and page editing without hiding the counter', () => {
    const onPage = vi.fn();
    const { rerender } = render(
      <Pager page={40} pageCount={80} label="vis sessions" onPage={onPage} disabled />,
    );
    const pager = screen.getByRole('navigation');
    const field = screen.getByRole('textbox', { name: 'Current page' });
    expect(pager).toBeVisible();
    expect(pager).toHaveAttribute('aria-disabled', 'true');
    expect(field).toHaveValue('40');
    for (const control of [...screen.getAllByRole('button'), field]) {
      expect(control).toBeDisabled();
      fireEvent.click(control);
    }
    field.focus();
    expect(field).not.toHaveFocus();
    expect(onPage).not.toHaveBeenCalled();

    rerender(<Pager page={40} pageCount={80} label="vis sessions" onPage={onPage} />);
    expect(screen.getByRole('navigation')).toBe(pager);
    expect(pager).not.toHaveAttribute('aria-disabled');
    expect(field).toBeEnabled();
    fireEvent.click(screen.getByRole('button', { name: 'Next page' }));
    expect(onPage).toHaveBeenCalledExactlyOnceWith(41);
  });

  it('discards an unfinished page edit when disabled without navigating on blur', () => {
    const onPage = vi.fn();
    const { rerender } = render(
      <Pager page={5} pageCount={104} label="vis sessions" onPage={onPage} />,
    );
    const field = screen.getByRole('textbox', { name: 'Current page' });
    field.focus();
    fireEvent.change(field, { target: { value: '64' } });
    rerender(<Pager page={5} pageCount={104} label="vis sessions" onPage={onPage} disabled />);
    expect(field).toBeDisabled();
    expect(field).toHaveValue('5');
    fireEvent.blur(field);
    expect(onPage).not.toHaveBeenCalled();
    rerender(<Pager page={5} pageCount={104} label="vis sessions" onPage={onPage} />);
    expect(field).toBeEnabled();
    expect(field).toHaveValue('5');
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
      if (target < 1 || target > 80) {
        expect(button).toBeDisabled();
        expect(button).not.toHaveAttribute('aria-hidden');
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

  it.each(['Enter', 'blur'])('jumps only when the edited number is committed with %s', (commit) => {
    const onPage = vi.fn();
    const { rerender } = render(
      <Pager page={1} pageCount={104} label="vis sessions" onPage={onPage} />,
    );
    const field = screen.getByRole('textbox', { name: 'Current page' });
    expect(field).toHaveAttribute('inputmode', 'numeric');
    expect(field).toHaveAttribute('enterkeyhint', 'go');
    field.focus();
    fireEvent.change(field, { target: { value: '64' } });
    expect(onPage).not.toHaveBeenCalled();
    if (commit === 'Enter') fireEvent.keyDown(field, { key: 'Enter' });
    else fireEvent.blur(field);
    expect(onPage).toHaveBeenCalledExactlyOnceWith(64);
    // Keep the committed counter with the visible rows while a requested page is loading.
    expect(field).toHaveValue('1');
    rerender(<Pager page={64} pageCount={104} label="vis sessions" onPage={onPage} />);
    expect(field).toHaveValue('64');
    expect(screen.getByText('Page 64 of 104')).toHaveAttribute('aria-live', 'polite');
  });

  it('cancels with Escape without committing on blur or blocking the next edit', () => {
    const onPage = vi.fn();
    render(<Pager page={5} pageCount={104} label="vis sessions" onPage={onPage} />);
    const field = screen.getByRole('textbox', { name: 'Current page' });
    field.focus();
    fireEvent.change(field, { target: { value: '64' } });
    fireEvent.keyDown(field, { key: 'Escape' });
    expect(field).toHaveValue('5');
    expect(field).not.toHaveFocus();
    expect(onPage).not.toHaveBeenCalled();
    field.focus();
    fireEvent.change(field, { target: { value: '8' } });
    fireEvent.keyDown(field, { key: 'Enter' });
    expect(onPage).toHaveBeenCalledExactlyOnceWith(8);
  });

  it.each(['', 'invalid', '2.5', '1e2', '5'])(
    'restores the current page without navigating for %j',
    (value) => {
      const onPage = vi.fn();
      render(<Pager page={5} pageCount={104} label="vis sessions" onPage={onPage} />);
      const field = screen.getByRole('textbox', { name: 'Current page' });
      field.focus();
      fireEvent.change(field, { target: { value } });
      fireEvent.keyDown(field, { key: 'Enter' });
      expect(onPage).not.toHaveBeenCalled();
      expect(field).toHaveValue('5');
    },
  );

  it.each([
    ['0', 1],
    ['999', 104],
    ['0012', 12],
  ])('normalizes %s to page %s', (value, target) => {
    const onPage = vi.fn();
    render(<Pager page={5} pageCount={104} label="vis sessions" onPage={onPage} />);
    const field = screen.getByRole('textbox', { name: 'Current page' });
    field.focus();
    fireEvent.change(field, { target: { value } });
    fireEvent.keyDown(field, { key: 'Enter' });
    expect(onPage).toHaveBeenCalledExactlyOnceWith(target);
  });

  it('uses the latest page count when the history shrinks during an edit', () => {
    const onPage = vi.fn();
    const { rerender } = render(
      <Pager page={5} pageCount={104} label="vis sessions" onPage={onPage} />,
    );
    const field = screen.getByRole('textbox', { name: 'Current page' });
    field.focus();
    fireEvent.change(field, { target: { value: '64' } });
    rerender(<Pager page={5} pageCount={7} label="vis sessions" onPage={onPage} />);
    fireEvent.keyDown(field, { key: 'Enter' });
    expect(onPage).toHaveBeenCalledExactlyOnceWith(7);
  });

});
