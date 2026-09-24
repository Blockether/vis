// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

import {
  HeaderActions,
  MachineAddresses,
  MachineProjectsButton,
  MachineTab,
  Pager,
  SectionHeader,
} from './SessionNavigator';

describe('machine selection', () => {
  // Regression, user report (the leading square was crossed out and the right dot
  // should become a full-tile notification tint): one chip, two independent states.
  it('tints the entire unread tile without a trailing mark and keeps selection visible', () => {
    const { rerender } = render(
      <MachineTab isOn={false} hasUnread onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const unread = screen.getByRole('button', { name: /tower\s*unread/ });
    expect(unread).toHaveAttribute('aria-pressed', 'false');
    expect(unread).toHaveClass('bg-machine-unread', 'text-white', 'font-bold');
    expect(unread).not.toHaveClass('bg-accent-surface', 'ring-accent-ink');
    expect(screen.getByText('unread')).toHaveClass('sr-only');
    expect(unread.querySelector('.bg-accent-ink')).toBeNull();

    rerender(
      <MachineTab isOn hasUnread onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const selectedUnread = screen.getByRole('button', { name: /tower\s*unread/ });
    expect(selectedUnread).toHaveAttribute('aria-pressed', 'true');
    expect(selectedUnread).toHaveClass('bg-machine-unread', 'ring-1', 'ring-inset', 'ring-white');
    expect(selectedUnread).not.toHaveClass('bg-accent-surface');
    expect(selectedUnread).toHaveClass('focus-visible:outline-2', 'focus-visible:outline-white');

    rerender(
      <MachineTab isOn onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const selectedRead = screen.getByRole('button', { name: 'tower' });
    expect(selectedRead).toHaveClass('bg-accent-surface', 'text-accent-ink');
    expect(selectedRead).not.toHaveClass('ring-1');
    expect(screen.queryByText('unread')).toBeNull();

    rerender(
      <MachineTab isOn hasUnread isDown label="Reconnect to tower" onClick={() => {}}>
        tower
      </MachineTab>,
    );
    const retry = screen.getByRole('button', { name: 'Reconnect to tower' });
    expect(retry).not.toHaveAttribute('aria-pressed');
    expect(retry).toHaveClass('text-err-ink');
    expect(retry).not.toHaveClass('bg-machine-unread', 'bg-accent-surface', 'ring-accent-ink');
    expect(screen.queryByText('unread')).toBeNull();
  });
});

// Regression, user visual review: the address choice and project action must read as
// part of one flat machine bar, without an icon-only action or a red selection state.
describe('machine bar', () => {
  it('shows the current address first and offers every distinct alternate without clipping', async () => {
    const onSelect = vi.fn();
    const conn = {
      url: 'http://10.0.0.5:7890',
      label: 'tower',
      alts: ['https://gateway.example.com', 'http://10.0.0.5:7890/'],
    };
    render(<MachineAddresses conn={conn} onSelect={onSelect} />);

    const addresses = screen.getByRole('group', { name: 'Addresses on tower' });
    const choices = Array.from(addresses.querySelectorAll('button'));
    expect(choices).toHaveLength(2);
    expect(addresses).toHaveClass('overflow-x-auto');
    const current = screen.getByRole('button', { name: 'Using 10.0.0.5:7890 on tower' });
    const alternate = screen.getByRole('button', { name: 'Use gateway.example.com on tower' });
    expect(choices).toEqual([current, alternate]);
    expect(current).toHaveAttribute('aria-pressed', 'true');
    expect(alternate).toHaveAttribute('aria-pressed', 'false');
    expect(current).toHaveClass('bg-dialog-title', 'text-dialog-title-foreground');
    expect(alternate).toHaveClass('bg-level-project');
    expect(current).not.toHaveClass('text-err');
    expect(alternate).not.toHaveClass('text-err');
    expect(current).toHaveTextContent('IN USE');
    expect(current).toHaveTextContent('IP ADDRESS');
    expect(alternate).toHaveTextContent('HOSTNAME');
    await userEvent.click(alternate);
    expect(onSelect).toHaveBeenCalledExactlyOnceWith('https://gateway.example.com');
    alternate.focus();
    await userEvent.keyboard('{Enter}');
    expect(onSelect).toHaveBeenCalledTimes(2);
  });

  it('does not repeat the active address when its saved URL includes a trailing slash', () => {
    const conn = {
      url: 'http://10.0.0.5:7890/',
      label: 'tower',
      alts: ['http://10.0.0.5:7890', 'https://gateway.example.com'],
    };
    render(<MachineAddresses conn={conn} onSelect={vi.fn()} />);
    const addresses = screen.getByRole('group', { name: 'Addresses on tower' });
    expect(addresses.querySelectorAll('button')).toHaveLength(2);
    expect(screen.getByRole('button', { name: 'Using 10.0.0.5:7890 on tower' })).toHaveAttribute(
      'aria-pressed', 'true',
    );
  });

  it('shows a sole address without suggesting an unavailable route choice', () => {
    render(<MachineAddresses conn={{ url: 'https://gateway.example.com', label: 'tower' }} />);
    expect(screen.getByRole('group', { name: 'Addresses on tower' })).toHaveTextContent(
      'gateway.example.com',
    );
    expect(screen.queryByRole('button', { name: /gateway.example.com/ })).toBeNull();
  });

  it('names the Projects action while preserving the quiet footer variant', async () => {
    const onPress = vi.fn();
    const { rerender } = render(<MachineProjectsButton machine="tower" onPress={onPress} />);
    const projects = screen.getByRole('button', { name: 'Projects on tower' });
    expect(projects).toHaveTextContent('Projects');
    expect(projects).toHaveClass('border-edge-strong');
    await userEvent.click(projects);
    expect(onPress).toHaveBeenCalledExactlyOnceWith(projects);
    rerender(<MachineProjectsButton machine="tower" isQuiet onPress={onPress} />);
    expect(screen.getByRole('button', { name: 'Projects on tower' })).not.toHaveTextContent('Projects');
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
    expect(current).toHaveClass('mouse:group-hover:enabled:not-focus:text-accent-ink');
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

  // The pager is a header's NAVIGATION, not a row's mark: under a pointer it steps
  // onto the band's 24px rhythm in metadata type so it reads as chrome beside the
  // project's facts, while touch keeps the 44px reach the same cluster needs.
  it('steps onto the band rhythm under a pointer without losing touch reach', () => {
    render(<Pager page={2} pageCount={147} label="vis sessions" onPage={vi.fn()} />);
    for (const name of ['Previous page', 'Next page']) {
      const step = screen.getByRole('button', { name });
      expect(step).toHaveClass('size-8', 'after:-inset-1.5', 'mouse:size-6', 'mouse:text-meta');
      expect(step).not.toHaveClass('mouse:size-7');
    }
    const counter = screen.getByRole('textbox', { name: 'Current page' }).closest('label');
    expect(counter).toHaveClass(
      'min-h-11',
      'min-w-11',
      'mouse:min-h-6',
      'mouse:min-w-6',
      'mouse:text-meta',
    );
    expect(screen.getByRole('navigation')).toHaveClass('gap-2', 'mouse:gap-1');
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

  // Reported over the project header, once the pager took that edge (paraphrased: a plus
  // standing on the left is unacceptable, the three dots belong on the right): a paged
  // project stood its own controls in the middle of the band while an unpaged one kept
  // them flush right. The controls hold the edge in ONE shape now, and the page steps
  // stand over the set they move (`SetHeader` in `SessionProjectGroups`) instead.
  it('gives a header’s own controls the trailing edge', () => {
    render(
      <SectionHeader>
        <span>vis</span>
        <HeaderActions align="center">
          <button type="button">New session on tower</button>
        </HeaderActions>
      </SectionHeader>,
    );
    const actions = screen.getByRole('button', { name: 'New session on tower' })
      .parentElement as HTMLElement;
    const header = actions.parentElement as HTMLElement;

    expect(header.tagName).toBe('HEADER');
    // The band's rule reaches the cluster through its LAST child, so the cluster is it.
    expect(header.lastElementChild).toBe(actions);
    // One shape for every section: the band never becomes a grid to make room for pages.
    expect(header.className).toContain('sticky top-0 flex');
    expect(screen.queryByRole('navigation')).not.toBeInTheDocument();
  });
});
