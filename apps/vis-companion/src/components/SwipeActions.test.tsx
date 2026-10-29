// @vitest-environment jsdom
import { act, fireEvent, render, screen } from '@testing-library/react';
import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';

import { SwipeActions } from './SwipeActions';
import { PencilIcon, StarIcon, TrashIcon } from './icons';

// Touch keeps its swipe gesture; desktop actions live behind one vertical-dot trigger.

// Six fixed-width cells exceeded the row width and clipped the first action on iPhones.
it.each([1, 3, 6, 8])(
  'sizes %i actions before the first swipe and keeps that size while open',
  (count) => {
    const actions = Array.from({ length: count }, (_, index) => ({
      key: String(index),
      label: index === count - 1 ? 'Delete' : 'Move to...',
      icon: <TrashIcon />,
      onSelect: () => {},
    }));
    const { container, rerender } = render(
      <SwipeActions label="a session" actions={actions}>
        <span>row</span>
      </SwipeActions>,
    );
    const track = container.querySelector<HTMLElement>('[data-swipe-track]')!;
    const strip = screen.getByRole('group', { name: 'a session actions' });
    expect((track.firstElementChild as HTMLElement).style.translate).toBe('');
    expect(strip).toHaveClass('max-w-full');
    expect(strip).toHaveStyle({ width: `${count * 4.5}rem` });
    for (const button of strip.querySelectorAll('button')) {
      expect(button).toHaveClass('min-w-0', 'flex-1');
      expect(button).not.toHaveClass('w-[4.5rem]');
      expect(button.lastElementChild).toHaveClass('max-w-full', 'truncate');
    }
    const sizing = strip.getAttribute('style');
    act(() => strip.querySelector('button')!.focus());
    rerender(
      <SwipeActions
        label="a session"
        actions={actions.map((action) => ({ ...action, label: 'Unarchive' }))}
      >
        <span>row updated while open</span>
      </SwipeActions>,
    );
    expect(strip.getAttribute('style')).toBe(sizing);
    rerender(
      <SwipeActions label="a session" actions={actions.slice(0, 1)}>
        <span>row</span>
      </SwipeActions>,
    );
    expect(strip).toHaveStyle({ width: '4.5rem' });
  },
);

describe('the slide', () => {
  const track = (index = 0) =>
    document.querySelectorAll<HTMLElement>('[data-swipe-track]')[index] as HTMLElement;
  /** How far the row stands to the left: '' is home. */
  const offset = (index = 0) => (track(index).firstElementChild as HTMLElement).style.translate;

  // jsdom lays nothing out: every drawer gets the width two verbs take on a phone.
  const WIDTH = 144;
  const offsetWidth = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'offsetWidth');
  beforeAll(() => {
    Object.defineProperty(HTMLElement.prototype, 'offsetWidth', {
      configurable: true,
      get: () => WIDTH,
    });
  });
  afterAll(() => {
    if (offsetWidth) Object.defineProperty(HTMLElement.prototype, 'offsetWidth', offsetWidth);
  });
  // A slide swallows the click that follows it, so each test starts long after the last one.
  beforeEach(() => {
    vi.useFakeTimers({ toFake: ['performance', 'Date'] });
    vi.advanceTimersByTime(1e6);
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  /** What a thumb does: land, move sideways in four steps over `ms`, then lift or be taken. */
  function drag(
    element: HTMLElement,
    by: number,
    {
      pointerType = 'touch',
      ms = 400,
      lift = true,
    }: { pointerType?: string; ms?: number; lift?: boolean } = {},
  ) {
    const at = { pointerId: 1, pointerType, isPrimary: true, clientY: 20 };
    fireEvent.pointerDown(element, { ...at, clientX: 300 });
    for (let step = 1; step <= 4; step += 1) {
      vi.advanceTimersByTime(ms / 4);
      fireEvent.pointerMove(element, { ...at, clientX: 300 - (by * step) / 4 });
    }
    if (lift) fireEvent.pointerUp(element, { ...at, clientX: 300 - by });
    else fireEvent.pointerCancel(element, { ...at, clientX: 300 - by });
  }

  /** The click that a slide swallows has passed. */
  const later = () => vi.advanceTimersByTime(1000);

  const row = (label: string, onOpen = () => {}) => (
    <SwipeActions
      key={label}
      label={label}
      actions={[
        { key: 'rename', label: 'Rename', icon: <PencilIcon />, onSelect: () => {} },
        { key: 'delete', label: 'Delete', icon: <TrashIcon />, tone: 'danger', onSelect: () => {} },
      ]}
    >
      <button type="button" onClick={onOpen}>
        {label}
      </button>
    </SwipeActions>
  );

  it('follows the finger, then opens past half its drawer and shuts short of it', () => {
    render(row('first'));
    const at = { pointerId: 1, pointerType: 'touch', isPrimary: true, clientY: 20 };
    fireEvent.pointerDown(track(), { ...at, clientX: 300 });
    fireEvent.pointerMove(track(), { ...at, clientX: 250 });
    expect(offset()).toBe('-50px');
    // The drawer is as wide as the finger can take it, and no wider.
    fireEvent.pointerMove(track(), { ...at, clientX: 0 });
    expect(offset()).toBe(`-${WIDTH}px`);
    fireEvent.pointerCancel(track(), at);
    expect(offset()).toBe(`-${WIDTH}px`);

    later();
    drag(track(), -WIDTH * 0.6);
    expect(offset()).toBe('');
    later();
    drag(track(), WIDTH * 0.4);
    expect(offset()).toBe('');
  });

  it('goes the way a quick flick goes, short of half', () => {
    render(row('first'));
    drag(track(), 40, { ms: 40 });
    expect(offset()).toBe(`-${WIDTH}px`);

    // A finger that rests before it lifts makes no flick.
    later();
    const at = { pointerId: 1, pointerType: 'touch', isPrimary: true, clientY: 20 };
    fireEvent.pointerDown(track(), { ...at, clientX: 300 });
    vi.advanceTimersByTime(10);
    fireEvent.pointerMove(track(), { ...at, clientX: 330 });
    vi.advanceTimersByTime(300);
    fireEvent.pointerUp(track(), { ...at, clientX: 330 });
    expect(offset()).toBe(`-${WIDTH}px`);
  });

  it('leaves a vertical drag to the list', () => {
    const onOpen = vi.fn();
    render(row('first', onOpen));
    const at = { pointerId: 1, pointerType: 'touch', isPrimary: true, clientX: 300 };
    fireEvent.pointerDown(track(), { ...at, clientY: 20 });
    fireEvent.pointerMove(track(), { ...at, clientX: 295, clientY: 60 });
    fireEvent.pointerMove(track(), { ...at, clientX: 200, clientY: 70 });
    fireEvent.pointerCancel(track(), at);
    expect(offset()).toBe('');
    // Nothing slid, so nothing swallows the next press.
    fireEvent.click(screen.getByRole('button', { name: 'first' }));
    expect(onOpen).toHaveBeenCalledOnce();
  });

  it('takes no slide from a mouse', () => {
    render(row('first'));
    drag(track(), WIDTH, { pointerType: 'mouse' });
    expect(offset()).toBe('');
  });

  it('swallows the click that ends a slide, and only that click', () => {
    const onOpen = vi.fn();
    render(row('first', onOpen));
    drag(track(), 30);
    expect(offset()).toBe('');
    fireEvent.click(screen.getByRole('button', { name: 'first' }));
    expect(onOpen).not.toHaveBeenCalled();

    later();
    fireEvent.click(screen.getByRole('button', { name: 'first' }));
    expect(onOpen).toHaveBeenCalledOnce();
  });

  it('opens one row at a time, so no second row keeps a delete armed', () => {
    render(
      <>
        {row('first')}
        {row('second')}
      </>,
    );
    drag(track(0), WIDTH);
    expect(offset(0)).toBe(`-${WIDTH}px`);

    // The open row shuts the moment another one starts to move.
    const at = { pointerId: 2, pointerType: 'touch', isPrimary: true, clientY: 20 };
    fireEvent.pointerDown(track(1), { ...at, clientX: 300 });
    fireEvent.pointerMove(track(1), { ...at, clientX: 280 });
    expect(offset(0)).toBe('');
    fireEvent.pointerMove(track(1), { ...at, clientX: 300 - WIDTH });
    fireEvent.pointerUp(track(1), { ...at, clientX: 300 - WIDTH });
    expect(offset(1)).toBe(`-${WIDTH}px`);
    expect(offset(0)).toBe('');
  });

  // Regression, user report (paraphrased: every update to the session hides what
  // I have open): a transcript following its end rewrites its own `scrollTop` on
  // every update it receives, and on the window that reached an open drawer as
  // "the list moved under me" — from a scroller the row does not stand in.
  it('shuts when the list under it moves, not when another scroller moves', () => {
    render(row('first'));
    drag(track(), WIDTH);
    const elsewhere = document.body.appendChild(document.createElement('div'));

    fireEvent.scroll(elsewhere);
    expect(offset()).toBe(`-${WIDTH}px`);
    elsewhere.remove();

    fireEvent.scroll(document.body);
    expect(offset()).toBe('');
  });

  it('closes on Escape, and the row is a navigation again at once', () => {
    const onOpen = vi.fn();
    render(row('first', onOpen));
    drag(track(), WIDTH);
    later();
    fireEvent.keyDown(window, { key: 'Escape' });
    expect(offset()).toBe('');

    fireEvent.click(screen.getByRole('button', { name: 'first' }));
    expect(onOpen).toHaveBeenCalledOnce();
  });

  // Regression, user report about the star on iOS ("first I don't see the star
  // automatically, only after I do slide once again ... there is some mismatch with
  // the state"): a verb closed the row with an animated slide home that the platform
  // could drop, so the strip stayed over a row whose state said shut. A verb puts the
  // row home at once, because starring moves the row in the same commit.
  it('puts the row home at once when a verb is pressed', () => {
    const onSelect = vi.fn();
    render(
      <SwipeActions
        label="first"
        actions={[{ key: 'favorite', label: 'Star', icon: <StarIcon />, tone: 'accent', onSelect }]}
      >
        <span>row</span>
      </SwipeActions>,
    );
    drag(track(), WIDTH);
    later();

    fireEvent.click(screen.getByRole('button', { name: 'Star' }));
    expect(onSelect).toHaveBeenCalledOnce();
    expect(offset()).toBe('');
    expect((track().firstElementChild as HTMLElement).style.transition).toBe('');
  });

  it('makes an open row a dismiss target, never a navigation', () => {
    const onOpen = vi.fn();
    render(row('first', onOpen));
    drag(track(), WIDTH);
    later();

    fireEvent.click(screen.getByRole('button', { name: 'first' }));
    // A thumb resting on the slid row closes it and goes nowhere.
    expect(onOpen).not.toHaveBeenCalled();
    expect(offset()).toBe('');
  });

  // Regression, user report about the star on iOS, second round (paraphrased: from the
  // fifth row down the star did not arrive on the first tap, only after sliding the row a
  // second time): every poll re-sorts the list and MOVES a row's node. The native track
  // went home on that move without an event, so the strip and `open` disagreed. The
  // offset is now the row's own style, and it moves with the node.
  it('keeps its place and its state when a re-sort moves the row', () => {
    const onOpen = vi.fn();
    const { rerender } = render(
      <>
        {row('first')}
        {row('second', onOpen)}
      </>,
    );
    drag(track(1), WIDTH);
    later();

    rerender(
      <>
        {row('second', onOpen)}
        {row('first')}
      </>,
    );
    expect(offset(0)).toBe(`-${WIDTH}px`);
    fireEvent.click(screen.getByRole('button', { name: 'second' }));
    expect(onOpen).not.toHaveBeenCalled();
    expect(offset(0)).toBe('');
  });

  it('opens the drawer for a focus that reaches a verb, so the clip never hides it', () => {
    render(row('first'));
    act(() => screen.getByRole('button', { name: 'Rename' }).focus());
    expect(offset()).toBe(`-${WIDTH}px`);
  });

  it('treats the permanent control as a dismiss target while the touch drawer is open', () => {
    const onDetails = vi.fn();
    render(
      <SwipeActions
        actions={[{ key: 'delete', label: 'Delete', icon: <TrashIcon />, onSelect: vi.fn() }]}
        trailing={<button onClick={onDetails}>Details</button>}
      >
        <span>row</span>
      </SwipeActions>,
    );
    drag(track(), WIDTH);
    later();
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    expect(onDetails).not.toHaveBeenCalled();
    expect(offset()).toBe('');
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    expect(onDetails).toHaveBeenCalledOnce();
  });
});
