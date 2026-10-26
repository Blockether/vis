// @vitest-environment jsdom
import { act, fireEvent, render, screen } from '@testing-library/react';
import { renderToStaticMarkup } from 'react-dom/server';
import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';

import { SwipeActions } from './SwipeActions';
import { PencilIcon, StarIcon, TrashIcon } from './icons';

// Touch keeps its swipe gesture; desktop actions live behind one vertical-dot trigger.

// Regression, user report ("the colour is the same as rename"): every action on the
// strip wore one neutral ink, so the star — the mark the human types in themselves,
// and the only yellow thing in the list — looked like one more grey verb.
describe('SwipeActions tones', () => {
  const strip = (tone?: 'neutral' | 'accent' | 'danger') =>
    renderToStaticMarkup(
      <SwipeActions
        label="a session"
        actions={[{ key: 'favorite', label: 'Star', icon: <StarIcon />, tone, onSelect: () => {} }]}
      >
        <span>row</span>
      </SwipeActions>,
    );

  // A neutral verb means nothing in colour, which is what an accent action must not
  // look like. It still owes paper of its own: `panel-2` equals `surface` in every
  // shipped palette, so the cell used to BE the row's paper, and its ink is the
  // theme's, because amber here says "waiting on a human".
  it('leaves a neutral action in the shared verb ink, on paper of its own', () => {
    const html = strip();
    expect(html).not.toContain('bg-accent/15');
    expect(html).toContain('bg-hover text-white');
    expect(html).not.toContain('bg-panel-2 text-accent-ink');
  });

  // The same split, in red: `--err` is a badge fill and reads 3.50:1 as a caption
  // on its own tint, under the 4.5 a 9px bold label owes. The list-safe pair
  // (`err-surface` + `err-ink`, the tokens the in-row delete confirm already uses)
  // reads 5.4:1 without turning half the row into an alarm.
  it('paints a danger action in the list-safe red, not the badge fill', () => {
    const html = strip('danger');
    expect(html).not.toContain('bg-err/15');
  });

  // Regression, user report (a screenshot of the open drawer, asking for better colours
  // between the icons): the cells were divided by `dialog-edge`, a dialog FRAME inside a
  // list row — 9.57:1 against the row paper in blockether-light, eight times the 1.18:1
  // of the rule between two rows, and so the loudest line on the screen. Each tone also
  // brought its own, so the rule left of `Delete` was red and the one left of `Rename`
  // near-black. One rule, the list's own, divides the whole strip.
  it('divides the strip with the list rule, not a dialog frame', () => {
    for (const html of [strip(), strip('accent'), strip('danger')]) {
      expect(html).toContain('border-l border-edge-strong');
      expect(html).not.toContain('border-dialog-edge bg-panel-2');
      expect(html).not.toContain('border-err-edge bg-err-surface');
      expect(html).not.toContain('border-accent/40 bg-accent/15');
    }
  });

  // Regression, user report ("this also has not full height of the parent"): the swipe
  // track is as tall as its TALLEST panel, and the action strip — a 16px icon over a
  // 10px caption — measured 34px against a 32px desktop session row. The row panel
  // stretched to 34 but the row inside it stayed 32, so the button's hover slab stopped
  // 2px short of the rule under it. The panel is a GRID: one child, stretched on both
  // axes, so whatever height the track ends up with is the row's height too.
  it('lets the row fill the swipe track', () => {
    expect(strip()).toContain('grid w-full shrink-0 grid-cols-[minmax(0,1fr)_auto] mouse:contents');
  });

  // The verbs are under the row's TRAILING edge and nowhere else: the row is panel
  // one at full width and the strip is panel two, so the slide always uncovers them
  // on the right.
  it('keeps the strip on the trailing edge, behind the whole row', () => {
    const html = strip();
    expect(html.indexOf('grid w-full')).toBeLessThan(html.indexOf('role="group"'));
  });
});

// The shared list inset left empty paper after the last action on mobile.
it.each([false, true])(
  'ends the touch strip at its last action, alignMenuWithHeader=%s',
  (alignMenuWithHeader) => {
    render(
      <SwipeActions
        label="a session"
        alignMenuWithHeader={alignMenuWithHeader}
        actions={[{ key: 'delete', label: 'Delete', icon: <TrashIcon />, onSelect: () => {} }]}
      >
        <span>row</span>
      </SwipeActions>,
    );
    const strip = screen.getByRole('group', { name: 'a session actions' });
    expect(strip).toHaveClass('shrink-0');
    expect(strip.className).not.toMatch(/\b(?:p|px|pr|pe)-/);
    expect(strip.lastElementChild).toBe(screen.getByRole('button', { name: 'Delete' }));
  },
);

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

// Regression, user report (paraphrased: on the desktop that sideways scrolling is
// broken and should not be there at all): every row of both lists was a scroll-snap
// track 288px wider than its own box, and a pointer has no swipe — so the only way
// one could reach it was a two-finger trackpad drag or shift+wheel, which slid a
// delete button under the cursor on whichever rows the sideways gesture crossed.
// A mouse gets no track and no scroll: one button opens the row's dropdown.
describe('a mouse never slides', () => {
  const markup = (alignMenuWithHeader = false) =>
    renderToStaticMarkup(
      <SwipeActions
        label="a session"
        alignMenuWithHeader={alignMenuWithHeader}
        actions={[
          {
            key: 'delete',
            label: 'Delete',
            icon: <TrashIcon />,
            tone: 'danger',
            onSelect: () => {},
          },
        ]}
      >
        <span>row</span>
      </SwipeActions>,
    );

  it('reserves only a menu trigger and hides the touch strip from a pointer', () => {
    const html = markup();
    expect(html).toContain('aria-label="Actions for a session"');
    expect(html).toContain('aria-haspopup="dialog"');
    expect(html).toContain('shrink-0 mouse:hidden');
    expect(html).not.toContain('group-hover/swipe:opacity');
  });

  // Regression, settings screenshot: a section action stood 8px to the right of its
  // row menu. Session lists keep their original inset and touch drawers stay put.
  it('aligns only opted-in desktop menus with settings header actions', () => {
    const session = markup();
    const settings = markup(true);
    expect(session).toContain('mouse:flex pr-3 sm:pr-4');
    expect(settings).toContain('mouse:flex pr-1 sm:pr-2');
    expect(settings).toContain('shrink-0 mouse:hidden');
  });

  // Regression, user report (paraphrased: a plus standing on the left is unacceptable,
  // the three dots belong on the right): a CSS reorder used to pull the menu trigger in
  // front of the row's permanent control on a pointer, so the dots of a row and the dots
  // of the header above it stood in different columns. Source order is the painted order
  // now: the permanent control first, the menu last.
  it('ends a pointer row with its menu, the permanent control one slot inside', () => {
    const html = renderToStaticMarkup(
      <SwipeActions
        label="a session"
        actions={[{ key: 'delete', label: 'Delete', icon: <TrashIcon />, onSelect: () => {} }]}
        trailing={<button type="button">Show details</button>}
      >
        <span>row</span>
      </SwipeActions>,
    );

    expect(html.indexOf('Show details')).toBeLessThan(
      html.indexOf('aria-label="Actions for a session"'),
    );
    expect(html).not.toContain('order-last');
  });

  // Regression, user report right after BLO-167 shipped: the kebab on a session row still
  // ran across the row. The trigger turned its mark a quarter turn, which was right while
  // `DotsIcon` drew a horizontal ellipsis and laid the vertical one on its side once the
  // icon itself started standing up.
  it('opens the row menu off a standing kebab, not one on its side', () => {
    const html = markup();
    const dots = [...html.matchAll(/<circle[^>]*>/g)].map(([tag]) => ({
      cx: Number(/cx="([\d.]+)"/.exec(tag)?.[1]),
      cy: Number(/cy="([\d.]+)"/.exec(tag)?.[1]),
    }));

    expect(dots).toHaveLength(3);
    expect(new Set(dots.map(({ cx }) => cx)).size).toBe(1);
    expect(new Set(dots.map(({ cy }) => cy)).size).toBe(3);
    expect(html).not.toContain('rotate-90');
  });

  // Row actions must not cover permanent controls such as session details.
  it('reserves space beside row controls rather than overlaying them', () => {
    const html = markup();
    expect(html).not.toContain('mouse:absolute');
    expect(html).toContain('mouse:flex-1');
    expect(html).toContain('>Delete</span>');
    expect(html).toContain('pr-3 sm:pr-4');
  });
});

it.each([false, true])('retains one permanent control with actions=%s', (hasActions) => {
  const onDetails = vi.fn();
  render(
    <SwipeActions
      actions={
        hasActions
          ? [{ key: 'delete', label: 'Delete', icon: <TrashIcon />, onSelect: vi.fn() }]
          : []
      }
      trailing={<button onClick={onDetails}>Details</button>}
    >
      <button>Open session</button>
    </SwipeActions>,
  );
  expect(screen.getAllByRole('button', { name: 'Details' })).toHaveLength(1);
  fireEvent.click(screen.getByRole('button', { name: 'Details' }));
  expect(onDetails).toHaveBeenCalledOnce();
  expect(screen.getByRole('button', { name: 'Open session' })).toBeInTheDocument();
});

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

  it('keeps captioned touch actions separate from the desktop trigger', () => {
    render(row('first'));
    const strip = screen.getByRole('group', { name: 'first actions' });
    expect(
      Array.from(strip.querySelectorAll('button')).map((button) =>
        button.getAttribute('aria-label'),
      ),
    ).toEqual(['Rename', 'Delete']);
    expect(screen.getByRole('button', { name: 'Actions for first' })).toHaveAttribute(
      'aria-expanded',
      'false',
    );
    expect(screen.getByRole('group', { name: 'first actions' })).toBeVisible();
  });

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

  // A row whose owner wired no verb has no drawer at all: `ConnectScreen` lists
  // machines as places to GO, and a track there would slide onto an empty strip.
  it('renders the row bare when it has no verbs', () => {
    render(
      <SwipeActions label="first" actions={[]}>
        <button type="button">first</button>
      </SwipeActions>,
    );
    expect(document.querySelector('.snap-x')).toBeNull();
  });

  // The caption is one word wide because the cell is 72px; the whole sentence is
  // what a screen reader hears, so `Primary` on a machine row is `Make tower primary`.
  it('lets a verb name the thing it acts on without widening the cell', () => {
    render(
      <SwipeActions
        label="tower"
        actions={[
          {
            key: 'primary',
            label: 'Primary',
            name: 'Make tower primary',
            icon: <StarIcon />,
            tone: 'accent',
            onSelect: () => {},
          },
        ]}
      >
        <span>row</span>
      </SwipeActions>,
    );
    const verb = screen.getByRole('button', { name: 'Make tower primary' });
    expect(verb.textContent).toBe('Primary');
    expect(verb.getAttribute('title')).toBe('Make tower primary');
  });

  // Regression, user report with a phone screenshot (440px): the project band ran
  // past the list — the path and the counts under the name were not truncated, and
  // the pager and the `+` stood off the edge, a bare `1` showing past the rows'
  // chevron column. The panel this track holds the row in is a grid, and a grid
  // item's automatic minimum is its min-content width: a band whose every part is
  // `nowrap` widened the track to 528px of a 393px list. The column must be free
  // to be narrower than what it holds, so the band inside truncates instead.
  it('lets the row it holds be narrower than its own nowrap content', () => {
    const html = renderToStaticMarkup(
      <SwipeActions
        label="a project"
        actions={[{ key: 'delete', label: 'Delete', icon: <TrashIcon />, onSelect: () => {} }]}
      >
        <span>row</span>
      </SwipeActions>,
    );
    const panel = /<div class="([^"]*\bgrid\b[^"]*)"/.exec(html)?.[1] ?? '';
    expect(panel).toContain('w-full');
    expect(panel).toContain('grid-cols-[minmax(0,1fr)_auto]');
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

// Regression, user report with a screenshot (paraphrased: pressing a row highlights
// only part of it, while the chevron keeps a different background). The press was
// painted by the row's own button, and that button is ONE CELL of a grid whose other
// cells are the permanent trailing controls and the desktop menu trigger. Everything
// standing beside it — the disclosure chevron, the kebab — kept the list's paper, so a
// pressed row read as half-selected. The row paints the press now, not the button.
describe('a press paints the whole row', () => {
  const PRESSED = 'has-[[data-row-surface]:active]:bg-hover';
  const FOCUSED = 'has-[[data-row-surface]:focus-visible]:bg-hover';

  const surface = <button type="button" data-row-surface="">row</button>;

  // Both layouts need their own paper: the desktop row IS the track, because the panel
  // inside it is `contents` there, and the touch row is that panel at full width.
  it('paints the track and the touch panel alike', () => {
    render(
      <SwipeActions
        label="a session"
        actions={[{ key: 'delete', label: 'Delete', icon: <TrashIcon />, onSelect: () => {} }]}
        trailing={<button type="button">Details</button>}
      >
        {surface}
      </SwipeActions>,
    );
    const painted: Element[] = [];
    for (
      let node = screen.getByRole('button', { name: 'row' }).parentElement;
      node;
      node = node.parentElement
    ) {
      if (node.className.includes(PRESSED)) painted.push(node);
    }
    expect(painted).toHaveLength(2);
    expect(painted.every((node) => node.className.includes(FOCUSED))).toBe(true);
    expect(painted.some((node) => node.hasAttribute('data-swipe-track'))).toBe(true);
  });

  it('keeps the trailing controls inside the paper the press paints', () => {
    render(
      <SwipeActions actions={[]} trailing={<button type="button">Details</button>}>
        {surface}
      </SwipeActions>,
    );
    const paper = screen.getByRole('button', { name: 'row' }).closest(`[class*="${PRESSED}"]`);
    expect(paper?.contains(screen.getByRole('button', { name: 'Details' }))).toBe(true);
  });

  // A row with no pressable half of its own — a static machine row, a skeleton — simply
  // never matches, so the paper stays the list's own.
  it('lights up for nothing but a row surface', () => {
    render(
      <SwipeActions actions={[]} trailing={<button type="button">Details</button>}>
        <span>row</span>
      </SwipeActions>,
    );
    const details = screen.getByRole('button', { name: 'Details' });
    expect(details.closest(`[class*="${PRESSED}"]`)?.querySelector('[data-row-surface]')).toBe(
      null,
    );
  });
});

// Regression, user reports (paraphrased: scrolling over a big group on the phone was extremely
// slow, then a hard swipe over a 445-session group lagged and showed blank stripes): each row's
// track was a native horizontal scroller, and WebKit paid for each one in the list's own scroll.
describe('a row is never a native scroller', () => {
  it('clips the row and leaves the page only the vertical pan', () => {
    const html = renderToStaticMarkup(
      <SwipeActions
        label="a session"
        actions={[{ key: 'rename', label: 'Rename', icon: <PencilIcon />, onSelect: () => {} }]}
      >
        <span>row</span>
      </SwipeActions>,
    );
    const track = /<div data-swipe-track="" class="([^"]*)"/.exec(html)?.[1] ?? '';
    expect(track.split(/\s+/)).toEqual(expect.arrayContaining(['overflow-clip', 'touch-pan-y']));
    expect(html).not.toMatch(/overflow-x-auto|overflow-auto/);
    expect(html).not.toMatch(/snap-x|snap-mandatory|snap-start|snap-end/);
  });
});
