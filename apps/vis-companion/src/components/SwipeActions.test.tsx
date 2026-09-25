// @vitest-environment jsdom
import { act, fireEvent, render, screen } from '@testing-library/react';
import { renderToStaticMarkup } from 'react-dom/server';
import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';

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
    expect(strip()).toContain('grid w-full shrink-0 grid-cols-[minmax(0,1fr)_auto] snap-start');
  });

  // The verbs are under the row's TRAILING edge and nowhere else: the row is panel
  // one at full width and the strip is panel two, so the slide always uncovers them
  // on the right, and a slide that stops halfway snaps to one of the two.
  it('keeps the strip on the trailing edge, behind the whole row', () => {
    const html = strip();
    expect(html.indexOf('snap-start')).toBeLessThan(html.indexOf('snap-end'));
    expect(html).toContain('snap-x snap-mandatory');
  });
});

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

  it('takes the horizontal track away from a pointer', () => {
    const html = markup();
    expect(html).toContain('mouse:snap-none');
    // The touch surface is untouched: a finger still slides the same snap track.
    expect(html).toContain('snap-x snap-mandatory overflow-x-auto');
  });

  it('reserves only a menu trigger and hides the touch strip from a pointer', () => {
    const html = markup();
    expect(html).toContain('aria-label="Actions for a session"');
    expect(html).toContain('aria-haspopup="dialog"');
    expect(html).toContain('snap-end mouse:hidden');
    expect(html).not.toContain('group-hover/swipe:opacity');
  });

  // Regression, settings screenshot: a section action stood 8px to the right of its
  // row menu. Session lists keep their original inset and touch drawers stay put.
  it('aligns only opted-in desktop menus with settings header actions', () => {
    const session = markup();
    const settings = markup(true);
    expect(session).toContain('mouse:flex pr-3 sm:pr-4');
    expect(settings).toContain('mouse:flex pr-1 sm:pr-2');
    expect(settings).toContain('snap-end mouse:hidden pr-3 sm:pr-4');
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
    document.querySelectorAll<HTMLElement>('.snap-x')[index] as HTMLElement;

  /** What a thumb does: the platform scrolls the track, the component reads it. */
  function slide(element: HTMLElement) {
    Object.defineProperty(element, 'scrollLeft', { value: 96, configurable: true });
    fireEvent.scroll(element);
  }

  /** Every drawer this component closed, in the order it closed them. */
  let closed: Element[] = [];
  /** How each of those closes asked to travel: the animation is the bug. */
  let asked: ScrollToOptions[] = [];
  const scrollTo = Element.prototype.scrollTo;
  beforeAll(() => {
    Element.prototype.scrollTo = function record(this: Element, options?: ScrollToOptions) {
      closed.push(this);
      if (options) asked.push(options);
    } as typeof Element.prototype.scrollTo;
  });
  afterEach(() => {
    closed = [];
    asked = [];
  });
  afterAll(() => {
    Element.prototype.scrollTo = scrollTo;
  });

  const row = (label: string, onOpen = () => {}) => (
    <SwipeActions
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

  it('opens one row at a time, so no second row keeps a delete armed', () => {
    render(
      <>
        {row('first')}
        {row('second')}
      </>,
    );
    slide(track(0));
    expect(closed).not.toContain(track(0));

    slide(track(1));
    // The row that was open is the row that closed — nobody pressed it.
    expect(closed).toContain(track(0));
    expect(closed).not.toContain(track(1));
  });

  // The row that CLOSES must not take the row that opened with it. `close()` animates
  // its own scrollLeft home and every frame of that animation is a scroll event on the
  // window; read as "the list moved under me", it shut the drawer the thumb had just
  // opened — measured in the browser as both rows sliding back to 0 together.
  it('keeps the opened row open while the row it replaced slides shut', () => {
    render(
      <>
        {row('first')}
        {row('second')}
      </>,
    );
    slide(track(0));
    slide(track(1));
    const shut = () => closed.filter((element) => element === track(1)).length;
    expect(shut()).toBe(0);

    // The first row animating home, frame by frame.
    fireEvent.scroll(track(0));
    expect(shut()).toBe(0);

    // The LIST moving under it still closes it: that scroll comes from a
    // scroller the row itself stands in.
    fireEvent.scroll(document.body);
    expect(shut()).toBe(1);
  });

  // Regression, user report (paraphrased: every update to the session hides what
  // I have open): a transcript following its end rewrites its own `scrollTop` on
  // every update it receives, and on the window that reached an open drawer as
  // "the list moved under me" — from a scroller the row does not stand in.
  it('keeps the drawer open while a scroller that does not carry the row moves', () => {
    render(row('first'));
    slide(track(0));
    const elsewhere = document.body.appendChild(document.createElement('div'));

    fireEvent.scroll(elsewhere);

    expect(closed).not.toContain(track(0));
    elsewhere.remove();
  });

  // A drawer animating home reports itself OPEN for every frame of that slide, and
  // the row took those frames for a fresh gesture: it re-opened itself, and in
  // re-opening closed whichever row had just replaced it.
  it('stays shut while it slides home, so the row is a navigation again', () => {
    const onOpen = vi.fn();
    render(row('first', onOpen));
    slide(track(0));
    fireEvent.keyDown(window, { key: 'Escape' });

    // Mid-animation: the track keeps reporting the offset it has not given back yet.
    fireEvent.scroll(track(0));
    fireEvent.click(screen.getByRole('button', { name: 'first' }));
    expect(onOpen).toHaveBeenCalledTimes(1);
  });

  it('closes on Escape, and gives the row back to the list', () => {
    render(row('first'));
    slide(track(0));
    fireEvent.keyDown(window, { key: 'Escape' });
    expect(closed).toContain(track(0));
  });

  // Regression, user report about the star on iOS ("first I don't see the star
  // automatically, only after I do slide once again ... there is some mismatch with
  // the state"): a verb closed the row by ASKING for an animated slide home while
  // `open` flipped on the spot, so the one time the platform declined that animation
  // — an animated `scrollTo` inside a mandatory scroll-snap track, measured in WebKit
  // still 216px from home 800ms after the call — the strip stayed standing over a row
  // whose state said shut. The mark the verb had just left was off-screen to the left
  // behind that strip, the row was a navigation again, and the next row opened beside
  // it instead of in place of it.
  it("takes the way home out of the platform's hands when a verb is pressed", () => {
    const onSelect = vi.fn();
    render(
      <SwipeActions
        label="first"
        actions={[{ key: 'favorite', label: 'Star', icon: <StarIcon />, tone: 'accent', onSelect }]}
      >
        <span>row</span>
      </SwipeActions>,
    );
    slide(track(0));

    fireEvent.click(screen.getByRole('button', { name: 'Star' }));
    expect(onSelect).toHaveBeenCalledTimes(1);
    expect(closed).toContain(track(0));
    // Home in the same frame the verb was pressed: no animation to lose, whatever
    // the row does next — and starring moves this row to the top of its project.
    expect(asked).toEqual([{ left: 0, behavior: 'auto' }]);
  });

  it('makes an open row a dismiss target, never a navigation', () => {
    const onOpen = vi.fn();
    render(row('first', onOpen));
    slide(track(0));

    fireEvent.click(screen.getByRole('button', { name: 'first' }));
    // A thumb resting on the slid row closes it and goes nowhere.
    expect(onOpen).not.toHaveBeenCalled();
    expect(closed).toContain(track(0));
  });

  // Regression, user report about the star on iOS, second round (paraphrased: from the
  // fifth row down the star did not arrive on the first tap, only after sliding the row a
  // second time, and the row's state disagreed with what was on the screen): the rows
  // above it are LIVE, every poll re-sorts the list, and a re-sort MOVES this row's node
  // to its new place. WebKit returns a moved scroller home in the same task and fires NO
  // scroll event for it — measured on iOS 26.5, Safari: 216 -> 0 synchronously, zero
  // scroll events — so the strip left the screen while `open` went on saying it was
  // standing there, and the row, which is a dismiss target while open, ate the tap that
  // was meant for the star. Only a second slide, which does fire events, put them back
  // in step.
  it('gives the row back the frame the platform shuts its drawer without an event', async () => {
    const onOpen = vi.fn();
    render(row('first', onOpen));
    slide(track(0));

    // The re-sort: the node is moved, the offset is gone with it, and nothing said so.
    Object.defineProperty(track(0), 'scrollLeft', { value: 0, configurable: true });
    await act(() => new Promise<void>((resolve) => requestAnimationFrame(() => resolve())));

    fireEvent.click(screen.getByRole('button', { name: 'first' }));
    // The tap the star was waiting for reaches the row, first time.
    expect(onOpen).toHaveBeenCalledTimes(1);
    // And nothing was asked to travel: the platform had already taken it home.
    expect(closed).not.toContain(track(0));
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
    slide(track());
    fireEvent.click(screen.getByRole('button', { name: 'Details' }));
    expect(onDetails).not.toHaveBeenCalled();
    expect(closed).toContain(track());
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
