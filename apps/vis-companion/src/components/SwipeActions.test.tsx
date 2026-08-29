// @vitest-environment jsdom
import { act, fireEvent, render, screen } from '@testing-library/react';
import { renderToStaticMarkup } from 'react-dom/server';
import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';

import { SwipeActions } from './SwipeActions';
import { PencilIcon, StarIcon, TrashIcon } from './icons';

// Regression, user reports about row verbs, in the order they arrived: "I don't
// need the ⋯ and swiping" — the mark beside the gesture said nothing and opened a
// menu holding what the gesture already held; "the swipe should be always right
// without this ⋯" — the slide stays, the mark goes; and then, after the verbs had
// been moved into permanently painted marks in every row, "you removed the slides
// from the session list and also from the machine — we should have the slide and
// just fix it". So the slide is the ONE row-verb surface, on both lists, with no
// mark standing beside it.

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

  // The strip's own colour lives in the slab, so a neutral verb has none: it is
  // the ink alone on the panel, which is what an accent action must not look like.
  it('leaves a neutral action in the shared verb ink', () => {
    const html = strip();
    expect(html).not.toContain('bg-accent/15');
  });

  // The same split, in red: `--err` is a badge fill and reads 3.50:1 as a caption
  // on its own tint, under the 4.5 a 9px bold label owes. The list-safe pair
  // (`err-surface` + `err-ink`, the tokens the in-row delete confirm already uses)
  // reads 5.4:1 without turning half the row into an alarm.
  it('paints a danger action in the list-safe red, not the badge fill', () => {
    const html = strip('danger');
    expect(html).not.toContain('bg-err/15');
  });

  // Regression, user report ("this also has not full height of the parent"): the swipe
  // track is as tall as its TALLEST panel, and the action strip — a 16px icon over a
  // 10px caption — measured 34px against a 32px desktop session row. The row panel
  // stretched to 34 but the row inside it stayed 32, so the button's hover slab stopped
  // 2px short of the rule under it. The panel is a GRID: one child, stretched on both
  // axes, so whatever height the track ends up with is the row's height too.
  it('lets the row fill the swipe track', () => {
    expect(strip()).toContain('grid w-full shrink-0 snap-start');
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
// A mouse gets no track and no scroll: the strip stands at the trailing edge and
// fades in on hover or on keyboard focus.
describe('a mouse never slides', () => {
  const markup = () =>
    renderToStaticMarkup(
      <SwipeActions
        label="a session"
        actions={[{ key: 'delete', label: 'Delete', icon: <TrashIcon />, tone: 'danger', onSelect: () => {} }]}
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

  it('reveals the strip on hover and on keyboard focus instead', () => {
    const html = markup();
    expect(html).toContain('group/swipe');
    expect(html).toContain('mouse:group-hover/swipe:opacity-100');
    expect(html).toContain('mouse:group-focus-within/swipe:opacity-100');
  });

  // Regression, user report (paraphrased: make sure things do not sit on top of
  // each other): the strip stood over the row's own status chip and timestamp
  // with no slab of its own, so an `accent/15` cell printed its caption over an
  // `IDLE` that read straight through it — and while invisible it still took the
  // pointer, four unseeable buttons covering the date of every row.
  // The slab is the row's HOVER tint, not the card's paper: the strip is only ever
  // seen while its row is hovered or focused, and `bg-panel` cut a plain-paper
  // rectangle out of the trailing end of the one row the pointer was lighting.
  it('owns the pixels it stands on, and takes no pointer while unseen', () => {
    const html = markup();
    expect(html).toContain('mouse:group-hover/swipe:pointer-events-auto');
    expect(html).toContain('mouse:group-focus-within/swipe:pointer-events-auto');
  });
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

  it('offers the verbs with no mark standing beside them', () => {
    render(row('first'));
    // Three buttons in this row: the row itself and its two verbs. A `⋯` would be
    // a fourth — the control the report asked to be rid of.
    expect(screen.getAllByRole('button').map((b) => b.getAttribute('aria-label'))).toEqual([
      null,
      'Rename',
      'Delete',
    ]);
    expect(screen.queryByRole('button', { name: /^Actions for/ })).toBeNull();
    expect(screen.getByRole('group', { name: 'first actions' })).toBeTruthy();
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

    // The LIST moving under it still closes it: that scroll comes from something
    // that is not a drawer.
    fireEvent.scroll(document.body);
    expect(shut()).toBe(1);
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
  it('takes the way home out of the platform\'s hands when a verb is pressed', () => {
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
});
