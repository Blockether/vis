// @vitest-environment jsdom
import { fireEvent, render, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

import type { GatewayConn } from '../lib/types';
import { MachineRows } from './Machines';

// Regression, user reports about the machines column, in the order they arrived:
// "I don't need the ⋯ and swiping" and then "the swipe should be always right
// without this ⋯" — the mark beside the gesture said nothing and opened a menu
// holding what the gesture already held. The verbs were moved first into a strip
// of full-width words under the ONE row the column was reading, then into marks
// painted permanently in every row's trailing cell, and the report on that was
// "you removed the slides from the session list and also from the machine — we
// should have the slide and just fix it". Touch keeps that slide without a menu;
// desktop now uses one dropdown instead of a row of hover icons.

const tower: GatewayConn = { url: 'http://10.0.0.5:7890', label: 'tower' };
const laptop: GatewayConn = { url: 'http://10.0.0.6:7890', label: 'laptop' };

/** The settings column's list: every verb wired, `tower` ranked, `laptop` read. */
function fleet(props: Partial<Parameters<typeof MachineRows>[0]> = {}) {
  return render(
    <MachineRows
      conns={[tower, laptop]}
      primaryUrl={tower.url}
      selectedUrl={laptop.url}
      health={{}}
      onPick={() => {}}
      onMakePrimary={() => {}}
      onRename={() => {}}
      onForget={() => {}}
      {...props}
    />,
  );
}

/** The strip waiting under ONE machine's row. */
const stripOf = (machine: string) =>
  within(screen.getByRole('group', { name: `${machine} actions` }));

/** What that machine can be told to do, in the order it is offered. */
const verbsOf = (machine: string) =>
  stripOf(machine)
    .getAllByRole('button')
    .map((button) => button.getAttribute('aria-label') ?? '');

describe('a machine keeps its verbs under its own row', () => {
  it('gives EVERY row a strip, not only the row being read', () => {
    fleet();
    expect(verbsOf('laptop')).toEqual(['Make laptop primary', 'Rename laptop', 'Forget laptop']);
    // `tower` is not the machine this column is reading, and it carries its own
    // verbs anyway: the row under the thumb is the row that acts.
    expect(verbsOf('tower')).toEqual(['Rename tower', 'Forget tower']);
  });

  it('keeps the rank verb off the machine that already holds the rank', () => {
    fleet({ selectedUrl: tower.url });
    expect(verbsOf('tower')).toEqual(['Rename tower', 'Forget tower']);
    expect(verbsOf('laptop')).toEqual(['Make laptop primary', 'Rename laptop', 'Forget laptop']);
  });

  it('keeps sliding on touch and reserves the dropdown for a mouse', () => {
    fleet();
    // The row is the whole width and the strip waits past its trailing edge: one
    // snap track per row, the row first, the verbs second.
    const tracks = document.querySelectorAll('.snap-x');
    expect(tracks).toHaveLength(2);
    const html = tracks[0]!.innerHTML;
    expect(html.indexOf('snap-start')).toBeLessThan(html.indexOf('snap-end'));

    // The menu is desktop-only: nothing stands beside the touch gesture.
    const menus = screen.getAllByRole('button', { name: /^Actions for/ });
    expect(menus).toHaveLength(2);
    for (const menu of menus) {
      expect(menu.parentElement).toHaveClass('hidden', 'mouse:flex');
    }
    // Two machine rows, five touch verbs and two desktop menu triggers.
    expect(screen.getAllByRole('button')).toHaveLength(9);

    // The captions stay one word wide — the cell is 72px — while the accessible
    // name says which machine the verb acts on.
    expect(
      stripOf('laptop')
        .getAllByRole('button')
        .map((b) => b.textContent),
    ).toEqual(['Primary', 'Rename', 'Forget']);
  });

  it('opens one machine at a time', () => {
    fleet();
    const closed: Element[] = [];
    const scrollTo = Element.prototype.scrollTo;
    Element.prototype.scrollTo = function record(this: Element) {
      closed.push(this);
    };
    try {
      const tracks = [...document.querySelectorAll<HTMLElement>('.snap-x')];
      for (const track of tracks) {
        Object.defineProperty(track, 'scrollLeft', { value: 96, configurable: true });
        fireEvent.scroll(track);
      }
      // Sliding the second row closed the first: two rows standing open is a list
      // with two right edges and a red verb armed under a thumb that moved on.
      expect(closed).toContain(tracks[0]);
      expect(closed).not.toContain(tracks[1]);
    } finally {
      Element.prototype.scrollTo = scrollTo;
    }
  });

  it('renames in the row, and an empty name gives the machine its host back', async () => {
    const user = userEvent.setup();
    const onRename = vi.fn();
    fleet({ onRename });

    await user.click(stripOf('laptop').getByRole('button', { name: 'Rename laptop' }));
    const field = screen.getByRole('textbox', { name: 'Rename laptop' });
    expect(field).toHaveValue('laptop');
    // The row it replaced is gone while it is being typed in — strip and all: the
    // name is edited where it stands, not in a dialog opened over the list.
    expect(screen.queryByRole('group', { name: 'laptop actions' })).toBeNull();

    await user.clear(field);
    await user.type(field, 'workshop');
    await user.tab();
    expect(onRename).toHaveBeenCalledWith(laptop, 'workshop');

    // The list is the caller's state, so the row still reads `laptop` here:
    // clearing the field is what says "no name of its own".
    await user.click(stripOf('laptop').getByRole('button', { name: 'Rename laptop' }));
    await user.clear(screen.getByRole('textbox', { name: 'Rename laptop' }));
    await user.tab();
    expect(onRename).toHaveBeenLastCalledWith(laptop, undefined);
  });

  it('asks before it forgets, in the row the verb was pressed in', async () => {
    const user = userEvent.setup();
    const onForget = vi.fn();
    fleet({ onForget });

    await user.click(stripOf('laptop').getByRole('button', { name: 'Forget laptop' }));
    const ask = screen.getByRole('group', { name: 'Forget laptop?' });
    expect(
      within(ask)
        .getAllByRole('button')
        .map((b) => b.textContent),
    ).toEqual(['No, keep', 'Yes, forget']);
    // What it costs is said where it is asked: the panel this verb came from
    // spent a paragraph on it, and the answer is worthless without the sentence.
    expect(screen.getByText(/access token from this device/)).toBeTruthy();

    await user.click(within(ask).getByRole('button', { name: 'No, keep' }));
    expect(onForget).not.toHaveBeenCalled();
    expect(stripOf('laptop').getByRole('button', { name: 'Forget laptop' })).toBeTruthy();

    await user.click(stripOf('laptop').getByRole('button', { name: 'Forget laptop' }));
    await user.click(screen.getByRole('button', { name: 'Yes, forget' }));
    expect(onForget).toHaveBeenCalledWith(laptop);
  });
});
