// @vitest-environment jsdom
import { act, render, screen, within } from '@testing-library/react';
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
    expect(verbsOf('laptop')).toEqual(['Make primary', 'Rename', 'Forget']);
    // `tower` is not the machine this column is reading, and it carries its own
    // verbs anyway: the row under the thumb is the row that acts.
    expect(verbsOf('tower')).toEqual(['Rename', 'Forget']);
  });

  it('opens one machine at a time', () => {
    fleet();
    // jsdom lays nothing out: each strip gets a phone's width for its verbs.
    const width = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'offsetWidth');
    Object.defineProperty(HTMLElement.prototype, 'offsetWidth', {
      configurable: true,
      get: () => 96,
    });
    try {
      const offset = (machine: string) =>
        (screen.getByRole('group', { name: `${machine} actions` }).previousElementSibling
          ?.previousElementSibling as HTMLElement).style.translate;
      // A verb that takes the focus opens its row, as a slide does.
      act(() => stripOf('tower').getAllByRole('button')[0]!.focus());
      expect(offset('tower')).toBe('-96px');
      act(() => stripOf('laptop').getAllByRole('button')[0]!.focus());
      // Opening the second row closed the first: two rows standing open is a list
      // with two right edges and a red verb armed under a thumb that moved on.
      expect(offset('tower')).toBe('');
      expect(offset('laptop')).toBe('-96px');
    } finally {
      if (width) Object.defineProperty(HTMLElement.prototype, 'offsetWidth', width);
    }
  });

  it('renames in the row, and an empty name gives the machine its host back', async () => {
    const user = userEvent.setup();
    const onRename = vi.fn();
    fleet({ onRename });

    await user.click(stripOf('laptop').getByRole('button', { name: 'Rename' }));
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
    await user.click(stripOf('laptop').getByRole('button', { name: 'Rename' }));
    await user.clear(screen.getByRole('textbox', { name: 'Rename laptop' }));
    await user.tab();
    expect(onRename).toHaveBeenLastCalledWith(laptop, undefined);
  });

  it('asks before it forgets, in the row the verb was pressed in', async () => {
    const user = userEvent.setup();
    const onForget = vi.fn();
    fleet({ onForget });

    await user.click(stripOf('laptop').getByRole('button', { name: 'Forget' }));
    const ask = screen.getByRole('group', { name: 'Forget laptop?' });
    expect(
      within(ask)
        .getAllByRole('button')
        .map((b) => b.textContent),
    ).toEqual(['No, keep', 'Yes, forget']);
    // The question stands alone: no sentence under it about the token or pairing.
    expect(within(ask).queryByText(/access token/)).toBeNull();

    await user.click(within(ask).getByRole('button', { name: 'No, keep' }));
    expect(onForget).not.toHaveBeenCalled();
    expect(stripOf('laptop').getByRole('button', { name: 'Forget' })).toBeVisible();

    await user.click(stripOf('laptop').getByRole('button', { name: 'Forget' }));
    await user.click(screen.getByRole('button', { name: 'Yes, forget' }));
    expect(onForget).toHaveBeenCalledWith(laptop);
  });
});
