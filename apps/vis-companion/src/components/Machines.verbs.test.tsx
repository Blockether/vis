// @vitest-environment jsdom
import { render, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

import type { GatewayConn } from '../lib/types';
import { MachineRows } from './Machines';

// Regression, user reports about the machines column ("I don't need the ⋯ and
// swiping", then "what for I need those two selected in red ... the swipe should
// be always right without this ⋯"): every verb a machine has — rank it primary,
// rename it, forget it — hid first behind a left swipe and a `⋯` at the end of the
// row, and then behind a strip of two full-width WORDS that opened under the ONE
// row the column happened to be reading. A gesture nobody can see, a mark that
// says nothing, and a second list of buttons under the first are the same bug
// three times over. The verbs are marks in every row's own trailing cell now.

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

/** The cell of verbs standing in ONE machine's row. */
const cellOf = (machine: string) =>
  within(screen.getByRole('group', { name: `${machine} actions` }));

/** What that machine can be told to do, in the order it is offered. */
const verbsOf = (machine: string) =>
  cellOf(machine)
    .getAllByRole('button')
    .map((button) => button.getAttribute('aria-label') ?? '');

describe('a machine wears its verbs in its own row', () => {
  it('stands them in EVERY row, not only in the row being read', () => {
    fleet();
    expect(verbsOf('laptop')).toEqual(['Make primary', 'Rename', 'Forget']);
    // `tower` is not the machine this column is reading, and it carries its own
    // verbs anyway: the row under the thumb is the row that acts.
    expect(verbsOf('tower')).toEqual(['Rename', 'Forget']);
  });

  it('keeps the rank verb off the machine that already holds the rank', () => {
    fleet({ selectedUrl: tower.url });
    expect(verbsOf('tower')).toEqual(['Rename', 'Forget']);
    expect(verbsOf('laptop')).toEqual(['Make primary', 'Rename', 'Forget']);
  });

  it('offers them as marks, and hides nothing behind a gesture', () => {
    fleet();
    const marks = cellOf('laptop').getAllByRole('button');
    // A mark, not a word: three captions per row are a second list running down
    // the trailing edge. The group names the row, so each glyph says only its verb.
    expect(marks.map((button) => button.textContent)).toEqual(['', '', '']);
    expect(marks.map((button) => button.getAttribute('title'))).toEqual([
      'Make primary',
      'Rename',
      'Forget',
    ]);
    // No `⋯`, no swipe track, and no strip of words under the row.
    expect(screen.queryByRole('button', { name: /^Actions for/ })).toBeNull();
    expect(document.querySelector('.snap-x')).toBeNull();
    // Two rows, five verbs, and every other control on this list is a machine.
    expect(screen.getAllByRole('button')).toHaveLength(7);
  });

  it('renames in the row, and an empty name gives the machine its host back', async () => {
    const user = userEvent.setup();
    const onRename = vi.fn();
    fleet({ onRename });

    await user.click(cellOf('laptop').getByRole('button', { name: 'Rename' }));
    const field = screen.getByRole('textbox', { name: 'Rename laptop' });
    expect(field).toHaveValue('laptop');
    // The row it replaced is gone while it is being typed in — verbs and all: the
    // name is edited where it stands, not in a dialog opened over the list.
    expect(screen.queryByRole('group', { name: 'laptop actions' })).toBeNull();

    await user.clear(field);
    await user.type(field, 'workshop');
    await user.tab();
    expect(onRename).toHaveBeenCalledWith(laptop, 'workshop');

    // The list is the caller's state, so the row still reads `laptop` here:
    // clearing the field is what says "no name of its own".
    await user.click(cellOf('laptop').getByRole('button', { name: 'Rename' }));
    await user.clear(screen.getByRole('textbox', { name: 'Rename laptop' }));
    await user.tab();
    expect(onRename).toHaveBeenLastCalledWith(laptop, undefined);
  });

  it('asks before it forgets, in the row the mark was pressed in', async () => {
    const user = userEvent.setup();
    const onForget = vi.fn();
    fleet({ onForget });

    await user.click(cellOf('laptop').getByRole('button', { name: 'Forget' }));
    const ask = screen.getByRole('group', { name: 'Forget laptop?' });
    expect(within(ask).getAllByRole('button').map((b) => b.textContent)).toEqual([
      'No, keep',
      'Yes, forget',
    ]);
    // What it costs is said where it is asked: the panel this verb came from
    // spent a paragraph on it, and the answer is worthless without the sentence.
    expect(screen.getByText(/access token from this device/)).toBeTruthy();

    await user.click(within(ask).getByRole('button', { name: 'No, keep' }));
    expect(onForget).not.toHaveBeenCalled();
    expect(cellOf('laptop').getByRole('button', { name: 'Forget' })).toBeTruthy();

    await user.click(cellOf('laptop').getByRole('button', { name: 'Forget' }));
    await user.click(screen.getByRole('button', { name: 'Yes, forget' }));
    expect(onForget).toHaveBeenCalledWith(laptop);
  });
});

// `ConnectScreen` lists the same machines for a different question — which one do
// I go to — so a row there is a place to GO, not a thing to manage.
describe('a machine row with nowhere to go carries no verbs', () => {
  it('renders the bare rows when no handler is given', () => {
    render(
      <MachineRows
        conns={[tower, laptop]}
        selectedUrl={laptop.url}
        health={{}}
        onPick={() => {}}
        actionLabel="Settings"
      />,
    );
    expect(screen.queryAllByRole('group', { name: /actions$/ })).toEqual([]);
    expect(screen.getAllByRole('button')).toHaveLength(2);
  });
});
