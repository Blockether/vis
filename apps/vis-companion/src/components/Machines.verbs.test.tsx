// @vitest-environment jsdom
import { render, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

import type { GatewayConn } from '../lib/types';
import { MachineRows } from './Machines';

// Regression, user report about the machines column ("I don't need the ⋯ and
// swiping"): every verb a machine has — rank it primary, rename it, forget it —
// hid behind a left swipe and a `⋯` at the end of the row, so the only way to
// learn a machine HAD verbs was to guess at a gesture or press a mark that says
// nothing. (The panel before that stood permanently open under the list and
// acted on whichever row the column happened to be reading, not on the row under
// the thumb.) They are words now, in the row this column is READING — the
// machine whose settings fill the rest of the column — and in no other row.

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

/** What every verb on screen says it does, in the order it is offered. */
const verbs = () =>
  screen
    .getAllByRole('button')
    .map((button) => button.getAttribute('aria-label') ?? '')
    .filter((name) => /^(Make|Rename|Forget) /.test(name));

describe('a machine wears its verbs in the row this column is reading', () => {
  it('offers them as words, and only on the row being read', () => {
    fleet();
    expect(verbs()).toEqual(['Make laptop primary', 'Rename laptop', 'Forget laptop']);

    // Three words the eye can read; the whole sentence is the accessible name.
    const strip = screen
      .getAllByRole('button')
      .filter((button) => /^(Make|Rename|Forget) /.test(button.getAttribute('aria-label') ?? ''));
    expect(strip.map((button) => button.textContent)).toEqual(['Primary', 'Rename', 'Forget']);

    // `tower` is a row, not a workbench: it is not the machine being read.
    expect(screen.getAllByRole('button', { name: /tower/ })).toHaveLength(1);
  });

  it('keeps the rank verb off the machine that already holds the rank', () => {
    fleet({ selectedUrl: tower.url });
    expect(verbs()).toEqual(['Rename tower', 'Forget tower']);
  });

  it('hides nothing behind a swipe or a mark', () => {
    fleet();
    expect(screen.queryByRole('button', { name: /^Actions for/ })).toBeNull();
    expect(screen.queryAllByRole('group', { name: /actions$/ })).toEqual([]);
    // Two rows and one strip of three: every control on this list is a word or a
    // machine, and nothing is reached by a gesture.
    expect(screen.getAllByRole('button')).toHaveLength(5);
  });

  it('renames in the row, and an empty name gives the machine its host back', async () => {
    const user = userEvent.setup();
    const onRename = vi.fn();
    fleet({ onRename });

    await user.click(screen.getByRole('button', { name: 'Rename laptop' }));
    const field = screen.getByRole('textbox', { name: 'Rename laptop' });
    expect(field).toHaveValue('laptop');
    // The row it replaced is gone while it is being typed in — verbs and all: the
    // name is edited where it stands, not in a dialog opened over the list.
    expect(verbs()).toEqual([]);

    await user.clear(field);
    await user.type(field, 'workshop');
    await user.tab();
    expect(onRename).toHaveBeenCalledWith(laptop, 'workshop');

    // The list is the caller's state, so the row still reads `laptop` here:
    // clearing the field is what says "no name of its own".
    await user.click(screen.getByRole('button', { name: 'Rename laptop' }));
    await user.clear(screen.getByRole('textbox', { name: 'Rename laptop' }));
    await user.tab();
    expect(onRename).toHaveBeenLastCalledWith(laptop, undefined);
  });

  it('asks before it forgets, in the row the word was pressed in', async () => {
    const user = userEvent.setup();
    const onForget = vi.fn();
    fleet({ onForget });

    await user.click(screen.getByRole('button', { name: 'Forget laptop' }));
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
    expect(screen.getByRole('button', { name: 'Forget laptop' })).toBeTruthy();

    await user.click(screen.getByRole('button', { name: 'Forget laptop' }));
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
    expect(verbs()).toEqual([]);
    expect(screen.getAllByRole('button')).toHaveLength(2);
  });
});
