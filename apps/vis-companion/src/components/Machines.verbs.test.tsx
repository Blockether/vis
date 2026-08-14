// @vitest-environment jsdom
import { render, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

import type { GatewayConn } from '../lib/types';
import { MachineRows } from './Machines';

// The user's own words about the machines column: "I want to redesign those
// things. Like more hidden, triggered by some action, button, icon like maybe
// with the slide like the star in the session." Every verb a machine has —
// rank it primary, rename it, forget it — used to stand permanently open in a
// `Saved connection` panel under the list, three controls for ONE machine, and
// they acted on whichever row the column happened to be READING rather than on
// the row under the thumb. They are the row's own now, behind the same swipe a
// session row hides `Star` / `Rename` / `Delete` behind.

const tower: GatewayConn = { url: 'http://10.0.0.5:7890', label: 'tower' };
const laptop: GatewayConn = { url: 'http://10.0.0.6:7890', label: 'laptop' };

/** The settings column's list: every verb wired, `tower` ranked first. */
function fleet(props: Partial<Parameters<typeof MachineRows>[0]> = {}) {
  return render(
    <MachineRows
      conns={[tower, laptop]}
      primaryUrl={tower.url}
      health={{}}
      onPick={() => {}}
      onMakePrimary={() => {}}
      onRename={() => {}}
      onForget={() => {}}
      {...props}
    />,
  );
}

describe('a machine hides its verbs behind its own row', () => {
  it('hangs them on the swipe strip, and offers no rank to the row that has it', () => {
    fleet();
    const strip = screen.getByRole('group', { name: 'laptop actions' });
    expect(
      within(strip)
        .getAllByRole('button')
        .map((button) => button.getAttribute('aria-label')),
    ).toEqual(['Primary', 'Rename', 'Forget']);

    // `tower` is already the primary machine: the rank verb is not on its strip.
    const primary = screen.getByRole('group', { name: 'tower actions' });
    expect(
      within(primary)
        .getAllByRole('button')
        .map((button) => button.getAttribute('aria-label')),
    ).toEqual(['Rename', 'Forget']);
  });

  it('paints the rank amber and the destructive one red, as the session strip does', () => {
    fleet();
    const strip = screen.getByRole('group', { name: 'laptop actions' });
    const [rank, rename, forget] = within(strip).getAllByRole('button');
    expect(rank.className).toContain('bg-accent/15');
    expect(rename.className).toContain('bg-panel-2');
    expect(forget.className).toContain('bg-err-surface');
  });

  it('gives a pointer and a keyboard the same list, because neither can swipe', async () => {
    const user = userEvent.setup();
    fleet();
    await user.click(screen.getByRole('button', { name: 'Actions for laptop' }));
    const menu = await screen.findByRole('menu', { name: 'Actions for laptop' });
    expect(
      within(menu)
        .getAllByRole('menuitem')
        .map((item) => item.textContent?.split('The ')[0].split('Deletes')[0].trim()),
    ).toEqual(['Make primary', 'Rename', 'Forget this machine']);
  });

  it('renames in the row, and an empty name gives the machine its host back', async () => {
    const user = userEvent.setup();
    const onRename = vi.fn();
    fleet({ onRename });

    await user.click(within(screen.getByRole('group', { name: 'laptop actions' })).getByRole('button', { name: 'Rename' }));
    const field = screen.getByRole('textbox', { name: 'Rename laptop' });
    expect(field).toHaveValue('laptop');
    // The row it replaced is gone while it is being typed in: the name is edited
    // where it stands, not in a dialog opened over the list.
    expect(screen.queryByRole('button', { name: 'Actions for laptop' })).toBeNull();

    await user.clear(field);
    await user.type(field, 'workshop');
    await user.tab();
    expect(onRename).toHaveBeenCalledWith(laptop, 'workshop');

    // The list is the caller's state, so the row still reads `laptop` here:
    // clearing the field is what says "no name of its own".
    await user.click(within(screen.getByRole('group', { name: 'laptop actions' })).getByRole('button', { name: 'Rename' }));
    await user.clear(screen.getByRole('textbox', { name: 'Rename laptop' }));
    await user.tab();
    expect(onRename).toHaveBeenLastCalledWith(laptop, undefined);
  });

  it('asks before it forgets, in the row that was swiped', async () => {
    const user = userEvent.setup();
    const onForget = vi.fn();
    fleet({ onForget });

    await user.click(within(screen.getByRole('group', { name: 'laptop actions' })).getByRole('button', { name: 'Forget' }));
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
    expect(screen.getByRole('button', { name: 'Actions for laptop' })).toBeTruthy();

    await user.click(within(screen.getByRole('group', { name: 'laptop actions' })).getByRole('button', { name: 'Forget' }));
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
        health={{}}
        onPick={() => {}}
        actionLabel="Settings"
      />,
    );
    expect(screen.queryByRole('group', { name: 'laptop actions' })).toBeNull();
    expect(screen.queryByRole('button', { name: 'Actions for laptop' })).toBeNull();
    expect(screen.getAllByRole('button')).toHaveLength(2);
  });
});
