import type { Meta, StoryObj } from '@storybook/react-vite';
import { useEffect, useState } from 'react';

import { CommandProvider, CommandRegistry, useCommands } from '../lib/commands';
import { KEYMAP_VERBS } from '../lib/keymap';
import { ChordBand, CommandPalette } from './CommandSurface';

/** Every verb of the key map can run, so the band lists all of its keys. */
const EVERY_VERB = Object.fromEntries(KEYMAP_VERBS.map((verb) => [verb.id, () => {}]));

function Verbs() {
  useCommands(EVERY_VERB);
  return null;
}

/** The palette after C-x p stands in the session pane, beside the session list. */
function Palette() {
  const [registry] = useState(() => new CommandRegistry());
  const [isPressed, setPressed] = useState(false);
  useEffect(() => setPressed(true), []);
  return (
    <CommandProvider registry={registry}>
      <Verbs />
      <div className="flex h-dvh">
        <div className="h-full w-[33%] min-w-80 shrink-0 border-r border-dialog-edge bg-page" />
        <div data-session-surface className="relative min-w-0 flex-1 bg-ink" />
      </div>
      {isPressed && <CommandPalette registry={registry} mode="palette" onClose={() => {}} />}
    </CommandProvider>
  );
}

/** The band after C-x (`group` null) or after C-x and a group key. */
function Band({ group, isBesideComposer = false }: { group: string | null; isBesideComposer?: boolean }) {
  const [registry] = useState(() => new CommandRegistry());
  // The band mounts after the verbs register, as the app mounts it when C-x is pressed.
  const [isPressed, setPressed] = useState(false);
  useEffect(() => setPressed(true), []);
  return (
    <CommandProvider registry={registry}>
      <Verbs />
      {isBesideComposer ? (
        <div className="flex h-dvh bg-ink">
          <div className="h-full w-[33%] min-w-80 shrink-0 border-r border-dialog-edge bg-page" />
          <div className="flex min-w-0 flex-1 flex-col justify-end p-4">
            <div data-chord-anchor className="h-12 border border-dialog-edge bg-input" />
          </div>
        </div>
      ) : (
        <div className="h-dvh bg-ink" />
      )}
      {isPressed && <ChordBand registry={registry} group={group} />}
    </CommandProvider>
  );
}

const meta = {
  title: 'Foundations/Command band',
  component: Band,
  parameters: { layout: 'fullscreen' },
} satisfies Meta<typeof Band>;

export default meta;
type Story = StoryObj<typeof meta>;

/** After C-x: the groups, the palette and the keys without a group. */
export const Groups: Story = { args: { group: null } };

/** After C-x s: the verbs of one group. */
export const SessionGroup: Story = { args: { group: 'session' } };

/** On a desk the band stands above the composer, beside the session list, never over it. */
export const AboveComposer: Story = { args: { group: null, isBesideComposer: true } };

/** After C-x p: the palette lists every command under its group's heading. */
export const PaletteList: Story = { args: { group: null }, render: () => <Palette /> };
