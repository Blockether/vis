// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';

import machinesSource from './Machines.tsx?raw';

import type { GatewayConn } from '../lib/types';
import { MachineRows } from './Machines';

// Regression, user report ("what for I need this stuff hmm? I really don't like
// and don't need to see it", drawn as a circle around `CURRENT PINNED` on the one
// machine that phone had paired): the row of a solo fleet wore `PRIMARY` and
// `CURRENT` for ever. Neither could vary — the only machine paired is both the one
// the app opens on and the one it is using — so two permanent words stood in the
// line whose name, address and latency the reader actually came for. In a fleet
// they still repeated each other on the primary's own row.
//
// Regression, user report (the settings design, "including that whole CURRENT"):
// the row went on wearing `CURRENT` for whichever machine the app happened to be
// talking to. It named no choice the reader had made — the app painted it on the
// machine the settings column was reading — so a rank a verb in this row SETS
// (`PRIMARY`) and a fact about the app's own bookkeeping stood in one box, in one
// ink, on rows the reader was pressing to change something else entirely.

const tower: GatewayConn = { url: 'http://192.168.0.241:7890', label: 'tower' };
const laptop: GatewayConn = { url: 'http://100.64.0.10:7890', label: 'laptop' };

/** The settings column's list, ranked and read the way settings renders it. */
function rows(props: Partial<Parameters<typeof MachineRows>[0]> = {}) {
  return render(
    <MachineRows
      conns={[tower]}
      primaryUrl={tower.url}
      selectedUrl={tower.url}
      health={{}}
      onPick={() => {}}
      {...props}
    />,
  );
}

/** Everything one machine's row says, in the order it says it. */
const lineOf = (machine: string) =>
  screen.getByText(machine).closest('button')?.textContent ?? '';

describe('what a machine row says about its rank', () => {
  it('says nothing about rank when there is one machine to rank', () => {
    rows({ conns: [{ ...tower, pinned: true }] });

    expect(screen.queryByText('Primary')).toBeNull();
    expect(screen.queryByText('Current')).toBeNull();
    // The pin stays: it is the only mark here that can vary, and it is why this
    // device is on THIS address instead of the most durable one.
    expect(screen.getByText('Pinned')).toBeTruthy();
    expect(lineOf('tower')).toContain('tower');
  });

  it('ranks the fleet, and says nothing else about it', () => {
    rows({ conns: [tower, laptop] });

    expect(lineOf('tower')).toContain('Primary');
    expect(lineOf('laptop')).not.toContain('Primary');
  });

  it('never says CURRENT, whichever machine the app is talking to', () => {
    rows({ conns: [tower, laptop] });

    expect(screen.queryByText('Current')).toBeNull();
    // And the prop that fed it is gone from the list's own contract.
    expect(machinesSource).not.toContain('activeUrl?:');
  });
});
