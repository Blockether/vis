// @vitest-environment jsdom

import { fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { FleetRail, type FleetRailEntry } from './FleetRail';

function entry(
  name: string,
  count: number,
  onPress: () => void = () => {},
): FleetRailEntry {
  return { key: name, name, count, onPress };
}

describe('FleetRail', () => {
  it('groups machine and project destinations under one navigation landmark', () => {
    const openProject = vi.fn();
    render(
      <FleetRail
        machines={[{ ...entry('tower', 12), isActive: true }]}
        projects={[entry('vis', 8, openProject)]}
        action={<span>Manage</span>}
      />,
    );

    expect(screen.getByRole('navigation', { name: 'Fleet' })).toBeInTheDocument();
    expect(screen.getByRole('heading', { name: 'Machines' })).toBeInTheDocument();
    expect(screen.getByRole('heading', { name: 'Projects' })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'tower — 12 sessions' })).toHaveAttribute(
      'aria-current',
      'true',
    );

    fireEvent.click(screen.getByRole('button', { name: 'vis — 8 sessions' }));
    expect(openProject).toHaveBeenCalledOnce();
  });

  it('omits a group that has neither destinations nor an action', () => {
    render(<FleetRail machines={[entry('tower', 1)]} projects={[]} action={null} />);

    expect(screen.queryByRole('heading', { name: 'Projects' })).not.toBeInTheDocument();
  });
});
