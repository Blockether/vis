// @vitest-environment jsdom
import { useState } from 'react';
import { describe, expect, it } from 'vitest';
import { act, fireEvent, render, screen } from '@testing-library/react';

import { dismissTopLayer } from '../lib/edge-back';
import { Menu } from './Menu';

function OpenMenu() {
  const [isOpen, setIsOpen] = useState(true);
  return isOpen ? (
    <Menu label="Session actions" at={{ left: 8, top: 8 }} onDismiss={() => setIsOpen(false)}>
      <button type="button">Export</button>
    </Menu>
  ) : null;
}

const scrim = () => screen.getAllByRole('presentation')[0];
const menu = () => screen.queryByRole('dialog', { name: 'Session actions' });

// Reported from an Android phone: a tap on the `⋮` of a session opened its menu on pointer-up,
// then the click of the same tap landed on the new scrim and closed the menu at once.
describe('a menu opened by a touch', () => {
  it("stays open when the opening tap's click lands on its scrim", () => {
    render(<OpenMenu />);
    fireEvent.click(scrim());
    expect(menu()).toBeInTheDocument();
  });

  it('closes when the touch began on the scrim', () => {
    render(<OpenMenu />);
    fireEvent.pointerDown(scrim());
    fireEvent.click(scrim());
    expect(menu()).not.toBeInTheDocument();
  });
});

// Reported from Android: back with the export menu open left the application.
describe('the back button of the phone with a menu open', () => {
  it('closes the menu and leaves the next back to the screen below', () => {
    render(<OpenMenu />);
    let spent = false;
    act(() => {
      spent = dismissTopLayer();
    });
    expect(spent).toBe(true);
    expect(menu()).not.toBeInTheDocument();
    expect(dismissTopLayer()).toBe(false);
  });
});
