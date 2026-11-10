// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { IconButton } from '../components/ui';
import { SettingsBandsOpen, SettingsPanel, SettingsSection } from './settings/SettingsLayout';

afterEach(() => {
  document.body.innerHTML = '';
});

describe('every settings band with a body folds', () => {
  it('starts open where the caller or a search asks for it', () => {
    render(
      <>
        <SettingsPanel title="Add a machine" defaultOpen>
          <p>Pairing code</p>
        </SettingsPanel>
        <SettingsBandsOpen.Provider value>
          <SettingsPanel title="Planning">
            <p>Plan mode</p>
          </SettingsPanel>
        </SettingsBandsOpen.Provider>
      </>,
    );
    expect(screen.getByText('Pairing code')).toBeVisible();
    expect(screen.getByText('Plan mode')).toBeVisible();
  });

  it('keeps the band action a separate control that opens its band', () => {
    let added = 0;
    render(
      <SettingsPanel
        title="MCP servers"
        action={
          <button type="button" onClick={() => (added += 1)}>
            Add an MCP server
          </button>
        }
      >
        <p>context7</p>
      </SettingsPanel>,
    );
    expect(screen.queryByText('context7')).toBeNull();
    // The verb runs once and shows what it adds; it does not toggle the fold.
    fireEvent.click(screen.getByRole('button', { name: 'Add an MCP server' }));
    expect(added).toBe(1);
    expect(screen.getByText('context7')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Add an MCP server' }));
    expect(screen.getByText('context7')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Hide MCP servers' }));
    expect(screen.queryByText('context7')).toBeNull();
  });

  // Reported: the Automations + made a new automation, but its band stayed folded.
  // An icon action presses on release, and iOS can drop the click that follows.
  it('opens the band when its icon action is pressed without a click', () => {
    let added = 0;
    render(
      <SettingsPanel
        title="Automations"
        action={
          <IconButton label="New automation" onClick={() => (added += 1)}>
            +
          </IconButton>
        }
      >
        <p>Morning summary</p>
      </SettingsPanel>,
    );
    const plus = screen.getByRole('button', { name: 'New automation' });
    fireEvent.pointerDown(plus, { button: 0 });
    fireEvent.pointerUp(plus, { button: 0 });
    expect(added).toBe(1);
    expect(screen.getByText('Morning summary')).toBeVisible();
  });

  // Reported: Speech engines and Experimental stood open under the opened General band.
  it('folds a subsection that asks for it and keeps other subsections open', () => {
    render(
      <SettingsSection title="General" defaultOpen>
        <SettingsPanel title="Notifications">
          <p>Turn finished</p>
        </SettingsPanel>
        <SettingsPanel title="Speech engines" isFoldable>
          <p>Parakeet</p>
        </SettingsPanel>
      </SettingsSection>,
    );
    expect(screen.getByText('Turn finished')).toBeVisible();
    expect(screen.queryByText('Parakeet')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Show Speech engines' }));
    expect(screen.getByText('Parakeet')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Hide Speech engines' }));
    expect(screen.queryByText('Parakeet')).toBeNull();
  });
});
