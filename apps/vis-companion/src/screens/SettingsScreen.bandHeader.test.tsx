// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { SettingsBandsOpen, SettingsPanel } from './settings/SettingsLayout';

afterEach(() => {
  document.body.innerHTML = '';
});

describe('every settings band with a body folds', () => {
  it('starts folded and shows its body behind its own chevron', () => {
    render(
      <SettingsPanel title="Theme">
        <p>Vis Light</p>
      </SettingsPanel>,
    );
    expect(screen.queryByText('Vis Light')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Show Theme' }));
    expect(screen.getByText('Vis Light')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Hide Theme' }));
    expect(screen.queryByText('Vis Light')).toBeNull();
  });

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

  it('shows no chevron on a band without a body', () => {
    render(
      <SettingsPanel title="Notifications" action={<button type="button">Notifications off</button>}>
        {false}
      </SettingsPanel>,
    );
    expect(screen.queryByRole('button', { name: /Notifications$/ })).toBeNull();
    expect(screen.getByRole('heading', { name: 'Notifications' })).toBeVisible();
  });
});
