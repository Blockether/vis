// @vitest-environment jsdom
import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { SettingsBandsOpen, SettingsPanel } from './settings/SettingsLayout';

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
});
