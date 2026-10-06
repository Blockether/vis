// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { SettingsBandsOpen, SettingsColumn, SettingsPanel, SettingsSection } from './SettingsLayout';

/** Each settings level reads smaller than its parent, so a child never looks like a parent. */
describe('settings heading hierarchy', () => {
  it('shrinks from the column to the band to its groups', () => {
    render(
      <SettingsBandsOpen.Provider value>
        <SettingsColumn title="Machines">
          <SettingsSection title="General" headingLevel={4}>
            <SettingsPanel title="Notifications">
              <p>Push alerts</p>
            </SettingsPanel>
          </SettingsSection>
        </SettingsColumn>
      </SettingsBandsOpen.Provider>,
    );

    expect(screen.getByRole('heading', { level: 3, name: 'Machines' })).toHaveClass('text-head', 'font-semibold');
    expect(screen.getByRole('heading', { level: 4, name: 'General' })).toHaveClass('text-subhead', 'font-semibold');
    const group = screen.getByRole('heading', { level: 5, name: 'Notifications' });
    expect(group).toHaveClass('text-ui', 'uppercase', 'text-dialog-hint');
    expect(group).not.toHaveClass('text-title');
  });
});
