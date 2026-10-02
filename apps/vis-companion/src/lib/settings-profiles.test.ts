import { describe, expect, it } from 'vitest';
import gatewaySchema from '../../../../packages/vis-contract/resources/vis-contract/schema/gateway.json';
import {
  SETTINGS_PROFILE_NAME_LIMIT,
  exportSettingsProfile,
  parseSettingsProfile,
  writeSettingsProfiles,
} from './settings-profiles';
import type { SettingsResponse } from './types';

const data: SettingsResponse = {
  revision: 'profile-1',
  groups: [
    {
      id: 'agent',
      title: 'Agent',
      toggles: [
        { id: 'plans', label: 'Plans', type: 'boolean', enabled: false, is_override: true },
        {
          id: 'provider_token',
          label: 'Token',
          type: 'string',
          value: 'not-exported',
          is_override: true,
        },
        {
          id: 'jail_environment',
          label: 'Environment',
          type: 'enum',
          value: 'declared',
          choices: ['declared', 'inherit'],
          is_override: false,
        },
      ],
    },
  ],
};
const settings = data.groups[0].toggles;
describe('safe configuration profiles', () => {
  it('exports only explicit supported configuration and never credential fields', () => {
    const profile = exportSettingsProfile('Work', data);
    expect(profile).toEqual({
      version: 1,
      name: 'Work',
      changes: [{ id: 'plans', action: 'value', value: false }],
    });
    expect(JSON.stringify(profile)).not.toContain('not-exported');
    expect(parseSettingsProfile(JSON.stringify(profile), settings)).toEqual(profile);
  });
  it.each(
    [
      [{ id: 'missing', action: 'value', value: true }],
      [{ id: 'provider_token', action: 'value', value: 'hidden' }],
      [{ id: 'plans', action: 'value', value: 'false' }],
      [{ id: 'plans', action: 'toggle' }],
      [{ id: 'plans', action: 'value', value: null }],
      [
        { id: 'plans', action: 'inherit' },
        { id: 'plans', action: 'value', value: true },
      ],
      [{ id: 'jail_environment', action: 'value', value: 'unsupported' }],
    ].map((changes) => [changes] as const),
  )('rejects an invalid profile as a whole: %j', (changes: unknown[]) => {
    expect(() =>
      parseSettingsProfile(JSON.stringify({ version: 1, name: 'Work', changes }), settings),
    ).toThrow();
  });
  it('imports explicit inheritance without converting it to a toggle', () => {
    expect(
      parseSettingsProfile(
        JSON.stringify({
          version: 1,
          name: 'Default',
          changes: [{ id: 'plans', action: 'inherit' }],
        }),
        settings,
      ).changes,
    ).toEqual([{ id: 'plans', action: 'inherit' }]);
  });
  it('counts profile name characters like the schema and rejects a longer name', () => {
    const profile = (name: string) =>
      JSON.stringify({ version: 1, name, changes: [{ id: 'plans', action: 'value', value: true }] });
    const longest = '🙂'.repeat(SETTINGS_PROFILE_NAME_LIMIT);
    expect(parseSettingsProfile(profile(longest), settings).name).toBe(longest);
    expect(() => parseSettingsProfile(profile(`${longest}🙂`), settings)).toThrow(
      'Use a version 1 profile with a name',
    );
  });
  it('refuses to save more profiles than the schema allows', async () => {
    const saved = exportSettingsProfile('Review', data);
    const limit = gatewaySchema.$defs.settings_profiles.maxItems;
    await expect(
      writeSettingsProfiles(Array.from({ length: limit + 1 }, () => saved)),
    ).rejects.toThrow(`Save at most ${limit} profiles.`);
  });
});
