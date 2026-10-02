import { Preferences } from '@capacitor/preferences';
import { settingValue } from './settings-model';
import type { SettingChange, SettingValue, SettingsResponse, Toggle } from './types';
import gatewaySchema from '../../../../packages/vis-contract/resources/vis-contract/schema/gateway.json';

const profileSchema = gatewaySchema.$defs.settings_profile;
const changeSchema = gatewaySchema.$defs.setting_change;
const maxBytes = profileSchema['x-vis-max-bytes'];
const maxChanges = profileSchema.properties.changes.maxItems;
export const SETTINGS_PROFILE_NAME_LIMIT = profileSchema.properties.name.maxLength;
const maxName = SETTINGS_PROFILE_NAME_LIMIT;
export const SETTINGS_PROFILE_COUNT_LIMIT = gatewaySchema.$defs.settings_profiles.maxItems;
const maxProfiles = SETTINGS_PROFILE_COUNT_LIMIT;

export interface SettingsProfile {
  version: 1;
  name: string;
  changes: SettingChange[];
}
const KEY = 'vis.settingsProfiles';
const STRUCTURED_IDS = new Set([
  'workspace_filesystem',
  'jail_filesystem',
  'jail_network',
  'jail_deny_exec',
]);
export function canProfile(setting: Toggle): boolean {
  return (
    ['boolean', 'enum', 'number'].includes(setting.type) ||
    STRUCTURED_IDS.has(setting.id) ||
    ['agent_name', 'jail_environment'].includes(setting.id)
  );
}
export function exportSettingsProfile(name: string, data: SettingsResponse): SettingsProfile {
  return {
    version: 1,
    name: name.trim() || 'Settings',
    changes: data.groups
      .flatMap((group) => group.toggles)
      .filter((setting) => canProfile(setting) && setting.is_override)
      .map((setting) => ({ id: setting.id, action: 'value', value: setting.own_value ?? settingValue(setting) })),
  };
}
export function parseSettingsProfile(text: string, settings: Toggle[]): SettingsProfile {
  if (new TextEncoder().encode(text).length > maxBytes)
    throw new Error(`The profile is too large. Use at most ${maxBytes} bytes.`);
  const value: unknown = JSON.parse(text);
  if (!value || typeof value !== 'object' || Array.isArray(value))
    throw new Error('Enter a settings profile object.');
  const profile = value as Partial<SettingsProfile>;
  if (
    Object.keys(profile).some((key) => !(key in profileSchema.properties)) ||
    profile.version !== profileSchema.properties.version.const ||
    typeof profile.name !== 'string' ||
    !profile.name.trim() ||
    [...profile.name].length > maxName ||
    !Array.isArray(profile.changes) ||
    profile.changes.length > maxChanges
  )
    throw new Error(`Use a version 1 profile with a name and up to ${maxChanges} changes.`);
  const seen = new Set<string>();
  for (const change of profile.changes) {
    if (
      !change ||
      typeof change !== 'object' ||
      Object.keys(change).some((key) => !(key in changeSchema.properties)) ||
      typeof change.id !== 'string' ||
      seen.has(change.id)
    )
      throw new Error('Every profile entry needs a unique setting ID.');
    seen.add(change.id);
    const setting = settings.find((setting) => setting.id === change.id);
    if (!setting || !canProfile(setting))
      throw new Error(`${change.id} is unavailable or cannot be included in a profile.`);
    if (change.action === 'inherit') {
      if ('value' in change) throw new Error(`${setting.label}: inherit cannot contain a value.`);
      continue;
    }
    if (change.action !== 'value' || change.value === null || change.value === undefined)
      throw new Error(`${setting.label} needs a value or inherit action.`);
    const value: SettingValue = change.value;
    const expected = setting.type === 'enum' ? 'string' : setting.type;
    if (
      expected === 'array'
        ? !Array.isArray(value)
        : expected === 'object'
          ? typeof value !== 'object' || Array.isArray(value)
          : typeof value !== expected
    )
      throw new Error(`${setting.label} needs a ${expected} value.`);
    if (setting.choices && !setting.choices.includes(String(value)))
      throw new Error(`${setting.label} has an unsupported choice.`);
    if (typeof value === 'string' && setting.max_length && value.length > setting.max_length)
      throw new Error(`${setting.label} is too long.`);
    if (typeof value === 'number' && !Number.isFinite(value))
      throw new Error(`${setting.label} needs a finite number.`);
  }
  return profile as SettingsProfile;
}
export async function readSettingsProfiles(): Promise<SettingsProfile[]> {
  const { value } = await Preferences.get({ key: KEY });
  const parsed: unknown = JSON.parse(value ?? '[]');
  if (!Array.isArray(parsed) || parsed.length > maxProfiles) throw new Error('Saved settings profiles are invalid.');
  return parsed as SettingsProfile[];
}
export async function writeSettingsProfiles(profiles: SettingsProfile[]): Promise<void> {
  if (profiles.length > maxProfiles) throw new Error(`Save at most ${maxProfiles} profiles.`);
  await Preferences.set({ key: KEY, value: JSON.stringify(profiles) });
}
