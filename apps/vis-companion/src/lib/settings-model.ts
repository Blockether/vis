import type { SettingChange, SettingValue, SettingsResponse, Toggle, ToggleGroup } from './types';

export const SETTINGS_CATEGORIES = [
  ['all', 'All settings'],
  ['basic', 'Basics'],
  ['response', 'Models and responses'],
  ['tools', 'Tools and integrations'],
  ['access', 'Files and permissions'],
  ['speech', 'Voice and notifications'],
  ['appearance', 'Appearance'],
  ['advanced', 'Advanced'],
] as const;
export type SettingsCategory = (typeof SETTINGS_CATEGORIES)[number][0];

export function categoryFor(group: ToggleGroup): SettingsCategory {
  if (group.id === 'access') return 'access';
  if (['provider', 'response', 'rlm', 'context'].includes(group.id)) return 'response';
  if (['engines', 'skills', 'mcp', 'tools', 'shell'].includes(group.id)) return 'tools';
  if (['experimental', 'sandbox', 'debug', 'improve'].includes(group.id)) return 'advanced';
  return 'basic';
}
export function settingValue(setting: Toggle): SettingValue {
  return setting.type === 'boolean' ? !!setting.enabled : (setting.value ?? '');
}
export function sameValue(a: unknown, b: unknown): boolean {
  const canonical = (value: unknown): unknown => {
    if (Array.isArray(value)) return value.map(canonical);
    if (value && typeof value === 'object')
      return Object.fromEntries(
        Object.entries(value)
          .sort(([a], [b]) => a.localeCompare(b))
          .map(([k, v]) => [k, canonical(v)]),
      );
    return value;
  };
  return JSON.stringify(canonical(a)) === JSON.stringify(canonical(b));
}
export function previewSetting(setting: Toggle, change?: SettingChange): Toggle {
  if (!change) return setting;
  const value =
    change.action === 'inherit' ? (setting.inherited_value ?? settingValue(setting)) : change.value;
  return {
    ...setting,
    is_override: change.action === 'value',
    source: change.action === 'inherit' ? setting.inherited_source : setting.scope,
    ...(setting.type === 'boolean' ? { enabled: !!value } : { value }),
  };
}
export function formatSettingValue(value: unknown): string {
  if (typeof value === 'boolean') return value ? 'On' : 'Off';
  if (Array.isArray(value))
    return value.length ? `${value.length} ${value.length === 1 ? 'entry' : 'entries'}` : 'None';
  if (value && typeof value === 'object')
    return Object.keys(value).length ? 'Configured' : 'Default';
  return String(value ?? 'Default');
}
export function matchingGroups(
  data: SettingsResponse,
  category: SettingsCategory,
  search: string,
  modified: boolean,
): ToggleGroup[] {
  const needle = search.trim().toLowerCase();
  return data.groups
    .map((group) => ({
      ...group,
      toggles: group.toggles.filter(
        (setting) =>
          (!modified || setting.is_override) &&
          (!!needle || category === 'all' || categoryFor(group) === category) &&
          (!needle ||
            `${group.title} ${setting.id} ${setting.label} ${setting.description ?? ''} ${setting.source ?? ''}`
              .toLowerCase()
              .includes(needle)),
      ),
    }))
    .filter((group) => group.toggles.length);
}
