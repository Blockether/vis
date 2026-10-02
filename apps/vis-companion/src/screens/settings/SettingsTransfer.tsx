import { useEffect, useState } from 'react';
import { Banner, Button, Input, Select, SettingsHeader, Text } from '../../components/ui';
import {
  canProfile,
  exportSettingsProfile,
  parseSettingsProfile,
  readSettingsProfiles,
  writeSettingsProfiles,
  SETTINGS_PROFILE_COUNT_LIMIT,
  SETTINGS_PROFILE_NAME_LIMIT,
  type SettingsProfile,
} from '../../lib/settings-profiles';
import type { SettingChange, SettingsResponse, Toggle } from '../../lib/types';

export function SettingsTransfer({
  data,
  hasDrafts,
  disabled,
  stage,
}: {
  data: SettingsResponse;
  hasDrafts: boolean;
  disabled: boolean;
  stage: (setting: Toggle, change: SettingChange) => void;
}) {
  const [profiles, setProfiles] = useState<SettingsProfile[]>([]);
  const [name, setName] = useState('');
  const [selected, setSelected] = useState('');
  const [text, setText] = useState('');
  const [exported, setExported] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [message, setMessage] = useState<string | null>(null);
  const [saving, setSaving] = useState(false);
  useEffect(() => {
    let cancelled = false;
    void readSettingsProfiles()
      .then((value) => {
        if (!cancelled) setProfiles(value);
      })
      .catch((err) => {
        if (!cancelled) setError((err as Error).message);
      });
    return () => {
      cancelled = true;
    };
  }, []);
  const settings = data.groups.flatMap((group) => group.toggles);
  const importProfile = (source: string) => {
    try {
      const profile = parseSettingsProfile(source, settings);
      for (const change of profile.changes)
        stage(
          settings.find((setting) => setting.id === change.id)!,
          change,
        );
      setError(null);
      setMessage(
        `${profile.changes.length} changes staged from ${profile.name}. Review them before Apply.`,
      );
    } catch (err) {
      setError((err as Error).message);
    }
  };
  const persist = async (next: SettingsProfile[]) => {
    setSaving(true);
    try {
      await writeSettingsProfiles(next);
      setProfiles(next);
      setError(null);
      setMessage('Profiles saved on this device.');
    } catch (err) {
      setError((err as Error).message);
    } finally {
      setSaving(false);
    }
  };
  return (
    <details className="border-b border-dialog-edge px-3 py-3 sm:px-4">
      <summary className="cursor-pointer">
        <SettingsHeader><Text variant="label">Profiles, import and export</Text></SettingsHeader>
      </summary>
      <div className="mt-3 space-y-3">
        <Text as="p" variant="description">
          Profiles contain configuration, not provider tokens, passwords or MCP credentials. Paths
          and domains can identify your workspace. Check them before sharing.
        </Text>
        <Text as="p" variant="description">
          Import stages only the included fields. Other settings stay unchanged. The gateway
          validates permissions when you apply.
        </Text>
        {error && <Banner kind="err">{error}</Banner>}
        {message && (
          <Text as="p" variant="description" role="status">
            {message}
          </Text>
        )}
        <div className="flex flex-wrap gap-2">
          <Input
            aria-label="Profile name"
            maxLength={SETTINGS_PROFILE_NAME_LIMIT}
            placeholder="Profile name"
            value={name}
            onChange={(event) => setName(event.target.value)}
            className="min-w-0 flex-1"
          />
          <Button
            variant="secondary"
            density="panel"
            disabled={
              disabled ||
              hasDrafts ||
              saving ||
              !name.trim() ||
              profiles.some((profile) => profile.name === name.trim()) ||
              profiles.length >= SETTINGS_PROFILE_COUNT_LIMIT
            }
            onClick={() => void persist([...profiles, exportSettingsProfile(name, data)])}
          >
            Save profile
          </Button>
          <Button
            variant="secondary"
            density="panel"
            disabled={disabled || hasDrafts}
            onClick={() => {
              setText(JSON.stringify(exportSettingsProfile(name, data), null, 2));
              setExported(true);
              setError(null);
            }}
          >
            Export overrides
          </Button>
        </div>
        {profiles.length > 0 && (
          <div className="flex flex-wrap gap-2">
            <Select
              aria-label="Saved profile"
              value={selected}
              onValueChange={setSelected}
              options={[
                { value: '', label: 'Choose a profile' },
                ...profiles.map((profile) => ({ value: profile.name, label: profile.name })),
              ]}
            />
            <Button
              variant="secondary"
              density="panel"
              disabled={disabled || !selected}
              onClick={() => {
                const profile = profiles.find((profile) => profile.name === selected);
                if (profile) importProfile(JSON.stringify(profile));
              }}
            >
              Stage profile
            </Button>
            <Button
              variant="secondary"
              density="panel"
              disabled={disabled || saving || !selected}
              onClick={() => {
                if (window.confirm(`Delete local profile "${selected}"? Gateway settings will not change.`))
                  void persist(profiles.filter((profile) => profile.name !== selected));
              }}
            >
              Delete local profile
            </Button>
          </div>
        )}
        <label className="block space-y-2">
          <Text variant="label">{exported ? 'Exported profile' : 'Import profile JSON'}</Text>
          <textarea
            className="min-h-32 w-full border border-dialog-edge bg-panel p-3 font-mono text-sm"
            aria-label="Profile JSON"
            value={text}
            disabled={disabled}
            onChange={(event) => {
              setText(event.target.value);
              setExported(false);
            }}
          />
        </label>
        <div className="flex flex-wrap gap-2">
          <Button
            variant="secondary"
            density="panel"
            disabled={disabled || !text.trim()}
            onClick={() => importProfile(text)}
          >
            Stage import
          </Button>
          <Button
            variant="secondary"
            density="panel"
            disabled={disabled}
            onClick={() => {
              settings
                .filter((setting) => canProfile(setting) && setting.is_override)
                .forEach((setting) => stage(setting, { id: setting.id, action: 'inherit' }));
              setMessage('Inherited defaults staged. Review all changes before Apply.');
            }}
          >
            Stage inherited defaults
          </Button>
        </div>
        {hasDrafts && (
          <Text as="p" variant="description">
            Apply or discard your draft before saving or exporting a profile.
          </Text>
        )}
      </div>
    </details>
  );
}
