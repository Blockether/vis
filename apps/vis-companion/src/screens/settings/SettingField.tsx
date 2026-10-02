import { Button, Input, Select, Switch, Text } from '../../components/ui';
import type { SettingValue, Toggle } from '../../lib/types';
import configSchema from '../../../../../packages/vis-contract/resources/vis-contract/schema/config.json';

type ObjectValue = { [key: string]: SettingValue | null };
type FieldProps = {
  setting: Toggle;
  value: SettingValue;
  disabled: boolean;
  onChange: (value: SettingValue, keepRaw?: boolean) => void;
  raw?: string;
  onRawChange: (text: string, error?: string) => void;
};
const textAreaClass =
  'min-h-24 w-full rounded-none border border-dialog-edge bg-panel px-3 py-2 text-sm text-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:opacity-45';
const objectValue = (value: SettingValue): ObjectValue =>
  !Array.isArray(value) && typeof value === 'object' ? value : {};
const workspaceProperties = configSchema.$defs.workspaceEntry.properties;
const networkRuleProperties = configSchema.$defs.networkRule.properties;
type NumericSchema = { type?: string; minimum?: number; maximum?: number };
function numericSchema(pointer?: string): NumericSchema {
  let node: unknown = configSchema;
  for (const key of (pointer?.split('#')[1] ?? '').split('/').slice(1)) {
    if (!node || typeof node !== 'object') return {};
    node = (node as Record<string, unknown>)[key.replace(/~1/g, '/').replace(/~0/g, '~')];
  }
  return node && typeof node === 'object' && pointer ? (node as NumericSchema) : {};
}

function StringList({
  label,
  value,
  onChange,
  disabled,
  numbers = false,
}: {
  label: string;
  value: SettingValue | null | undefined;
  onChange: (value: SettingValue[]) => void;
  disabled: boolean;
  numbers?: boolean;
}) {
  const rows = Array.isArray(value) ? value : [];
  return (
    <fieldset disabled={disabled} className="min-w-0 space-y-2">
      <legend className="mb-1 text-sm">{label}</legend>
      {rows.map((entry, index) => (
        <div key={index} className="flex min-w-0 gap-2">
          <Input
            className="min-w-0 flex-1"
            aria-label={`${label} ${index + 1}`}
            type={numbers ? 'number' : 'text'}
            value={String(entry)}
            onChange={(event) =>
              onChange(
                rows.map((row, i) =>
                  i === index ? (numbers ? Number(event.target.value) : event.target.value) : row,
                ),
              )
            }
          />
          <Button
            variant="secondary"
            density="panel"
            aria-label={`Remove ${label} ${index + 1}`}
            onClick={() => onChange(rows.filter((_, i) => i !== index))}
          >
            Remove
          </Button>
        </div>
      ))}
      {!rows.length && (
        <Text as="p" variant="description">
          No entries
        </Text>
      )}
      <Button
        variant="secondary"
        density="panel"
        onClick={() => onChange([...rows, numbers ? 0 : ''])}
      >
        Add {label.toLowerCase()}
      </Button>
    </fieldset>
  );
}
function JsonEditor({ setting, value, disabled, onChange, raw, onRawChange }: FieldProps) {
  return (
    <label className="block space-y-2">
      <Text variant="label">{setting.label} JSON</Text>
      <textarea
        aria-label={`${setting.label} JSON`}
        className={`${textAreaClass} font-mono`}
        spellCheck={false}
        disabled={disabled}
        value={raw ?? JSON.stringify(value, null, 2)}
        onChange={(event) => {
          const text = event.target.value;
          try {
            const parsed: unknown = JSON.parse(text);
            if (
              parsed === null ||
              (setting.type === 'array'
                ? !Array.isArray(parsed)
                : typeof parsed !== 'object' || Array.isArray(parsed))
            )
              throw new Error(`Enter a JSON ${setting.type}.`);
            onRawChange(text);
            onChange(parsed as SettingValue, true);
          } catch (err) {
            onRawChange(text, (err as Error).message);
          }
        }}
      />
      <Text as="p" variant="description">
        Advanced options use the configuration schema. Existing attributes are kept when you edit
        the form.
      </Text>
    </label>
  );
}
function WorkspaceEditor({ value, disabled, onChange }: FieldProps) {
  const rows = Array.isArray(value) ? value : [];
  const update = (index: number, key: string, next: SettingValue) =>
    onChange(rows.map((row, i) => {
      if (i !== index) return row;
      const updated = { ...objectValue(row), [key]: next };
      if ((key === 'python_name' || key === 'description') && next === '') delete updated[key];
      return updated;
    }));
  return (
    <fieldset disabled={disabled} className="min-w-0 space-y-3">
      <legend className="mb-2 text-sm">Workspace roots</legend>
      {rows.map((entry, index) => {
        const row = objectValue(entry);
        return (
          <div key={index} className="space-y-2 border-l-2 border-dialog-edge pl-3">
            <div className="grid gap-2 sm:grid-cols-2">
              <Input
                aria-label={`Root name ${index + 1}`}
                placeholder="Root name"
                value={String(row.id ?? '')}
                onChange={(event) => update(index, 'id', event.target.value)}
              />
              <Input
                aria-label={`Root path ${index + 1}`}
                placeholder="Absolute path or ~/path"
                value={String(row.path ?? '')}
                onChange={(event) => update(index, 'path', event.target.value)}
              />
              <Select
                aria-label={`Root access ${index + 1}`}
                value={String(row.access ?? 'read-write')}
                options={[
                  ...new Set([...workspaceProperties.access.enum, String(row.access ?? 'read-write')]),
                ].map((value) => ({ value, label: value }))}
                onValueChange={(next) => update(index, 'access', next)}
              />
              <Input
                aria-label={`Python name ${index + 1}`}
                placeholder="Python name (optional)"
                value={String(row.python_name ?? '')}
                onChange={(event) => update(index, 'python_name', event.target.value)}
              />
              <Input
                aria-label={`Root description ${index + 1}`}
                placeholder="Description (optional)"
                value={String(row.description ?? '')}
                onChange={(event) => update(index, 'description', event.target.value)}
              />
              <Select
                aria-label={`Root draft mode ${index + 1}`}
                value={String(row.draft ?? 'shared')}
                options={workspaceProperties.draft.enum.map((value) => ({
                  value,
                  label: value,
                }))}
                onValueChange={(next) => update(index, 'draft', next)}
              />
            </div>
            <div className="flex flex-wrap items-center gap-3">
              <Text variant="description">Search this root</Text>
              <Switch
                label={`Search root ${index + 1}`}
                isOn={row.search !== false}
                onClick={() => update(index, 'search', row.search === false)}
              />
              <Text variant="description">Optional</Text>
              <Switch
                label={`Optional root ${index + 1}`}
                isOn={row.optional === true}
                onClick={() => update(index, 'optional', row.optional !== true)}
              />
              <Button
                variant="secondary"
                density="panel"
                onClick={() => onChange(rows.filter((_, i) => i !== index))}
              >
                Remove root {index + 1}
              </Button>
            </div>
          </div>
        );
      })}
      <Button
        variant="secondary"
        density="panel"
        onClick={() => onChange([...rows, { id: '', path: '', access: 'read-write' }])}
      >
        Add workspace root
      </Button>
    </fieldset>
  );
}
function RequestRules({
  rule,
  value,
  onChange,
  disabled,
}: {
  /** One-based host rule number; it keeps each request control name unique. */
  rule: number;
  value: SettingValue | null | undefined;
  onChange: (value: SettingValue[]) => void;
  disabled: boolean;
}) {
  const rows = Array.isArray(value) ? value : [];
  const update = (index: number, key: string, text: string) =>
    onChange(
      rows.map((entry, i) => {
        if (i !== index) return entry;
        const next = { ...objectValue(entry), [key]: text };
        if (key === 'path' && !text) delete next.path;
        return next;
      }),
    );
  return (
    <fieldset disabled={disabled} className="space-y-2">
      <legend className="mb-1 text-sm">Allowed requests</legend>
      {rows.map((entry, index) => {
        const row = objectValue(entry);
        return (
          <div key={index} className="flex flex-wrap gap-2">
            <Input
              aria-label={`Rule ${rule} request ${index + 1} method`}
              placeholder="GET"
              value={String(row.method ?? '')}
              onChange={(event) => update(index, 'method', event.target.value)}
            />
            <Input
              aria-label={`Rule ${rule} request ${index + 1} path`}
              placeholder="/api/* (optional)"
              value={String(row.path ?? '')}
              onChange={(event) => update(index, 'path', event.target.value)}
            />
            <Button
              variant="secondary"
              density="panel"
              aria-label={`Remove rule ${rule} request ${index + 1}`}
              onClick={() => onChange(rows.filter((_, i) => i !== index))}
            >
              Remove
            </Button>
          </div>
        );
      })}
      <Button
        variant="secondary"
        density="panel"
        aria-label={`Add allowed request to rule ${rule}`}
        onClick={() => onChange([...rows, { method: 'GET' }])}
      >
        Add allowed request
      </Button>
    </fieldset>
  );
}

function NetworkRules({
  value,
  onChange,
  disabled,
}: {
  value: SettingValue | null | undefined;
  onChange: (value: SettingValue[]) => void;
  disabled: boolean;
}) {
  const rows = Array.isArray(value) ? value : [];
  const update = (index: number, key: string, next: SettingValue) =>
    onChange(
      rows.map((entry, i) => (i === index ? { ...objectValue(entry), [key]: next } : entry)),
    );
  return (
    <fieldset disabled={disabled} className="space-y-3">
      <legend className="mb-2 text-sm">Host rules</legend>
      {rows.map((entry, index) => {
        const row = objectValue(entry);
        return (
          <div key={index} className="space-y-2 border-l-2 border-dialog-edge pl-3">
            <Input
              aria-label={`Rule host ${index + 1}`}
              placeholder="gateway.example.com"
              value={String(row.host ?? '')}
              onChange={(event) => update(index, 'host', event.target.value)}
            />
            <Select
              aria-label={`Rule access ${index + 1}`}
              value={String(row.access ?? 'read-write')}
              options={[
                ...new Set([...networkRuleProperties.access.enum, String(row.access ?? 'read-write')]),
              ].map((value) => ({ value, label: value }))}
              onValueChange={(next) => update(index, 'access', next)}
            />
            <StringList
              label={`Rule ${index + 1} methods`}
              value={row.methods}
              disabled={disabled}
              onChange={(next) => update(index, 'methods', next)}
            />
            <StringList
              label={`Rule ${index + 1} ports`}
              numbers
              value={row.ports}
              disabled={disabled}
              onChange={(next) => update(index, 'ports', next)}
            />
            <RequestRules
              rule={index + 1}
              value={row.allow}
              disabled={disabled}
              onChange={(next) => update(index, 'allow', next)}
            />
            <Button
              variant="secondary"
              density="panel"
              onClick={() => onChange(rows.filter((_, i) => i !== index))}
            >
              Remove host rule {index + 1}
            </Button>
          </div>
        );
      })}
      <Button
        variant="secondary"
        density="panel"
        onClick={() => onChange([...rows, { host: '', access: 'read-only' }])}
      >
        Add host rule
      </Button>
    </fieldset>
  );
}

/**
 * The editor for a number, list or object setting. Switches, choices and text
 * keep their own rows in `MachineSettings`.
 */
export function SettingField(props: FieldProps) {
  const { setting, value, onChange, disabled } = props;
  if (setting.type === 'number') {
    const schema = numericSchema(setting.schema);
    return (
      <Input
        type="number"
        aria-label={setting.label}
        disabled={disabled}
        min={schema.minimum}
        max={schema.maximum}
        step={schema.type === 'integer' ? 1 : 'any'}
        value={props.raw ?? String(value)}
        onChange={(event) => {
          const text = event.target.value;
          const number = Number(text);
          const error = !text.trim() || !Number.isFinite(number) ? 'Enter a number.'
            : schema.type === 'integer' && !Number.isInteger(number) ? 'Enter a whole number.'
            : schema.minimum !== undefined && number < schema.minimum ? `Use at least ${schema.minimum}.`
            : schema.maximum !== undefined && number > schema.maximum ? `Use at most ${schema.maximum}.`
            : undefined;
          props.onRawChange(text, error);
          if (!error) onChange(number, true);
        }}
      />
    );
  }
  const rawEditor = (
    <details className="pt-2">
      <summary className="cursor-pointer text-sm text-dialog-hint">Advanced JSON</summary>
      <div className="pt-3">
        <JsonEditor {...props} />
      </div>
    </details>
  );
  if (setting.editor === 'paths')
    return (
      <div className="space-y-3">
        <WorkspaceEditor {...props} />
        {rawEditor}
      </div>
    );
  if (setting.editor === 'filesystem') {
    const object = objectValue(value);
    return (
      <div className="space-y-3">
        {[
          ['allow', 'Allowed paths'],
          ['deny_read', 'Blocked read paths'],
          ['deny_write', 'Blocked write paths'],
        ].map(([key, label]) => (
          <StringList
            key={key}
            label={label}
            value={object[key]}
            disabled={disabled}
            onChange={(next) => onChange({ ...object, [key]: next })}
          />
        ))}
        {rawEditor}
      </div>
    );
  }
  if (setting.editor === 'network') {
    const object = objectValue(value);
    return (
      <div className="space-y-3">
        {[
          ['allowed_domains', 'Allowed domains'],
          ['denied_domains', 'Blocked domains'],
          ['exclude_domains', 'Excluded domains'],
        ].map(([key, label]) => (
          <StringList
            key={key}
            label={label}
            value={object[key]}
            disabled={disabled}
            onChange={(next) => onChange({ ...object, [key]: next })}
          />
        ))}
        <div className="flex items-center gap-3">
          <Text variant="label">Private network access</Text>
          <Switch
            label="Private network access"
            isOn={object.allow_private === true}
            disabled={disabled}
            onClick={() => onChange({ ...object, allow_private: object.allow_private !== true })}
          />
        </div>
        <StringList
          label="Inbound ports"
          value={object.inbound_ports}
          numbers
          disabled={disabled}
          onChange={(next) => onChange({ ...object, inbound_ports: next })}
        />
        <NetworkRules
          value={object.rules}
          disabled={disabled}
          onChange={(next) => onChange({ ...object, rules: next })}
        />
        {rawEditor}
      </div>
    );
  }
  if (setting.editor === 'list')
    return (
      <div className="space-y-3">
        <StringList label={setting.label} value={value} disabled={disabled} onChange={onChange} />
        {rawEditor}
      </div>
    );
  return <JsonEditor {...props} />;
}
