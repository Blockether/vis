import { IconButton, Input, Select, Switch, Text } from '../../components/ui';
import { createContext, useContext, useState, type ReactNode } from 'react';
import { FormLabel } from './SettingsLayout';
import { PlusIcon, ProjectsIcon, TrashIcon } from '../../components/icons';
import { ProjectFolderSheet } from '../../components/ProjectFolderSheet';
import type { GatewayClient } from '../../lib/gateway';
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
const objectValue = (value: SettingValue): ObjectValue =>
  !Array.isArray(value) && typeof value === 'object' ? value : {};
const workspaceProperties = configSchema.$defs.workspaceEntry.properties;
const networkRuleProperties = configSchema.$defs.networkRule.properties;
type EnumSchema = { enum: string[]; 'x-vis-enum-aliases'?: Record<string, string> };
function enumValue(schema: EnumSchema, value: string): string {
  return schema['x-vis-enum-aliases']?.[value] ?? value;
}
function enumOptions(schema: EnumSchema, value: string) {
  return [...new Set([...schema.enum, value].map((option) => enumValue(schema, option)))].map(
    (value) => ({ value, label: value }),
  );
}

type NumericSchema = { type?: string; minimum?: number; maximum?: number };
function numericSchema(pointer?: string): NumericSchema {
  let node: unknown = configSchema;
  for (const key of (pointer?.split('#')[1] ?? '').split('/').slice(1)) {
    if (!node || typeof node !== 'object') return {};
    node = (node as Record<string, unknown>)[key.replace(/~1/g, '/').replace(/~0/g, '~')];
  }
  return node && typeof node === 'object' && pointer ? (node as NumericSchema) : {};
}

/** The machine whose settings these are, so a folder field can browse its disk. */
export const SettingMachine = createContext<{ client: GatewayClient; label: string } | null>(null);

const HTTP_METHODS = ['GET', 'HEAD', 'OPTIONS', 'POST', 'PUT', 'PATCH', 'DELETE'];
function methodOptions(extra: string[] = []) {
  return [...new Set([...HTTP_METHODS, ...extra.filter(Boolean)])].map((value) => ({ value, label: value }));
}

/** The last folder of a path, the default name of a root. */
function folderName(path: string): string {
  return path.replace(/\/+$/, '').split('/').pop()?.replace(/^~$/, 'home') ?? '';
}

/** The Python variable that the engine gives a root without `python_name`. */
function defaultPythonName(path: string): string {
  const base = folderName(path).toLowerCase().replace(/[^a-z0-9]+/g, '_').replace(/^_+|_+$/g, '');
  return base ? `${/^[a-z]/.test(base) ? '' : 'root_'}${base}_path` : 'docs_path';
}

/** A list legend with its add action on the same trailing rail as each row's trash. */
function ListHeader({
  label,
  onAdd,
  children,
}: {
  label: string;
  onAdd: () => void;
  children: ReactNode;
}) {
  return (
    <legend className="mb-1 flex w-full items-center justify-between gap-2">
      <Text variant="label" className="min-w-0 truncate">
        {children}
      </Text>
      <IconButton variant="quiet" label={label} title={label} onClick={onAdd}>
        <PlusIcon className="size-4" />
      </IconButton>
    </legend>
  );
}

/** One card of a repeated editor: a small `n / total` caption only when there are several. */
function Entry({ index, count, children }: { index: number; count: number; children: ReactNode }) {
  return (
    <div className="space-y-1">
      {count > 1 && (
        <p aria-hidden className="font-mono text-meta font-bold text-dialog-hint">
          {index + 1} / {count}
        </p>
      )}
      <div className="space-y-2 border border-dialog-edge p-3">{children}</div>
    </div>
  );
}

function StringList({
  label,
  name = label,
  hint,
  value,
  onChange,
  disabled,
  numbers = false,
}: {
  /** The visible legend. */
  label: string;
  /** One sentence on what the list does. */
  hint?: string;
  /** The accessible prefix; it keeps nested control names unique. */
  name?: string;
  value: SettingValue | null | undefined;
  onChange: (value: SettingValue[]) => void;
  disabled: boolean;
  numbers?: boolean;
}) {
  const rows = Array.isArray(value) ? value : [];
  return (
    <fieldset disabled={disabled} className="min-w-0 space-y-2">
      <ListHeader label={`Add ${name.toLowerCase()}`} onAdd={() => onChange([...rows, numbers ? 0 : ''])}>
        {label}
      </ListHeader>
      {hint && (
        <Text as="p" variant="description">
          {hint}
        </Text>
      )}
      {rows.map((entry, index) => (
        <div key={index} className="flex min-w-0 items-center gap-2">
          <Input
            className="min-w-0 flex-1"
            aria-label={`${name} ${index + 1}`}
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
          <IconButton
            label={`Remove ${name} ${index + 1}`}
            variant="remove"
            onClick={() => onChange(rows.filter((_, i) => i !== index))}
          >
            <TrashIcon className="size-3.5" />
          </IconButton>
        </div>
      ))}
      {!rows.length && (
        <Text as="p" variant="description">
          No entries
        </Text>
      )}
    </fieldset>
  );
}
const ROOT_ACCESS: Record<string, string> = { 'read-write': 'Read and write', 'read-only': 'Read only' };
const ROOT_DRAFT: Record<string, string> = {
  shared: 'Edit the real folder',
  'copy-and-apply': 'Edit a copy, apply on approval',
  'copy-only': 'Edit a copy, never apply',
  'not-allowed': 'Block the folder',
};
const RULE_ACCESS_HINT: Record<string, string> = {
  'read-only': 'Only GET, HEAD and OPTIONS requests.',
  'read-write': 'Every HTTP method.',
  none: 'No requests to this host.',
};
const RULE_ACCESS: Record<string, string> = {
  'read-only': 'Read only',
  'read-write': 'All methods',
  none: 'Blocked',
};
function labelled(options: { value: string; label: string }[], labels: Record<string, string>) {
  return options.map((option) => ({ ...option, label: labels[option.value] ?? option.label }));
}

/** A field in a card, with the card's trash on the same row when it is the first field. */
function CardField({
  label,
  hint,
  onRemove,
  removeLabel,
  children,
}: {
  label: string;
  hint?: string;
  onRemove?: () => void;
  removeLabel?: string;
  children: ReactNode;
}) {
  const field = (
    <FormLabel label={label} hint={hint}>
      {children}
    </FormLabel>
  );
  if (!onRemove) return field;
  return (
    <div className="flex min-w-0 items-end gap-2">
      <div className="min-w-0 flex-1">{field}</div>
      <span className="flex shrink-0">
        <IconButton label={removeLabel ?? 'Remove'} variant="remove" onClick={onRemove}>
          <TrashIcon className="size-3.5" />
        </IconButton>
      </span>
    </div>
  );
}

/** A switch on the right with its name and effect on the left. */
function ToggleRow({
  label,
  hint,
  name,
  isOn,
  disabled,
  onClick,
}: {
  label: string;
  hint: string;
  /** The accessible name; it keeps the switch unique among cards. */
  name: string;
  isOn: boolean;
  disabled?: boolean;
  onClick: () => void;
}) {
  return (
    <div className="flex min-w-0 items-center gap-3">
      <div className="min-w-0 flex-1">
        <Text variant="label" className="block">
          {label}
        </Text>
        <Text variant="description" className="block">
          {hint}
        </Text>
      </div>
      <Switch density="compact" label={name} isOn={isOn} disabled={disabled} onClick={onClick} />
    </div>
  );
}

function WorkspaceEditor({ value, disabled, onChange }: FieldProps) {
  const machine = useContext(SettingMachine);
  // The root that the folder sheet fills: an index, `'new'` for a new root, or closed.
  const [picking, setPicking] = useState<number | 'new' | null>(null);
  const rows = Array.isArray(value) ? value : [];
  const update = (index: number, key: string, next: SettingValue) =>
    onChange(rows.map((row, i) => {
      if (i !== index) return row;
      const updated = { ...objectValue(row), [key]: next };
      if ((key === 'python_name' || key === 'description') && next === '') delete updated[key];
      return updated;
    }));
  // The name follows the folder until someone types a different one.
  const setPath = (index: number, path: string) =>
    onChange(rows.map((row, i) => {
      if (i !== index) return row;
      const old = objectValue(row);
      const id = String(old.id ?? '');
      const isNameDerived = !id || id === folderName(String(old.path ?? ''));
      return { ...old, path, ...(isNameDerived ? { id: folderName(path) } : {}) };
    }));
  const choose = (path: string) => {
    if (picking === 'new') onChange([...rows, { id: folderName(path), path, access: 'read-write' }]);
    else if (picking !== null) setPath(picking, path);
    setPicking(null);
  };
  return (
    <fieldset disabled={disabled} className="min-w-0 space-y-3">
      <ListHeader
        label="Add workspace root"
        onAdd={() =>
          machine ? setPicking('new') : onChange([...rows, { id: '', path: '', access: 'read-write' }])
        }
      >
        Workspace roots
      </ListHeader>
      <Text as="p" variant="description">
        Folders that Vis can use besides the current project.
      </Text>
      {rows.map((entry, index) => {
        const row = objectValue(entry);
        const access = String(row.access ?? 'read-write');
        const path = String(row.path ?? '');
        return (
          <Entry key={index} index={index} count={rows.length}>
            <CardField
              label="Folder"
              removeLabel={`Remove root ${index + 1}`}
              onRemove={() => onChange(rows.filter((_, i) => i !== index))}
            >
              <span className="flex min-w-0 items-center gap-2">
                <Input
                  className="min-w-0 flex-1"
                  aria-label={`Root path ${index + 1}`}
                  placeholder="~/docs"
                  value={path}
                  onChange={(event) => setPath(index, event.target.value)}
                />
                {machine && (
                  <IconButton
                    label={`Choose folder for root ${index + 1}`}
                    title="Choose folder"
                    onClick={() => setPicking(index)}
                  >
                    <ProjectsIcon className="size-4" />
                  </IconButton>
                )}
              </span>
            </CardField>
            <CardField label="Name" hint="Filled in from the folder. Change it only if two roots share a name.">
              <Input
                aria-label={`Root name ${index + 1}`}
                placeholder={folderName(path) || 'docs'}
                value={String(row.id ?? '')}
                onChange={(event) => update(index, 'id', event.target.value)}
              />
            </CardField>
            <div className="grid gap-2 sm:grid-cols-2">
              <CardField label="Access">
                <Select
                  aria-label={`Root access ${index + 1}`}
                  className="w-full"
                  value={enumValue(workspaceProperties.access, access)}
                  options={labelled(enumOptions(workspaceProperties.access, access), ROOT_ACCESS)}
                  onValueChange={(next) => update(index, 'access', next)}
                />
              </CardField>
              <CardField label="In a draft session" hint="A draft works on copies that you approve.">
                <Select
                  aria-label={`Root draft mode ${index + 1}`}
                  className="w-full"
                  value={String(row.draft ?? 'shared')}
                  options={workspaceProperties.draft.enum.map((value) => ({
                    value,
                    label: ROOT_DRAFT[value] ?? value,
                  }))}
                  onValueChange={(next) => update(index, 'draft', next)}
                />
              </CardField>
            </div>
            <CardField label="Description" hint="Optional. Tells the agent what the folder holds.">
              <Input
                aria-label={`Root description ${index + 1}`}
                placeholder="Project notes"
                value={String(row.description ?? '')}
                onChange={(event) => update(index, 'description', event.target.value)}
              />
            </CardField>
            <CardField label="Python variable" hint="Optional. Leave it empty to use the name shown.">
              <Input
                aria-label={`Python name ${index + 1}`}
                placeholder={defaultPythonName(path)}
                value={String(row.python_name ?? '')}
                onChange={(event) => update(index, 'python_name', event.target.value)}
              />
            </CardField>
            <ToggleRow
              label="Include in search"
              hint="A search without a folder also looks here."
              name={`Search root ${index + 1}`}
              isOn={row.search !== false}
              onClick={() => update(index, 'search', row.search === false)}
            />
            <ToggleRow
              label="Skip when missing"
              hint="A machine without this folder ignores it, with no error."
              name={`Optional root ${index + 1}`}
              isOn={row.optional === true}
              onClick={() => update(index, 'optional', row.optional !== true)}
            />
          </Entry>
        );
      })}
      {machine && picking !== null && (
        <ProjectFolderSheet
          label={machine.label}
          mode="pick"
          at={null}
          client={machine.client}
          startAt={picking === 'new' ? null : String(objectValue(rows[picking]).path ?? '') || null}
          knownRoots={new Set()}
          onCancel={() => setPicking(null)}
          onChoose={choose}
        />
      )}
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
    <fieldset disabled={disabled} className="min-w-0 space-y-2">
      <ListHeader label={`Add allowed request to rule ${rule}`} onAdd={() => onChange([...rows, { method: 'GET' }])}>
        Allowed requests
      </ListHeader>
      <Text as="p" variant="description">
        Requests allowed beyond the access level, for example POST to /v1/issues/**.
      </Text>
      {rows.map((entry, index) => {
        const row = objectValue(entry);
        return (
          <div key={index} className="flex min-w-0 items-center gap-2">
            <div className="w-28 shrink-0">
              <Select
                aria-label={`Rule ${rule} request ${index + 1} method`}
                className="w-full"
                value={String(row.method ?? 'GET')}
                options={methodOptions([String(row.method ?? '')])}
                onValueChange={(next) => update(index, 'method', next)}
              />
            </div>
            <Input
              className="min-w-0 flex-1"
              aria-label={`Rule ${rule} request ${index + 1} path`}
              placeholder="Any path, or /v1/issues/**"
              value={String(row.path ?? '')}
              onChange={(event) => update(index, 'path', event.target.value)}
            />
            <IconButton
              label={`Remove rule ${rule} request ${index + 1}`}
              variant="remove"
              onClick={() => onChange(rows.filter((_, i) => i !== index))}
            >
              <TrashIcon className="size-3.5" />
            </IconButton>
          </div>
        );
      })}
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
      <ListHeader label="Add host rule" onAdd={() => onChange([...rows, { host: '', access: 'read-only' }])}>
        Host rules
      </ListHeader>
      {rows.map((entry, index) => {
        const row = objectValue(entry);
        const ruleAccess = enumValue(networkRuleProperties.access, String(row.access ?? 'read-write'));
        return (
          <Entry key={index} index={index} count={rows.length}>
            <CardField
              label="Host"
              removeLabel={`Remove host rule ${index + 1}`}
              onRemove={() => onChange(rows.filter((_, i) => i !== index))}
            >
              <Input
                aria-label={`Rule host ${index + 1}`}
                placeholder="api.example.com"
                value={String(row.host ?? '')}
                onChange={(event) => update(index, 'host', event.target.value)}
              />
            </CardField>
            <CardField label="Access" hint={RULE_ACCESS_HINT[ruleAccess]}>
              <Select
                aria-label={`Rule access ${index + 1}`}
                className="w-full"
                value={ruleAccess}
                options={labelled(
                  enumOptions(networkRuleProperties.access, String(row.access ?? 'read-write')),
                  RULE_ACCESS,
                )}
                onValueChange={(next) => update(index, 'access', next)}
              />
            </CardField>
            <CardField label="Methods" hint="Optional. Pick methods to replace the access level.">
              <Select
                aria-label={`Rule ${index + 1} methods`}
                className="w-full"
                values={Array.isArray(row.methods) ? row.methods.map(String) : []}
                noneLabel="As the access allows"
                options={methodOptions(Array.isArray(row.methods) ? row.methods.map(String) : [])}
                onValuesChange={(next) => update(index, 'methods', next)}
              />
            </CardField>
            <StringList
              label="Ports"
              hint="Optional. Only these ports, for example 443."
              name={`Rule ${index + 1} ports`}
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
          </Entry>
        );
      })}
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
  if (setting.editor === 'paths') return <WorkspaceEditor {...props} />;
  if (setting.editor === 'filesystem') {
    const object = objectValue(value);
    return (
      <div className="space-y-3">
        {[
          ['allow', 'Allowed paths', 'Workspace roots or folders that sandboxed commands can use.'],
          ['deny_read', 'Blocked read paths', 'No command can read these, for example **/.env*.'],
          ['deny_write', 'Blocked write paths', 'No command can change these.'],
        ].map(([key, label, hint]) => (
          <StringList
            key={key}
            label={label}
            hint={hint}
            value={object[key]}
            disabled={disabled}
            onChange={(next) => onChange({ ...object, [key]: next })}
          />
        ))}
      </div>
    );
  }
  if (setting.editor === 'network') {
    const object = objectValue(value);
    return (
      <div className="space-y-3">
        {[
          ['allowed_domains', 'Allowed domains', 'Hosts that commands can reach. *.pypi.org also works.'],
          ['denied_domains', 'Blocked domains', 'Blocked even when another list allows them.'],
          ['exclude_domains', 'Excluded domains', 'No HTTPS inspection, for clients that pin certificates.'],
        ].map(([key, label, hint]) => (
          <StringList
            key={key}
            label={label}
            hint={hint}
            value={object[key]}
            disabled={disabled}
            onChange={(next) => onChange({ ...object, [key]: next })}
          />
        ))}
        <ToggleRow
          label="Private network access"
          hint="Commands can reach private addresses, such as 10.0.0.5."
          name="Private network access"
          isOn={object.allow_private === true}
          disabled={disabled}
          onClick={() => onChange({ ...object, allow_private: object.allow_private !== true })}
        />
        <StringList
          label="Inbound ports"
          hint="Ports where a server in the sandbox accepts connections."
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
      </div>
    );
  }
  if (setting.editor === 'list')
    return (
      <StringList label={setting.label} value={value} disabled={disabled} onChange={onChange} />
    );
  return (
    <Text as="p" variant="description">
      Edit this setting in your configuration file.
    </Text>
  );
}
