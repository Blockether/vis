import { useState, type ReactNode } from 'react';
import { Banner, Button, Checkbox, Input, Select } from '../components/ui';
import { FormLabel } from './settings/SettingsLayout';
import {
  ANSWER_OPTIONS,
  NAME_MAX,
  RUN_EVENT_OPTIONS,
  SIGNATURE_OPTIONS,
  TARGET_OPTIONS,
  TRIGGER_KIND_OPTIONS,
  TRIGGERS_MAX,
  UNIT_OPTIONS,
  automationDraft,
  deviceTimeZone,
  draftProblem,
  triggerDraft,
  type Automation,
  type AutomationDraft,
  type TriggerDraft,
} from '../lib/automations';

const TEXT_AREA =
  'min-h-32 w-full resize-y rounded-none border border-edge bg-input p-3 font-mono text-body text-white focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted';

/** Codes, paths and addresses: no automatic capitals or corrections. */
const CODE_INPUT = { autoCapitalize: 'none', autoCorrect: 'off', spellCheck: false } as const;

function Section({ title, children }: { title: string; children: ReactNode }) {
  return (
    <section className="space-y-3">
      <h3 className="font-mono text-ui font-bold text-white">{title}</h3>
      {children}
    </section>
  );
}

function TriggerFields({
  trigger,
  label,
  isWebhookTaken,
  busy,
  onChange,
  onRemove,
}: {
  trigger: TriggerDraft;
  label: string;
  /** Another trigger is the one webhook trigger of this automation. */
  isWebhookTaken: boolean;
  busy: boolean;
  onChange: (fields: Partial<TriggerDraft>) => void;
  onRemove?: () => void;
}) {
  const saved = trigger.saved?.kind === 'webhook' ? trigger.saved : null;
  const filters = trigger.kind === 'webhook' ? (saved?.filters?.length ?? 0) : 0;
  return (
    <div role="group" aria-label={label} className="space-y-3 border border-dialog-edge p-3">
      <p className="font-mono text-ui text-dialog-hint">{label}</p>
      <Select
        aria-label={`${label} kind`}
        value={trigger.kind}
        disabled={busy}
        onValueChange={(kind) => onChange({ kind })}
        options={TRIGGER_KIND_OPTIONS.map((option) => ({
          ...option,
          disabled: option.value === 'webhook' && isWebhookTaken,
        }))}
      />
      {trigger.kind === 'every' && (
        <div className="flex items-center gap-2 font-mono text-ui">
          <span>Every</span>
          <span className="w-24 shrink-0">
            <Input
              aria-label={`${label} interval`}
              type="number"
              inputMode="numeric"
              min={1}
              step={1}
              value={trigger.count}
              disabled={busy}
              onChange={(event) => onChange({ count: event.target.value })}
            />
          </span>
          <Select
            aria-label={`${label} unit`}
            value={trigger.unit}
            disabled={busy}
            onValueChange={(unit) => onChange({ unit })}
            options={UNIT_OPTIONS}
            className="min-w-0 flex-1"
          />
        </div>
      )}
      {trigger.kind === 'cron' && (
        <>
          <FormLabel
            label="Cron expression"
            hint="Five fields: minute, hour, day of the month, month and day of the week. 0 9 * * 1-5 runs at 9:00 on weekdays."
          >
            <Input
              value={trigger.expression}
              disabled={busy}
              {...CODE_INPUT}
              onChange={(event) => onChange({ expression: event.target.value })}
            />
          </FormLabel>
          <FormLabel label="Time zone" hint="Leave it empty to use the time zone of the machine.">
            <Input
              value={trigger.timezone}
              placeholder="Europe/Warsaw"
              disabled={busy}
              {...CODE_INPUT}
              onChange={(event) => onChange({ timezone: event.target.value })}
            />
          </FormLabel>
        </>
      )}
      {trigger.kind === 'once' && (
        <FormLabel label="Date and time" hint="The time on this device.">
          <Input
            type="datetime-local"
            value={trigger.at}
            disabled={busy}
            onChange={(event) => onChange({ at: event.target.value })}
          />
        </FormLabel>
      )}
      {trigger.kind === 'webhook' && (
        <>
          <FormLabel label="Signature" hint="Choose the signature that the sending service uses.">
            <Select
              aria-label={`${label} signature`}
              value={trigger.signature}
              disabled={busy}
              onValueChange={(signature) => onChange({ signature })}
              options={SIGNATURE_OPTIONS}
            />
          </FormLabel>
          <FormLabel
            label="Events"
            hint="Event names with commas between them, for example pull_request, push. Leave it empty to accept every event."
          >
            <Input
              value={trigger.events}
              disabled={busy}
              {...CODE_INPUT}
              onChange={(event) => onChange({ events: event.target.value })}
            />
          </FormLabel>
          {filters > 0 && (
            <p className="font-mono text-meta text-dialog-hint">
              {`This trigger keeps its ${filters} payload ${filters === 1 ? 'filter' : 'filters'}. Ask Vis in a chat to change ${filters === 1 ? 'it' : 'them'}.`}
            </p>
          )}
        </>
      )}
      {onRemove && (
        <Button type="button" variant="secondary" disabled={busy} onClick={onRemove}>
          Remove {label.toLowerCase()}
        </Button>
      )}
    </div>
  );
}

/** Create an automation, or edit a saved one. The caller sends the request. */
export function AutomationForm({
  automation,
  busy,
  onCancel,
  onSave,
}: {
  /** Null for a new automation. */
  automation: Automation | null;
  busy: boolean;
  onCancel: () => void;
  onSave: (draft: AutomationDraft) => void;
}) {
  const [draft, setDraft] = useState(() =>
    automationDraft(automation, Date.now(), deviceTimeZone()),
  );
  const [problem, setProblem] = useState<string | null>(null);
  const change = (fields: Partial<AutomationDraft>) =>
    setDraft((current) => ({ ...current, ...fields }));
  const changeTrigger = (index: number, fields: Partial<TriggerDraft>) =>
    setDraft((current) => ({
      ...current,
      triggers: current.triggers.map((trigger, at) =>
        at === index ? { ...trigger, ...fields } : trigger,
      ),
    }));
  const submit = () => {
    const next = draftProblem(draft);
    setProblem(next);
    if (!next) onSave(draft);
  };
  const title = automation ? `Edit ${automation.name}` : 'New automation';
  const webhookAt = draft.triggers.findIndex((trigger) => trigger.kind === 'webhook');

  return (
    <form
      aria-label={title}
      noValidate
      className="flex min-h-0 flex-1 flex-col"
      onSubmit={(event) => {
        event.preventDefault();
        submit();
      }}
    >
      <div className="flex shrink-0 flex-wrap items-center gap-3 border-b border-dialog-edge p-3">
        <Button type="submit" disabled={busy}>
          {automation ? 'Save automation' : 'Create automation'}
        </Button>
        <Button type="button" variant="secondary" disabled={busy} onClick={onCancel}>
          Cancel
        </Button>
      </div>
      <div className="min-h-0 flex-1 space-y-4 overflow-y-auto p-3">
        <h2 className="break-words font-mono text-title font-bold text-white">{title}</h2>
        {problem && <Banner kind="err">{problem}</Banner>}
        <FormLabel label="Name" hint="Lists and phone alerts show this name.">
          <Input
            value={draft.name}
            maxLength={NAME_MAX}
            disabled={busy}
            onChange={(event) => change({ name: event.target.value })}
          />
        </FormLabel>
        <FormLabel
          label="Prompt"
          hint="Vis gets this request at each run. With a webhook, {pull_request.title} inserts a value from the payload."
        >
          <textarea
            value={draft.prompt}
            rows={6}
            disabled={busy}
            className={TEXT_AREA}
            onChange={(event) => change({ prompt: event.target.value })}
          />
        </FormLabel>
        <Section title="When it runs">
          {draft.triggers.map((trigger, index) => (
            <TriggerFields
              key={index}
              trigger={trigger}
              label={draft.triggers.length > 1 ? `Trigger ${index + 1}` : 'Trigger'}
              isWebhookTaken={webhookAt >= 0 && webhookAt !== index}
              busy={busy}
              onChange={(fields) => changeTrigger(index, fields)}
              onRemove={
                draft.triggers.length > 1
                  ? () => change({ triggers: draft.triggers.filter((_, at) => at !== index) })
                  : undefined
              }
            />
          ))}
          {draft.triggers.length < TRIGGERS_MAX && (
            <Button
              type="button"
              variant="secondary"
              disabled={busy}
              onClick={() =>
                change({
                  triggers: [...draft.triggers, triggerDraft(null, Date.now(), deviceTimeZone())],
                })
              }
            >
              Add trigger
            </Button>
          )}
        </Section>
        <Section title="Where it runs">
          <FormLabel label="Session">
            <Select
              aria-label="Session"
              value={draft.target}
              disabled={busy}
              onValueChange={(target) => change({ target })}
              options={TARGET_OPTIONS}
            />
          </FormLabel>
          {draft.target === 'session' ? (
            <FormLabel
              label="Session ID"
              hint="In the session, open Session actions and select Copy session id."
            >
              <Input
                value={draft.sessionId}
                disabled={busy}
                {...CODE_INPUT}
                onChange={(event) => change({ sessionId: event.target.value })}
              />
            </FormLabel>
          ) : (
            <FormLabel
              label="Folder"
              hint={
                draft.target === 'temporary'
                  ? 'A folder on the machine, or empty for the default folder. Vis deletes the session after each run and keeps the answer.'
                  : 'A folder on the machine, or empty for the default folder.'
              }
            >
              <Input
                value={draft.root}
                placeholder="/home/me/project"
                disabled={busy}
                {...CODE_INPUT}
                onChange={(event) => change({ root: event.target.value })}
              />
            </FormLabel>
          )}
        </Section>
        <Section title="Model">
          <Select
            aria-label="Model"
            value={draft.answer}
            disabled={busy}
            onValueChange={(answer) => change({ answer })}
            options={ANSWER_OPTIONS}
          />
          {draft.answer === 'model' && (
            <>
              <FormLabel label="Model name">
                <Input
                  value={draft.model}
                  disabled={busy}
                  {...CODE_INPUT}
                  onChange={(event) => change({ model: event.target.value })}
                />
              </FormLabel>
              <FormLabel label="Provider" hint="Optional. Leave it empty to let the machine choose.">
                <Input
                  value={draft.provider}
                  disabled={busy}
                  {...CODE_INPUT}
                  onChange={(event) => change({ provider: event.target.value })}
                />
              </FormLabel>
            </>
          )}
        </Section>
        <Section title="Results">
          <Checkbox isOn={draft.push} disabled={busy} onClick={() => change({ push: !draft.push })}>
            Phone alert for each run
          </Checkbox>
          <FormLabel
            label="Callback address"
            hint="Optional. Vis sends each run event to this address and signs it with the callback secret. Create the secret after you save."
          >
            <Input
              type="url"
              inputMode="url"
              value={draft.callbackUrl}
              placeholder="https://gateway.example.com/results"
              disabled={busy}
              {...CODE_INPUT}
              onChange={(event) => change({ callbackUrl: event.target.value })}
            />
          </FormLabel>
          {draft.callbackUrl.trim() && (
            <FormLabel label="Callback events">
              <Select
                aria-label="Callback events"
                values={draft.callbackEvents}
                noneLabel="All run events"
                disabled={busy}
                onValuesChange={(callbackEvents) => change({ callbackEvents })}
                options={RUN_EVENT_OPTIONS}
              />
            </FormLabel>
          )}
        </Section>
      </div>
    </form>
  );
}
