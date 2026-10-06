import { useState, type ReactNode } from 'react';
import {
  Banner,
  Button,
  Checkbox,
  ChoiceCell,
  CloseButton,
  ConfirmRow,
  Input,
  Select,
} from '../components/ui';
import { FormLabel } from './settings/SettingsLayout';
import {
  ANSWER_OPTIONS,
  NAME_MAX,
  RUN_EVENT_OPTIONS,
  SIGNATURE_OPTIONS,
  TARGET_OPTIONS,
  TRIGGER_CHOICES,
  TRIGGER_KIND_OPTIONS,
  TRIGGERS_MAX,
  UNIT_OPTIONS,
  automationDraft,
  automationInput,
  deliveryLabel,
  deviceTimeZone,
  draftStepProblem,
  stepProblem,
  targetLabel,
  triggerDraft,
  triggerLabel,
  type Automation,
  type AutomationDraft,
  type AutomationStep,
  type TriggerDraft,
} from '../lib/automations';

const TEXT_AREA =
  'min-h-32 w-full resize-y rounded-none border border-edge bg-input p-3 font-mono text-body text-white focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted';

/** Codes, paths and addresses: no automatic capitals or corrections. */
const CODE_INPUT = { autoCapitalize: 'none', autoCorrect: 'off', spellCheck: false } as const;

/**
 * THE FORM ASKS ONE QUESTION AT A TIME. One long form with every field made you
 * learn the whole automation model before you could start. The wizard first asks
 * what starts the automation, then shows only the fields of that answer.
 */
type Step = 'start' | AutomationStep | 'review';

const STEPS: { id: Step; label: string; question: string }[] = [
  { id: 'start', label: 'Start', question: 'What starts this automation?' },
  { id: 'when', label: 'When', question: 'When does it run?' },
  { id: 'task', label: 'Task', question: 'What does Vis do at each run?' },
  { id: 'place', label: 'Session', question: 'Where does it run?' },
  { id: 'answer', label: 'Answer', question: 'How does Vis answer?' },
  { id: 'review', label: 'Review', question: 'Check the automation.' },
];

const LAST = STEPS.length - 1;

/** A group of choices, one of which is the answer. */
function Choices({ label, children }: { label: string; children: ReactNode }) {
  return (
    <div
      role="group"
      aria-label={label}
      className="grid grid-cols-1 gap-px border border-dialog-edge bg-dialog-edge"
    >
      {children}
    </div>
  );
}

function TriggerFields({
  trigger,
  label,
  isKindShown,
  isWebhookTaken,
  busy,
  onChange,
  onRemove,
}: {
  trigger: TriggerDraft;
  label: string;
  /** The first trigger takes its kind from the first step. */
  isKindShown: boolean;
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
      {isKindShown && (
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
      )}
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
          <Choices label={`${label} signature`}>
            {SIGNATURE_OPTIONS.map((option) => (
              <ChoiceCell
                key={option.value}
                title={option.label}
                sub={option.hint}
                isSelected={trigger.signature === option.value}
                disabled={busy}
                onClick={() => onChange({ signature: option.value })}
              />
            ))}
          </Choices>
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
          <p className="font-mono text-meta text-dialog-hint">
            After you save, create the webhook secret and give the webhook address to the sender.
          </p>
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

/** One line of the review, with a way back to the step that sets it. */
function ReviewRow({
  label,
  busy,
  onChange,
  children,
}: {
  label: string;
  busy: boolean;
  onChange: () => void;
  children: ReactNode;
}) {
  return (
    <div className="flex items-start justify-between gap-3 border-b border-dialog-edge py-2 last:border-b-0">
      <div className="min-w-0 space-y-1">
        <dt className="font-mono text-meta text-dialog-hint">{label}</dt>
        <dd className="whitespace-pre-wrap break-words font-mono text-body text-white">
          {children}
        </dd>
      </div>
      <Button
        type="button"
        variant="secondary"
        density="inline"
        aria-label={`Change ${label.toLowerCase()}`}
        disabled={busy}
        onClick={onChange}
      >
        Change
      </Button>
    </div>
  );
}

const answerLabel = (draft: AutomationDraft) =>
  draft.answer === 'model'
    ? [draft.provider.trim(), draft.model.trim()].filter(Boolean).join(' · ')
    : (ANSWER_OPTIONS.find((option) => option.value === draft.answer)?.label ?? draft.answer);

/** Create an automation step by step, or edit a saved one. The caller sends the request. */
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
  const isNew = !automation;
  const [initial] = useState(() => automationDraft(automation, Date.now(), deviceTimeZone()));
  const [draft, setDraft] = useState(initial);
  // A saved automation opens on its review: every answer is there, with a way to each step.
  const [at, setAt] = useState(isNew ? 0 : LAST);
  // The furthest step that you can open. A new automation opens each step in turn.
  const [reached, setReached] = useState(isNew ? 0 : LAST);
  const [problem, setProblem] = useState<string | null>(null);
  const [isCancelAsked, setIsCancelAsked] = useState(false);
  const step = STEPS[at];
  const change = (fields: Partial<AutomationDraft>) =>
    setDraft((current) => ({ ...current, ...fields }));
  const changeTrigger = (index: number, fields: Partial<TriggerDraft>) =>
    setDraft((current) => ({
      ...current,
      triggers: current.triggers.map((trigger, place) =>
        place === index ? { ...trigger, ...fields } : trigger,
      ),
    }));
  const go = (next: number) => {
    setProblem(null);
    setAt(next);
    setReached((current) => Math.max(current, next));
  };
  const goTo = (id: Step) => go(STEPS.findIndex((item) => item.id === id));
  const next = () => {
    const found = step.id === 'start' || step.id === 'review' ? null : stepProblem(step.id, draft);
    setProblem(found);
    if (!found) go(Math.min(at + 1, LAST));
  };
  const submit = () => {
    const found = draftStepProblem(draft);
    if (found) {
      goTo(found.step);
      setProblem(found.problem);
      return;
    }
    setProblem(null);
    onSave(draft);
  };
  const title = automation ? `Edit ${automation.name}` : 'New automation';
  const webhookAt = draft.triggers.findIndex((trigger) => trigger.kind === 'webhook');
  const first = draft.triggers[0];
  const input = step.id === 'review' ? automationInput(draft) : null;
  const isSaveShown = !isNew || at === LAST;
  // The first choice of a new automation moves on by itself, so that step has no actions.
  const isFooterShown = !isNew || at > 0;
  const isChanged = JSON.stringify(draft) !== JSON.stringify(initial);
  const cancel = () => (isChanged ? setIsCancelAsked(true) : onCancel());
  const question =
    step.id === 'when' && first?.kind === 'webhook' ? 'Which webhook starts it?' : step.question;

  return (
    <form
      aria-label={title}
      noValidate
      className="flex min-h-0 flex-1 flex-col"
      onSubmit={(event) => {
        event.preventDefault();
        if (isSaveShown) submit();
        else next();
      }}
    >
      <div className="flex shrink-0 items-center justify-between gap-3 border-b border-dialog-edge px-3 py-2">
        <h2 className="min-w-0 break-words font-mono text-title font-bold text-white">{title}</h2>
        <CloseButton label="Cancel" disabled={busy} onClick={cancel} />
      </div>
      <ol
        aria-label="Steps"
        className="flex shrink-0 flex-wrap gap-x-4 gap-y-1 border-b border-dialog-edge px-3 py-2 font-mono text-ui"
      >
        {STEPS.map((item, index) => (
          <li key={item.id}>
            <button
              type="button"
              aria-current={index === at ? 'step' : undefined}
              disabled={busy || index > reached}
              onClick={() => go(index)}
              className={`min-h-8 transition-colors duration-150 disabled:opacity-45 motion-reduce:transition-none ${
                index === at
                  ? 'font-bold text-accent-ink'
                  : 'text-dialog-hint enabled:hover:text-white'
              }`}
            >
              {`${index + 1}. ${item.label}`}
            </button>
          </li>
        ))}
      </ol>
      <div className="min-h-0 flex-1 space-y-4 overflow-y-auto p-3">
        <h3 className="font-mono text-ui font-bold text-white">{question}</h3>
        {problem && <Banner kind="err">{problem}</Banner>}
        {step.id === 'start' && first && (
          <Choices label="What starts it">
            {TRIGGER_CHOICES.map((choice) => (
              <ChoiceCell
                key={choice.value}
                title={choice.label}
                sub={choice.hint}
                isSelected={first.kind === choice.value}
                disabled={busy || (choice.value === 'webhook' && webhookAt > 0)}
                onClick={() => {
                  changeTrigger(0, { kind: choice.value });
                  if (isNew) go(at + 1);
                }}
              />
            ))}
          </Choices>
        )}
        {step.id === 'when' && (
          <>
            {draft.triggers.map((trigger, index) => (
              <TriggerFields
                key={index}
                trigger={trigger}
                label={draft.triggers.length > 1 ? `Trigger ${index + 1}` : 'Trigger'}
                isKindShown={index > 0}
                isWebhookTaken={webhookAt >= 0 && webhookAt !== index}
                busy={busy}
                onChange={(fields) => changeTrigger(index, fields)}
                onRemove={
                  index > 0
                    ? () => change({ triggers: draft.triggers.filter((_, place) => place !== index) })
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
          </>
        )}
        {step.id === 'task' && (
          <>
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
          </>
        )}
        {step.id === 'place' && (
          <>
            <Choices label="Session">
              {TARGET_OPTIONS.map((option) => (
                <ChoiceCell
                  key={option.value}
                  title={option.label}
                  sub={option.hint}
                  isSelected={draft.target === option.value}
                  disabled={busy}
                  onClick={() => change({ target: option.value })}
                />
              ))}
            </Choices>
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
              <FormLabel label="Folder" hint="A folder on the machine, or empty for the default folder.">
                <Input
                  value={draft.root}
                  placeholder="/home/me/project"
                  disabled={busy}
                  {...CODE_INPUT}
                  onChange={(event) => change({ root: event.target.value })}
                />
              </FormLabel>
            )}
          </>
        )}
        {step.id === 'answer' && (
          <>
            <Choices label="Model">
              {ANSWER_OPTIONS.map((option) => (
                <ChoiceCell
                  key={option.value}
                  title={option.label}
                  sub={option.hint}
                  isSelected={draft.answer === option.value}
                  disabled={busy}
                  onClick={() => change({ answer: option.value })}
                />
              ))}
            </Choices>
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
          </>
        )}
        {input && (
          <dl aria-label="Review" className="border border-dialog-edge px-3">
            <ReviewRow label="When it runs" busy={busy} onChange={() => goTo('when')}>
              {input.triggers.map((trigger) => triggerLabel(trigger)).join('\n')}
            </ReviewRow>
            <ReviewRow label="Name" busy={busy} onChange={() => goTo('task')}>
              {input.name || '—'}
            </ReviewRow>
            <ReviewRow label="Prompt" busy={busy} onChange={() => goTo('task')}>
              {input.prompt.trim() || '—'}
            </ReviewRow>
            <ReviewRow label="Session" busy={busy} onChange={() => goTo('place')}>
              {targetLabel(input.target)}
            </ReviewRow>
            <ReviewRow label="Model" busy={busy} onChange={() => goTo('answer')}>
              {answerLabel(draft) || '—'}
            </ReviewRow>
            <ReviewRow label="Results" busy={busy} onChange={() => goTo('answer')}>
              {input.delivery ? deliveryLabel(input.delivery) : '—'}
            </ReviewRow>
          </dl>
        )}
      </div>
      {isCancelAsked ? (
        <ConfirmRow
          question={isNew ? 'Discard this automation?' : 'Discard your changes?'}
          keepLabel="Keep editing"
          confirmLabel="Discard"
          onKeep={() => setIsCancelAsked(false)}
          onConfirm={onCancel}
        />
      ) : (
        isFooterShown && (
          <div
            role="group"
            aria-label="Step actions"
            className="flex shrink-0 items-center gap-3 border-t border-dialog-edge p-3"
          >
            {at > 0 && (
              <Button type="button" variant="secondary" disabled={busy} onClick={() => go(at - 1)}>
                Back
              </Button>
            )}
            <Button type="submit" disabled={busy} className="ml-auto">
              {isSaveShown
                ? automation
                  ? 'Save automation'
                  : 'Create automation'
                : `Next: ${STEPS[at + 1].label}`}
            </Button>
          </div>
        )
      )}
    </form>
  );
}
