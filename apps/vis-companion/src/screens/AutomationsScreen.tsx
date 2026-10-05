import { useEffect, useMemo, useState, type ReactNode } from 'react';
import { GatewayClient } from '../lib/gateway';
import type { GatewayConn } from '../lib/types';
import { timeLabel } from '../lib/fleet';
import {
  automationInput,
  automationPatch,
  automationSummary,
  deliveryLabel,
  runMillis,
  runReason,
  targetLabel,
  triggerLabel,
  wordLabel,
  type Automation,
  type AutomationDraft,
  type AutomationList,
  type AutomationRun,
  type AutomationSecret,
  type AutomationSecretKind,
} from '../lib/automations';
import { Banner, Button, CopyChip, DialogFrame, ListRow, Modal, Select } from '../components/ui';
import {
  AutomationsIcon,
  CircleAlertIcon,
  CircleCheckIcon,
  CircleDashedIcon,
  CircleDotIcon,
  CircleSlashIcon,
  CircleXIcon,
  PauseIcon,
} from '../components/icons';
import { AutomationForm } from './AutomationForm';

export type AutomationsClient = Pick<
  GatewayClient,
  | 'automations'
  | 'createAutomation'
  | 'automationRuns'
  | 'updateAutomation'
  | 'runAutomation'
  | 'createAutomationSecret'
  | 'deleteAutomation'
>;

const message = (error: unknown) =>
  error instanceof Error ? error.message : 'Automations could not complete this request.';

const countLabel = (total: number) => `${total} ${total === 1 ? 'automation' : 'automations'}`;

export function AutomationsDialog({
  gateways,
  initialUrl,
  onClose,
}: {
  gateways: GatewayConn[];
  initialUrl?: string;
  onClose: () => void;
}) {
  const [url, setUrl] = useState(initialUrl ?? gateways[0]?.url ?? '');
  const gateway = gateways.find((item) => item.url === url) ?? gateways[0];
  const client = useMemo(() => (gateway ? new GatewayClient(gateway) : null), [gateway]);
  return (
    <Modal onDismiss={onClose}>
      <DialogFrame
        title="Automations"
        subtitle="Prompts that run on a schedule or a webhook"
        onClose={onClose}
      >
        {gateways.length > 1 && (
          <label className="flex items-center gap-3 border-b border-dialog-edge px-3 py-2 font-mono text-ui">
            Machine
            <Select
              aria-label="Automations machine"
              value={gateway?.url ?? ''}
              onValueChange={setUrl}
              className="min-w-0 flex-1"
              options={gateways.map((item) => ({ value: item.url, label: item.label || item.url }))}
            />
          </label>
        )}
        {client ? (
          <AutomationsWorkspace key={gateway!.url} client={client} gatewayUrl={gateway!.url} />
        ) : (
          <Banner kind="neutral">Pair a machine to use automations.</Banner>
        )}
      </DialogFrame>
    </Modal>
  );
}

/** Production workflow; stories and tests replace only the authenticated gateway boundary. */
export function AutomationsWorkspace({
  client,
  gatewayUrl,
}: {
  client: AutomationsClient;
  /** The address this app uses for the machine; it completes the webhook path. */
  gatewayUrl?: string;
}) {
  const [list, setList] = useState<AutomationList | null>(null);
  const [selectedId, setSelectedId] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [notice, setNotice] = useState<string | null>(null);
  const [revision, setRevision] = useState(0);
  /** The open form: a null id creates an automation. */
  const [form, setForm] = useState<{ id: string | null } | null>(null);

  useEffect(() => {
    const controller = new AbortController();
    void client
      .automations(controller.signal)
      .then((next) => {
        if (!controller.signal.aborted) setList(next);
      })
      .catch((reason) => {
        if (!controller.signal.aborted) setError(message(reason));
      });
    return () => controller.abort();
  }, [client, revision]);

  const run = async (action: () => Promise<void>) => {
    setBusy(true);
    setError(null);
    setNotice(null);
    try {
      await action();
    } catch (reason) {
      setError(message(reason));
    } finally {
      setBusy(false);
    }
  };
  const refresh = () => {
    setError(null);
    setRevision((value) => value + 1);
  };
  const open = (id: string | null) => {
    setSelectedId(id);
    setError(null);
    setNotice(null);
  };
  const selected = list?.automations.find((item) => item.id === selectedId) ?? null;
  const editing = form?.id
    ? (list?.automations.find((item) => item.id === form.id) ?? null)
    : null;
  const edit = (id: string | null) => {
    setForm({ id });
    setError(null);
    setNotice(null);
  };
  const keep = (automation: Automation) =>
    setList(
      (current) =>
        current && {
          ...current,
          automations: current.automations.some((item) => item.id === automation.id)
            ? current.automations.map((item) => (item.id === automation.id ? automation : item))
            : [...current.automations, automation],
        },
    );
  const save = (draft: AutomationDraft) =>
    void run(async () => {
      if (editing) {
        const changes = automationPatch(draft, editing);
        const isChanged = Object.keys(changes).length > 0;
        if (isChanged) keep(await client.updateAutomation(editing.id, changes));
        setForm(null);
        setNotice(isChanged ? 'Automation saved.' : 'No changes to save.');
        if (!isChanged) return;
      } else {
        const created = await client.createAutomation(automationInput(draft));
        keep(created);
        setForm(null);
        setSelectedId(created.id);
        setNotice(
          created.webhook
            ? 'Automation created. Create the webhook secret, then give the webhook address to the sender.'
            : 'Automation created.',
        );
      }
      refresh();
    });

  return (
    <div className="flex min-h-0 flex-1 flex-col">
      <div className="flex shrink-0 flex-wrap items-center justify-between gap-3 border-b border-dialog-edge px-3 py-3">
        <span className="flex min-w-0 items-center gap-2 font-mono text-ui text-dialog-hint">
          <AutomationsIcon />
          {list ? countLabel(list.automations.length) : 'Loading automations…'}
        </span>
        <span className="flex flex-wrap items-center gap-3">
          {!form && (
            <Button disabled={busy} onClick={() => edit(null)}>
              New automation
            </Button>
          )}
          <Button variant="secondary" disabled={busy} onClick={refresh}>
            Refresh
          </Button>
        </span>
      </div>
      {error && (
        <div className="px-3 pt-3">
          <Banner kind="err">{error}</Banner>
        </div>
      )}
      {notice && (
        <div className="px-3 pt-3">
          <Banner kind="ok">{notice}</Banner>
        </div>
      )}
      {form && (!form.id || editing) ? (
        <AutomationForm
          key={form.id ?? 'new'}
          automation={editing}
          busy={busy}
          onCancel={() => {
            setForm(null);
            setError(null);
          }}
          onSave={save}
        />
      ) : selected ? (
        <AutomationDetail
          key={selected.id}
          automation={selected}
          client={client}
          gatewayUrl={gatewayUrl}
          busy={busy}
          refreshKey={revision}
          run={run}
          onBack={() => open(null)}
          onEdit={() => edit(selected.id)}
          onChanged={refresh}
          onNotice={setNotice}
          onDeleted={() => {
            setSelectedId(null);
            setNotice('Automation deleted.');
            refresh();
          }}
        />
      ) : (
        <div className="min-h-0 flex-1 overflow-y-auto" aria-busy={!list && !error}>
          {list?.automations.length === 0 && (
            <div className="space-y-2 p-4 font-mono text-body">
              <p className="font-bold">No automations on this machine</p>
              <p className="text-dialog-hint">
                Select New automation, or ask Vis in a chat. For example: “Every weekday at 9:00,
                summarize the new issues.”
              </p>
            </div>
          )}
          {list?.automations.map((automation) => (
            <div key={automation.id} className="border-b border-dialog-edge">
              <ListRow disabled={busy} onClick={() => open(automation.id)}>
                {automation.enabled ? <AutomationsIcon /> : <PauseIcon />}
                <span className="min-w-0 flex-1">
                  <span className="block break-words font-mono text-body font-bold text-white">
                    {automation.name}
                  </span>
                  <span className="block font-mono text-meta text-dialog-hint">
                    {automationSummary(automation)}
                  </span>
                </span>
              </ListRow>
            </div>
          ))}
          {!list && error && (
            <div className="p-3">
              <Button variant="secondary" disabled={busy} onClick={refresh}>
                Retry
              </Button>
            </div>
          )}
        </div>
      )}
    </div>
  );
}

function RunStatusIcon({ status }: { status: string }) {
  switch (status) {
    case 'completed':
      return <CircleCheckIcon />;
    case 'failed':
      return <CircleXIcon />;
    case 'running':
      return <CircleDotIcon />;
    case 'queued':
      return <CircleDashedIcon />;
    case 'cancelled':
    case 'skipped':
      return <CircleSlashIcon />;
    default:
      return <CircleAlertIcon />;
  }
}

function Fact({ label, children }: { label: string; children: ReactNode }) {
  return (
    <>
      <dt className="text-dialog-hint">{label}</dt>
      <dd className="min-w-0 break-words text-white">{children}</dd>
    </>
  );
}

function modelLabel(automation: Automation): string {
  if (automation.deliver_only) return 'None. The run sends the prompt as the answer.';
  if (!automation.model) return 'Machine default';
  return [automation.model.provider, automation.model.model].filter(Boolean).join(' · ');
}

function AutomationDetail({
  automation,
  client,
  gatewayUrl,
  busy,
  refreshKey,
  run,
  onBack,
  onEdit,
  onChanged,
  onNotice,
  onDeleted,
}: {
  automation: Automation;
  client: AutomationsClient;
  gatewayUrl?: string;
  busy: boolean;
  refreshKey: number;
  run: (action: () => Promise<void>) => Promise<void>;
  onBack: () => void;
  onEdit: () => void;
  onChanged: () => void;
  onNotice: (text: string) => void;
  onDeleted: () => void;
}) {
  const [runs, setRuns] = useState<AutomationRun[] | null>(null);
  const [runsError, setRunsError] = useState<string | null>(null);
  const [confirm, setConfirm] = useState<'delete' | AutomationSecretKind | null>(null);
  const [secret, setSecret] = useState<AutomationSecret | null>(null);

  useEffect(() => {
    const controller = new AbortController();
    void client
      .automationRuns(automation.id, controller.signal)
      .then((next) => {
        if (controller.signal.aborted) return;
        setRuns(next.runs);
        setRunsError(null);
      })
      .catch((reason) => {
        if (!controller.signal.aborted) setRunsError(message(reason));
      });
    return () => controller.abort();
  }, [client, automation.id, refreshKey]);

  const secretKinds: AutomationSecretKind[] = [
    ...(automation.webhook ? (['webhook'] as const) : []),
    ...(automation.delivery.callback ? (['callback'] as const) : []),
  ];
  const createSecret = (kind: AutomationSecretKind) =>
    void run(async () => {
      setSecret(await client.createAutomationSecret(automation.id, kind));
      setConfirm(null);
      onChanged();
    });
  const replacing = confirm === 'webhook' || confirm === 'callback' ? confirm : null;
  const webhookUrl = automation.webhook
    ? `${gatewayUrl?.replace(/\/+$/, '') ?? ''}${automation.webhook.path}`
    : null;

  return (
    <div className="flex min-h-0 flex-1 flex-col">
      <div className="flex shrink-0 flex-wrap items-center gap-3 border-b border-dialog-edge p-3">
        <Button variant="secondary" onClick={onBack} disabled={busy}>
          Back to automations
        </Button>
        <Button
          disabled={busy}
          onClick={() =>
            void run(async () => {
              await client.runAutomation(automation.id);
              onNotice('Run started. The answer goes to the target session.');
              onChanged();
            })
          }
        >
          Run now
        </Button>
        <Button variant="secondary" disabled={busy} onClick={onEdit}>
          Edit
        </Button>
        <Button
          variant="secondary"
          disabled={busy}
          onClick={() =>
            void run(async () => {
              await client.updateAutomation(automation.id, { enabled: !automation.enabled });
              onNotice(automation.enabled ? 'Automation paused.' : 'Automation resumed.');
              onChanged();
            })
          }
        >
          {automation.enabled ? 'Pause' : 'Resume'}
        </Button>
        <Button variant="danger" disabled={busy} onClick={() => setConfirm('delete')}>
          Delete
        </Button>
      </div>
      <div className="min-h-0 flex-1 space-y-4 overflow-y-auto p-3">
        {confirm === 'delete' && (
          <div className="space-y-3">
            <Banner kind="warn">
              Delete {automation.name}? Its schedule and webhook stop at once. You cannot undo this.
            </Banner>
            <div className="flex flex-wrap gap-3">
              <Button
                variant="danger"
                disabled={busy}
                onClick={() =>
                  void run(async () => {
                    await client.deleteAutomation(automation.id);
                    onDeleted();
                  })
                }
              >
                Delete automation
              </Button>
              <Button variant="secondary" disabled={busy} onClick={() => setConfirm(null)}>
                Keep automation
              </Button>
            </div>
          </div>
        )}
        {replacing && (
          <div className="space-y-3">
            <Banner kind="warn">
              {`The current ${replacing} secret stops working at once. Update every ${replacing === 'webhook' ? 'sender' : 'receiver'} that uses it.`}
            </Banner>
            <div className="flex flex-wrap gap-3">
              <Button variant="danger" disabled={busy} onClick={() => createSecret(replacing)}>
                Replace {replacing} secret now
              </Button>
              <Button variant="secondary" disabled={busy} onClick={() => setConfirm(null)}>
                Keep current secret
              </Button>
            </div>
          </div>
        )}
        {secret && (
          <div className="space-y-3">
            <Banner kind="warn" title={`New ${secret.kind} secret`}>
              Copy this secret now. Vis does not show it again.
            </Banner>
            <div className="flex items-start gap-2">
              <code className="min-w-0 flex-1 select-all break-all font-mono text-body text-white">
                {secret.secret}
              </code>
              <CopyChip value={secret.secret} label={`Copy ${secret.kind} secret`} />
            </div>
            <Button variant="secondary" onClick={() => setSecret(null)}>
              Hide secret
            </Button>
          </div>
        )}
        <div className="space-y-2">
          <p className="font-mono text-ui text-dialog-hint">
            {automation.enabled ? 'On' : 'Paused'} · {automation.id}
          </p>
          <h2 className="break-words font-mono text-title font-bold text-white">
            {automation.name}
          </h2>
        </div>
        <dl className="grid grid-cols-[auto_minmax(0,1fr)] gap-x-4 gap-y-2 font-mono text-ui">
          <Fact label="Triggers">
            {automation.triggers.map((trigger) => triggerLabel(trigger)).join(', ')}
          </Fact>
          <Fact label="Next run">
            {automation.enabled && automation.next_run_at
              ? timeLabel(automation.next_run_at)
              : 'Not scheduled'}
          </Fact>
          <Fact label="Target">{targetLabel(automation.target)}</Fact>
          <Fact label="Delivery">{deliveryLabel(automation.delivery)}</Fact>
          <Fact label="Model">{modelLabel(automation)}</Fact>
          {webhookUrl && (
            <Fact label="Webhook">
              <span className="flex items-start gap-2">
                <span className="min-w-0 flex-1 break-all">{webhookUrl}</span>
                <CopyChip value={webhookUrl} label="Copy webhook address" />
              </span>
            </Fact>
          )}
        </dl>
        {webhookUrl && (
          <p className="font-mono text-meta text-dialog-hint">
            The sender must reach this address. Sign each request with the webhook secret.
          </p>
        )}
        {secretKinds.length > 0 && (
          <div className="flex flex-wrap gap-3">
            {secretKinds.map((kind) => (
              <Button
                key={kind}
                variant="secondary"
                disabled={busy}
                onClick={() => (automation.secrets[kind] ? setConfirm(kind) : createSecret(kind))}
              >
                {automation.secrets[kind] ? 'Replace' : 'Create'} {kind} secret
              </Button>
            ))}
          </div>
        )}
        <section className="space-y-2">
          <h3 className="font-mono text-ui font-bold text-white">Prompt</h3>
          <pre className="whitespace-pre-wrap break-words font-mono text-body text-white">
            {automation.prompt}
          </pre>
        </section>
        <section className="space-y-2">
          <h3 className="font-mono text-ui font-bold text-white">Recent runs</h3>
          {runsError && <Banner kind="err">{runsError}</Banner>}
          {!runs && !runsError && (
            <p className="font-mono text-ui text-dialog-hint">Loading runs…</p>
          )}
          {runs?.length === 0 && <p className="font-mono text-ui text-dialog-hint">No runs yet.</p>}
          {runs && runs.length > 0 && (
            <ul className="border-t border-dialog-edge">
              {runs.map((item) => (
                <li
                  key={item.id}
                  className="flex items-start gap-2 border-b border-dialog-edge py-2 font-mono text-ui"
                >
                  <RunStatusIcon status={item.status} />
                  <span className="min-w-0 flex-1">
                    <span className="block text-white">
                      {[
                        wordLabel(item.status),
                        wordLabel(item.trigger),
                        timeLabel(runMillis(item)),
                        item.is_silent ? 'Silent' : null,
                      ]
                        .filter(Boolean)
                        .join(' · ')}
                    </span>
                    {runReason(item) && (
                      <span className="block break-words text-dialog-hint">{runReason(item)}</span>
                    )}
                  </span>
                </li>
              ))}
            </ul>
          )}
        </section>
      </div>
    </div>
  );
}
