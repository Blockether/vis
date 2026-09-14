import { useEffect, useMemo, useState, type FormEvent } from 'react';
import { GatewayClient, GatewayError } from '../lib/gateway';
import type { GatewayConn, ProjectOverview, RouterProvider } from '../lib/types';
import { homeifyPath } from '../lib/path';
import {
  IMPROVE_MODE_LABELS,
  improveParents,
  type ImproveCreate,
  type ImproveMode,
  type ImprovePage,
  type ImproveRecord,
  type ImproveSettings,
  type ImproveUpdate,
} from '../lib/improve';
import { Markdown } from '../components/ChatContent';
import {
  Banner,
  Button,
  DialogFrame,
  Disclosure,
  Input,
  ListRow,
  Modal,
  Select,
} from '../components/ui';
import { CircleCheckIcon, CircleDotIcon, ImproveIcon } from '../components/icons';

export type ImproveClient = Pick<
  GatewayClient,
  | 'improveSettings'
  | 'setImproveSettings'
  | 'improveProjects'
  | 'improveRecords'
  | 'improveRecord'
  | 'createImproveRecord'
  | 'updateImproveRecord'
  | 'reviewImprove'
  | 'router'
>;

const message = (error: unknown) =>
  error instanceof GatewayError && error.status === 409
    ? 'This issue changed elsewhere. Your draft is kept. Reload the latest version before saving.'
    : error instanceof Error
      ? error.message
      : 'Improve could not complete this request.';

export function ImproveDialog({
  gateways,
  initialUrl,
  onClose,
}: {
  gateways: GatewayConn[];
  initialUrl?: string;
  onClose: () => void;
}) {
  const [url, setUrl] = useState(initialUrl ?? gateways[0]?.url ?? '');
  const [dirty, setDirty] = useState(false);
  const [confirmExit, setConfirmExit] = useState(false);
  const requestClose = () => (dirty ? setConfirmExit(true) : onClose());
  const gateway = gateways.find((item) => item.url === url) ?? gateways[0];
  const client = useMemo(() => (gateway ? new GatewayClient(gateway) : null), [gateway]);
  return (
    <Modal onDismiss={requestClose}>
      <DialogFrame
        title="Improve"
        subtitle="Project issues and improvement groups"
        onClose={requestClose}
      >
        {gateways.length > 1 && (
          <label className="flex items-center gap-3 border-b border-dialog-edge px-3 py-2 font-mono text-ui">
            Machine
            <Select
              aria-label="Improve machine"
              value={gateway?.url ?? ''}
              onValueChange={setUrl}
              disabled={dirty}
              className="min-w-0 flex-1"
              options={gateways.map((item) => ({ value: item.url, label: item.label || item.url }))}
            />
          </label>
        )}
        {client ? (
          <ImproveWorkspace key={gateway!.url} client={client} onDirtyChange={setDirty} />
        ) : (
          <Banner kind="neutral">Pair a machine to use Improve.</Banner>
        )}
      </DialogFrame>
      {confirmExit && (
        <Modal size="fit" onDismiss={() => setConfirmExit(false)}>
          <DialogFrame title="Discard this draft?" onClose={() => setConfirmExit(false)}>
            <div className="space-y-4 p-3">
              <p className="font-mono text-body">
                Your issue has not been saved. Keep editing to preserve the draft.
              </p>
              <div className="flex gap-3">
                <Button onClick={() => setConfirmExit(false)}>Keep editing</Button>
                <Button variant="danger" onClick={onClose}>
                  Discard draft
                </Button>
              </div>
            </div>
          </DialogFrame>
        </Modal>
      )}
    </Modal>
  );
}

/** Production workflow; stories replace only the authenticated gateway boundary. */
export function ImproveWorkspace({
  client,
  onDirtyChange,
}: {
  client: ImproveClient;
  onDirtyChange?: (dirty: boolean) => void;
}) {
  const [settings, setSettings] = useState<ImproveSettings | null>(null);
  const [projects, setProjects] = useState<ProjectOverview[] | null>(null);
  const [project, setProject] = useState('');
  const [page, setPage] = useState<ImprovePage | null>(null);
  const [status, setStatus] = useState('open');
  const [selected, setSelected] = useState<ImproveRecord | null>(null);
  const [editing, setEditing] = useState(false);
  const [creating, setCreating] = useState(false);
  const [showSettings, setShowSettings] = useState(false);
  const [busy, setBusy] = useState(false);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [notice, setNotice] = useState<string | null>(null);
  const [revision, setRevision] = useState(0);

  useEffect(() => {
    onDirtyChange?.(editing || creating);
    return () => onDirtyChange?.(false);
  }, [editing, creating, onDirtyChange]);
  useEffect(() => {
    const controller = new AbortController();
    void Promise.all([
      client.improveSettings(controller.signal),
      client.improveProjects(controller.signal),
    ])
      .then(([nextSettings, overview]) => {
        if (controller.signal.aborted) return;
        setSettings(nextSettings);
        setProjects(overview.projects.filter((item) => item.project_id));
      })
      .catch((reason) => {
        if (!controller.signal.aborted) setError(message(reason));
      });
    return () => controller.abort();
  }, [client, revision]);

  useEffect(() => {
    const controller = new AbortController();
    setLoading(true);
    void client
      .improveRecords(project || null, 0, controller.signal)
      .then((next) => {
        if (controller.signal.aborted) return;
        setPage(next);
        setLoading(false);
      })
      .catch((reason) => {
        if (controller.signal.aborted) return;
        setError(message(reason));
        setLoading(false);
      });
    return () => controller.abort();
  }, [client, project, revision]);

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
  const save = async (values: Pick<ImproveCreate, 'title' | 'content' | 'parent_id'>) =>
    run(async () => {
      const record =
        selected && !creating
          ? await client.updateImproveRecord(selected.id, {
              ...values,
              expected_version: selected.version,
            })
          : await client.createImproveRecord({ ...values, project_id: project || null });
      setSelected(record);
      setEditing(false);
      setCreating(false);
      refresh();
      setNotice('Issue saved.');
    });
  const update = async (changes: Omit<ImproveUpdate, 'expected_version'>) =>
    run(async () => {
      if (!selected) return;
      setSelected(
        await client.updateImproveRecord(selected.id, {
          ...changes,
          expected_version: selected.version,
        }),
      );
      refresh();
    });
  const back = () => {
    setSelected(null);
    setEditing(false);
    setCreating(false);
    setError(null);
    setNotice(null);
  };
  const rows = (page?.records ?? []).filter(
    (record) => status === 'all' || record.status === status,
  );
  const disabled = busy || loading;

  return (
    <div className="flex min-h-0 flex-1 flex-col">
      <div className="flex shrink-0 items-center justify-between gap-2 border-b border-dialog-edge px-3 py-3">
        <span className="flex min-w-0 items-center gap-2 font-mono text-ui text-dialog-hint">
          <ImproveIcon />
          {settings ? IMPROVE_MODE_LABELS[settings.mode] : 'Loading settings…'}
        </span>
        <Button
          variant="secondary"
          onClick={() => setShowSettings(!showSettings)}
          aria-expanded={showSettings}
          disabled={busy || editing || creating}
        >
          {showSettings ? 'Back to issues' : 'Review settings'}
        </Button>
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
      {showSettings && settings ? (
        <ImproveSettingsForm
          key={`${settings.mode}:${settings.provider}:${settings.model}:${settings.interval_minutes}`}
          settings={settings}
          client={client}
          onSave={async (next) =>
            run(async () => {
              setSettings(await client.setImproveSettings(next));
              setNotice('Review settings saved.');
            })
          }
          busy={busy}
        />
      ) : settings?.mode === 'off' ? (
        <div className="space-y-3 p-3">
          <Banner kind="neutral">
            Improve is off. Existing reports are kept. Choose a review mode in Review settings to
            work with them.
          </Banner>
        </div>
      ) : creating || editing ? (
        <ImproveEditor
          key={creating ? 'new' : selected!.id}
          record={creating ? null : selected}
          records={page?.records ?? []}
          busy={busy}
          hasMore={page?.has_more ?? false}
          onMore={() =>
            void run(async () => {
              const next = await client.improveRecords(project || null, page?.after ?? 0);
              setPage({ ...next, records: [...(page?.records ?? []), ...next.records] });
            })
          }
          onSave={save}
          onCancel={() => {
            setEditing(false);
            setCreating(false);
          }}
          onReload={
            selected && !creating
              ? () =>
                  void run(async () => {
                    setSelected(await client.improveRecord(selected.id));
                    setEditing(false);
                  })
              : undefined
          }
        />
      ) : selected ? (
        <ImproveDetail
          record={selected}
          records={page?.records ?? []}
          busy={busy}
          onBack={back}
          onEdit={() => setEditing(true)}
          onUpdate={update}
          onOpen={(id) => void run(async () => setSelected(await client.improveRecord(id)))}
        />
      ) : (
        <>
          <div className="grid shrink-0 grid-cols-[minmax(0,1fr)_auto] items-end gap-3 border-b border-dialog-edge px-3 py-3">
            <label className="flex min-w-0 flex-col gap-2 font-mono text-ui text-dialog-hint">
              Project
              <Select
                aria-label="Improve project"
                value={project}
                disabled={busy}
                onValueChange={(value) => {
                  setProject(value);
                  setPage(null);
                  setError(null);
                }}
                options={[
                  { value: '', label: 'Unassigned' },
                  ...(projects ?? []).map((item) => ({
                    value: item.project_id!,
                    label: homeifyPath(item.root),
                  })),
                ]}
              />
            </label>
            <label className="flex flex-col gap-2 font-mono text-ui text-dialog-hint">
              Status
              <Select
                aria-label="Issue status"
                value={status}
                onValueChange={setStatus}
                options={[
                  { value: 'open', label: 'Open' },
                  { value: 'closed', label: 'Closed' },
                  { value: 'all', label: 'All' },
                ]}
              />
            </label>
          </div>
          <div className="flex shrink-0 flex-wrap items-center justify-between gap-3 border-b border-dialog-edge px-3 py-3">
            <span className="font-mono text-ui text-dialog-hint">
              {loading
                ? 'Loading issues…'
                : `${rows.length} loaded ${rows.length === 1 ? 'issue' : 'issues'}`}
            </span>
            <div className="flex items-center gap-2">
              <Button variant="secondary" disabled={busy} onClick={refresh}>
                Refresh
              </Button>
              <Button
                disabled={!settings || !projects || disabled}
                onClick={() => setCreating(true)}
              >
                New issue
              </Button>
            </div>
          </div>
          <div className="min-h-0 flex-1 overflow-y-auto" aria-busy={loading}>
            {!loading && page && rows.length === 0 && (
              <div className="space-y-2 p-4 font-mono text-body">
                <p className="font-bold">
                  No {status === 'all' ? '' : `${status} `}issues in this project
                </p>
                <p className="text-dialog-hint">
                  Collected reports appear here. You can also write an issue yourself.
                </p>
              </div>
            )}
            {rows.map((record) => (
              <div key={record.id} className="border-b border-dialog-edge">
                <ListRow
                  disabled={busy}
                  onClick={() => {
                    setSelected(record);
                    setError(null);
                    setNotice(null);
                  }}
                >
                  {record.status === 'closed' ? <CircleCheckIcon /> : <CircleDotIcon />}
                  <span className="min-w-0 flex-1">
                    <span className="block break-words font-mono text-body font-bold text-white">
                      {record.title}
                    </span>
                    <span className="block font-mono text-meta text-dialog-hint">
                      #{record.id} · {record.status}
                      {record.parent_id ? ` · Group #${record.parent_id}` : ''}
                    </span>
                  </span>
                </ListRow>
              </div>
            ))}
            {page?.has_more && (
              <div className="p-3">
                <Button
                  variant="secondary"
                  disabled={disabled}
                  onClick={() =>
                    void run(async () => {
                      const next = await client.improveRecords(project || null, page.after);
                      setPage({ ...next, records: [...page.records, ...next.records] });
                    })
                  }
                >
                  Load more issues
                </Button>
              </div>
            )}
            {!page && error && (
              <div className="p-3">
                <Button variant="secondary" disabled={busy} onClick={refresh}>
                  Retry
                </Button>
              </div>
            )}
          </div>
        </>
      )}
    </div>
  );
}

function ImproveEditor({
  record,
  records,
  busy,
  hasMore,
  onMore,
  onSave,
  onCancel,
  onReload,
}: {
  record: ImproveRecord | null;
  records: ImproveRecord[];
  busy: boolean;
  hasMore: boolean;
  onMore: () => void;
  onSave: (values: Pick<ImproveCreate, 'title' | 'content' | 'parent_id'>) => Promise<void>;
  onCancel: () => void;
  onReload?: () => void;
}) {
  const [title, setTitle] = useState(record?.title ?? '');
  const [content, setContent] = useState(record?.content ?? '');
  const [parent, setParent] = useState(record?.parent_id?.toString() ?? '');
  const parents = record
    ? improveParents(records, record)
    : records.filter((item) => item.status === 'open');
  return (
    <form
      className="flex min-h-0 flex-1 flex-col"
      onSubmit={(event) => {
        event.preventDefault();
        void onSave({ title: title.trim(), content, parent_id: parent ? Number(parent) : null });
      }}
    >
      <div className="min-h-0 flex-1 space-y-4 overflow-y-auto p-3">
        <h2 className="font-mono text-title font-bold">
          {record ? `Edit issue #${record.id}` : 'New issue'}
        </h2>
        <label className="flex flex-col gap-2 font-mono text-ui">
          Title
          <Input
            required
            maxLength={500}
            aria-label="Issue title"
            value={title}
            disabled={busy}
            onChange={(event) => setTitle(event.target.value)}
          />
        </label>
        <label className="flex flex-col gap-2 font-mono text-ui">
          Improvement group
          <Select
            aria-label="Improvement group"
            value={parent}
            disabled={busy}
            onValueChange={setParent}
            options={[
              { value: '', label: 'No group' },
              ...(parent && !parents.some((item) => String(item.id) === parent)
                ? [{ value: parent, label: `Current group #${parent}` }]
                : []),
              ...parents.map((item) => ({
                value: String(item.id),
                label: `#${item.id} · ${item.title}`,
              })),
            ]}
          />
        </label>
        {hasMore && (
          <Button type="button" variant="secondary" disabled={busy} onClick={onMore}>
            Load more group choices
          </Button>
        )}
        <label className="flex flex-col gap-2 font-mono text-ui">
          Content · Markdown
          <textarea
            aria-label="Issue content"
            value={content}
            onChange={(event) => setContent(event.target.value)}
            disabled={busy}
            rows={12}
            className="min-h-44 w-full resize-y rounded-none border border-edge bg-input p-3 font-mono text-body text-white focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted"
          />
        </label>
        <p className="font-mono text-ui text-dialog-hint">
          Describe reproduction, evidence and suggested changes. The original report is kept
          separately.
        </p>
      </div>
      <div className="flex shrink-0 flex-wrap items-center gap-3 border-t border-dialog-edge p-3">
        <Button type="submit" disabled={busy || !title.trim()}>
          {busy ? 'Saving…' : 'Save issue'}
        </Button>
        <Button type="button" variant="secondary" disabled={busy} onClick={onCancel}>
          Cancel
        </Button>
        {onReload && (
          <Button type="button" variant="quiet" disabled={busy} onClick={onReload}>
            Reload latest
          </Button>
        )}
      </div>
    </form>
  );
}

function ImproveDetail({
  record,
  records,
  busy,
  onBack,
  onEdit,
  onUpdate,
  onOpen,
}: {
  record: ImproveRecord;
  records: ImproveRecord[];
  busy: boolean;
  onBack: () => void;
  onEdit: () => void;
  onUpdate: (changes: Omit<ImproveUpdate, 'expected_version'>) => Promise<void>;
  onOpen: (id: number) => void;
}) {
  const [confirmClose, setConfirmClose] = useState(false);
  const [sourceOpen, setSourceOpen] = useState(false);
  const children = records.filter((item) => item.parent_id === record.id);
  return (
    <div className="flex min-h-0 flex-1 flex-col">
      <div className="flex shrink-0 flex-wrap items-center gap-3 border-b border-dialog-edge p-3">
        <Button variant="secondary" onClick={onBack} disabled={busy}>
          Back to issues
        </Button>
        <Button onClick={onEdit} disabled={busy}>
          Edit issue
        </Button>
        <Button
          variant="secondary"
          disabled={busy}
          onClick={() =>
            record.status === 'closed' ? void onUpdate({ status: 'open' }) : setConfirmClose(true)
          }
        >
          {record.status === 'closed' ? 'Reopen' : 'Close issue'}
        </Button>
      </div>
      <div className="min-h-0 flex-1 space-y-4 overflow-y-auto p-3">
        {confirmClose && (
          <div className="space-y-3">
            <Banner kind="warn">
              Closing this issue also closes every child in its group, including children not loaded
              here. Reports are kept.
            </Banner>
            <div className="flex flex-wrap gap-3">
              <Button
                variant="danger"
                disabled={busy}
                onClick={() =>
                  void onUpdate({ status: 'closed' }).then(() => setConfirmClose(false))
                }
              >
                Close issue and children
              </Button>
              <Button variant="secondary" disabled={busy} onClick={() => setConfirmClose(false)}>
                Keep open
              </Button>
            </div>
          </div>
        )}
        <div className="space-y-2">
          <p className="font-mono text-ui text-dialog-hint">
            #{record.id} · {record.status} · Version {record.version}
          </p>
          <h2 className="break-words font-mono text-title font-bold text-white">{record.title}</h2>
          {record.parent_id && (
            <Button variant="quiet" disabled={busy} onClick={() => onOpen(record.parent_id!)}>
              Open group #{record.parent_id}
            </Button>
          )}
          {record.status === 'closed' && (
            <p className="font-mono text-ui text-dialog-hint">
              Reopening also reopens this issue’s ancestor groups, not its children.
            </p>
          )}
        </div>
        {record.content ? (
          <Markdown>{record.content}</Markdown>
        ) : (
          <p className="font-mono text-body text-dialog-hint">
            No analysis yet. Edit this issue to add reproduction steps and suggested changes.
          </p>
        )}
        {children.length > 0 && (
          <section className="space-y-2 border-t border-dialog-edge pt-3">
            <h3 className="font-mono text-ui font-bold">Loaded children</h3>
            {children.map((child) => (
              <ListRow key={child.id} disabled={busy} onClick={() => onOpen(child.id)}>
                <span className="break-words font-mono text-body">
                  #{child.id} · {child.title} · {child.status}
                </span>
              </ListRow>
            ))}
          </section>
        )}
        {record.source_content !== null && (
          <section className="space-y-3 border-t border-dialog-edge pt-3">
            <Disclosure isOpen={sourceOpen} onClick={() => setSourceOpen(!sourceOpen)}>
              Original report · read only
            </Disclosure>
            {sourceOpen && (
              <>
                <Markdown>{record.source_content}</Markdown>
                {record.session_id && (
                  <p className="break-all font-mono text-meta text-dialog-hint">
                    Session {record.session_id} · Entry #{record.entry_id}
                  </p>
                )}
                {record.source_ref && (
                  <pre className="overflow-x-auto whitespace-pre-wrap break-all font-mono text-meta text-dialog-hint">
                    {JSON.stringify(record.source_ref, null, 2)}
                  </pre>
                )}
              </>
            )}
          </section>
        )}
      </div>
    </div>
  );
}

function ImproveSettingsForm({
  settings,
  client,
  busy,
  onSave,
}: {
  settings: ImproveSettings;
  client: ImproveClient;
  busy: boolean;
  onSave: (settings: Partial<ImproveSettings>) => Promise<void>;
}) {
  const [mode, setMode] = useState(settings.mode);
  const [provider, setProvider] = useState(settings.provider ?? '');
  const [model, setModel] = useState(settings.model ?? '');
  const [interval, setInterval] = useState(String(settings.interval_minutes));
  const [providers, setProviders] = useState<RouterProvider[]>([]);
  const [error, setError] = useState<string | null>(null);
  useEffect(() => {
    if (mode !== 'automatic') return;
    const controller = new AbortController();
    void client
      .router(controller.signal)
      .then((next) => {
        if (!controller.signal.aborted) {
          setProviders(next);
          setError(null);
        }
      })
      .catch((reason) => {
        if (!controller.signal.aborted) setError(message(reason));
      });
    return () => controller.abort();
  }, [client, mode]);
  const models = providers.find((item) => item.id === provider)?.models ?? [];
  const valid =
    mode !== 'automatic' ||
    (provider &&
      model &&
      Number.isInteger(Number(interval)) &&
      Number(interval) >= 1 &&
      Number(interval) <= 1440);
  const submit = (event: FormEvent) => {
    event.preventDefault();
    void onSave(
      mode === 'automatic'
        ? { mode, provider, model, interval_minutes: Number(interval) }
        : { mode },
    );
  };
  return (
    <form onSubmit={submit} className="min-h-0 flex-1 space-y-4 overflow-y-auto p-3">
      <h2 className="font-mono text-title font-bold">Review settings</h2>
      <label className="flex flex-col gap-2 font-mono text-ui">
        Review mode
        <Select
          aria-label="Review mode"
          value={mode}
          onValueChange={(value) => setMode(value as ImproveMode)}
          disabled={busy}
          options={Object.entries(IMPROVE_MODE_LABELS).map(([value, label]) => ({ value, label }))}
        />
      </label>
      <p className="font-mono text-body text-dialog-hint">
        {mode === 'automatic'
          ? 'The agent periodically reviews open issues, writes Markdown analysis and groups related reports within each project. Model calls may incur charges. It does not apply code changes or close issues.'
          : mode === 'human'
            ? 'Only people write analysis, reproduction notes and groups. No automatic review or model calls.'
            : 'Hide Improve and stop automatic review. Existing reports and their source evidence are kept.'}
      </p>
      {mode === 'automatic' && (
        <>
          {error && <Banner kind="err">{error}</Banner>}
          <label className="flex flex-col gap-2 font-mono text-ui">
            Provider
            <Select
              aria-label="Improve provider"
              value={provider}
              disabled={busy}
              onValueChange={(value) => {
                setProvider(value);
                setModel('');
              }}
              options={[
                { value: '', label: 'Choose a provider' },
                ...(provider && !providers.some((item) => item.id === provider)
                  ? [{ value: provider, label: `${provider} · unavailable` }]
                  : []),
                ...providers.map((item) => ({ value: item.id, label: item.label })),
              ]}
            />
          </label>
          <label className="flex flex-col gap-2 font-mono text-ui">
            Model
            <Select
              aria-label="Improve model"
              value={model}
              disabled={busy || !provider}
              onValueChange={setModel}
              options={[
                { value: '', label: 'Choose a model' },
                ...(model && !models.includes(model)
                  ? [{ value: model, label: `${model} · unavailable` }]
                  : []),
                ...models.map((item) => ({ value: item, label: item })),
              ]}
            />
          </label>
          <label className="flex flex-col gap-2 font-mono text-ui">
            Review interval · minutes
            <Input
              aria-label="Review interval in minutes"
              type="number"
              min={1}
              max={1440}
              step={1}
              required
              value={interval}
              disabled={busy}
              onChange={(event) => setInterval(event.target.value)}
            />
          </label>
          <Banner kind="neutral">
            Reproduction is not executed in this draft. The agent must mark it as not attempted and
            describe the safe steps needed to verify it. Only the selected provider and model are
            used; there is no fallback.
          </Banner>
        </>
      )}
      <Button type="submit" disabled={busy || !valid}>
        {busy ? 'Saving…' : 'Save review settings'}
      </Button>
    </form>
  );
}
