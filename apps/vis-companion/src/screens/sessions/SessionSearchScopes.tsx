import { useEffect, useId, useMemo, useState, type ReactNode } from 'react';
import { Select } from '../../components/ui';
import { SessionRow } from '../../components/SessionList';
import { EMPTY_DRAFT_MESSAGE, draftMessageKey } from '../../lib/draft-messages';
import { machineKey, machineLabel, projectLabel, sessionRowKey } from '../../lib/fleet';
import type { GatewayClient } from '../../lib/gateway';
import type { GatewayConn, Project, Session, SessionGroup } from '../../lib/types';
import type { SessionRowsContext } from './SessionProjectGroups';

const ALL_SCOPE = null;
const ALL_PROJECTS = 'all';
const NO_PROJECT = 'none';
const keyFor = (machine: string, id: string) => `${machine}\u0000${id}`;
type CatalogProject = { conn: GatewayConn; project: Project; groups: SessionGroup[] };
type Filters = { machineScope: string | null; project: string; groups: string[] };

/** Search scopes use durable catalogs, not the session page currently on the glass. */
export function useSessionSearchScope(
  conns: GatewayConn[],
  isOpen: boolean,
  machineScope: string | null,
  getClient: (conn: GatewayConn) => GatewayClient,
) {
  const [catalog, setCatalog] = useState<CatalogProject[]>([]);
  const [failure, setFailure] = useState<string | null>(null);
  const [stored, setStored] = useState<Filters>({ machineScope, project: ALL_PROJECTS, groups: [] });
  const filters = useMemo(() => stored.machineScope === machineScope
    ? stored : { machineScope, project: ALL_PROJECTS, groups: [] }, [stored, machineScope]);
  useEffect(() => {
    if (!isOpen) return;
    const controller = new AbortController();
    void Promise.allSettled(conns.map(async (conn) => {
      const api = getClient(conn);
      const projects = await api.listProjects(controller.signal);
      const entries = [...projects, { id: NO_PROJECT, name: 'No project', workspace_root: '' }];
      return Promise.all(entries.map(async (project) => {
        const page = await api.listSessionGroups(String(project.workspace_root ?? ''), controller.signal, 'include');
        return { conn, project, groups: (page.groups ?? []).filter((group) =>
          project.id === NO_PROJECT ? !group.project_id : group.project_id === project.id) };
      }));
    })).then((answers) => {
      if (controller.signal.aborted) return;
      setCatalog(answers.flatMap((answer) => answer.status === 'fulfilled' ? answer.value : []));
      setFailure(answers.some((answer) => answer.status === 'rejected')
        ? 'Some project and group choices could not be read. Reopen search to retry.' : null);
    });
    return () => controller.abort();
  }, [isOpen, conns, getClient]);
  const projects = catalog.filter((entry) => machineScope === ALL_SCOPE || machineKey(entry.conn) === machineScope);
  const chosen = projects.find((entry) => keyFor(machineKey(entry.conn), entry.project.id) === filters.project);
  // All projects already means all groups: only a chosen project, or the unfiled
  // sessions under No project, has groups to narrow to.
  const groups = filters.project === ALL_PROJECTS ? [] : projects.filter((entry) =>
    filters.project === NO_PROJECT ? entry.project.id === NO_PROJECT : entry === chosen);
  // The project picker already names the project, so a group needs only its own name.
  const options = groups.flatMap((entry) => entry.groups.map((group) => ({
    value: keyFor(machineKey(entry.conn), group.id),
    label: `${group.name}${machineScope === ALL_SCOPE ? ` / ${machineLabel(entry.conn)}` : ''}`,
  })));
  const wire = useMemo(() => {
    const selected = filters.project.split('\u0000');
    const machine = selected.length === 2 ? selected[0] : machineScope;
    const accepts = (key: string) => (machine === ALL_SCOPE || key === machine) &&
      (filters.groups.length === 0 || filters.groups.some((group) => group.startsWith(`${key}\u0000`)));
    const request = (key: string) => ({
      ...(selected.length === 2 ? { projectId: selected[1] } : {}),
      ...(filters.project === NO_PROJECT ? { root: '' } : {}),
      ...(filters.groups.length > 0 ? { groupIds: filters.groups.filter((group) =>
        group.startsWith(`${key}\u0000`)).map((group) => group.slice(key.length + 1)) } : {}),
    });
    const includes = (conn: GatewayConn, session: Session) => {
      const key = machineKey(conn);
      const scope = request(key);
      return accepts(key) && (!scope.projectId || scope.projectId === session.project_id) &&
        (scope.root !== '' || !session.project_id) &&
        (!scope.groupIds || scope.groupIds.includes(session.group_id ?? ''));
    };
    return { key: JSON.stringify([machineScope, filters.project, filters.groups]), machine, accepts, request, includes };
  }, [filters, machineScope]);
  const location = (conn: GatewayConn, session: Session) => {
    const entries = catalog.filter((entry) => machineKey(entry.conn) === machineKey(conn));
    const group = entries.flatMap((entry) => entry.groups).find((entry) => entry.id === session.group_id) ?? null;
    const project = entries.find((entry) => entry.project.id === session.project_id)?.project;
    return {
      project: project?.name || session.project_name || (session.project_id ? session.project_id : projectLabel([session])),
      group: group?.name || (session.group_id ? session.group_id : 'No group'),
      groupRecord: group,
    };
  };
  return {
    wire, location,
    projects: projects.filter((entry) => entry.project.id !== NO_PROJECT), options, filters, failure,
    setProject: (project: string) => setStored({ machineScope, project, groups: [] }),
    setGroups: (groups: string[]) => setStored({ ...filters, groups }),
  };
}

export type SearchScope = ReturnType<typeof useSessionSearchScope>;

type Choice = { value: string; label: string; disabled?: boolean };

/**
 * WHERE A SEARCH LOOKS, as one row of labelled choices: the machine (when there is more
 * than one), the project and, once a project with groups narrows the search, its groups.
 * Each is the app's own picker, never the system one: the project used to be a bare
 * system select and the groups a text button that unfolded checkboxes, so on a phone the
 * band read as loose words, not as controls. All projects already means all groups, so
 * choosing it widens the search again; no separate control repeats it. The line under
 * the choices says what came back.
 */
export function SessionSearchScopes({ scope, machine = null, report = null }: {
  scope: SearchScope;
  /** The machine the search asks; `null` when there is no other machine to choose. */
  machine?: { value: string; options: readonly Choice[]; onChange: (key: string) => void } | null;
  /** What the search came back with. */
  report?: ReactNode;
}) {
  const id = useId();
  const projects = [
    { value: ALL_PROJECTS, label: 'All projects' },
    { value: NO_PROJECT, label: 'No project' },
    ...scope.projects.map(({ conn, project }) => ({
      value: keyFor(machineKey(conn), project.id),
      label: `${project.name}${scope.filters.machineScope === ALL_SCOPE ? ` / ${machineLabel(conn)}` : ''}`,
    })),
  ];
  return (
    <div className="flex flex-col gap-2">
      <div className="grid grid-cols-[repeat(auto-fit,minmax(min(100%,10rem),1fr))] gap-x-3 gap-y-2">
        {machine && (
          <ScopeChoice id={`${id}machine`} label="Machine">
            <Select
              aria-labelledby={`${id}machine`}
              value={machine.value}
              onValueChange={machine.onChange}
              options={machine.options}
              className="w-full"
            />
          </ScopeChoice>
        )}
        <ScopeChoice id={`${id}project`} label="Project">
          <Select
            aria-labelledby={`${id}project`}
            value={scope.filters.project}
            onValueChange={scope.setProject}
            options={projects}
            className="w-full"
          />
        </ScopeChoice>
        {(scope.options.length > 0 || scope.filters.groups.length > 0) && (
          <ScopeChoice id={`${id}groups`} label="Groups">
            <Select
              aria-labelledby={`${id}groups`}
              values={scope.filters.groups}
              onValuesChange={scope.setGroups}
              noneLabel="All groups"
              options={scope.options}
              className="w-full"
            />
          </ScopeChoice>
        )}
      </div>
      {(report || scope.failure) && (
        <div className="flex min-h-6 min-w-0 flex-wrap items-center gap-x-3 gap-y-1">
          {report}
          {scope.failure && (
            <p role="status" className="font-mono text-meta text-dialog-hint">{scope.failure}</p>
          )}
        </div>
      )}
    </div>
  );
}

/** One labelled choice: the caption names the picker under it. */
function ScopeChoice({ id, label, children }: { id: string; label: string; children: ReactNode }) {
  return (
    <div className="flex min-w-0 flex-col gap-1">
      <span id={id} className="font-mono text-meta text-dialog-hint">{label}</span>
      {children}
    </div>
  );
}

/** Search answers are flat, in gateway order, with literal location names instead of palette marks. */
export function SearchSessionRows({ conn, sessions, context, scope }: {
  conn: GatewayConn; sessions: Session[]; context: SessionRowsContext; scope: SearchScope;
}) {
  return sessions.map((session) => {
    const at = scope.location(conn, session);
    const rowKey = sessionRowKey(conn, session.id);
    const deletion = context.actions.deletion;
    return (
      <SessionRow key={rowKey} conn={conn} session={session} group={at.groupRecord}
        location={{ project: at.project, group: at.group }}
        draft={context.drafts[draftMessageKey(conn.url, session.id)] ?? EMPTY_DRAFT_MESSAGE}
        needle={context.needle} commands={context.actions.commands}
        deletion={deletion.target?.session.id === session.id && machineKey(deletion.target.conn) === machineKey(conn) ? deletion : null}
        isOpen={context.openRow === rowKey} isPreviewed={context.previewId === session.id}
        seenAnswers={context.readFloors?.get(rowKey)}
        onSelectionClick={context.preview ? (id) => {
          if (context.previewId === id) return false;
          context.preview?.(id);
          return true;
        } : undefined}
      />
    );
  });
}
