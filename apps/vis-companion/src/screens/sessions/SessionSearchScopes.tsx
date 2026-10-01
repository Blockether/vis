import { useEffect, useMemo, useState } from 'react';
import { TextButton } from '../../components/ui';
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
  const groups = projects.filter((entry) => filters.project === ALL_PROJECTS ||
    (filters.project === NO_PROJECT ? entry.project.id === NO_PROJECT : entry === chosen));
  const options = groups.flatMap((entry) => entry.groups.map((group) => ({
    key: keyFor(machineKey(entry.conn), group.id),
    label: `${entry.project.name} / ${group.name}${machineScope === ALL_SCOPE ? ` / ${machineLabel(entry.conn)}` : ''}`,
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
    toggleGroup: (group: string) => setStored({ ...filters, groups: filters.groups.includes(group)
      ? filters.groups.filter((id) => id !== group) : [...filters.groups, group] }),
    allGroups: () => setStored({ ...filters, groups: [] }),
    everything: () => setStored({ machineScope, project: ALL_PROJECTS, groups: [] }),
  };
}

export type SearchScope = ReturnType<typeof useSessionSearchScope>;

export function SessionSearchScopes({ scope, onEverything }: {
  scope: SearchScope;
  onEverything: () => void;
}) {
  const [groupsOpen, setGroupsOpen] = useState(false);
  return (
    <div className="flex flex-wrap items-center gap-3 border-t border-edge px-3 py-2 font-mono text-meta text-dialog-hint">
      <label className="flex min-w-0 items-center gap-2">
        Project
        <select
          aria-label="Search project"
          value={scope.filters.project}
          onChange={(event) => scope.setProject(event.target.value)}
          className="min-w-0 max-w-64 bg-panel px-2 py-1 text-white focus-visible:outline focus-visible:outline-current"
        >
          <option value={ALL_PROJECTS}>All projects</option>
          <option value={NO_PROJECT}>No project</option>
          {scope.projects.map(({ conn, project }) => (
            <option
              key={keyFor(machineKey(conn), project.id)}
              value={keyFor(machineKey(conn), project.id)}
            >
              {project.name}
              {scope.filters.machineScope === ALL_SCOPE ? ` / ${machineLabel(conn)}` : ''}
            </option>
          ))}
        </select>
      </label>
      <TextButton
        aria-label="Search groups"
        aria-expanded={groupsOpen}
        onClick={() => setGroupsOpen(!groupsOpen)}
      >
        Groups: {scope.filters.groups.length ? `${scope.filters.groups.length} selected` : 'All groups'}
      </TextButton>
      <TextButton onClick={() => {
        scope.everything();
        setGroupsOpen(false);
        onEverything();
      }}>
        Search everything
      </TextButton>
      {groupsOpen && (
        <fieldset
          className="flex max-h-40 w-full flex-wrap gap-x-4 gap-y-2 overflow-y-auto"
          aria-label="Search groups"
        >
          <legend className="sr-only">Search any selected group</legend>
          <TextButton onClick={scope.allGroups}>All groups</TextButton>
          {scope.options.map((group) => (
            <label key={group.key} className="flex items-center gap-2">
              <input
                type="checkbox"
                checked={scope.filters.groups.includes(group.key)}
                onChange={() => scope.toggleGroup(group.key)}
              />
              {group.label}
            </label>
          ))}
          {scope.options.length === 0 && <span>No groups in this scope</span>}
        </fieldset>
      )}
      {scope.failure && (
        <p role="status" className="w-full">{scope.failure}</p>
      )}
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
