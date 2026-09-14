/** Editable workflow fields; source evidence remains immutable on the gateway. */
export type ImproveMode = 'off' | 'human' | 'automatic';
export const IMPROVE_MODE_LABELS: Record<ImproveMode, string> = {
  off: 'Off',
  human: 'Governed by human',
  automatic: 'Automatic',
};

export interface ImproveSettings {
  mode: ImproveMode;
  provider: string | null;
  model: string | null;
  interval_minutes: number;
}

export interface ImproveRecord {
  id: number;
  entry_id: number | null;
  project_id: string | null;
  title: string;
  content: string;
  status: 'open' | 'closed';
  parent_id: number | null;
  version: number;
  created_at: number;
  updated_at: number;
  source_content: string | null;
  source_ref: Record<string, unknown> | null;
  session_id: string | null;
}

export interface ImproveCreate {
  project_id: string | null;
  title: string;
  content: string;
  parent_id?: number | null;
}

export interface ImproveUpdate {
  title?: string;
  content?: string;
  status?: ImproveRecord['status'];
  parent_id?: number | null;
  expected_version: number;
}

export interface ImprovePage {
  records: ImproveRecord[];
  after: number;
  has_more: boolean;
}

/** Loaded descendants only; the gateway always checks the complete hierarchy. */
export function improveDescendants(records: ImproveRecord[], id: number): Set<number> {
  const descendants = new Set<number>([id]);
  let grew = true;
  while (grew) {
    grew = false;
    for (const record of records) {
      if (
        record.parent_id !== null &&
        descendants.has(record.parent_id) &&
        !descendants.has(record.id)
      ) {
        descendants.add(record.id);
        grew = true;
      }
    }
  }
  descendants.delete(id);
  return descendants;
}

export function improveParents(records: ImproveRecord[], record: ImproveRecord): ImproveRecord[] {
  const descendants = improveDescendants(records, record.id);
  return records.filter(
    (candidate) =>
      candidate.project_id === record.project_id &&
      candidate.id !== record.id &&
      !descendants.has(candidate.id) &&
      (record.status === 'closed' || candidate.status === 'open'),
  );
}
