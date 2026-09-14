import contract from '../../../../packages/vis-contract/resources/vis-contract/diff.json';
import type { MarkdownComment } from './markdown-annotations';

export interface DiffEnvelope {
  schema_version: 1;
  patch: string;
  source: {
    type: 'draft' | 'workspace';
    backend?: 'worktree' | 'rift';
    label?: string;
    base_revision?: string;
    head_revision?: string;
  };
  comments: MarkdownComment[];
}

function object(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}
function keys(value: Record<string, unknown>, allowed: string[]): boolean {
  return Object.keys(value).every((key) => allowed.includes(key));
}
function optionalText(value: Record<string, unknown>, names: string[]): boolean {
  return names.every(
    (key) => value[key] === undefined || (typeof value[key] === 'string' && value[key].length > 0),
  );
}
/** Validate the portable envelope before exposing review controls. Never normalize patch text. */
export function parseDiff(text: string): DiffEnvelope {
  const value: unknown = JSON.parse(text);
  if (
    !object(value) ||
    !keys(value, ['schema_version', 'patch', 'source', 'comments']) ||
    value.schema_version !== 1 ||
    typeof value.patch !== 'string' ||
    !object(value.source) ||
    !keys(value.source, ['type', 'backend', 'label', 'base_revision', 'head_revision']) ||
    (value.source.type !== 'draft' && value.source.type !== 'workspace') ||
    (value.source.backend !== undefined &&
      value.source.backend !== 'worktree' &&
      value.source.backend !== 'rift') ||
    !optionalText(value.source, ['label', 'base_revision', 'head_revision']) ||
    !Array.isArray(value.comments) ||
    value.comments.some(
      (comment: unknown) =>
        !object(comment) ||
        !keys(comment, ['quote', 'body']) ||
        typeof comment.quote !== 'string' ||
        typeof comment.body !== 'string' ||
        comment.body.length === 0,
    )
  ) {
    throw new Error('Invalid diff attachment. Ask for a new snapshot.');
  }
  return value as unknown as DiffEnvelope;
}

export function diffReviewRequest(filename: string, version: number): string {
  if (!filename.trim() || !Number.isSafeInteger(version) || version < 1)
    throw new Error('Open a saved diff version before sending comments.');
  return `Read \`${filename}\` v${version} with read_attachment(${JSON.stringify(filename)}, version=${version}).\n${contract.revision_request}`;
}

export function diffSourceLabel(source: DiffEnvelope['source']): string {
  return [source.type === 'draft' ? 'Draft' : 'Workspace', source.label, source.backend]
    .filter(Boolean)
    .join(' · ');
}
