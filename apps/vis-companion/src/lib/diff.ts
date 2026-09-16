import schema from '../../../../packages/vis-contract/resources/vis-contract/schema/diff.json';
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
function optionalText(
  value: Record<string, unknown>,
  properties: Record<string, { type?: string; minLength?: number; enum?: string[] }>,
): boolean {
  return Object.entries(properties).every(
    ([key, definition]) =>
      definition.type !== 'string' ||
      value[key] === undefined ||
      (typeof value[key] === 'string' && value[key].length >= (definition.minLength ?? 0)),
  );
}
/** Validate the portable envelope before exposing review controls. Never normalize patch text. */
export function parseDiff(text: string): DiffEnvelope {
  const value: unknown = JSON.parse(text);
  if (
    !object(value) ||
    !keys(value, Object.keys(schema.$defs.envelope.properties)) ||
    value.schema_version !== schema.$defs.envelope.properties.schema_version.const ||
    typeof value.patch !== 'string' ||
    !object(value.source) ||
    !keys(value.source, Object.keys(schema.$defs.source.properties)) ||
    typeof value.source.type !== 'string' ||
    !schema.$defs.source.properties.type.enum.includes(value.source.type) ||
    (value.source.backend !== undefined &&
      (typeof value.source.backend !== 'string' ||
        !schema.$defs.source.properties.backend.enum.includes(value.source.backend))) ||
    !optionalText(value.source, schema.$defs.source.properties) ||
    !Array.isArray(value.comments) ||
    value.comments.some(
      (comment: unknown) =>
        !object(comment) ||
        !keys(comment, Object.keys(schema.$defs.comment.properties)) ||
        typeof comment.quote !== 'string' ||
        typeof comment.body !== 'string' ||
        comment.body.length < schema.$defs.comment.properties.body.minLength,
    )
  ) {
    throw new Error('Invalid diff attachment. Ask for a new snapshot.');
  }
  return value as unknown as DiffEnvelope;
}

export function diffReviewRequest(filename: string, version: number): string {
  if (!filename.trim() || !Number.isSafeInteger(version) || version < 1)
    throw new Error('Open a saved diff version before sending comments.');
  return `Read \`${filename}\` v${version} with read_attachment(${JSON.stringify(filename)}, version=${version}).\n${schema.$defs.envelope['x-vis-review-request']}`;
}

export function diffSourceLabel(source: DiffEnvelope['source']): string {
  return [source.type === 'draft' ? 'Draft' : 'Workspace', source.label, source.backend]
    .filter(Boolean)
    .join(' · ');
}
