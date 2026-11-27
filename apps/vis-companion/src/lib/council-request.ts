import type { CouncilRequest } from './types';

/** Full Council text in arrival order, or the entry from an older gateway. */
export function councilRequestContent(council?: CouncilRequest): string | undefined {
  return council?.entries?.length ? council.entries.map(({ content }) => content).join('\n\n') : council?.content;
}
