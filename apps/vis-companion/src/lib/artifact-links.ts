import { createContext } from 'react';

/** Same-session filenames in Markdown open the indexed artifact, never an app route. */
export const ArtifactLinkContext = createContext<{
  byName: ReadonlyMap<string, string>;
  open: (attachmentId: string) => void;
} | null>(null);
