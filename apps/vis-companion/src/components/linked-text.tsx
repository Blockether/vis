import type { ReactNode } from 'react';

import { filePathOfUrl, messageLinkParts } from '../lib/message-links';

/** The look of a link in running text. */
export const TEXT_LINK = 'font-medium text-link underline underline-offset-3 break-all hover:text-link-hover';

export type OpenPath = (path: string, line?: number) => void;

/**
 * A `file:///` URL that the reader can press. It names a file on the gateway machine, so
 * the press shows that file in the preview of the app, not in a browser (#449). It is an
 * inline span, so `JustifiedProse` measures it as text. Without an opener it stays text.
 */
export function fileUrlLink(path: string, label: ReactNode, openPath: OpenPath | null, key?: number) {
  if (!openPath) return <span key={key}>{label}</span>;
  const press = (event: { preventDefault: () => void; stopPropagation: () => void }) => {
    event.preventDefault();
    event.stopPropagation();
    openPath(path);
  };
  return (
    <span
      key={key}
      role="button"
      tabIndex={0}
      title={path}
      className={`${TEXT_LINK} cursor-pointer`}
      onPointerDown={(event) => event.stopPropagation()}
      onPointerUp={(event) => event.stopPropagation()}
      onClick={press}
      onKeyDown={(event) => {
        if (event.key === 'Enter' || event.key === ' ') press(event);
      }}
    >
      {label}
    </span>
  );
}

/**
 * Plain text with each bare URL as a link that opens outside the app (#446). The parts are
 * plain `a` elements, so `JustifiedProse` still measures the line. A `file:///` URL opens
 * the file preview of the app instead (#449).
 */
export function linkedText(text: string, openPath: OpenPath | null): ReactNode[] {
  return messageLinkParts(text).map((part, index) => {
    if (typeof part === 'string') return part;
    // A file URL is never a browser link, also when its encoding does not decode.
    if (/^file:/i.test(part.url)) {
      const filePath = filePathOfUrl(part.url);
      return filePath ? fileUrlLink(filePath, part.url, openPath, index) : part.url;
    }
    return (
      <a key={index} href={part.url} className={TEXT_LINK} target="_blank" rel="noreferrer">
        {part.url}
      </a>
    );
  });
}
