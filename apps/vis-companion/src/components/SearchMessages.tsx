import { Fragment } from 'react';
import type { Element, Root, Text } from 'hast';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';

import { timeLabel } from '../lib/fleet';
import type { SessionMatch, SessionMatchHit } from '../lib/gateway';
import { searchRanges, searchSegments, searchTerms } from '../lib/search-highlight';
import { Button } from './ui';

const MARK = 'bg-accent/20 px-0.5 font-bold text-white';

/**
 * The messages a session search found in ONE session, beside the list of sessions it
 * found them in.
 *
 * The list answers "which session"; this pane answers "where in it". It shows the
 * session the reader picked in the list, and every message the gateway returned for
 * it, freshest first: who wrote it, when, and the words around the match, with the
 * query's words marked. It is the same view the terminal's session switcher shows
 * to the right of its list.
 *
 * Opening stays one step away: the Open button, or any message, opens the session.
 */
export function SearchMessages({
  title,
  match,
  query,
  isSearching,
  onOpen,
  className = '',
}: {
  /** The picked session's name, as its row shows it. */
  title: string;
  /** What the gateway found in that session; null until it answers, or when only the title matched. */
  match: SessionMatch | null;
  /** The query the match answers. */
  query: string;
  /** The answer for this query is still on its way. */
  isSearching: boolean;
  onOpen: () => void;
  className?: string;
}) {
  const terms = searchTerms(query);
  const rows = match ? matchRows(match) : [];
  const note =
    isSearching && !match
      ? 'Searching messages...'
      : match?.inTitle || searchRanges(title, terms).length > 0
        ? 'The title matches. No message matches.'
        : 'No message matches.';
  return (
    <section
      aria-label="Matching messages"
      className={`flex min-h-0 min-w-0 flex-col bg-page ${className}`}
    >
      <div className="flex min-h-12 shrink-0 items-center gap-3 border-b border-dialog-edge py-1.5 pl-4 pr-2 mouse:min-h-9 mouse:py-1">
        <h2 className="min-w-0 flex-1 truncate font-mono text-body font-bold text-dialog-foreground">
          <Highlighted text={title} terms={terms} />
        </h2>
        <Button variant="secondary" onClick={onOpen}>
          Open
        </Button>
      </div>
      <div className="min-h-0 flex-1 overflow-y-auto overscroll-contain pb-[env(safe-area-inset-bottom)]">
        {rows.length > 0 ? (
          <ol className="divide-y divide-edge">
            {rows.map((hit, index) => (
              <li key={`${hit.side}-${hit.at ?? index}`}>
                <button
                  type="button"
                  onClick={onOpen}
                  className="block w-full py-2 pl-4 pr-3 text-left active:bg-hover focus-visible:bg-hover focus-visible:outline-none"
                >
                  <span className="flex items-baseline gap-2 font-mono text-meta">
                    <span
                      className={`font-bold ${hit.side === 'request' ? 'text-you-role' : 'text-vis-role'}`}
                    >
                      {hit.side === 'request' ? 'You' : 'Vis'}
                    </span>
                    {hit.side === 'thinking' && <span className="text-dialog-hint">thinking</span>}
                    {hit.at !== null && (
                      <span className="ml-auto whitespace-nowrap text-dialog-hint tabular-nums">
                        {timeLabel(new Date(hit.at).toISOString())}
                      </span>
                    )}
                  </span>
                  <span className="mt-1 block whitespace-pre-wrap break-words font-mono text-ui text-dialog-foreground">
                    <ReactMarkdown
                      remarkPlugins={[remarkGfm]}
                      rehypePlugins={[[highlightMarkdown, terms]]}
                      skipHtml
                      allowedElements={['p', 'strong', 'em', 'del', 'code', 'br', 'mark']}
                      unwrapDisallowed
                      components={{
                        p: ({ children }) => <span className="block">{children}</span>,
                        strong: ({ children }) => <strong className="font-bold">{children}</strong>,
                        code: ({ children }) => (
                          <code className="bg-panel-2 px-0.5 font-mono">{children}</code>
                        ),
                        mark: ({ children }) => <mark className={MARK}>{children}</mark>,
                      }}
                    >
                      {hit.snippet}
                    </ReactMarkdown>
                  </span>
                </button>
              </li>
            ))}
          </ol>
        ) : (
          <p className="px-4 py-3 font-mono text-meta text-dialog-hint">{note}</p>
        )}
      </div>
    </section>
  );
}

/** The gateway's hits, or the two first snippets an older gateway sends instead. */
function matchRows(match: SessionMatch): SessionMatchHit[] {
  if (match.hits.length > 0) return match.hits;
  const fallback: SessionMatchHit[] = [
    { side: 'request', snippet: match.requestSnippet?.trim() ?? '', at: null },
    { side: 'reply', snippet: match.replySnippet?.trim() ?? '', at: null },
  ];
  return fallback.filter((hit) => hit.snippet.length > 0);
}

function Highlighted({ text, terms }: { text: string; terms: readonly string[] }) {
  return searchSegments(text, terms).map((segment, index) =>
    segment.isMatch ? (
      <mark key={index} className={MARK}>
        {segment.text}
      </mark>
    ) : (
      <Fragment key={index}>{segment.text}</Fragment>
    ),
  );
}

// Mark words in the parsed text, never in Markdown syntax or link targets. An image
// becomes its label first, so a search result cannot fetch remote content.
function highlightMarkdown(terms: readonly string[]) {
  return (tree: Root) => {
    function visit(node: Root | Element) {
      // Work backwards so inserted marks are not visited or marked again.
      for (let index = node.children.length - 1; index >= 0; index -= 1) {
        let child = node.children[index];
        if (child.type === 'element' && child.tagName === 'img') {
          child = { type: 'text', value: String(child.properties.alt ?? '') };
          node.children[index] = child;
        }
        if (child.type === 'element') {
          visit(child);
        } else if (child.type === 'text') {
          const segments = searchSegments(child.value, terms);
          if (!segments.some((segment) => segment.isMatch)) continue;
          node.children.splice(
            index,
            1,
            ...segments.map<Element | Text>((segment) =>
              segment.isMatch
                ? {
                    type: 'element',
                    tagName: 'mark',
                    properties: {},
                    children: [{ type: 'text', value: segment.text }],
                  }
                : { type: 'text', value: segment.text },
            ),
          );
        }
      }
    }
    visit(tree);
  };
}
