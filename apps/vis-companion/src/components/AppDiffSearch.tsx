import { useEffect, useMemo, useRef, useState, type RefObject } from 'react';

import {
  anchorKey,
  findDiffMatches,
  type AppDiffFile,
  type AppDiffLine,
  type DiffSearchOptions,
} from '../lib/app-diff';

/** One state owner for find-in-diff, occurrence navigation, and focus return. */
export function useDiffSearch(
  files: readonly AppDiffFile[],
  container: RefObject<HTMLDivElement | null>,
  revealMatch: (line: AppDiffLine | undefined) => void,
) {
  const [findOpen, setFindOpen] = useState(false);
  const [query, setQuery] = useState('');
  const [matchIndex, setMatchIndex] = useState(0);
  const [searchOptions, setSearchOptions] = useState<DiffSearchOptions>({});
  const matches = useMemo(() => findDiffMatches(files, query, searchOptions), [files, query, searchOptions]);
  const currentResult = matches.length ? matches[matchIndex % matches.length] : undefined;
  const currentMatch = currentResult?.line;
  const searchAnchor = currentMatch ? anchorKey(currentMatch.anchor) : undefined;
  useEffect(() => {
    if (searchAnchor)
      container.current
        ?.querySelector('[data-search-current="true"]')
        ?.scrollIntoView({ block: 'center', inline: 'nearest' });
  }, [searchAnchor, currentResult?.start]);
  const moveMatch = (delta: number) => {
    if (!matches.length) return;
    const next = (matchIndex + delta + matches.length) % matches.length;
    setMatchIndex(next);
    revealMatch(matches[next]?.line);
  };
  const toggleSearchOption = (key: keyof DiffSearchOptions) => {
    const next = { ...searchOptions, [key]: !searchOptions[key] };
    setSearchOptions(next);
    setMatchIndex(0);
    revealMatch(findDiffMatches(files, query, next)[0]?.line);
  };
  const closeFind = () => {
    setFindOpen(false);
    setQuery('');
    container.current?.focus();
  };
  const input = useRef<HTMLInputElement>(null);
  const show = () => {
    setFindOpen(true);
    input.current?.focus();
  };
  const onQuery = (value: string) => {
    setQuery(value);
    setMatchIndex(0);
    revealMatch(findDiffMatches(files, value, searchOptions)[0]?.line);
  };
  return {
    findOpen,
    query,
    matchIndex,
    matches,
    searchOptions,
    currentResult,
    searchAnchor,
    show,
    closeFind,
    moveMatch,
    toggleSearchOption,
    onQuery,
    input,
  };
}

/** The search controls stay independent from code layout and comment state. */
export function DiffSearch({ search }: { search: ReturnType<typeof useDiffSearch> }) {
  const {
    findOpen,
    query,
    matchIndex,
    matches,
    searchOptions,
    closeFind,
    moveMatch,
    toggleSearchOption,
    onQuery,
    input,
  } = search;
  if (!findOpen) return null;
  return (
    <form
      className="diff-search"
      role="search"
      onSubmit={(event) => {
        event.preventDefault();
        moveMatch(1);
      }}
      onKeyDown={(event) => {
        if (event.key === 'Escape') {
          event.preventDefault();
          event.stopPropagation();
          closeFind();
        }
        if (event.key === 'Enter' && event.shiftKey) {
          event.preventDefault();
          moveMatch(-1);
        }
      }}
    >
      <input
        autoFocus
        ref={input}
        aria-label="Find in diff"
        placeholder="Find in diff"
        value={query}
        onChange={(event) => onQuery(event.target.value)}
      />
      <button
        type="button"
        className="app-focus-ring"
        aria-label="Match case"
        title="Match case"
        aria-pressed={Boolean(searchOptions.caseSensitive)}
        onClick={() => toggleSearchOption('caseSensitive')}
      >
        Aa
      </button>
      <button
        type="button"
        className="app-focus-ring"
        aria-label="Whole word"
        title="Whole word"
        aria-pressed={Boolean(searchOptions.wholeWord)}
        onClick={() => toggleSearchOption('wholeWord')}
      >
        Ab|
      </button>
      <button
        type="button"
        className="app-focus-ring"
        aria-label="Regular expression"
        title="Regular expression"
        aria-pressed={Boolean(searchOptions.regex)}
        onClick={() => toggleSearchOption('regex')}
      >
        .*
      </button>
      <output aria-live="polite">
        {matches.length ? `${(matchIndex % matches.length) + 1} of ${matches.length}` : 'No results'}
      </output>
      <button
        type="button"
        className="app-focus-ring"
        aria-label="Previous match"
        disabled={!matches.length}
        onClick={() => moveMatch(-1)}
      >
        Previous
      </button>
      <button type="submit" className="app-focus-ring" aria-label="Next match" disabled={!matches.length}>
        Next
      </button>
      <button type="button" className="app-focus-ring" onClick={closeFind}>
        Close
      </button>
    </form>
  );
}
