import type { FileSuggestion, SlashCommand } from '../lib/types';
import { OptionRow } from './ui';

type FileSuggestions = {
  kind: 'files';
  items: readonly FileSuggestion[];
  selectedIndex: number;
  onSelect: (file: FileSuggestion) => void;
};

type SlashSuggestions = {
  kind: 'slashes';
  items: readonly SlashCommand[];
  selectedIndex: number;
  onSelect: (command: SlashCommand) => void;
};

export type ComposerSuggestionsProps = FileSuggestions | SlashSuggestions;

export function composerSuggestionListId(kind: ComposerSuggestionsProps['kind']): string {
  return kind === 'files' ? 'file-mention-list' : 'slash-command-list';
}

// The composer footer is this list's positioned ancestor, and that footer owns the
// dock's width: safe areas, 46rem at `sm`, the 6xl container on a pointer device.
// Stretching edge to edge inside it keeps the list exactly as wide as the input box.
// Re-deriving gutters here instead pinned it to the 46rem reading column while the
// composer widened, so on a wide screen the list stood visibly narrower than the input.
const FRAME =
  'absolute bottom-full left-0 right-0 mb-1.5 max-h-[min(20rem,55dvh)] overflow-y-auto rounded-none border border-dialog-edge bg-panel shadow-float transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none';

/** The one completion list used by both `@file` and `/command`. */
export function ComposerSuggestions(props: ComposerSuggestionsProps) {
  if (props.items.length === 0) return null;
  const selectedIndex = Math.min(props.selectedIndex, Math.max(0, props.items.length - 1));
  const files = props.kind === 'files';

  return (
    <div
      id={composerSuggestionListId(props.kind)}
      role="listbox"
      aria-label={files ? 'File mentions' : 'Slash commands'}
      className={FRAME}
    >
      <div className="bg-dialog-title px-3 py-2 font-mono text-meta font-bold text-dialog-title-foreground">
        {files ? 'Attach a file' : 'Slash commands'}
      </div>
      {files
        ? props.items.map((file, index) => (
            <OptionRow
              key={file.name}
              isActive={index === selectedIndex}
              className="grid-cols-[1fr_auto] items-center"
              onClick={() => props.onSelect(file)}
            >
              <code
                className={`truncate font-mono text-ui font-semibold ${
                  index === selectedIndex ? 'text-accent-foreground' : 'text-accent-ink'
                }`}
              >
                {file.name}
              </code>
              <span
                className={`shrink-0 font-mono text-chip ${
                  index === selectedIndex ? 'text-accent-foreground' : 'text-dialog-hint'
                }`}
              >
                {[file.size, file.age, file.status !== 'clean' ? file.status : '']
                  .filter(Boolean)
                  .join(' · ')}
              </span>
            </OptionRow>
          ))
        : props.items.map((command, index) => (
            <OptionRow
              key={command.name}
              isActive={index === selectedIndex}
              className="grid-cols-[7.5rem_1fr] items-start sm:grid-cols-[10rem_1fr]"
              onClick={() => props.onSelect(command)}
            >
              <code
                className={`break-words font-mono text-ui font-semibold ${
                  index === selectedIndex ? 'text-accent-foreground' : 'text-accent-ink'
                }`}
              >
                {command.name}
              </code>
              <span
                className={`line-clamp-2 text-meta ${
                  index === selectedIndex ? 'text-accent-foreground' : 'text-dialog-hint'
                }`}
              >
                {command.doc}
              </span>
            </OptionRow>
          ))}
    </div>
  );
}
