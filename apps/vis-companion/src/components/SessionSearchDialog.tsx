import { useEffect, useRef, type ReactNode, type RefObject } from 'react';
import { SearchIcon } from './icons';
import { CloseButton, DialogFrame, Input, Modal } from './ui';

/**
 * THE SESSION SEARCH IS A DIALOG OF ITS OWN.
 *
 * It used to be a mode of the session list: the app bar turned into the field and
 * the list under it filtered in place, so every search took over the sidebar the
 * reader was working from. Reported: a search needs its own dialog. The list now
 * stays as it was, and the search stands over the app with its field, the machine
 * it asks, the sessions it found and the messages that matched.
 *
 * On a phone, the message preview stays hidden until the query contains text.
 * Clearing the query hides it again, so recent sessions use the full available height.
 * From 40rem of dialog width, sessions and messages stay side by side, even before you type.
 * Closing the dialog clears the query, so the next search starts from recent sessions.
 */
export function SessionSearchDialog({
  query,
  onQuery,
  onClose,
  scope = null,
  results,
  messages = null,
}: {
  /** What the field holds. */
  query: string;
  onQuery: (next: string) => void;
  /** Leave the search. The shell clears the query with it. */
  onClose: () => void;
  /** The band under the field: where the search looks and what came back. */
  scope?: ReactNode;
  /** The sessions the search found, or the state that stands in for them. */
  results: ReactNode;
  /** The messages of the picked session; `null` leaves the pane empty while no session is listed. */
  messages?: ReactNode;
}) {
  const inputRef = useRef<HTMLInputElement>(null);
  // The caret belongs to the dialog that just opened: a search a human still has to
  // tap into asks for the tap twice.
  useEffect(() => {
    inputRef.current?.focus();
  }, []);
  // Escape leaves, because a dialog that covers the screen has to be leavable without
  // aiming at a control. A key that something inside already handled stays its own.
  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape' || event.defaultPrevented) return;
      event.preventDefault();
      onClose();
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);
  return (
    <Modal onDismiss={onClose} size="split">
      <DialogFrame title="Search sessions" onClose={onClose} closeLabel="Close search">
        <div className="shrink-0 border-b border-dialog-edge px-3 py-3 sm:px-4">
          <SearchField inputRef={inputRef} value={query} onValue={onQuery} />
          {scope && <div className="mt-3">{scope}</div>}
        </div>
        <div className="@container/search flex min-h-0 flex-1 flex-col">
          <div className="flex min-h-0 flex-1 flex-col @min-[40rem]/search:flex-row">
            {/* Rows answer their width to the pane they stand in (`@container`), exactly as
                they do in the list. */}
            <div
              role="region"
              aria-label={query.trim() ? 'Matching sessions' : 'Recent sessions'}
              className="@container min-h-0 flex-1 touch-pan-y overflow-x-hidden overflow-y-auto overscroll-contain bg-page pb-3 @min-[40rem]/search:w-[clamp(20rem,45%,30rem)] @min-[40rem]/search:flex-none"
            >
              {results}
            </div>
            {/* On narrow dialogs, show messages below the sessions only while searching.
                Wide dialogs always show both panes side by side. */}
            <div
              className={`${query.trim() ? 'flex' : 'hidden'} h-[45%] min-h-0 min-w-0 shrink-0 flex-col border-t border-dialog-edge @min-[40rem]/search:flex @min-[40rem]/search:h-auto @min-[40rem]/search:flex-1 @min-[40rem]/search:border-t-0 @min-[40rem]/search:border-l`}
            >
              {messages}
            </div>
          </div>
        </div>
      </DialogFrame>
    </Modal>
  );
}

/** The dialog's field owns its value, its clear action and the focus that clearing returns. */
function SearchField({
  inputRef,
  value,
  onValue,
}: {
  inputRef: RefObject<HTMLInputElement | null>;
  value: string;
  onValue: (value: string) => void;
}) {
  return (
    <Input
      ref={inputRef}
      value={value}
      onChange={(event) => onValue(event.target.value)}
      type="search"
      enterKeyHint="search"
      autoCorrect="off"
      autoCapitalize="none"
      spellCheck={false}
      placeholder="Search titles and messages…"
      aria-label="Search session titles and messages"
      icon={<SearchIcon className="size-3" />}
      action={
        value ? (
          <CloseButton
            label="Clear search"
            onClick={() => {
              onValue('');
              inputRef.current?.focus();
            }}
          />
        ) : null
      }
    />
  );
}
