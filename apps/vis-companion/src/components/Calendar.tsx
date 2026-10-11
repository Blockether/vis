/**
 * The app's own calendar: a month grid, a time field and the field that opens both.
 *
 * A native date picker draws another control on each platform, in the platform's type,
 * line height and palette. These parts keep the app's face: the mono type, square
 * corners and rows that keep their height, so the panel never jumps between months.
 *
 * A day is a `YYYY-MM-DD` string in the calendar of this device. A moment is the
 * `YYYY-MM-DDTHH:mm` value that a `datetime-local` input gives. Day arithmetic runs in
 * UTC, so a daylight-saving change never moves a day.
 */
import { useId, useLayoutEffect, useRef, useState, type KeyboardEvent, type RefObject } from 'react';
import { createPortal } from 'react-dom';

import { useBackLayer } from '../lib/edge-back';
import { CalendarIcon, ChevronIcon, DotIcon } from './icons';
import { Button, useAnchoredLayer } from './ui';

const DAY_MS = 86_400_000;

/** Six weeks: every month fits, so the grid has the same height in each month. */
const GRID_DAYS = 42;

/** The tallest that the open panel grows before it scrolls inside itself, in CSS pixels. */
const PANEL_HEIGHT = 480;

const pad = (value: number) => String(value).padStart(2, '0');

/** A `YYYY-MM-DD` day as epoch ms at UTC midnight, or NaN. */
function dayMillis(day: string): number {
  const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(day);
  return match ? Date.UTC(+match[1], +match[2] - 1, +match[3]) : Number.NaN;
}

/** The UTC day of epoch ms. */
function utcDay(millis: number): string {
  const date = new Date(millis);
  return `${date.getUTCFullYear()}-${pad(date.getUTCMonth() + 1)}-${pad(date.getUTCDate())}`;
}

/** The day of epoch ms on the clock of this device. */
export function localDay(millis: number): string {
  const date = new Date(millis);
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}`;
}

const addDays = (day: string, count: number) => utcDay(dayMillis(day) + count * DAY_MS);

/** The same day `count` months away, or the last day of a shorter month. */
function addMonths(day: string, count: number): string {
  const date = new Date(dayMillis(day));
  const year = date.getUTCFullYear();
  const month = date.getUTCMonth() + count;
  const last = new Date(Date.UTC(year, month + 1, 0)).getUTCDate();
  return utcDay(Date.UTC(year, month, Math.min(date.getUTCDate(), last)));
}

const monthOf = (day: string) => day.slice(0, 7);

/** The column of a day in a week that starts on Monday: 0 for Monday, 6 for Sunday. */
const weekday = (day: string) => (new Date(dayMillis(day)).getUTCDay() + 6) % 7;

/** The six weeks that show a month, from the Monday on or before its first day. */
function monthDays(month: string): string[] {
  const first = `${month}-01`;
  const start = addDays(first, -weekday(first));
  return Array.from({ length: GRID_DAYS }, (_, index) => addDays(start, index));
}

// Each list renders many labels on each render. Build each format once, in the device locale.
const formats = new Map<string, Intl.DateTimeFormat>();

function format(key: string, options: Intl.DateTimeFormatOptions): Intl.DateTimeFormat {
  let found = formats.get(key);
  if (!found) {
    found = new Intl.DateTimeFormat(undefined, { ...options, timeZone: 'UTC' });
    formats.set(key, found);
  }
  return found;
}

const monthTitle = (month: string) =>
  format('month', { month: 'long', year: 'numeric' }).format(dayMillis(`${month}-01`));

const dayName = (day: string) =>
  format('day', { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric' }).format(dayMillis(day));

const dayLabel = (day: string) =>
  format('label', { weekday: 'short', day: 'numeric', month: 'short', year: 'numeric' }).format(dayMillis(day));

/** The column heads, Monday first: a two-letter head and the full name for assistive tools. */
const WEEKDAYS = Array.from({ length: 7 }, (_, index) => {
  // 1 January 2024 is a Monday.
  const millis = Date.UTC(2024, 0, 1 + index);
  return {
    short: format('weekday', { weekday: 'short' }).format(millis).slice(0, 2),
    long: format('weekday-long', { weekday: 'long' }).format(millis),
  };
});

/** The day and the time of a `YYYY-MM-DDTHH:mm` moment. An unreadable value has no day. */
function splitMoment(value: string): { day: string; hour: number; minute: number } {
  const match = /^(\d{4}-\d{2}-\d{2})T(\d{2}):(\d{2})/.exec(value);
  return match && !Number.isNaN(dayMillis(match[1]))
    ? { day: match[1], hour: Math.min(+match[2], 23), minute: Math.min(+match[3], 59) }
    : { day: '', hour: 9, minute: 0 };
}

const NAV_BUTTON =
  'grid h-9 min-w-9 shrink-0 place-items-center px-1.5 text-dialog-hint outline-none transition-colors duration-150 hover:text-accent-ink focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent motion-reduce:transition-none mouse:h-7 mouse:min-w-7';

/**
 * One month as a grid of days. A press or Enter chooses a day; the arrows move a day
 * or a week, Home and End go to the ends of the week, and Page Up and Page Down move a
 * month (a year with Shift). Days before `min` show but cannot be chosen.
 */
export function Calendar({
  value,
  onValueChange,
  today,
  min,
}: {
  /** The chosen `YYYY-MM-DD` day, or an empty string. */
  value: string;
  onValueChange: (day: string) => void;
  /** Today on the reader's clock. A fixture gives a fixed day. */
  today: string;
  /** The first day that can be chosen. */
  min?: string;
}) {
  const [focus, setFocus] = useState(value || today);
  const [seenValue, setSeenValue] = useState(value);
  // A new value from outside shows its own month.
  if (value !== seenValue) {
    setSeenValue(value);
    if (value) setFocus(value);
  }
  const month = monthOf(focus);
  const days = monthDays(month);
  const titleId = useId();
  const gridRef = useRef<HTMLDivElement>(null);
  // Only a move from the keyboard takes the focus with it. A press on an arrow keeps its own.
  const movedRef = useRef(false);

  useLayoutEffect(() => {
    if (!movedRef.current) return;
    movedRef.current = false;
    gridRef.current?.querySelector<HTMLElement>(`[data-day="${focus}"]`)?.focus({ preventScroll: true });
  }, [focus]);

  const isBefore = (day: string) => min !== undefined && day < min;
  const move = (day: string) => {
    movedRef.current = true;
    setFocus(day);
  };
  const choose = (day: string) => {
    if (isBefore(day)) return;
    setFocus(day);
    onValueChange(day);
  };

  const onGridKeyDown = (event: KeyboardEvent<HTMLDivElement>) => {
    const years = event.shiftKey ? 12 : 1;
    const steps: Record<string, () => string> = {
      ArrowLeft: () => addDays(focus, -1),
      ArrowRight: () => addDays(focus, 1),
      ArrowUp: () => addDays(focus, -7),
      ArrowDown: () => addDays(focus, 7),
      Home: () => addDays(focus, -weekday(focus)),
      End: () => addDays(focus, 6 - weekday(focus)),
      PageUp: () => addMonths(focus, -years),
      PageDown: () => addMonths(focus, years),
    };
    const step = steps[event.key];
    if (!step) return;
    event.preventDefault();
    event.stopPropagation();
    move(step());
  };

  return (
    <div className="flex select-none flex-col font-mono text-white">
      <div className="flex items-center gap-1 pb-1">
        <span id={titleId} aria-live="polite" className="min-w-0 flex-1 truncate pl-1.5 text-title font-bold">
          {monthTitle(month)}
        </span>
        {month !== monthOf(today) && (
          <button type="button" className={`${NAV_BUTTON} text-meta font-bold uppercase`} onClick={() => move(today)}>
            Today
          </button>
        )}
        <button
          type="button"
          aria-label="Previous month"
          className={NAV_BUTTON}
          onClick={() => setFocus(addMonths(focus, -1))}
        >
          <ChevronIcon back className="size-3.5" />
        </button>
        <button type="button" aria-label="Next month" className={NAV_BUTTON} onClick={() => setFocus(addMonths(focus, 1))}>
          <ChevronIcon className="size-3.5" />
        </button>
      </div>
      <div ref={gridRef} role="grid" aria-labelledby={titleId} onKeyDown={onGridKeyDown}>
        <div role="row" className="grid grid-cols-7">
          {WEEKDAYS.map((name) => (
            <span
              key={name.long}
              role="columnheader"
              aria-label={name.long}
              className="grid h-7 place-items-center text-meta font-bold uppercase text-dialog-hint mouse:h-6"
            >
              {name.short}
            </span>
          ))}
        </div>
        {[0, 1, 2, 3, 4, 5].map((week) => (
          <div key={week} role="row" className="grid grid-cols-7">
            {days.slice(week * 7, week * 7 + 7).map((day) => {
              const isChosen = day === value;
              const isToday = day === today;
              const isUnavailable = isBefore(day);
              const tone = isChosen
                ? 'bg-accent font-bold text-accent-foreground'
                : isUnavailable
                  ? 'cursor-not-allowed text-muted opacity-50'
                  : isToday
                    ? 'font-bold text-accent-ink'
                    : monthOf(day) !== month
                      ? 'text-dialog-hint hover:text-accent-ink'
                      : 'text-white hover:text-accent-ink';
              return (
                <span key={day} role="gridcell" aria-selected={isChosen} className="grid">
                  <button
                    type="button"
                    data-day={day}
                    tabIndex={day === focus ? 0 : -1}
                    aria-label={dayName(day)}
                    aria-current={isToday ? 'date' : undefined}
                    aria-disabled={isUnavailable || undefined}
                    onClick={() => choose(day)}
                    className={`relative grid h-10 place-items-center text-body tabular-nums leading-none outline-none transition-colors duration-150 focus-visible:ring-1 focus-visible:ring-inset motion-reduce:transition-none mouse:h-8 mouse:text-ui ${
                      isChosen ? 'focus-visible:ring-accent-foreground' : 'focus-visible:ring-accent'
                    } ${tone}`}
                  >
                    {Number(day.slice(8))}
                    {isToday && !isChosen && <DotIcon className="absolute bottom-1 left-1/2 size-1 -translate-x-1/2" />}
                  </button>
                </span>
              );
            })}
          </div>
        ))}
      </div>
    </div>
  );
}

/** One two-digit part of a time: type the digits, or step it with the arrows. */
function TimeSegment({
  label,
  value,
  max,
  disabled,
  inputRef,
  nextRef,
  onValueChange,
  onDone,
}: {
  label: string;
  value: number;
  max: number;
  disabled: boolean;
  inputRef?: RefObject<HTMLInputElement | null>;
  /** The segment that takes the focus when this one is full. */
  nextRef?: RefObject<HTMLInputElement | null>;
  onValueChange: (value: number) => void;
  onDone?: () => void;
}) {
  // The digits that the reader types. `null` shows the value itself.
  const [typed, setTyped] = useState<string | null>(null);
  const set = (next: number) => {
    setTyped(null);
    onValueChange(next);
  };
  const onKeyDown = (event: KeyboardEvent<HTMLInputElement>) => {
    const steps: Record<string, number> = {
      ArrowUp: value + 1,
      ArrowDown: value - 1,
      PageUp: value + 10,
      PageDown: value - 10,
      Home: 0,
      End: max,
    };
    const step = steps[event.key];
    if (step !== undefined) {
      event.preventDefault();
      event.stopPropagation();
      set((step + max + 1) % (max + 1));
    } else if (event.key === 'Enter') {
      event.preventDefault();
      onDone?.();
    }
  };
  return (
    <input
      ref={inputRef}
      type="text"
      role="spinbutton"
      inputMode="numeric"
      autoComplete="off"
      enterKeyHint="done"
      aria-label={label}
      aria-valuemin={0}
      aria-valuemax={max}
      aria-valuenow={value}
      aria-valuetext={pad(value)}
      disabled={disabled}
      value={typed ?? pad(value)}
      onFocus={(event) => {
        setTyped(null);
        event.currentTarget.select();
      }}
      onBlur={() => setTyped(null)}
      onKeyDown={onKeyDown}
      onChange={(event) => {
        // Read the typed digits, not the whole text: the caret can sit anywhere in it.
        const input = event.nativeEvent as InputEvent;
        if (input.inputType?.startsWith('delete')) {
          setTyped('');
          return;
        }
        const added = (input.data ?? event.target.value).replace(/\D/g, '');
        if (added === '') return;
        // A new entry starts after a full one, as in a native time field.
        const kept = typed === null || typed.length >= 2 ? '' : typed;
        let digits = `${kept}${added}`.slice(-2);
        if (Number(digits) > max) digits = added.slice(-1);
        setTyped(digits);
        onValueChange(Number(digits));
        // A full segment, or a digit that no second digit can follow, moves on.
        if (digits.length === 2 || Number(digits) * 10 > max) nextRef?.current?.focus();
      }}
      className="h-8 w-9 rounded-none border border-edge bg-input text-center font-mono text-body tabular-nums leading-none text-white transition-[border-color,box-shadow] duration-150 focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted motion-reduce:transition-none mouse:h-7 mouse:text-ui"
    />
  );
}

/** A 24-hour time as two segments, hours and minutes. */
export function TimeField({
  hour,
  minute,
  onTimeChange,
  onDone,
  disabled = false,
}: {
  hour: number;
  minute: number;
  onTimeChange: (hour: number, minute: number) => void;
  /** Enter in a segment. */
  onDone?: () => void;
  disabled?: boolean;
}) {
  const minuteRef = useRef<HTMLInputElement>(null);
  return (
    <span role="group" aria-label="Time" className="inline-flex items-center gap-1 font-mono text-body text-dialog-hint">
      <TimeSegment
        label="Hours"
        value={hour}
        max={23}
        disabled={disabled}
        nextRef={minuteRef}
        onValueChange={(next) => onTimeChange(next, minute)}
        onDone={onDone}
      />
      <span aria-hidden="true">:</span>
      <TimeSegment
        label="Minutes"
        value={minute}
        max={59}
        disabled={disabled}
        inputRef={minuteRef}
        onValueChange={(next) => onTimeChange(hour, next)}
        onDone={onDone}
      />
    </span>
  );
}

/** Every control in the panel that Tab can reach, in order. */
const tabStops = (panel: HTMLElement) => [
  ...panel.querySelectorAll<HTMLElement>('button:not([tabindex="-1"]):not(:disabled), input:not(:disabled)'),
];

/**
 * A date and a time in one field. The field shows the moment; a press opens a calendar
 * and a time field beside it. A change commits at once, so Done, Escape or a press
 * outside only closes the panel. The value is a `YYYY-MM-DDTHH:mm` moment.
 */
export function DateTimeField({
  value,
  onValueChange,
  today: fixedToday,
  isFutureOnly = false,
  disabled = false,
  id,
  'aria-label': label,
}: {
  value: string;
  onValueChange: (value: string) => void;
  /** A fixed today for a fixture. Without it, the field reads the device clock when it opens. */
  today?: string;
  /** Days before today cannot be chosen. */
  isFutureOnly?: boolean;
  disabled?: boolean;
  id?: string;
  'aria-label'?: string;
}) {
  const [open, setOpen] = useState(false);
  const [today, setToday] = useState(fixedToday ?? '');
  const triggerRef = useRef<HTMLButtonElement>(null);
  const layerRef = useRef<HTMLDivElement>(null);
  const panelRef = useRef<HTMLDivElement>(null);
  // Only a close that the reader asked for gives the focus back to the field.
  const returnFocusRef = useRef(false);
  const panelId = useId();
  const { day, hour, minute } = splitMoment(value);
  const isOpen = open && !disabled;

  const dismiss = () => {
    returnFocusRef.current = true;
    setOpen(false);
  };
  const commit = (nextDay: string, nextHour: number, nextMinute: number) =>
    onValueChange(`${nextDay}T${pad(nextHour)}:${pad(nextMinute)}`);

  // The phone's back closes the open panel before the screen under it.
  useBackLayer(isOpen ? dismiss : null);
  useAnchoredLayer({
    isOpen,
    layerRef,
    panelRef,
    triggerRef,
    maxHeight: PANEL_HEIGHT,
    isTriggerWide: false,
    onOpen: (panel) => panel.querySelector<HTMLElement>('[data-day][tabindex="0"]')?.focus({ preventScroll: true }),
    onOutsidePress: () => setOpen(false),
    onClose: (trigger) => {
      if (!returnFocusRef.current) return;
      returnFocusRef.current = false;
      trigger.focus({ preventScroll: true });
    },
  });

  const onPanelKeyDown = (event: KeyboardEvent<HTMLDivElement>) => {
    // Do not let the surrounding dialog read a key of the open panel.
    event.stopPropagation();
    if (event.key === 'Escape') {
      event.preventDefault();
      dismiss();
    } else if (event.key === 'Tab') {
      // Keep Tab inside the panel, as in a dialog.
      const stops = tabStops(event.currentTarget);
      const at = stops.indexOf(event.target as HTMLElement);
      const next = event.shiftKey ? (at <= 0 ? stops.length - 1 : at - 1) : (at + 1) % stops.length;
      event.preventDefault();
      stops[next]?.focus();
    }
  };

  return (
    <>
      <button
        ref={triggerRef}
        id={id}
        type="button"
        aria-label={label}
        aria-haspopup="dialog"
        aria-expanded={isOpen}
        aria-controls={isOpen ? panelId : undefined}
        data-state={isOpen ? 'open' : 'closed'}
        disabled={disabled}
        onClick={() => {
          if (isOpen) {
            dismiss();
            return;
          }
          setToday(fixedToday ?? localDay(Date.now()));
          setOpen(true);
        }}
        className="relative flex h-8 w-full min-w-0 items-center gap-2 rounded-none border border-edge bg-input px-2.5 text-left font-mono text-ui text-white after:absolute after:inset-x-0 after:-inset-y-1.5 after:content-[''] enabled:hover:text-accent-ink focus-visible:border-accent focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent/30 disabled:cursor-not-allowed disabled:text-muted data-[state=open]:border-accent sm:px-3 mouse:h-7 mouse:after:content-none"
      >
        <CalendarIcon className="size-3.5 shrink-0 text-dialog-hint" />
        <span className={`min-w-0 flex-1 truncate ${day ? '' : 'text-dialog-hint'}`}>
          {day ? (
            <>
              {dayLabel(day)}
              <span className="px-1.5 text-dialog-hint">·</span>
              <span className="tabular-nums">
                {pad(hour)}:{pad(minute)}
              </span>
            </>
          ) : (
            'Choose a date and time'
          )}
        </span>
        <ChevronIcon open className={`size-3 shrink-0 ${isOpen ? 'rotate-180' : ''}`} />
      </button>
      {isOpen &&
        createPortal(
          <div
            ref={layerRef}
            role="presentation"
            className="fixed inset-0 z-[60] touch-none"
            onClick={(event) => {
              if (event.target === event.currentTarget) dismiss();
            }}
          >
            <div
              ref={panelRef}
              id={panelId}
              role="dialog"
              aria-label={label ?? 'Choose a date and time'}
              className="absolute w-[18.5rem] max-w-[calc(100vw-1.5rem)] overflow-y-auto overscroll-contain rounded-none border border-dialog-edge bg-panel p-2 shadow-float mouse:w-[16.5rem]"
              onKeyDown={onPanelKeyDown}
            >
              <Calendar
                value={day}
                today={today}
                min={isFutureOnly ? today : undefined}
                onValueChange={(next) => commit(next, hour, minute)}
              />
              <div className="mt-2 flex items-center gap-2 border-t border-edge pt-2">
                <span className="pl-1.5 font-mono text-meta font-bold uppercase text-dialog-hint">Time</span>
                <TimeField
                  hour={hour}
                  minute={minute}
                  onTimeChange={(nextHour, nextMinute) => commit(day || today, nextHour, nextMinute)}
                  onDone={dismiss}
                />
                <span className="flex-1" />
                <Button variant="primary" density="compact" pressEffect="none" onClick={dismiss}>
                  Done
                </Button>
              </div>
            </div>
          </div>,
          document.body,
        )}
    </>
  );
}
