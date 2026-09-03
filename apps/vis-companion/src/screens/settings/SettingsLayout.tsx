import { useSyncExternalStore, type ReactNode } from "react";

import { ChevronIcon } from "../../components/icons";
import { IconButton, ListRow } from "../../components/ui";
export function FormLabel({
  label,
  hint,
  children,
}: {
  label: string;
  hint?: string;
  children: ReactNode;
}) {
  return (
    <label className="block space-y-1">
      <span className="block font-mono text-chip font-bold text-white">
        {label}
      </span>
      {children}
      {hint && (
        <span className="block font-mono text-chip text-dialog-hint">
          {hint}
        </span>
      )}
    </label>
  );
}

/** WHERE THE DIALOG'S TWO COLUMNS STAND SIDE BY SIDE: the same `sm:` the settings
 *  grid stacks at, so the fold and the layout it serves can never disagree. */
const WIDE_COLUMNS = "(min-width: 640px)";

const subscribeWideColumns = (onStoreChange: () => void) => {
  const media = window.matchMedia?.(WIDE_COLUMNS);
  media?.addEventListener("change", onStoreChange);
  return () => media?.removeEventListener("change", onStoreChange);
};

/** True once there is room for both of the dialog's columns beside each other. */
const useWideColumns = () =>
  useSyncExternalStore(
    subscribeWideColumns,
    () => window.matchMedia?.(WIDE_COLUMNS).matches ?? false,
  );

/** Settings owned by this companion installation, never by a gateway. */
/**
 * ONE COLUMN OF SETTINGS, and the dialog has two of them.
 *
 * A column is the level ABOVE a `SettingsPanel`: it says whose settings these are —
 * this copy of Vis, or the machine — and every band under it belongs to that owner.
 * It is the sentence the two dialogs used to spend a whole header band saying.
 *
 * A BAND NAMES ITS GROUP AND NEVER EXPLAINS IT. Both levels took a `description`
 * under the title, and every one of them said what the rows under it already say
 * — "every palette Vis ships" over the list of palettes, "how many sessions a
 * project lists" over 5/10/15 — so a group of 48px rows opened with two lines of
 * grey prose nobody reads twice. Reported over this screen as pointless; the prop
 * went with the last three call sites that used it.
 */

export function SettingsColumn({
  title,
  meta,
  action,
  disclosure,
  children,
}: {
  title: string;
  meta?: ReactNode;
  /** The column's ONE bare verb, at the physical end of its band. */
  action?: ReactNode;
  /**
   * The fold to expose while the two columns stack on a phone, or nothing where
   * they stand beside each other. `ProjectCrumb`'s shape: the caller owns the
   * state, the band carries the chevron.
   */
  disclosure?: {
    isOpen: boolean;
    onToggle: () => void;
    /** What the fold is called to a screen reader: `Show application settings`. */
    label: string;
  };
  children: ReactNode;
}) {
  // The fold lives ONLY where the columns stack — below the same `sm:` that makes
  // the dialog one column wide. Beside each other there is nothing to fold, and a
  // chevron on a band that hides nothing is one more lie on the screen.
  const isWide = useWideColumns();
  const fold = disclosure && !isWide ? disclosure : null;
  const body = (
    /* A COLUMN CLOSES ITS OWN LAST GROUP. Reported over this screenshot: the
       dialog's last panel simply stopped. On a phone the frame is full-bleed
       and carries no bottom edge, so the selected amber cell of the last
       choice ran out into paper with no hairline under it — measured at 390px,
       the column body ended at the cell's own 2655px — and the two stacked
       halves were told apart by the GRID rather than by the column that ends.
       The body draws the rule it owes below itself; on `sm:` the columns stand
       side by side and the frame's own 1px bottom border is that edge, so it
       is dropped there rather than doubled. */
    <div className="min-w-0 divide-y divide-dialog-edge border-b border-dialog-edge sm:min-h-0 sm:flex-1 sm:overflow-y-auto sm:overscroll-contain sm:border-b-0">
      {children}
    </div>
  );
  return (
    <section className="flex min-w-0 flex-col sm:min-h-0">
      {/* A BAND NAMES THE COLUMN IN ONE LINE, and its verb is a BARE MARK at the
          physical trailing edge. The title and optional meta wrap in their own cell;
          the action owns the remaining hit area without painting a second object in
          the band. Column and nested-panel bands keep one 36px touch / 32px pointer
          rhythm; their level comes from paper and type, not from a circle around ＋. */}
      <header className="min-w-0 shrink-0 border-b border-dialog-edge bg-level-machine">
        <div className="flex min-h-9 min-w-0 items-center gap-3 px-3 py-0.5 sm:px-4 mouse:min-h-8">
          <div className="flex min-w-0 flex-auto flex-wrap items-baseline gap-x-3 gap-y-1">
            <h3 className="min-w-0 flex-auto truncate font-mono text-ui font-black uppercase tracking-[0.12em] text-white">
              {title}
            </h3>
            {meta && (
              <span className="ms-auto min-w-0 max-w-full break-words text-right font-mono text-chip font-bold uppercase tracking-wider text-dialog-hint">
                {meta}
              </span>
            )}
          </div>
          <span className="flex shrink-0 items-center empty:hidden">
            {action}
            {fold && (
              /* THE FOLD IS THE BAND'S OWN MARK, and it stands where the machines'
                 ＋ stands: the trailing edge, one bare chevron that TURNS rather than
                 swaps glyph. Hidden is HIDDEN — the panels under this band are not on
                 the page at all until the mark is pressed. */
              <IconButton
                variant="quiet"
                edge
                label={fold.label}
                title={fold.label}
                aria-expanded={fold.isOpen}
                onClick={fold.onToggle}
              >
                <ChevronIcon open={fold.isOpen} className="size-4" />
              </IconButton>
            )}
          </span>
        </div>
      </header>
      {!fold || fold.isOpen ? body : null}
    </section>
  );
}

export function SettingsPanel({
  title,
  meta,
  action,
  disclosure,
  children,
}: {
  title: string;
  meta?: ReactNode;
  /** One bare verb for the whole band, aligned to its physical trailing edge. */
  action?: ReactNode;
  /** Makes the whole named band the disclosure target; the caller owns its state. */
  disclosure?: {
    isOpen: boolean;
    onToggle: () => void;
    label: string;
  };
  children: ReactNode;
}) {
  const TitleContainer = disclosure ? "span" : "div";
  const TitleHeading = disclosure ? "span" : "h3";
  const titleBlock = (
    <TitleContainer
      className={`flex min-w-0 flex-auto flex-wrap items-baseline gap-x-3 gap-y-1 ${
        disclosure ? "sm:ms-1" : ""
      }`}
    >
      <TitleHeading
        role="heading"
        aria-level={3}
        className="min-w-0 flex-auto truncate font-mono text-chip font-bold uppercase tracking-[0.14em] text-dialog-hint"
      >
        {title}
      </TitleHeading>
      {meta && (
        <span className="ms-auto min-w-0 max-w-full break-words text-right font-mono text-chip font-bold uppercase tracking-wider text-dialog-hint">
          {meta}
        </span>
      )}
    </TitleContainer>
  );

  return (
    // A BAND, not a card. This section used to carry its own frame inside the
    // dialog's frame, so every settings group sat in a box inside a box — two
    // concentric hairlines 16px apart, and a third around each control inside it.
    // The dialog is the only box; a group is separated from the next by the one
    // rule its container divides on, exactly as a project is separated from the
    // next in the sessions list.
    <section className="min-w-0 overflow-hidden bg-panel transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
      {/* A HEADER LINE IS NOT A COMPETITION FOR ONE ROW. The status used to be
          `shrink-0` beside the name, so it took its whole intrinsic width first
          and the name lived on what was left: measured on a 390px iPhone,
          "0 devices · via <relay host>" claimed 339 of 390, the title box
          collapsed to 15px and clipped to one syllable, the sentence under it
          wrapped one word per line, and the band grew 213px tall. The row WRAPS
          instead — the name is measured at its own width so a status that does
          not fit beside it drops to its own line. */}
      {/* A NESTED BAND IS NOT A COLUMN BAND. It keeps the smaller hint-colour title
          but shares the column band's height and gutter. A disclosure IS the band,
          so its whole named row responds instead of leaving inert copy beside a
          tiny trailing target. An ordinary action still occupies only the edge. */}
      {disclosure ? (
        <header className="border-b border-dialog-edge">
          <ListRow
            density="compact"
            aria-label={disclosure.label}
            aria-expanded={disclosure.isOpen}
            onClick={disclosure.onToggle}
          >
            {titleBlock}
            <ChevronIcon
              open={disclosure.isOpen}
              className="size-4 shrink-0 sm:me-1"
            />
          </ListRow>
        </header>
      ) : (
        <header className="flex min-h-9 min-w-0 items-center gap-3 border-b border-dialog-edge px-3 py-0.5 sm:px-4 mouse:min-h-8">
          {titleBlock}
          {action && (
            <span className="flex shrink-0 items-center empty:hidden">
              {action}
            </span>
          )}
        </header>
      )}
      {/* A PANEL BODY DIVIDES ITS OWN PARTS. `divide-y` draws only BETWEEN
          siblings, so a panel holding one list is unchanged, and a panel whose
          last child is a verb gets the hairline that verb needs to be a row. */}
      <div className="divide-y divide-dialog-edge">{children}</div>
    </section>
  );
}
