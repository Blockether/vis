import type { ReactNode } from "react";

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
  children,
}: {
  title: string;
  meta?: ReactNode;
  /** The column's ONE bare verb, at the physical end of its band. */
  action?: ReactNode;
  children: ReactNode;
}) {
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
          {action && (
            <span className="flex shrink-0 items-center empty:hidden">
              {action}
            </span>
          )}
        </div>
      </header>
      {/* A COLUMN CLOSES ITS OWN LAST GROUP. Reported over this screenshot: the
          dialog's last panel simply stopped. On a phone the frame is full-bleed
          and carries no bottom edge, so the selected amber cell of the last
          choice ran out into paper with no hairline under it — measured at 390px,
          the column body ended at the cell's own 2655px — and the two stacked
          halves were told apart by the GRID rather than by the column that ends.
          The body draws the rule it owes below itself; on `sm:` the columns stand
          side by side and the frame's own 1px bottom border is that edge, so it
          is dropped there rather than doubled. */}
      <div className="min-w-0 divide-y divide-dialog-edge border-b border-dialog-edge sm:min-h-0 sm:flex-1 sm:overflow-y-auto sm:overscroll-contain sm:border-b-0">
        {children}
      </div>
    </section>
  );
}

export function SettingsPanel({
  title,
  meta,
  action,
  children,
}: {
  title: string;
  meta?: ReactNode;
  /** One bare verb for the whole band, aligned to its physical trailing edge. */
  action?: ReactNode;
  children: ReactNode;
}) {
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
          but shares the column band's height and gutter. Its action occupies the
          trailing edge as hit area while the visible plus remains bare, so an action
          neither changes the band's height nor introduces a floating object. */}
      <header className="flex min-h-9 min-w-0 items-center gap-3 border-b border-dialog-edge px-3 py-0.5 sm:px-4 mouse:min-h-8">
        <div className="flex min-w-0 flex-auto flex-wrap items-baseline gap-x-3 gap-y-1">
          <h3 className="min-w-0 flex-auto truncate font-mono text-chip font-bold uppercase tracking-[0.14em] text-dialog-hint">
            {title}
          </h3>
          {meta && (
            <span className="ms-auto min-w-0 max-w-full break-words text-right font-mono text-chip font-bold uppercase tracking-wider text-dialog-hint">
              {meta}
            </span>
          )}
        </div>
        {action && (
          <span className="flex shrink-0 items-center empty:hidden">
            {action}
          </span>
        )}
      </header>
      {/* A PANEL BODY DIVIDES ITS OWN PARTS. `divide-y` draws only BETWEEN
          siblings, so a panel holding one list is unchanged, and a panel whose
          last child is a verb gets the hairline that verb needs to be a row. */}
      <div className="divide-y divide-dialog-edge">{children}</div>
    </section>
  );
}
