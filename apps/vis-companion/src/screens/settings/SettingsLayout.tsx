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
  /** The column's ONE verb, at the end of its band: the amber ＋. */
  action?: ReactNode;
  children: ReactNode;
}) {
  return (
    <section className="flex min-w-0 flex-col sm:min-h-0">
      {/* A BAND NAMES THE COLUMN IN ONE LINE, and its verb is a MARK.
          The title, a meta and the ＋ used to share one wrapping flex line whose
          height was the button's: the two words sat at the top of it on their
          baseline while the ＋ centred itself in the rest, 8px lower than the title
          it stands beside. Then the reader asked what the meta and the sentence
          under it were FOR — a column that lists every machine does not need to
          name one of them in its own header, and "tap a row" is not news. The verb
          spelled itself out in WORDS for a while, which is how a band ends up as
          wide as its longest verb; it is the disc again, and the ＋ adds the thing
          the band is NAMED after — so the mark opening a session in the project
          band of the list and the mark adding a machine here are one rule, not one
          glyph meaning two things. The name and its meta wrap inside their own
          cell, the verb is the band's trailing cell centred against whatever that
          cell grows to. Reported since (paraphrased: the ＋ in MACHINES and the ones
          by notifications and by MCP are uneven): this band stood 48px tall and
          padded 16px on `sm:` while the bands nested inside a machine stood 36 and
          padded 12, so one 32px mark floated in 8px of paper and the next had 2 —
          and measured on a desktop the two did not even share a vertical line, 595
          against 599. Every band in the dialog is ONE height now, 36px on touch and
          32 under a pointer, on ONE gutter; the LEVEL is the paper, the white and
          the amber fill, not twelve extra pixels. */}
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
  /** One verb for the whole band, sitting in it — the panel's own ＋. */
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
      {/* A NESTED BAND IS NOT A COLUMN BAND. Reported over this screen: Providers,
          Notifications and MCP servers did not read as parts OF a machine — each
          wore the same slab, the same white and nearly the same weight as the
          `MACHINES` band above them, so four peers stood where there are two
          levels. The column keeps the paper, the size and the white; a panel
          inside a machine keeps neither, and speaks in the hint colour one step
          smaller. Reported since (paraphrased: bin that rail on the left, line the
           borders up): the 2px accent tick it wore was the only label on the screen
           standing 10px right of every other one. Reported since (paraphrased: the
           pluses in MACHINES and the ones by notifications and MCP are uneven): a band
           that carried a verb PADDED around it and stood 45px tall while the sibling
           band without one stayed 32, so the column's rules were spaced by whether a
           panel had anything to press; and the title, the meta and the face shared
           ONE baseline line, which left the label's optical centre 9px above the
           mark's. The band owns ONE height at each density — 36px on touch, 32px
           under a pointer, the project band's own rhythm, the same the column band
           above now keeps — the name and its meta wrap inside their own cell, and
           the verb is the trailing cell, centred against it. */}
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
