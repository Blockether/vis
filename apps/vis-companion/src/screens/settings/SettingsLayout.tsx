import type { ReactNode } from 'react';

import { SettingsHeader, Text } from '../../components/ui';

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
      <Text variant="label" className="block">
        {label}
      </Text>
      {children}
      {hint && (
        <Text variant="description" className="block">
          {hint}
        </Text>
      )}
    </label>
  );
}

type HeaderDisclosure = {
  isOpen: boolean;
  onToggle: () => void;
  /** What the fold is called to a screen reader. */
  label: string;
};

export function SettingsPanel({
  title,
  headingLevel = 4,
  meta,
  action,
  disclosure,
  children,
}: {
  title: string;
  /** Heading level beneath the surrounding dialog. */
  headingLevel?: 3 | 4;
  meta?: ReactNode;
  /** One icon action or switch; the header owns its alignment and trailing space. */
  action?: ReactNode;
  /** Makes the whole named band the disclosure target; the caller owns its state. */
  disclosure?: HeaderDisclosure;
  children: ReactNode;
}) {
  const TitleContainer = disclosure ? 'span' : 'div';
  const TitleHeading = disclosure ? 'span' : headingLevel === 3 ? 'h3' : 'h4';
  const titleBlock = (
    <TitleContainer className="flex min-w-0 flex-auto flex-wrap items-baseline gap-x-3 gap-y-1">
      <Text
        as={TitleHeading}
        variant="section"
        role="heading"
        aria-level={headingLevel}
        className="min-w-0 flex-auto truncate"
      >
        {title}
      </Text>
      {meta && (
        <span className="ms-auto min-w-0 max-w-full break-words text-right">
          <Text variant="meta">{meta}</Text>
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
    <section className="min-w-0 bg-panel">
      {/* A HEADER LINE IS NOT A COMPETITION FOR ONE ROW. The status used to be
          `shrink-0` beside the name, so it took its whole intrinsic width first
          and the name lived on what was left: measured on a 390px iPhone,
          "0 devices · via <relay host>" claimed 339 of 390, the title box
          collapsed to 15px and clipped to one syllable, the sentence under it
          wrapped one word per line, and the band grew 213px tall. The row WRAPS
          instead — the name is measured at its own width so a status that does
          not fit beside it drops to its own line. */}
      {/* The same centered header fits a lone switch without an empty body below. */}
      <header>
        <SettingsHeader action={action} disclosure={disclosure}>
          {titleBlock}
        </SettingsHeader>
      </header>
      {/* A visible body owns one header divider; an empty body adds no second rule. */}
      <div className="overflow-hidden divide-y divide-dialog-edge border-t border-dialog-edge empty:hidden">
        {children}
      </div>
    </section>
  );
}
