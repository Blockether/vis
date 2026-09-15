import type { PendingAttachment } from "./attachments";

/** Only owned tokens remove payloads. Unknown literal markers stay ordinary text. */
export function referencedAttachments(
  text: string,
  attachments: PendingAttachment[],
): PendingAttachment[] {
  return attachments.filter(
    (attachment) =>
      !attachment.reference || text.includes(attachment.reference),
  );
}

/** Allocate beyond both previously issued and authored numbers: literals never acquire bytes. */
export function insertImageReferences(
  text: string,
  caret: number,
  incoming: PendingAttachment[],
  counter: number,
) {
  for (const match of text.matchAll(/\[IMAGE #(\d+)\]/g))
    counter = Math.max(counter, Number(match[1]));
  const attachments = incoming.map((attachment) =>
    attachment.media_type.startsWith("image/")
      ? { ...attachment, reference: `[IMAGE #${++counter}]` }
      : attachment,
  );
  const tokens = attachments
    .flatMap((attachment) =>
      attachment.reference ? [attachment.reference] : [],
    )
    .join(" ");
  if (!tokens) return { text, caret, attachments, counter };
  const before = text.slice(0, caret);
  const after = text.slice(caret);
  const inserted = `${before && !/\s$/.test(before) ? " " : ""}${tokens}${after && !/^\s/.test(after) ? " " : ""}`;
  return {
    text: before + inserted + after,
    caret: caret + inserted.length,
    attachments,
    counter,
  };
}

/** Restore only incoming ownership; existing image numbers and literal tokens stay intact. */
export function restoreImageReferences(
  currentText: string,
  current: PendingAttachment[],
  text: string,
  incoming: PendingAttachment[],
  counter: number,
) {
  for (const match of `${currentText}\n${text}`.matchAll(/\[IMAGE #(\d+)\]/g))
    counter = Math.max(counter, Number(match[1]));
  const replacements = new Map<string, string>();
  const attachments = [...current];
  for (const attachment of referencedAttachments(text, incoming)) {
    const existing = current.find((item) => item.id === attachment.id);
    if (existing) {
      if (attachment.reference && existing.reference)
        replacements.set(attachment.reference, existing.reference);
      continue;
    }
    const reference = attachment.reference;
    if (reference && currentText.includes(reference)) {
      const next = `[IMAGE #${++counter}]`;
      replacements.set(reference, next);
      attachments.push({ ...attachment, reference: next });
    } else {
      attachments.push(attachment);
    }
  }
  return {
    text: text.replace(
      /\[IMAGE #\d+\]/g,
      (token) => replacements.get(token) ?? token,
    ),
    attachments,
    counter,
  };
}
