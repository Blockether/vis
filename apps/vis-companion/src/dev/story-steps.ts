import { userEvent, within } from 'storybook/test';

/**
 * Opens each closed step digest in a story, as a reader does before reading the
 * thinking, code and Activity of the steps under a note.
 */
export async function openStepDigests(canvasElement: HTMLElement): Promise<void> {
  for (const digest of within(canvasElement).queryAllByRole('button', { name: /^Expand steps/ }))
    await userEvent.click(digest);
}
