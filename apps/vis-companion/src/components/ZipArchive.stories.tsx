import type { Meta, StoryObj } from '@storybook/react-vite';
import { storedZip } from '../lib/zip-harness';
import { ZipArchive } from './ZipArchive';

/**
 * A ZIP ARCHIVE OPENED IN THE ARTIFACT SHEET.
 *
 * The app reads the archive itself: one row for each file, with its size. A row opens a
 * preview of a text or image entry, and the glyph beside it saves that entry alone.
 */
const meta = {
  title: 'Components/ZIP archive',
  component: ZipArchive,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof ZipArchive>;

export default meta;

type Story = StoryObj<typeof meta>;

const encode = (text: string) => new TextEncoder().encode(text);

/** A typical results bundle: notes, a table and a binary file without a preview. */
export const Results: Story = {
  args: {
    name: 'results.zip',
    blob: storedZip([
      { name: 'README.md', data: encode('# Results\n\nAll checks passed.\n') },
      { name: 'data/coverage.csv', data: encode('file,lines\ncore.clj,92\n') },
      { name: 'bin/report.pdf', data: encode('%PDF-1.7') },
    ]),
  },
};

/** An archive without files still says what it is. */
export const Empty: Story = {
  args: { name: 'empty.zip', blob: storedZip([]) },
};

/** Bytes that are not a ZIP archive: the reader says so and keeps the save action. */
export const Broken: Story = {
  args: { name: 'broken.zip', blob: new Blob(['not an archive at all, really']) },
};
