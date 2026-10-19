/**
 * A ZIP archive opened in the app: its entries, a preview of one text or image entry,
 * and the original bytes of each entry.
 *
 * The bytes never leave the device for this. `lib/zip` reads the central directory, and
 * an entry is inflated only when the person selects it.
 */

import { useEffect, useState } from 'react';
import { attachmentBytes } from '../lib/artifacts';
import {
  canExtractZipEntry,
  extractZipEntry,
  readZipEntries,
  type ZipEntry,
} from '../lib/zip';
import { ChevronIcon } from './icons';
import { MediaShareButton } from './Media';
import { ListRow } from './ui';

const TEXT_EXTENSIONS = new Set([
  'txt', 'md', 'markdown', 'csv', 'tsv', 'json', 'jsonl', 'ndjson', 'log', 'xml', 'html', 'htm',
  'css', 'js', 'mjs', 'ts', 'tsx', 'jsx', 'py', 'clj', 'cljs', 'cljc', 'edn', 'yml', 'yaml',
  'toml', 'ini', 'sh', 'sql', 'rs', 'java', 'go', 'rb', 'c', 'h', 'cpp', 'svg',
]);
const IMAGE_MEDIA: Record<string, string> = {
  png: 'image/png',
  jpg: 'image/jpeg',
  jpeg: 'image/jpeg',
  gif: 'image/gif',
  webp: 'image/webp',
  avif: 'image/avif',
};
const TEXT_PREVIEW_BYTES = 200_000;

function extensionOf(name: string): string {
  const base = name.split('/').pop() ?? '';
  const dot = base.lastIndexOf('.');
  return dot > 0 ? base.slice(dot + 1).toLowerCase() : '';
}

/** The media type of an entry, from its extension. */
export function zipEntryMediaType(name: string): string {
  const extension = extensionOf(name);
  if (IMAGE_MEDIA[extension]) return IMAGE_MEDIA[extension];
  if (extension === 'zip') return 'application/zip';
  if (TEXT_EXTENSIONS.has(extension)) return 'text/plain';
  return 'application/octet-stream';
}

type Preview =
  | { kind: 'text'; text: string; truncated: boolean }
  | { kind: 'image'; url: string }
  | { kind: 'none'; reason: string };

function EntryPreview({ buffer, entry }: { buffer: ArrayBuffer; entry: ZipEntry }) {
  const [preview, setPreview] = useState<Preview | null>(null);
  useEffect(() => {
    let live = true;
    let url = '';
    const media = zipEntryMediaType(entry.name);
    if (!canExtractZipEntry(entry)) {
      setPreview({ kind: 'none', reason: entry.encrypted ? 'This entry is encrypted.' : 'This entry cannot be opened in the app.' });
      return;
    }
    if (media === 'text/plain' && entry.size > TEXT_PREVIEW_BYTES * 20) {
      setPreview({ kind: 'none', reason: 'This file is too large to preview. Save it to read it.' });
      return;
    }
    if (media !== 'text/plain' && !media.startsWith('image/')) {
      setPreview({ kind: 'none', reason: 'No preview for this file type. Save it to open it.' });
      return;
    }
    extractZipEntry(buffer, entry)
      .then((bytes) => {
        if (!live) return;
        if (media.startsWith('image/')) {
          url = URL.createObjectURL(new Blob([bytes], { type: media }));
          setPreview({ kind: 'image', url });
          return;
        }
        const truncated = bytes.byteLength > TEXT_PREVIEW_BYTES;
        const text = new TextDecoder('utf-8').decode(truncated ? bytes.subarray(0, TEXT_PREVIEW_BYTES) : bytes);
        setPreview({ kind: 'text', text, truncated });
      })
      .catch((error: unknown) => {
        if (live) setPreview({ kind: 'none', reason: error instanceof Error ? error.message : 'This entry could not be read.' });
      });
    return () => {
      live = false;
      if (url) URL.revokeObjectURL(url);
    };
  }, [buffer, entry]);

  if (!preview) return <p className="px-3 py-2 font-mono text-meta text-dialog-hint">Loading…</p>;
  if (preview.kind === 'image') {
    return <img src={preview.url} alt={entry.name} className="mx-auto max-h-96 max-w-full object-contain p-3" />;
  }
  if (preview.kind === 'text') {
    return (
      <div className="px-3 py-2">
        <pre className="max-h-96 overflow-auto whitespace-pre-wrap break-words border border-dialog-edge bg-panel p-2 font-mono text-meta text-answer-foreground">
          {preview.text}
        </pre>
        {preview.truncated ? (
          <p className="pt-1 font-mono text-meta text-dialog-hint">Preview shows the start of the file. Save it to read all of it.</p>
        ) : null}
      </div>
    );
  }
  return <p className="px-3 py-2 font-mono text-meta text-dialog-hint">{preview.reason}</p>;
}

/** The entries of one ZIP archive, readable and savable one by one. */
export function ZipArchive({ blob, name }: { blob: Blob; name: string }) {
  const [archive, setArchive] = useState<{ buffer: ArrayBuffer; entries: ZipEntry[] } | null>(null);
  const [error, setError] = useState('');
  const [selected, setSelected] = useState<string | null>(null);

  useEffect(() => {
    let live = true;
    blob
      .arrayBuffer()
      .then((buffer) => {
        const entries = readZipEntries(buffer).filter((entry) => !entry.directory);
        if (live) setArchive({ buffer, entries });
      })
      .catch((reason: unknown) => {
        if (live) setError(reason instanceof Error ? reason.message : 'This archive could not be read.');
      });
    return () => {
      live = false;
    };
  }, [blob]);

  if (error) {
    return (
      <p role="alert" className="font-mono text-meta text-dialog-hint">
        {`${name} could not be read: ${error} Save the archive to open it in another app.`}
      </p>
    );
  }
  if (!archive) return <p className="font-mono text-meta text-dialog-hint">Reading archive…</p>;

  const total = archive.entries.reduce((sum, entry) => sum + entry.size, 0);
  const count = archive.entries.length;
  return (
    <div className="-mx-3 sm:-mx-4">
      <p className="px-3 pb-2 font-mono text-meta text-dialog-hint sm:px-4">
        {`ZIP · ${count} ${count === 1 ? 'file' : 'files'} · ${attachmentBytes(total)} unpacked`}
      </p>
      {count === 0 ? <p className="px-3 font-mono text-meta text-dialog-hint sm:px-4">This archive is empty.</p> : null}
      <ul aria-label={`Files in ${name}`}>
        {archive.entries.map((entry) => {
          const isOpen = selected === entry.name;
          const media = zipEntryMediaType(entry.name);
          return (
            <li key={`${entry.offset}:${entry.name}`} className="border-b border-dialog-edge">
              <div className="flex items-center gap-2 pr-3 sm:pr-4">
                <ListRow
                  density="compact"
                  isSelected={isOpen}
                  aria-expanded={isOpen}
                  className="min-w-0 flex-1"
                  onClick={() => setSelected(isOpen ? null : entry.name)}
                >
                  <span className="flex w-full min-w-0 items-center gap-2 font-mono text-meta">
                    <ChevronIcon className={`size-3 shrink-0 ${isOpen ? 'rotate-90' : ''}`} />
                    <span className="min-w-0 flex-1 truncate text-left">{entry.name}</span>
                    <span className="shrink-0 text-dialog-hint">
                      {entry.encrypted ? 'encrypted' : attachmentBytes(entry.size)}
                    </span>
                  </span>
                </ListRow>
                {canExtractZipEntry(entry) ? (
                  <MediaShareButton
                    name={entry.name.split('/').pop() || entry.name}
                    mediaType={media}
                    load={async () => new Blob([await extractZipEntry(archive.buffer, entry)], { type: media })}
                  />
                ) : null}
              </div>
              {isOpen ? <EntryPreview buffer={archive.buffer} entry={entry} /> : null}
            </li>
          );
        })}
      </ul>
    </div>
  );
}
