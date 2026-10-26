import { memo, useEffect, useState } from 'react';

import { parseDiff, diffSourceLabel, type DiffEnvelope } from '../lib/diff';
import { PlainText, type DocumentChrome } from './MarkdownArtifact';
import { readArtifactText } from './TextArtifact';

/** A stored patch snapshot, read only. A review with threads is an app attachment. */
export const DiffArtifact = memo(function DiffArtifact({
  source,
  chrome,
}: {
  source: Blob | string;
  chrome: DocumentChrome;
}) {
  const [loaded, setLoaded] = useState<{
    source: Blob | string;
    envelope: DiffEnvelope;
  } | null>(null);
  const [failed, setFailed] = useState<Blob | string | null>(null);
  useEffect(() => {
    let alive = true;
    readArtifactText(source)
      .then(parseDiff)
      .then((envelope) => {
        if (alive) setLoaded({ source, envelope });
      })
      .catch(() => {
        if (alive) setFailed(source);
      });
    return () => {
      alive = false;
    };
  }, [source]);
  const error = failed === source ? 'This diff could not be read. Ask for a new snapshot.' : '';
  if (error || !loaded || loaded.source !== source)
    return chrome({
      actions: null,
      note: 'Diff',
      body: (
        <p role={error ? 'alert' : 'status'} className="p-4 font-mono text-body text-dialog-hint">
          {error || 'Loading diff…'}
        </p>
      ),
    });
  return chrome({
    actions: null,
    note: `Diff · ${diffSourceLabel(loaded.envelope.source)}`,
    body: (
      <div className="min-h-0 min-w-0 flex-1 overflow-y-auto bg-panel p-3 sm:p-4">
        <PlainText text={loaded.envelope.patch} diff />
      </div>
    ),
  });
});
