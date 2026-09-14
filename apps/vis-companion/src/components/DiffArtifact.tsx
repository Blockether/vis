import { memo, useEffect, useState } from 'react';

import { DIFF_MEDIA } from '../lib/artifacts';
import { annotationDraftKey } from '../lib/annotation-drafts';
import { parseDiff, diffReviewRequest, diffSourceLabel, type DiffEnvelope } from '../lib/diff';
import type { GatewayClient } from '../lib/gateway';
import { MarkdownAnnotator, PlainText, type DocumentChrome } from './MarkdownArtifact';
import { readArtifactText } from './TextArtifact';

/** A real patch snapshot. Human revisions replace only its separate comments array. */
export const DiffArtifact = memo(function DiffArtifact({
  client,
  sid,
  iterationId,
  name,
  source,
  version,
  commentable = false,
  chrome,
}: {
  client: GatewayClient;
  sid: string;
  iterationId: string;
  name: string;
  source: Blob | string;
  version?: number;
  commentable?: boolean;
  chrome: DocumentChrome;
}) {
  const [loaded, setLoaded] = useState<{
    source: Blob | string;
    envelope: DiffEnvelope;
  } | null>(null);
  const [error, setError] = useState('');
  useEffect(() => {
    let alive = true;
    setError('');
    readArtifactText(source)
      .then(parseDiff)
      .then((envelope) => {
        if (alive) setLoaded({ source, envelope });
      })
      .catch(() => {
        if (alive) setError('This diff could not be read. Ask for a new snapshot.');
      });
    return () => {
      alive = false;
    };
  }, [source]);
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
  const envelope = loaded.envelope;
  const sourceLabel = diffSourceLabel(envelope.source);
  if (!commentable || !version)
    return chrome({
      actions: null,
      note: `Diff · ${sourceLabel}`,
      body: (
        <div className="min-h-0 min-w-0 flex-1 overflow-y-auto bg-panel p-3 sm:p-4">
          <PlainText text={envelope.patch} diff />
          {envelope.comments.length ? (
            <ul aria-label="Review comments" className="mt-3 space-y-2 text-body text-foreground">
              {envelope.comments.map((comment, at) => (
                <li key={at}>
                  <blockquote>{comment.quote}</blockquote>
                  {comment.body}
                </li>
              ))}
            </ul>
          ) : null}
        </div>
      ),
    });
  return (
    <MarkdownAnnotator
      key={`${name}:${version}`}
      text={envelope.patch}
      initialComments={envelope.comments}
      plain
      chrome={chrome}
      draftKey={annotationDraftKey(client.base, sid, iterationId, `${name}:v${version}`)}
      review={{
        version,
        sourceLabel,
        onSend: async (savedVersion) => {
          await client.submitTurn(sid, diffReviewRequest(name, savedVersion));
        },
      }}
      onSave={async (_text, comments) => {
        const saved = await client.saveArtifactText(
          sid,
          iterationId,
          name,
          DIFF_MEDIA,
          JSON.stringify({ ...envelope, comments: comments ?? [] }),
        );
        return saved.version;
      }}
    />
  );
});
