import { attachmentBytes } from '../lib/artifacts';
import type { LiveMediaNode, LiveMediaSource } from '../lib/live-view';
import { ClipVideo, RecordingPlayer } from './Media';

function MediaVersion({
  source,
  kind,
  alt,
  version,
}: {
  source: LiveMediaSource;
  kind: LiveMediaNode['kind'];
  alt: string;
  version?: 'Before' | 'After';
}) {
  const name = version ? `${version}: ${alt}` : alt;
  const src = 'url' in source
    ? source.url
    : 'data' in source ? `data:${source.media_type};base64,${source.data}` : undefined;
  return (
    <div role="group" aria-label={name} className="min-w-0 space-y-2 border border-code-edge bg-code p-2">
      {version && <p className="font-mono text-meta text-dialog-hint">{version}</p>}
      {src ? (
        kind === 'audio' ? (
          <RecordingPlayer src={src} />
        ) : (
          <ClipVideo src={src} aria-label={name} controls className="max-h-[28rem] w-full" />
        )
      ) : (
        <p className="break-words font-mono text-meta text-dialog-hint">
          {alt} · {kind} · {source.media_type}{'bytes' in source && ` · ${attachmentBytes(source.bytes)}`}
        </p>
      )}
    </div>
  );
}

export function MediaNodeView({ node }: { node: LiveMediaNode }) {
  return node.before ? (
    <div className="grid min-w-0 gap-2 sm:grid-cols-2">
      <MediaVersion source={node.before} kind={node.kind} alt={node.alt} version="Before" />
      <MediaVersion source={node} kind={node.kind} alt={node.alt} version="After" />
    </div>
  ) : (
    <MediaVersion source={node} kind={node.kind} alt={node.alt} />
  );
}
