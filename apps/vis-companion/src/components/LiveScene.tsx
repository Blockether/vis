import { useEffect, useRef, useState } from 'react';
import type { LiveSceneNode } from '../lib/live-view';
import type { SceneEngine } from '../lib/scene-three';

/**
 * A live `scene`, drawn by three.js. The renderer loads on first use, so a view
 * without a scene never downloads it. Without WebGL the node says what it holds.
 */
export function LiveScene({ node }: { node: LiveSceneNode }) {
  const host = useRef<HTMLDivElement>(null);
  const engine = useRef<SceneEngine | null>(null);
  const latest = useRef(node);
  const [isUnavailable, setUnavailable] = useState(false);
  latest.current = node;

  useEffect(() => {
    let isMounted = true;
    import('../lib/scene-three')
      .then(({ createSceneEngine }) => {
        if (!isMounted || !host.current) return;
        engine.current = createSceneEngine(host.current);
        engine.current.update(latest.current);
      })
      .catch(() => {
        if (isMounted) setUnavailable(true);
      });
    return () => {
      isMounted = false;
      engine.current?.dispose();
      engine.current = null;
    };
  }, []);

  useEffect(() => {
    engine.current?.update(node);
  }, [node]);

  const summary = `Scene, ${node.objects.length} objects`;
  return (
    <div
      ref={host}
      role="img"
      aria-label={node.label || summary}
      className="relative w-full max-h-[70vh] overflow-hidden"
      style={{ aspectRatio: `${node.width} / ${node.height}`, background: node.background }}
    >
      {isUnavailable && (
        <p className="p-2 font-mono text-ui text-dialog-hint">{summary}. This device has no WebGL.</p>
      )}
    </div>
  );
}
