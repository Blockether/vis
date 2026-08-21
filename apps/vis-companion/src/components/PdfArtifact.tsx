import { memo, useCallback, useEffect, useState } from "react";

import type { GatewayClient } from "../lib/gateway";
import {
  renderPdfPage,
  stampPdfPage,
  type RenderedPage,
} from "../lib/pdf-annotate";
import { ImageViewer } from "./ImageViewer";
import { Button } from "./ui";

/** A PDF page fitted by the app, with edits saved and shown immediately. */
export const PdfAnnotator = memo(function PdfAnnotator({
  client,
  sid,
  iterationId,
  name,
  mediaType,
  url,
}: {
  client: GatewayClient;
  sid: string;
  iterationId: string;
  name: string;
  mediaType: string;
  url: string;
}) {
  const [page, setPage] = useState(1);
  const [source, setSource] = useState<ArrayBuffer | null>(null);
  const [rendered, setRendered] = useState<RenderedPage | null>(null);
  const [drawn, setDrawn] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [status, setStatus] = useState("");

  useEffect(() => {
    let live = true;
    setBusy(true);
    setStatus("");
    void (async () => {
      try {
        const bytes = await (await fetch(url)).arrayBuffer();
        const next = await renderPdfPage(bytes, page);
        if (!live) return;
        setSource(bytes);
        setRendered(next);
      } catch {
        if (live) setStatus("This PDF could not be opened.");
      } finally {
        if (live) setBusy(false);
      }
    })();
    return () => {
      live = false;
    };
  }, [url, page]);

  const apply = useCallback(
    async (edited: Blob) => {
      if (!source) return;
      setStatus("");
      try {
        const stamped = await stampPdfPage(
          source,
          page,
          await edited.arrayBuffer(),
        );
        const saved = await client.saveArtifactBytes(
          sid,
          iterationId,
          name,
          mediaType || "application/pdf",
          stamped,
        );
        const bytes = stamped.slice().buffer as ArrayBuffer;
        const next = await renderPdfPage(bytes, page);
        setSource(bytes);
        setRendered(next);
        setDrawn(null);
        setStatus(saved.version ? `Saved as v${saved.version}` : "Saved");
      } catch {
        setStatus("Could not save this revision.");
      }
    },
    [client, sid, iterationId, name, mediaType, source, page],
  );

  const pageCount = rendered?.pageCount ?? 0;
  return (
    <div className="flex min-h-0 min-w-0 flex-1 flex-col bg-input">
      <div className="flex min-h-0 flex-1 items-center justify-center overflow-hidden p-2 sm:p-4">
        {rendered ? (
          <img
            src={rendered.src}
            aria-label={`Page ${page} of ${name}`}
            className="block max-h-full max-w-full object-contain shadow-lg"
          />
        ) : (
          <p className="text-meta text-footer-muted">
            {busy ? "Rendering PDF…" : status || "PDF unavailable"}
          </p>
        )}
      </div>
      <div className="flex shrink-0 flex-wrap items-center gap-2 border-t border-dialog-edge px-3 py-2 sm:px-4">
        <Button
          type="button"
          variant="quiet"
          aria-label="Previous page"
          disabled={page <= 1 || busy}
          onClick={() => setPage((at) => Math.max(1, at - 1))}
        >
          Prev
        </Button>
        <span className="font-mono text-chip text-dialog-hint">
          {pageCount ? `Page ${page} of ${pageCount}` : `Page ${page}`}
        </span>
        <Button
          type="button"
          variant="quiet"
          aria-label="Next page"
          disabled={busy || (pageCount > 0 && page >= pageCount)}
          onClick={() => setPage((at) => at + 1)}
        >
          Next
        </Button>
        <Button
          type="button"
          aria-label={`Draw on page ${page}`}
          onClick={() => rendered && setDrawn(rendered.src)}
          disabled={busy || !rendered}
        >
          Draw on page {page}
        </Button>
        {status ? (
          <span role="status" className="text-meta text-dialog-hint">
            {status}
          </span>
        ) : null}
      </div>
      {drawn ? (
        <ImageViewer
          src={drawn}
          name={`${name} — page ${page}`}
          onApply={apply}
          onClose={() => setDrawn(null)}
        />
      ) : null}
    </div>
  );
});
