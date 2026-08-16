import { memo, useCallback, useState } from "react";

import type { GatewayClient } from "../lib/gateway";
import { renderPdfPage, stampPdfPage } from "../lib/pdf-annotate";
import { ImageViewer } from "./ImageViewer";
import { Button } from "./ui";

/**
 * A PDF YOU CAN DRAW ON.
 *
 * The document itself is read in its own frame, as before. `Draw` rasterises
 * the page that is being read, hands it to the app's existing pen (the image
 * viewer already owns zoom, pan, colours and undo), and stamps the flattened
 * result back onto that page — so what is saved is a PDF, under the same
 * filename, which the gateway files as the artifact's NEXT VERSION exactly the
 * way an annotated markdown note is.
 */
export const PdfAnnotator = memo(function PdfAnnotator({
  client,
  sid,
  iterationId,
  name,
  mediaType,
  url,
  frame,
}: {
  client: GatewayClient;
  sid: string;
  iterationId: string;
  name: string;
  mediaType: string;
  /** The artifact's own bytes. */
  url: string;
  /** The reading view — supplied by the caller so this file owns only the pen. */
  frame: React.ReactNode;
}) {
  const [page, setPage] = useState(1);
  const [pageCount, setPageCount] = useState(0);
  const [drawn, setDrawn] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [status, setStatus] = useState("");

  const draw = useCallback(async () => {
    setBusy(true);
    setStatus("");
    try {
      const bytes = await (await fetch(url)).arrayBuffer();
      const rendered = await renderPdfPage(bytes, page);
      setPageCount(rendered.pageCount);
      setDrawn(rendered.src);
    } catch {
      setStatus("This page could not be opened for drawing.");
    } finally {
      setBusy(false);
    }
  }, [url, page]);

  const apply = useCallback(
    async (edited: Blob) => {
      setStatus("");
      try {
        const original = await (await fetch(url)).arrayBuffer();
        const stamped = await stampPdfPage(
          original,
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
        setStatus(saved.version ? `Saved as v${saved.version}` : "Saved");
      } catch {
        setStatus("Could not save this revision.");
      }
    },
    [client, sid, iterationId, name, mediaType, url, page],
  );

  const pages = Math.max(pageCount, page);

  return (
    <div className="flex min-h-0 min-w-0 flex-1 flex-col">
      {frame}
      <div className="flex shrink-0 flex-wrap items-center gap-2 border-t border-dialog-edge px-3 py-2 sm:px-4">
        <Button
          type="button"
          variant="quiet"
          aria-label="Previous page"
          disabled={page <= 1}
          onClick={() => setPage((at) => Math.max(1, at - 1))}
        >
          Prev
        </Button>
        <span className="font-mono text-chip text-dialog-hint">
          {pageCount ? `Page ${page} of ${pages}` : `Page ${page}`}
        </span>
        <Button
          type="button"
          variant="quiet"
          aria-label="Next page"
          disabled={pageCount > 0 && page >= pageCount}
          onClick={() => setPage((at) => at + 1)}
        >
          Next
        </Button>
        <Button
          type="button"
          aria-label={`Draw on page ${page}`}
          onClick={draw}
          disabled={busy}
        >
          {busy ? "Opening…" : `Draw on page ${page}`}
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
