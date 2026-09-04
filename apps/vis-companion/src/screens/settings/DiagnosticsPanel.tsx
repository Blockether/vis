import { useState } from "react";

import { APP_BUILD_COMMIT, APP_BUILD_NUMBER } from "../../lib/build-info";
import {
  APP_MIN_GATEWAY_PROTOCOL,
  APP_PROTOCOL,
  APP_VERSION,
} from "../../lib/compat";
import { RETAINED_LOG_POLICY, exportDiagnostics } from "../../lib/diagnostics";
import { Banner, Button } from "../../components/ui";
import { SettingsPanel } from "./SettingsLayout";

/** ONE COMPACT FACT CELL, and the answer is the fact. Diagnostics is reference
 *  material, not a second settings form, so six facts pair into the same three-row
 *  rhythm as the machine panels instead of consuming six full-width rows. The label
 *  keeps the hint ink while the value takes the row's white and its weight: one
 *  `text-meta` size for both sides, with ink plus weight carrying the hierarchy.
 *  Each cell keeps its answer at the trailing edge; the one-pixel grid supplies the
 *  only internal rules, rather than boxing every fact. */
function DiagnosticFact({ label, value }: { label: string; value: string }) {
  return (
    <div className="flex min-w-0 items-baseline gap-2 bg-panel px-3 py-1.5 sm:px-4">
      <dt className="min-w-0 flex-1 break-words font-mono text-meta text-dialog-hint">
        {label}
      </dt>
      <dd className="max-w-[70%] break-words text-right font-mono text-meta font-bold text-white">
        {value}
      </dd>
    </div>
  );
}

/** Source identity, wire compatibility and the one deliberate way out for
 *  app-private logs — one band that opens, and nothing painted until it does.
 *
 *  The whole named band is the control. A trailing chevron turns to confirm the
 *  state, but it is not a tiny separate target beside inert copy. Reported in the
 *  app: pressing Diagnostics itself did nothing even though the neighbouring
 *  Application band opened as expected. Hidden remains HIDDEN — the facts, the
 *  trust sentence and the verb are not on the page until the band is pressed. */
export function DiagnosticsPanel({
  isOpen,
  onToggle,
}: {
  isOpen: boolean;
  onToggle: () => void;
}) {
  const [isExporting, setIsExporting] = useState(false);
  const [exported, setExported] = useState("");
  const [exportError, setExportError] = useState("");

  async function exportLogs() {
    setIsExporting(true);
    setExported("");
    setExportError("");
    try {
      setExported(await exportDiagnostics());
    } catch (cause) {
      setExportError(
        cause instanceof Error && cause.message
          ? cause.message
          : "The app logs could not be exported.",
      );
    } finally {
      setIsExporting(false);
    }
  }

  const foldLabel = isOpen ? "Hide diagnostics" : "Show diagnostics";

  return (
    <SettingsPanel
      title="Diagnostics"
      disclosure={{ isOpen, onToggle, label: foldLabel }}
    >
      {/* THE PANEL'S FACTS ARE A COMPACT MATRIX, NOT SIX SETTINGS ROWS. Related
          identity, protocol and retention facts pair across three shared rows; no
          fact disappears merely to make the panel quiet. The one sentence that
          survives is the trust promise beside the export verb it qualifies. */}
      {isOpen && (
        <>
          <dl
            aria-label="Application diagnostics"
            className="grid grid-cols-2 gap-px bg-dialog-edge"
          >
            <DiagnosticFact label="Version" value={APP_VERSION} />
            <DiagnosticFact label="Build" value={APP_BUILD_NUMBER} />
            <DiagnosticFact label="Commit" value={APP_BUILD_COMMIT} />
            <DiagnosticFact
              label="Gateway protocol"
              value={`${APP_MIN_GATEWAY_PROTOCOL}+`}
            />
            <DiagnosticFact label="Client protocol" value={`${APP_PROTOCOL}`} />
            <DiagnosticFact
              label="Retention"
              value={`${RETAINED_LOG_POLICY.days} days · ${RETAINED_LOG_POLICY.megabytes} MB`}
            />
          </dl>
          <div className="space-y-2 p-3 sm:p-4">
            <p className="font-mono text-meta text-dialog-hint">
              No credentials or request bodies are recorded.
            </p>
            {exported && <Banner kind="ok">{exported}</Banner>}
            {exportError && <Banner kind="err">{exportError}</Banner>}
            <Button
              variant="secondary"
              density="panel"
              className="w-full"
              disabled={isExporting}
              aria-busy={isExporting}
              onClick={() => void exportLogs()}
            >
              {isExporting ? "Preparing logs…" : "Export app logs"}
            </Button>
          </div>
        </>
      )}
    </SettingsPanel>
  );
}
