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

/** ONE FACT ROW, and the answer is the fact. The label is the same word in every
 *  build — Version, Commit — so it takes the hint ink while the value takes the
 *  row's white and its weight: one size, the panel's own mono `text-ui` voice,
 *  and ink plus weight carry the hierarchy a second size would only flatten.
 *  Labels take the spare width; values hold the trailing edge. The row is FLEX,
 *  not grid: the value's percentage cap resolves against the row here, while a
 *  grid AUTO track cannot resolve it and collapses to a two-character column
 *  that wraps every answer. */
function DiagnosticFact({ label, value }: { label: string; value: string }) {
  return (
    <div className="flex min-w-0 items-baseline gap-4 px-3 py-2 sm:px-4">
      <dt className="min-w-0 flex-1 break-words font-mono text-ui text-dialog-hint">
        {label}
      </dt>
      <dd className="max-w-[65%] break-words text-right font-mono text-ui font-bold text-white">
        {value}
      </dd>
    </div>
  );
}

/** Source identity, wire compatibility and the one deliberate way out for app-private logs. */
export function DiagnosticsPanel() {
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

  return (
    <SettingsPanel title="Diagnostics" meta="app logs">
      {/* THE PANEL'S FACTS ARE ROWS, NOT A PARAGRAPH UNDER THEM. This band used
          to explain itself with three sentences of grey prose — retention, cap,
          what the export button does — the same description-under-the-band that
          was reported as pointless over the rest of this dialog. A number the
          reader can scan is a row; the one sentence that survives is the trust
          promise beside the verb it qualifies. The wire fact split the same
          way: "Protocol 12+ · must accept client 12" was a sentence wrapping
          itself ragged in the trailing column, and each half is one fact that
          never wraps. */}
      <dl className="divide-y divide-dialog-edge">
        <DiagnosticFact label="Version" value={APP_VERSION} />
        <DiagnosticFact label="Build" value={APP_BUILD_NUMBER} />
        <DiagnosticFact label="Commit" value={APP_BUILD_COMMIT} />
        <DiagnosticFact
          label="Gateway protocol"
          value={`${APP_MIN_GATEWAY_PROTOCOL}+`}
        />
        <DiagnosticFact label="Client protocol" value={`${APP_PROTOCOL}`} />
        <DiagnosticFact
          label="Log retention"
          value={`${RETAINED_LOG_POLICY.days} days · ${RETAINED_LOG_POLICY.megabytes} MB`}
        />
      </dl>
      <div className="space-y-2 p-3 sm:p-4">
        <p className="font-mono text-ui text-dialog-hint">
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
    </SettingsPanel>
  );
}
