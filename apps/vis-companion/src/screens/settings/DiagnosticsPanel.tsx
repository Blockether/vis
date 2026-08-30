import { useState } from "react";

import { APP_BUILD_COMMIT, APP_BUILD_NUMBER } from "../../lib/build-info";
import {
  APP_MIN_GATEWAY_PROTOCOL,
  APP_PROTOCOL,
  APP_VERSION,
} from "../../lib/compat";
import { exportDiagnostics } from "../../lib/diagnostics";
import { Banner, Button } from "../../components/ui";
import { SettingsPanel } from "./SettingsLayout";

function DiagnosticFact({ label, value }: { label: string; value: string }) {
  return (
    <div className="flex min-h-12 min-w-0 items-center gap-3 px-3 py-2 mouse:min-h-9 mouse:py-1.5">
      <dt className="min-w-0 flex-1 text-body text-white">{label}</dt>
      <dd className="max-w-[65%] break-words text-right font-mono text-ui text-dialog-hint">
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
      <dl className="divide-y divide-dialog-edge">
        <DiagnosticFact label="Version" value={APP_VERSION} />
        <DiagnosticFact label="Build" value={APP_BUILD_NUMBER} />
        <DiagnosticFact label="Commit" value={APP_BUILD_COMMIT} />
        <DiagnosticFact
          label="Gateway compatibility"
          value={`Protocol ${APP_MIN_GATEWAY_PROTOCOL}+ · must accept client ${APP_PROTOCOL}`}
        />
      </dl>
      <div className="space-y-2 px-3 py-3">
        <p className="text-body text-dialog-hint">
          App events stay in app-private files for seven days, capped at 8 MB.
          Export uses the system share sheet or saves a file. Gateway
          credentials and request bodies are not recorded.
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
