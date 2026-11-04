import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { TranscriptExport } from './screens/TranscriptExport';
import { GatewayClient } from './lib/gateway';
import { applyTheme } from './lib/theme';
import { DEFAULT_THEME } from './lib/themes.generated';
import { pinTranscriptDisplay } from './lib/transcript-display';
import {
  parseTranscriptExport,
  SNAPSHOT_GATEWAY,
  snapshotFetch,
  TRANSCRIPT_EXPORT_DATA_ID,
} from './lib/transcript-export';
import './index.css';

// An export always shows Compact steps with their code, whatever this browser stored before.
pinTranscriptDisplay();
// An export opens in a browser tab, so it takes the tab styling of the web app (`web:`).
document.documentElement.dataset.host = 'web';
const root = document.getElementById('root')!;
try {
  const data = parseTranscriptExport(document.getElementById(TRANSCRIPT_EXPORT_DATA_ID)?.textContent);
  // A theme is a device setting of the web app, so an export takes the default palette.
  applyTheme(DEFAULT_THEME);
  if (data.session.title) document.title = data.session.title;
  // The web app's own client reads the artifacts, and the export answers it.
  window.fetch = snapshotFetch(data, window.fetch.bind(window));
  const client = new GatewayClient({ url: SNAPSHOT_GATEWAY });
  createRoot(root).render(
    <StrictMode>
      <TranscriptExport data={data} client={client} />
    </StrictMode>,
  );
} catch (cause) {
  root.textContent = `This transcript cannot be shown: ${(cause as Error).message}`;
}
