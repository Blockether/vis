import { Capacitor } from '@capacitor/core';
import { downloadArtifact, shareArtifact, sharedFilename } from './artifact-share';
import { isXlsxMedia, XLSX_MEDIA } from './artifacts';
import { desktopInvoke } from './desktop';

/** Browsers can save a workbook, but cannot launch an installed desktop app. */
export function spreadsheetOpenVerb(): 'Open in app' | 'Download' {
  return desktopInvoke() || Capacitor.isNativePlatform() ? 'Open in app' : 'Download';
}

/** Give the original workbook to the platform, without a remote document viewer. */
export async function openSpreadsheet(blob: Blob, name: string, mediaType = ''): Promise<string> {
  if (!isXlsxMedia(mediaType || blob.type, name)) throw new Error('Not an XLSX workbook.');
  const safe = sharedFilename(name);
  // A MIME-only workbook still needs the extension used by the OS association.
  const filename = /\.xlsx$/i.test(safe) ? safe : `${safe}.xlsx`;
  const invoke = desktopInvoke();
  if (invoke) {
    const result = (await invoke('open_xlsx', {
      filename,
      data: Array.from(new Uint8Array(await blob.arrayBuffer())),
    })) as { opened: boolean };
    return result.opened
      ? 'Workbook opened in your spreadsheet app.'
      : 'Workbook saved in Downloads/Vis Artifacts. Install a spreadsheet app to open it.';
  }
  if (Capacitor.isNativePlatform()) {
    return shareArtifact(blob, filename, XLSX_MEDIA, {
      title: name,
      dialogTitle: 'Open workbook in an app',
      noun: 'Workbook',
      retainCacheFile: true,
    });
  }
  return downloadArtifact(blob, filename, 'Workbook');
}
