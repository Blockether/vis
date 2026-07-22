// QR scanning is optional: on a device with the ML Kit barcode plugin present
// we scan a pairing QR; otherwise the UI falls back to paste/manual entry. The
// plugin is a dynamic import so the web build never hard-depends on it.

export async function scanQr(): Promise<string | null> {
  try {
    const mod = await import('@capacitor-mlkit/barcode-scanning');
    const { BarcodeScanner } = mod;
    const supported = await BarcodeScanner.isSupported();
    if (!supported.supported) return null;
    const perm = await BarcodeScanner.requestPermissions();
    if (perm.camera !== 'granted' && perm.camera !== 'limited') return null;
    const { barcodes } = await BarcodeScanner.scan();
    return barcodes[0]?.rawValue ?? null;
  } catch {
    // Plugin missing (web) or scan cancelled.
    return null;
  }
}

export async function scanSupported(): Promise<boolean> {
  try {
    const mod = await import('@capacitor-mlkit/barcode-scanning');
    const { supported } = await mod.BarcodeScanner.isSupported();
    return supported;
  } catch {
    return false;
  }
}
