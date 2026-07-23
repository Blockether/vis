// Pairing-QR scanning that actually works on Capacitor 8 iOS.
//
// The ML Kit barcode plugin needs GoogleMLKit, which is CocoaPods-only and does
// NOT link into the SPM iOS project — so on iPhone `isSupported()` is always
// false. We therefore capture a still with the SPM-native @capacitor/camera and
// decode it in JS with jsQR. ML Kit is still preferred on Android (live scan).
import jsQR from 'jsqr';
import { Capacitor } from '@capacitor/core';

async function scanWithMlKit(): Promise<string | null> {
  try {
    const { BarcodeScanner } = await import('@capacitor-mlkit/barcode-scanning');
    const supported = await BarcodeScanner.isSupported();
    if (!supported.supported) return null;
    const perm = await BarcodeScanner.requestPermissions();
    if (perm.camera !== 'granted' && perm.camera !== 'limited') return null;
    const { barcodes } = await BarcodeScanner.scan();
    return barcodes[0]?.rawValue ?? null;
  } catch {
    // Plugin missing or scan dismissed — fall through to the camera path.
    return null;
  }
}

function decodeQrFromBase64(base64: string, format: string): Promise<string | null> {
  return new Promise((resolve, reject) => {
    const img = new Image();
    img.onload = () => {
      const canvas = document.createElement('canvas');
      canvas.width = img.naturalWidth;
      canvas.height = img.naturalHeight;
      const ctx = canvas.getContext('2d');
      if (!ctx) {
        resolve(null);
        return;
      }
      ctx.drawImage(img, 0, 0);
      const { data, width, height } = ctx.getImageData(0, 0, canvas.width, canvas.height);
      const code = jsQR(data, width, height, { inversionAttempts: 'attemptBoth' });
      resolve(code?.data ?? null);
    };
    img.onerror = () => reject(new Error('Could not read the captured photo'));
    img.src = `data:image/${format || 'jpeg'};base64,${base64}`;
  });
}

async function scanWithCamera(): Promise<string | null> {
  const { Camera, CameraResultType, CameraSource } = await import('@capacitor/camera');
  const permission = await Camera.requestPermissions({ permissions: ['camera'] });
  if (permission.camera === 'denied') {
    throw new Error('Camera access was denied — enable it in Settings');
  }
  const photo = await Camera.getPhoto({
    quality: 100,
    allowEditing: false,
    resultType: CameraResultType.Base64,
    source: CameraSource.Camera,
    saveToGallery: false,
  });
  if (!photo.base64String) return null;
  return decodeQrFromBase64(photo.base64String, photo.format);
}

// Returns the decoded pairing string, or null when no QR was found.
// Throws only on a real camera error (permission denied); user cancellation
// surfaces as a thrown "cancel" message the caller can silently ignore.
export async function scanQr(): Promise<string | null> {
  const viaMlKit = await scanWithMlKit();
  if (viaMlKit) return viaMlKit;
  if (Capacitor.isNativePlatform()) return scanWithCamera();
  return null;
}

export async function scanSupported(): Promise<boolean> {
  if (Capacitor.isNativePlatform()) return true;
  try {
    const mod = await import('@capacitor-mlkit/barcode-scanning');
    const { supported } = await mod.BarcodeScanner.isSupported();
    return supported;
  } catch {
    return false;
  }
}
