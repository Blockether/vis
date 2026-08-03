/**
 * The two JWTs this relay signs: Apple's ES256 provider token and Google's
 * RS256 service-account assertion. WebCrypto only — no npm crypto, no polyfill.
 *
 * WebCrypto's ECDSA signature is already the raw `r || s` JOSE form Apple
 * wants, so unlike a JCA/OpenSSL signer there is no DER unwrapping here.
 */

const encoder = new TextEncoder();

export function base64url(bytes: Uint8Array): string {
  let binary = "";
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

export function base64urlText(text: string): string {
  return base64url(encoder.encode(text));
}

/** A PKCS#8 PEM (`.p8` from Apple, `private_key` from Google) as raw DER. */
export function pemToPkcs8(pem: string): ArrayBuffer {
  const body = pem
    .replace(/-----[A-Z ]+-----/g, "")
    .replace(/\\n/g, "")
    .replace(/\s+/g, "");
  const binary = atob(body);
  const der = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) der[i] = binary.charCodeAt(i);
  return der.buffer;
}

export type JwtAlgorithm = "ES256" | "RS256";

export async function signJwt(
  alg: JwtAlgorithm,
  pem: string,
  header: Record<string, unknown>,
  claims: Record<string, unknown>,
): Promise<string> {
  const key = await crypto.subtle.importKey(
    "pkcs8",
    pemToPkcs8(pem),
    alg === "ES256"
      ? { name: "ECDSA", namedCurve: "P-256" }
      : { name: "RSASSA-PKCS1-v1_5", hash: "SHA-256" },
    false,
    ["sign"],
  );
  const signingInput = `${base64urlText(JSON.stringify({ alg, ...header }))}.${base64urlText(
    JSON.stringify(claims),
  )}`;
  const signature = await crypto.subtle.sign(
    alg === "ES256" ? { name: "ECDSA", hash: "SHA-256" } : { name: "RSASSA-PKCS1-v1_5" },
    key,
    encoder.encode(signingInput),
  );
  return `${signingInput}.${base64url(new Uint8Array(signature))}`;
}

export async function sha256Hex(text: string): Promise<string> {
  const digest = await crypto.subtle.digest("SHA-256", encoder.encode(text));
  return [...new Uint8Array(digest)].map((b) => b.toString(16).padStart(2, "0")).join("");
}
