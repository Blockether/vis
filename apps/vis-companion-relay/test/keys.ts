/** Real keys, generated per run: the tests verify signatures, not shapes. */

function pem(label: string, der: ArrayBuffer): string {
  const bytes = new Uint8Array(der);
  let binary = "";
  for (const byte of bytes) binary += String.fromCharCode(byte);
  const body = btoa(binary).replace(/(.{64})/g, "$1\n");
  return `-----BEGIN ${label}-----\n${body}\n-----END ${label}-----\n`;
}

export async function generateEs256(): Promise<{ pem: string; publicKey: CryptoKey }> {
  const pair = (await crypto.subtle.generateKey({ name: "ECDSA", namedCurve: "P-256" }, true, [
    "sign",
    "verify",
  ])) as CryptoKeyPair;
  return {
    pem: pem("PRIVATE KEY", (await crypto.subtle.exportKey("pkcs8", pair.privateKey)) as ArrayBuffer),
    publicKey: pair.publicKey,
  };
}

export async function generateRs256(): Promise<{ pem: string; publicKey: CryptoKey }> {
  const pair = (await crypto.subtle.generateKey(
    {
      name: "RSASSA-PKCS1-v1_5",
      modulusLength: 2048,
      publicExponent: new Uint8Array([1, 0, 1]),
      hash: "SHA-256",
    },
    true,
    ["sign", "verify"],
  )) as CryptoKeyPair;
  return {
    pem: pem("PRIVATE KEY", (await crypto.subtle.exportKey("pkcs8", pair.privateKey)) as ArrayBuffer),
    publicKey: pair.publicKey,
  };
}

export function decodeJwt(jwt: string): { header: Record<string, unknown>; claims: Record<string, unknown> } {
  const [header, claims] = jwt.split(".");
  const decode = (part: string) =>
    JSON.parse(Buffer.from(part.replace(/-/g, "+").replace(/_/g, "/"), "base64").toString("utf8"));
  return { header: decode(header), claims: decode(claims) };
}

export async function verifyJwt(jwt: string, publicKey: CryptoKey, alg: "ES256" | "RS256"): Promise<boolean> {
  const [header, claims, signature] = jwt.split(".");
  const raw = Buffer.from(signature.replace(/-/g, "+").replace(/_/g, "/"), "base64");
  return await crypto.subtle.verify(
    alg === "ES256" ? { name: "ECDSA", hash: "SHA-256" } : { name: "RSASSA-PKCS1-v1_5" },
    publicKey,
    raw,
    new TextEncoder().encode(`${header}.${claims}`),
  );
}
