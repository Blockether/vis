export type GatewayPairing = {
  gatewayUrl: string;
  token?: string;
};

const urlWithoutTrailingSlash = (u: URL, query = ""): string => {
  const path = u.pathname.replace(/\/+$/, "");
  return `${u.protocol}//${u.host}${path === "/" ? "" : path}${query ? `?${query}` : ""}`;
};

const normalizeUrl = (raw: string): string => {
  const trimmed = raw.trim();
  if (!trimmed) throw new Error("pairing QR did not include a gateway URL");
  const withScheme = /^https?:\/\//i.test(trimmed)
    ? trimmed
    : `http://${trimmed}`;
  return urlWithoutTrailingSlash(new URL(withScheme));
};

const fromHttpUrl = (raw: string): GatewayPairing => {
  const u = new URL(raw.trim());
  const token =
    u.searchParams.get("token") ??
    u.searchParams.get("bearer") ??
    u.searchParams.get("auth") ??
    undefined;
  u.searchParams.delete("token");
  u.searchParams.delete("bearer");
  u.searchParams.delete("auth");
  u.hash = "";
  const query = u.searchParams.toString();
  return {
    gatewayUrl: urlWithoutTrailingSlash(u, query),
    ...(token ? { token } : {}),
  };
};

const fromVisUrl = (raw: string): GatewayPairing => {
  const u = new URL(raw.trim());
  const url =
    u.searchParams.get("url") ??
    u.searchParams.get("gateway_url") ??
    u.searchParams.get("gatewayUrl");
  if (!url) throw new Error("pairing QR did not include a gateway URL");
  const token =
    u.searchParams.get("token") ?? u.searchParams.get("bearer") ?? undefined;
  return { gatewayUrl: normalizeUrl(url), ...(token ? { token } : {}) };
};

const fromJson = (raw: string): GatewayPairing => {
  const body = JSON.parse(raw) as Record<string, unknown>;
  const url =
    body.url ??
    body.gateway_url ??
    body.gatewayUrl ??
    body.base_url ??
    body.baseUrl;
  if (typeof url !== "string")
    throw new Error("pairing JSON did not include a gateway URL");
  const token = body.token ?? body.bearer ?? body.secret;
  return {
    gatewayUrl: normalizeUrl(url),
    ...(typeof token === "string" && token.trim()
      ? { token: token.trim() }
      : {}),
  };
};

export const parseGatewayPairing = (raw: string): GatewayPairing => {
  const text = raw.trim();
  if (!text) throw new Error("empty pairing QR");
  if (text.startsWith("{")) return fromJson(text);
  if (/^vis:\/\//i.test(text)) return fromVisUrl(text);
  if (/^https?:\/\//i.test(text)) return fromHttpUrl(text);
  return { gatewayUrl: normalizeUrl(text) };
};

export const pairingDisplayHost = (pairing: GatewayPairing): string => {
  try {
    return new URL(pairing.gatewayUrl).host;
  } catch {
    return pairing.gatewayUrl;
  }
};
