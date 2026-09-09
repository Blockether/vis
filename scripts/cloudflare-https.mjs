import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { isDeepStrictEqual } from 'node:util';

const phase = 'http_request_dynamic_redirect';
const isId = value => typeof value === 'string' && /^[a-f0-9]{32}$/.test(value);

function publicURL(value) {
  let url;
  try { url = new URL(value); } catch { /* Report configuration, not its contents. */ }
  if (!url || !/^https:\/\/[a-z0-9.-]+(?:[/?]|$)/.test(value) || url.username || url.password || url.port || url.hash || url.hostname.length > 253 || !/^(?:[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\.)+[a-z](?:[a-z0-9-]{0,61}[a-z0-9])?$/.test(url.hostname)) {
    throw new Error('Configure HTTPS_URL as a public HTTPS URL without credentials, port or fragment');
  }
  return url;
}

function redirectRule(hostname) {
  return {
    ref: 'vis_https_' + createHash('sha256').update(hostname).digest('hex').slice(0, 24),
    description: `Require HTTPS for ${hostname}`,
    expression: `(http.host eq "${hostname}" and http.request.scheme eq "http")`,
    action: 'redirect',
    action_parameters: {from_value: {
      target_url: {expression: 'concat("https://", http.host, http.request.uri.path)'},
      status_code: 308,
      preserve_query_string: true,
    }},
    enabled: true,
  };
}

function checkedRuleset(value) {
  if (!value || !isId(value.id) || !Array.isArray(value.rules) || value.phase !== phase || value.kind !== 'zone') throw new Error('Unexpected Cloudflare HTTPS ruleset response');
  return value;
}

// Only manage our hostname's rule; never replace the zone's rules or settings.
export async function configureHTTPS(env) {
  const {hostname} = publicURL(env.HTTPS_URL);
  if (!isId(env.CLOUDFLARE_ACCOUNT_ID) || !env.CLOUDFLARE_API_TOKEN || !env.WORKER_NAME) throw new Error('Configure Cloudflare credentials and WORKER_NAME');
  async function request(method, path, body) {
    let response, data;
    try {
      response = await fetch('https://api.cloudflare.com/client/v4' + path, {
        method, redirect: 'error', signal: AbortSignal.timeout(20000),
        headers: {Authorization: `Bearer ${env.CLOUDFLARE_API_TOKEN}`, 'Content-Type': 'application/json'},
        ...(body ? {body: JSON.stringify(body)} : {}),
      });
      data = await response.json();
    } catch { throw new Error('Cloudflare HTTPS API connection or response failed'); }
    if (!response.ok || data?.success !== true) {
      const codes = Array.isArray(data?.errors) ? data.errors.map(error => error?.code).filter(Number.isInteger) : [];
      const error = new Error(`Cloudflare HTTPS API failed (HTTP ${response.status}; codes ${codes.join(',')})`);
      error.status = response.status;
      error.codes = codes;
      throw error;
    }
    return data.result;
  }
  const domains = await request('GET', `/accounts/${env.CLOUDFLARE_ACCOUNT_ID}/workers/domains`);
  const matches = Array.isArray(domains) ? domains.filter(domain => domain?.hostname === hostname) : [];
  const domain = matches[0];
  if (matches.length !== 1 || domain.enabled !== true || domain.service !== env.WORKER_NAME || domain.environment !== 'production' || !isId(domain.zone_id)) throw new Error('HTTPS hostname must be an enabled production Custom Domain of WORKER_NAME in this account');
  const zonePath = `/zones/${domain.zone_id}/rulesets`;
  async function entrypoint() {
    try { return checkedRuleset(await request('GET', `${zonePath}/phases/${phase}/entrypoint`)); }
    catch (error) {
      if (error.status === 404 && error.codes.includes(10003)) return null;
      throw error;
    }
  }
  const rule = redirectRule(hostname);
  let ruleset = await entrypoint();
  if (!ruleset) {
    try {
      checkedRuleset(await request('POST', zonePath, {name: 'HTTPS redirects', kind: 'zone', phase, rules: [rule]}));
      return;
    } catch (error) {
      // Docs and relay can create the shared phase concurrently on first deploy.
      if (![400, 409].includes(error.status)) throw error;
      ruleset = await entrypoint();
      if (!ruleset) throw error;
    }
  }
  const owned = ruleset.rules.filter(existing => existing.ref === rule.ref);
  if (owned.length > 1 || (owned.length && !isId(owned[0].id))) throw new Error('Unexpected Cloudflare HTTPS rule identity');
  const existing = owned[0];
  if (existing && Object.keys(rule).every(key => isDeepStrictEqual(existing[key], rule[key]))) return;
  await request(existing ? 'PATCH' : 'POST', `${zonePath}/${ruleset.id}/rules${existing ? '/' + existing.id : ''}`, {...rule, position: {index: 1}});
}

// Inspect Location directly: following it would also accept HTTP serving content.
export async function verifyHTTPS(value, paths) {
  const url = publicURL(String(value));
  for (const path of paths ?? [url.pathname + url.search, '/__vis_https_probe__/a%2Fb?check=1&value=x%2Fy']) {
    const target = new URL(path, url);
    if (target.origin !== url.origin || target.hash) throw new Error('HTTPS verification paths must stay on the configured origin');
    const source = new URL(target);
    source.protocol = 'http:';
    for (const method of ['GET', 'HEAD']) {
      let response;
      try { response = await fetch(source, {method, redirect: 'manual', signal: AbortSignal.timeout(20000)}); }
      catch { throw new Error('HTTP to HTTPS verification connection failed'); }
      const location = response.headers.get('location');
      await response.body?.cancel();
      if (response.status !== 308 || location !== target.href) throw new Error('Expected HTTP 308 to the same HTTPS host, path and query');
    }
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  try {
    if (process.argv[2] === 'configure') await configureHTTPS(process.env);
    else if (process.argv[2] === 'verify') await verifyHTTPS(process.env.HTTPS_URL);
    else throw new Error('Use configure or verify');
    console.log('HTTPS redirect ' + process.argv[2] + ' succeeded');
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
