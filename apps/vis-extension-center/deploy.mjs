import { readFileSync, writeFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

export function deploymentConfig(base, env) {
  const database = env.CENTER_D1_DATABASE_ID;
  const siteKey = env.CENTER_TURNSTILE_SITE_KEY;
  const namespace = env.CENTER_RATE_LIMIT_NAMESPACE;
  if (!/^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(database || '') || /^0{8}-/.test(database)) throw new Error('Configure CENTER_D1_DATABASE_ID');
  if (!/^[A-Za-z0-9_-]{20,100}$/.test(siteKey || '') || siteKey.startsWith('1x000') || siteKey.startsWith('2x000') || siteKey.startsWith('3x000')) throw new Error('Configure a production CENTER_TURNSTILE_SITE_KEY');
  if (!/^[1-9][0-9]{0,8}$/.test(namespace || '')) throw new Error('Configure CENTER_RATE_LIMIT_NAMESPACE');
  return {...base, d1_databases: [{...base.d1_databases[0], database_id: database}], ratelimits: [{...base.ratelimits[0], namespace_id: namespace}], vars: {DOCS_URL: base.vars.DOCS_URL, TURNSTILE_SITE_KEY: siteKey}};
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const base = JSON.parse(readFileSync(new URL('./wrangler.jsonc', import.meta.url), 'utf8'));
  writeFileSync(new URL('./.deployment.json', import.meta.url), JSON.stringify(deploymentConfig(base, process.env), null, 2));
}
