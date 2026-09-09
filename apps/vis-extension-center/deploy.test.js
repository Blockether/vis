import { expect, test } from 'vitest';
import { readFileSync } from 'node:fs';
import { deploymentConfig } from './deploy.mjs';
const base=JSON.parse(readFileSync('wrangler.jsonc','utf8'));
const env={CENTER_D1_DATABASE_ID:'12345678-1234-1234-1234-123456789abc',CENTER_TURNSTILE_SITE_KEY:'production-public-site-key',CENTER_RATE_LIMIT_NAMESPACE:'1002'};
test('deployment configuration whitelists public values only',()=>{
  const config=deploymentConfig(base,{...env,CLOUDFLARE_API_TOKEN:'private-deploy-canary',TURNSTILE_SECRET_KEY:'private-turnstile-canary',GITHUB_TOKEN:'private-github-canary'});
  expect(config.d1_databases[0].database_id).toBe(env.CENTER_D1_DATABASE_ID);
  expect(config.ratelimits[0].namespace_id).toBe('1002');
  expect(JSON.stringify(config)).not.toContain('private-');
  expect(config.vars).toEqual({DOCS_URL:base.vars.DOCS_URL,TURNSTILE_SITE_KEY:env.CENTER_TURNSTILE_SITE_KEY});
});
test('deployment fails closed for missing configuration and testing keys',()=>{
  for(const key of Object.keys(env)) expect(()=>deploymentConfig(base,{...env,[key]:''})).toThrow();
  expect(()=>deploymentConfig(base,{...env,CENTER_D1_DATABASE_ID:base.d1_databases[0].database_id})).toThrow();
  expect(()=>deploymentConfig(base,{...env,CENTER_TURNSTILE_SITE_KEY:'1x00000000000000000000AA'})).toThrow();
});
