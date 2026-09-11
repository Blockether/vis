import { readFileSync, writeFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { verifyHTTPS } from '../../scripts/cloudflare-https.mjs';

function docsOrigin(hostname) {
  if (typeof hostname !== 'string' || hostname.length > 253 || !/^(?:[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\.)+[a-z](?:[a-z0-9-]{0,61}[a-z0-9])?$/.test(hostname)) throw new Error('Configure DOCS_HOSTNAME as a public hostname without scheme, port or path');
  return new URL('https://' + hostname);
}
export function deploymentConfig(base, env) {
  const database = env.DOCS_D1_DATABASE_ID;
  const siteKey = env.DOCS_TURNSTILE_SITE_KEY;
  const namespace = env.DOCS_RATE_LIMIT_NAMESPACE;
  if (!/^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(database || '') || /^0{8}-/.test(database)) throw new Error('Configure DOCS_D1_DATABASE_ID');
  if (!/^[A-Za-z0-9_-]{20,100}$/.test(siteKey || '') || siteKey.startsWith('1x000') || siteKey.startsWith('2x000') || siteKey.startsWith('3x000')) throw new Error('Configure a production DOCS_TURNSTILE_SITE_KEY');
  if (!/^[1-9][0-9]{0,8}$/.test(namespace || '')) throw new Error('Configure DOCS_RATE_LIMIT_NAMESPACE');
  const hostname = docsOrigin(env.DOCS_HOSTNAME).hostname;
  return {...base, workers_dev: false, routes: [{pattern: hostname, custom_domain: true}], d1_databases: [{...base.d1_databases[0], database_id: database}], ratelimits: [{...base.ratelimits[0], namespace_id: namespace}], vars: {TURNSTILE_SITE_KEY: siteKey}};
}

export async function verifyDeployment(hostname) {
  const origin = docsOrigin(hostname);
  for (let attempt = 0; attempt < 8; attempt++) {
    try {
      for (const [path, marker] of [
        ['/', 'class="center-link" href="/extensions/"'],
        ['/extending.html', 'Extending Vis'],
        ['/extensions/', 'id="catalog-data"'],
        ['/assets/theme.css', ':root'],
        ['/assets/prism.min.js', 'Prism'],
        ['/robots.txt', 'Sitemap:'],
        ['/sitemap.xml', '<sitemapindex'],
        ['/sitemap-docs.xml', '<urlset'],
        ['/extensions/sitemap.xml', '<urlset'],
        ['/llms.txt', '## Documentation'],
        ['/llms-full.txt', '# Vis documentation'],
        ['/extensions/llms.txt', '# Vis Extension Center'],
        ['/extending.md', '# Extending Vis'],
        ['/site.webmanifest', '"icons"'],
      ]) {
        const response = await fetch(new URL(path, origin), {signal: AbortSignal.timeout(20000), redirect: 'error'});
        const content = await response.text();
        if (!response.ok || !content.includes(marker) || (path === '/' && content.includes('id="catalog-data"'))) throw new Error('Public site is not ready');
      }
      for (const path of ['/favicon.ico','/favicon-32.png','/favicon-48.png','/apple-touch-icon.png']) {
        const response=await fetch(new URL(path,origin),{signal:AbortSignal.timeout(20000),redirect:'error'});
        const bytes=new Uint8Array(await response.arrayBuffer());
        const signature=path.endsWith('.ico')?[0,0,1,0]:[137,80,78,71,13,10,26,10];
        if(!response.ok||!signature.every((byte,index)=>bytes[index]===byte)) throw new Error('Site icons are not ready');
      }
      const api = await fetch(new URL('/api/extensions', origin), {signal: AbortSignal.timeout(20000), redirect: 'error'});
      if (!api.ok || !Array.isArray((await api.json()).extensions)) throw new Error('Catalog is not ready');
      await verifyHTTPS(origin, ['/', '/extensions/', '/assets/theme.css', '/api/extensions', '/extensions/a%2Fb?check=1&value=x%2Fy']);
      return;
    } catch {
      if (attempt === 7) throw new Error('Deployed docs, assets, catalog and HTTPS redirect verification failed after 8 attempts');
      await new Promise(resolve => setTimeout(resolve, 5000));
    }
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  if (process.argv[2] === '--verify') {
    await verifyDeployment(process.env.DOCS_HOSTNAME);
    console.log('Deployed documentation, assets and catalog verified');
  } else {
    const base = JSON.parse(readFileSync(new URL('./wrangler.jsonc', import.meta.url), 'utf8'));
    writeFileSync(new URL('./.deployment.json', import.meta.url), JSON.stringify(deploymentConfig(base, process.env), null, 2));
  }
}
