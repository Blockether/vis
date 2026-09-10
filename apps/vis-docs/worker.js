import { renderPage } from './web/render.js';
import { security } from './headers.js';
import { sitemap, catalogText } from './web/discovery.js';
import { identity, inspectRepository, readBounded, RequestError } from './github.js';

const reply=(body,status=200,html=false,cache='no-store')=>new Response(html?body:JSON.stringify(body),{status,headers:{...security,'Content-Type':html?'text/html; charset=utf-8':'application/json; charset=utf-8','Cache-Control':cache}});

async function catalog(env,origin,ctx) {
  // Cache one canonical snapshot, not a new D1 scan for every filter/search URL.
  const cache=globalThis.caches?.default, key=new Request(origin+'/api/extensions');
  const saved=await cache?.match(key); if(saved) return saved.json();
  const {results}=await env.DB.prepare('SELECT id, metadata, added_at FROM extensions ORDER BY id').all();
  const data={extensions:results.map(row=>({...JSON.parse(row.metadata),added_at:row.added_at}))};
  if(cache) ctx.waitUntil(cache.put(key,reply(data,200,false,'public, max-age=60')));
  return data;
}
async function protectedSource(request,env,path) {
  const url=new URL(request.url);
  if(request.headers.get('Origin')!==url.origin) throw new RequestError('Submit from this Extension Center.',403);
  if(request.headers.get('Content-Type')?.split(';')[0]!=='application/json') throw new RequestError('Use application/json.',415);
  if(!env.TURNSTILE_SITE_KEY||!env.TURNSTILE_SECRET_KEY||!env.SUBMISSIONS_LIMITER) throw new RequestError('Submissions are not configured yet. Try again later.',503);
  const ip=request.headers.get('CF-Connecting-IP')||'unknown';
  if(!(await env.SUBMISSIONS_LIMITER.limit({key:ip})).success) throw new RequestError('Too many submissions. Wait a minute and try again.',429);
  let source; try {source=JSON.parse(await readBounded(request,4096));} catch(error) {if(error instanceof RequestError) throw error;throw new RequestError('Supply valid JSON.');}
  if(!source||Array.isArray(source)||typeof source!=='object'||Object.keys(source).some(key=>!['repository_url','subdirectory','revision','turnstile_token'].includes(key))) throw new RequestError('Supply a repository URL and optional project folder.');
  const token=source.turnstile_token;
  if(typeof token!=='string'||!token||token.length>2048) throw new RequestError('Complete the anti-spam check.',403);
  const action=path==='/api/preview'?'extension-preview':'extension-submit';
  const verification=await fetch('https://challenges.cloudflare.com/turnstile/v0/siteverify',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({secret:env.TURNSTILE_SECRET_KEY,response:token,remoteip:ip}),signal:AbortSignal.timeout(10000)});
  if(!verification.ok) throw new RequestError('Anti-spam verification is unavailable. Try again.',503);
  const result=JSON.parse(await readBounded(verification,8192));
  if(!result.success||result.hostname!==url.hostname||result.action!==action) throw new RequestError('Anti-spam check expired or failed. Try again.',403);
  if(path==='/api/submissions'&&!/^[0-9a-f]{40}$/.test(source.revision||'')) throw new RequestError('Review a pinned commit before submitting.');
  return source;
}
async function handle(request,env,ctx) {
  const url=new URL(request.url), path=url.pathname;
  const page=path==='/extensions/'||/^\/extensions\/[0-9a-f]{24}$/.test(path);
  if(['GET','HEAD'].includes(request.method)) {
    if(path==='/extensions') return new Response(null,{status:308,headers:{...security,Location:'/extensions/'+url.search}});
    if(path==='/extensions/sitemap.xml'||path==='/extensions/llms.txt') {
      const items=(await catalog(env,url.origin,ctx)).extensions;
      const xml=path.endsWith('.xml');
      return new Response(xml?sitemap(['/extensions/',...items.map(item=>'/extensions/'+item.id)]):catalogText(items),
        {headers:{...security,'Content-Type':xml?'application/xml; charset=utf-8':'text/plain; charset=utf-8','Cache-Control':'public, max-age=60'}});
    }
    if(page) {
      const data={items:[],search:url.search,siteKey:env.TURNSTILE_SITE_KEY||''};
      let status=200;
      try {
        data.items=(await catalog(env,url.origin,ctx)).extensions;
        if(path!=='/extensions/') {data.item=data.items.find(item=>item.id===path.split('/').at(-1));if(!data.item) {data.detailError=true;status=404;}}
      } catch {data.error='Could not load the catalog. Try again later.';data.detailError=path!=='/extensions/';status=503;}
      return reply(renderPage(data),status,true);
    }
    if(path==='/api/extensions') return reply(await catalog(env,url.origin,ctx),200,false,'public, max-age=60');
    if(/^\/api\/extensions\/[0-9a-f]{24}$/.test(path)) {
      const row=await env.DB.prepare('SELECT metadata, added_at FROM extensions WHERE id = ?').bind(path.split('/').at(-1)).first();
      return row?reply({...JSON.parse(row.metadata),added_at:row.added_at},200,false,'public, max-age=60'):reply({error:'Repository not listed.'},404);
    }
    if(path.startsWith('/api/')) return reply({error:'Not found.'},404);
    return env.ASSETS.fetch(request);
  }
  if(request.method!=='POST'||!['/api/preview','/api/submissions'].includes(path)) return reply({error:'Method not allowed.'},405);
  const source=await protectedSource(request,env,path);
  const metadata=await inspectRepository(source,env);
  if(path==='/api/preview') return reply(metadata);
  const now=new Date().toISOString(), id=await identity(metadata.id+'\n'+metadata.revision);
  await env.DB.prepare('INSERT INTO submissions (id, extension_id, revision, metadata, submitted_at) VALUES (?, ?, ?, ?, ?) ON CONFLICT(extension_id, revision) DO NOTHING').bind(id,metadata.id,metadata.revision,JSON.stringify({...metadata,checked_at:now}),now).run();
  return reply({id,status:'pending'},202);
}
export default {
  async fetch(request,env,ctx) {
    let response;
    try {response=await handle(request,env,ctx);} catch(error) {
      response=reply({error:error instanceof RequestError?error.message:'Catalog or GitHub is unavailable. Try again later.'},error instanceof RequestError?error.status:503);
    }
    if(response.status===429) response.headers.set('Retry-After','60');
    if(new URL(request.url).pathname.startsWith('/api/')||response.status>=400) {
      response=new Response(response.body,response);
      response.headers.set('X-Robots-Tag','noindex');
    }
    return request.method==='HEAD'?new Response(null,response):response;
  },
};
