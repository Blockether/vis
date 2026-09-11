import { renderPage } from './web/render.js';
import { security } from './headers.js';
import { sitemap, catalogText } from './web/discovery.js';
import { inspectRepository, RequestError } from './github.js';
import { discoverReleases, extensionDetail, queueRelease, refreshRepositoryStats, withRepositoryStats } from './releases.js';
import { protectedBody } from './antispam.js';
import { readCommunity, writeCommunity } from './community.js';

const reply=(body,status=200,html=false,cache='no-store')=>new Response(html?body:JSON.stringify(body),{status,headers:{...security,'Content-Type':html?'text/html; charset=utf-8':'application/json; charset=utf-8','Cache-Control':cache}});

async function catalog(env,origin,ctx) {
  // Cache one canonical snapshot, not a new D1 scan for every filter/search URL.
  const cache=globalThis.caches?.default, key=new Request(origin+'/api/extensions');
  const saved=await cache?.match(key); if(saved) return saved.json();
  const {results}=await env.DB.prepare("SELECT e.id,json_remove(e.metadata,'$.readme') AS metadata,e.added_at,s.stars,s.checked_at FROM extensions e LEFT JOIN repository_stats s ON s.repository_url=lower(json_extract(e.metadata,'$.repository_url')) ORDER BY e.id").all();
  const data={extensions:results.map(row=>({...withRepositoryStats(JSON.parse(row.metadata),row),added_at:row.added_at}))};
  if(cache) ctx.waitUntil(cache.put(key,reply(data,200,false,'public, max-age=60')));
  return data;
}
async function protectedSource(request,env,path) {
  const action=path==='/api/preview'?'extension-preview':'extension-submit';
  const source=await protectedBody(request,env,action,['repository_url','subdirectory','revision','release_tag']);
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
        if(path!=='/extensions/') {
          data.item=await extensionDetail(env,path.split('/').at(-1),url.searchParams.get('version'));
          if(!data.item) {data.detailError=true;status=404;}
        }
      } catch {data.error='Could not load the catalog. Try again later.';data.detailError=path!=='/extensions/';status=503;}
      return reply(renderPage(data),status,true);
    }
    if(path==='/api/extensions') return reply(await catalog(env,url.origin,ctx),200,false,'public, max-age=60');
    if(/^\/api\/extensions\/[0-9a-f]{24}$/.test(path)) {
      const item=await extensionDetail(env,path.split('/').at(-1),url.searchParams.get('version'));
      return item?reply(item,200,false,'public, max-age=60'):reply({error:'Repository or approved version not listed.'},404);
    }
    const community=path.match(/^\/api\/extensions\/([0-9a-f]{24})\/community$/);
    if(community) return reply(await readCommunity(request,env,community[1]));
    if(path.startsWith('/api/')) return reply({error:'Not found.'},404);
    return env.ASSETS.fetch(request);
  }
  const feedback=path.match(/^\/api\/extensions\/([0-9a-f]{24})\/(?:(vote|comments)|comments\/([1-9][0-9]{0,15})\/vote)$/);
  if(request.method==='POST'&&feedback) return reply(await writeCommunity(request,env,feedback[1],feedback[2],feedback[3]),feedback[2]==='comments'?202:200);
  if(request.method!=='POST'||!['/api/preview','/api/submissions'].includes(path)) return reply({error:'Method not allowed.'},405);
  const source=await protectedSource(request,env,path);
  const metadata=await inspectRepository(source,env);
  if(path==='/api/preview') return reply(metadata);
  const submission=await queueRelease(env,metadata);
  return reply(submission,submission.status==='pending'?202:200);
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
  async scheduled(_controller,env,ctx) {ctx.waitUntil(Promise.all([discoverReleases(env),refreshRepositoryStats(env)]));},
};
