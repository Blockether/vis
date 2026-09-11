import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { parseArgs } from 'node:util';
import { identity, inspectRepository } from './github.js';

export function moderationStatements(action,id) {
  if(action==='list-comments') return ["SELECT id, extension_id, name, body, created_at FROM comments WHERE status='pending' ORDER BY id"];
  if(['approve-comment','reject-comment'].includes(action)) {
    if(!/^[1-9][0-9]{0,15}$/.test(id||'')||!Number.isSafeInteger(Number(id))) throw new Error('Use the full numeric comment reference.');
    return [`UPDATE comments SET status='${action==='approve-comment'?'approved':'rejected'}' WHERE id=${id}`];
  }
  if(action==='list-sync') return ['SELECT extension_id,page,position,checked_at,error FROM release_sync ORDER BY checked_at'];
  if(action==='list') return ["SELECT id, extension_id, revision, submitted_at, json_extract(metadata, '$.repository_url') AS repository, json_extract(metadata, '$.subdirectory') AS folder, json_extract(metadata, '$.version') AS version, json_extract(metadata, '$.release_tag') AS tag FROM submissions ORDER BY submitted_at"];
  if(!['approve','reject'].includes(action)||!(/^[0-9a-f]{24}$/).test(id||'')) throw new Error('Use list, approve ID or reject ID. ID must be the full 24-character submission reference.');
  const remove=`DELETE FROM submissions WHERE id = '${id}'`;
  const save=`INSERT INTO releases (extension_id,version,revision,metadata,status,reviewed_at) SELECT extension_id,json_extract(metadata,'$.version'),revision,metadata,'${action==='approve'?'approved':'rejected'}',strftime('%Y-%m-%dT%H:%M:%fZ','now') FROM submissions WHERE id='${id}' ON CONFLICT(extension_id,version) ${action==='reject'?'DO NOTHING':"DO UPDATE SET revision=CASE WHEN releases.revision=excluded.revision THEN releases.revision ELSE NULL END,status='approved'"}`;
  if(action==='reject') return [save,remove];
  const publish=`INSERT INTO extensions (id,metadata,added_at) SELECT r.extension_id,r.metadata,r.reviewed_at FROM releases r WHERE r.extension_id=(SELECT extension_id FROM submissions WHERE id='${id}') AND r.status='approved' ORDER BY COALESCE(json_extract(r.metadata,'$.prerelease'),0),json_extract(r.metadata,'$.version_key') DESC,r.reviewed_at DESC LIMIT 1 ON CONFLICT(id) DO UPDATE SET metadata=excluded.metadata`;
  return [save,publish,remove];
}
export async function publicationStatements(metadata) {
  // Called only by the authenticated operator CLI after GitHub inspection.
  if(!/^[0-9a-f]{24}$/.test(metadata.id)||!/^[0-9a-f]{40}$/.test(metadata.revision)) throw new Error('Inspect a pinned release before publication.');
  const id=await identity(metadata.id+'\n'+metadata.revision),quote=value=>"'"+String(value).replaceAll("'","''")+"'";
  const now=new Date().toISOString(),body=JSON.stringify({...metadata,checked_at:now});
  const insert=`INSERT INTO submissions (id,extension_id,revision,metadata,submitted_at) VALUES (${quote(id)},${quote(metadata.id)},CASE WHEN EXISTS(SELECT 1 FROM extensions WHERE id=${quote(metadata.id)} AND json_extract(metadata,'$.name')!=${quote(metadata.name)}) THEN NULL ELSE ${quote(metadata.revision)} END,${quote(body)},${quote(now)}) ON CONFLICT(extension_id,revision) DO UPDATE SET metadata=excluded.metadata`;
  return [insert,...moderationStatements('approve',id)];
}

if(process.argv[1]===fileURLToPath(import.meta.url)) {
  const {values,positionals}=parseArgs({options:{remote:{type:'boolean'},config:{type:'string'}},allowPositionals:true});
  const [action,...args]=positionals;
  let statements;
  if(action==='publish') {
    const [repository_url,subdirectory,release_tag,revision]=args;
    if(args.length!==4||!/^[0-9a-f]{40}$/.test(revision)) throw new Error('Use publish REPOSITORY FOLDER RELEASE_TAG FULL_SHA after reviewing that source.');
    const metadata=await inspectRepository({repository_url,subdirectory,release_tag,revision},process.env);
    statements=await publicationStatements(metadata);
    console.log('Publishing reviewed release:',metadata.name,metadata.version,metadata.revision);
  } else statements=moderationStatements(action,args[0]);
  for(const sql of statements) execFileSync(process.execPath,['node_modules/wrangler/bin/wrangler.js','d1','execute','vis-extension-center',values.remote?'--remote':'--local',...(values.config?['--config',values.config]:[]),'--command',sql],{stdio:'inherit'});
}
