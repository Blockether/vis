import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

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
if(process.argv[1]===fileURLToPath(import.meta.url)) {
  const args=process.argv.slice(2),remote=args.includes('--remote');
  const [action,id]=args.filter(arg=>arg!=='--remote');
  for(const sql of moderationStatements(action,id)) execFileSync(process.execPath,['node_modules/wrangler/bin/wrangler.js','d1','execute','vis-extension-center',remote?'--remote':'--local','--command',sql],{stdio:'inherit'});
}
