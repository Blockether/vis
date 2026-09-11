import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

export function moderationStatements(action,id) {
  if(action==='list-comments') return ["SELECT id, extension_id, name, body, created_at FROM comments WHERE status='pending' ORDER BY id"];
  if(['approve-comment','reject-comment'].includes(action)) {
    if(!/^[1-9][0-9]{0,15}$/.test(id||'')||!Number.isSafeInteger(Number(id))) throw new Error('Use the full numeric comment reference.');
    return [`UPDATE comments SET status='${action==='approve-comment'?'approved':'rejected'}' WHERE id=${id}`];
  }
  if(action==='list') return ["SELECT id, extension_id, revision, submitted_at, json_extract(metadata, '$.repository_url') AS repository, json_extract(metadata, '$.subdirectory') AS folder FROM submissions ORDER BY submitted_at"];
  if(!['approve','reject'].includes(action)||!(/^[0-9a-f]{24}$/).test(id||'')) throw new Error('Use list, approve ID or reject ID. ID must be the full 24-character submission reference.');
  const remove=`DELETE FROM submissions WHERE id = '${id}'`;
  return action==='reject'?[remove]:[`INSERT INTO extensions (id, metadata, added_at) SELECT extension_id, metadata, submitted_at FROM submissions WHERE id = '${id}' ON CONFLICT(id) DO UPDATE SET metadata = excluded.metadata`,remove];
}
if(process.argv[1]===fileURLToPath(import.meta.url)) {
  const args=process.argv.slice(2),remote=args.includes('--remote');
  const [action,id]=args.filter(arg=>arg!=='--remote');
  for(const sql of moderationStatements(action,id)) execFileSync(process.execPath,['node_modules/wrangler/bin/wrangler.js','d1','execute','vis-extension-center',remote?'--remote':'--local','--command',sql],{stdio:'inherit'});
}
