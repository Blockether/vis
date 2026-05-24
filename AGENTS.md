# Vis agent notes

Keep this file short: Vis injects full AGENTS.md into provider prompt with no truncation.

## Dev REPL

Restart project nREPL:

```bash
old=$(lsof -tiTCP:7888 -sTCP:LISTEN || true)
[ -n "$old" ] && kill $old
rm -f .nrepl-port
nohup ./bin/dev nrepl > .nrepl.log 2>&1 &
```

Eval through running REPL:

```bash
clj-nrepl-eval -p $(cat .nrepl-port) '(+ 1 2 3)'
```

## DB basics

Default SQLite DB is directory-backed:

```bash
DB="$HOME/.vis/vis.mdb/vis.db"
sqlite3 "$DB" '.tables'
```

Important tables:

- `session_soul`: stable conversation id, channel, external id.
- `session_state`: title/model/workspace state; join via `session_soul_id`.
- `session_turn_soul`: user request per turn.
- `session_turn_state`: turn status + aggregated token/cost counters.
- `session_turn_iteration`: per provider call; prompts, raw response, code, token counters.
- `llm_routing_event`: router/provider trace per iteration.

Token query for conversation id:

```bash
SID='c8dc39b1-faf0-451e-85ad-8cd054067229'
sqlite3 -header -column "$DB" "
select t.position turn, t.user_request req, ts.status, ts.iteration_count iters,
       ts.llm_input_tokens input, ts.llm_output_tokens output,
       ts.llm_cached_tokens cached, printf('%.6f', ts.llm_total_cost_usd) cost
from session_soul so
join session_state ss on ss.session_soul_id = so.id
join session_turn_soul t on t.session_state_id = ss.id
join session_turn_state ts on ts.session_turn_soul_id = t.id
where so.id = '$SID'
order by t.position;"
```

Iteration prompt-size query:

```bash
sqlite3 -header -column "$DB" "
select t.position turn, i.position iter, i.status,
       i.llm_input_tokens input, i.llm_cached_tokens cached,
       length(i.llm_system_prompt) sys_chars,
       length(i.llm_user_prompt) user_chars,
       length(i.llm_assistant_message) assistant_chars
from session_soul so
join session_state ss on ss.session_soul_id = so.id
join session_turn_soul t on t.session_state_id = ss.id
join session_turn_state ts on ts.session_turn_soul_id = t.id
join session_turn_iteration i on i.session_turn_state_id = ts.id
where so.id = '$SID'
order by t.position, i.position;"
```

## Transcript extraction

Preferred: use app exporter through nREPL.

```bash
clj-nrepl-eval -p $(cat .nrepl-port) '
(require (quote [com.blockether.vis.core :as vis]) :reload)
(let [db (vis/db-shared-connection! (vis/resolve-db-spec))
      sid "c8dc39b1-faf0-451e-85ad-8cd054067229"
      md (vis/session->markdown db sid)]
  (spit "target/c8dc39b1-transcript.md" md)
  {:path "target/c8dc39b1-transcript.md" :chars (count md)})'
```

Raw turn transcript via SQL:

```bash
sqlite3 -json "$DB" "
select t.position, t.user_request, ts.status, ts.answer_markdown
from session_soul so
join session_state ss on ss.session_soul_id = so.id
join session_turn_soul t on t.session_state_id = ss.id
join session_turn_state ts on ts.session_turn_soul_id = t.id
where so.id = '$SID'
order by t.position;"
```

## Token-forensics checklist

1. Check turn rows first: interrupted turns with `iteration_count = 0` burned no provider tokens.
2. Check iteration rows: tokens are stored per `session_turn_iteration` and summed on `session_turn_state`.
3. Parse `llm_user_prompt` JSON. It includes system blocks plus current rendered `;; ctx`.
4. If `llm_user_prompt` is huge, inspect `;; ctx`, especially `:session/trailer`.
5. Root cause to look for: unbounded trailer pins. `ctx_renderer.clj` intentionally renders full trailer values; big tool results (`v/cat`, broad `v/rg`, large `v/ls`) ride into every later turn until model emits `(done {:trailer-drop [...]})` or `(done {:trailer-summarize [...]})`.
