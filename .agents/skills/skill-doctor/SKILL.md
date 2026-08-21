---
name: skill-doctor
description: Audit this repo's own skills against the sessions that actually read them — score each SKILL.md by post-read divergence and answer patch-ready edits. Use when asked to review, improve, prune or shrink skills, when a skill was ignored, re-read mid-task, or a skill-guided task went sideways.
---

# Skill doctor

A skill is judged by what happened AFTER it was read, never by how the
conversation felt. Session quality is confounded by the task; post-read
divergence is not. Score the SKILL against the trajectories that consulted it.

Refuse to produce a "quality score" for a conversation. It ranks nothing that a
diff can act on, and every skill edit it suggests is unfalsifiable.

## 1. Evidence

Journals are the cheap prefilter (`~/.vis/gateway/events/<id>.ndjson`, gateway
sessions only, never `grep` — that root is `no_search`, read it in Python).
`list_sessions()` / `read_session(id)` is the complete index; fall back to it
when a skill scores zero reads, because absence of a journal is not absence of
use.

```python
SK = Path(session["workspace"]["root"])/".agents"/"skills"
SKILLS = sorted(p.name for p in SK.iterdir() if p.is_dir())
ev = Path(os.path.expanduser("~/.vis/gateway/events"))
pat = re.compile(r'(?:doc\(["\']|/skill:)(' + "|".join(map(re.escape, SKILLS)) + r')\b')

hits = collections.defaultdict(collections.Counter)      # skill -> session -> reads
for p in ev.glob("*.ndjson"):
    for m in pat.finditer(p.read_text(errors="ignore")):
        hits[m.group(1)][p.stem] += 1

def blocks(sess):                                        # ordered (scope, code, result)
    return [(str(b.get("scope") or ""), str(b.get("code") or ""),
             str(b.get("result") or "") + " " + str(b.get("result_summary") or ""))
            for t in sess["transcript"]["turns"] for it in t["iterations"]
            for b in (it.get("blocks") or [])]
```

Read only the sessions that hit (`await gather(*[read_session(s) for s in ...])`),
then walk each read event's next ~15 blocks. Cap the window: what a skill caused
shows up immediately, and everything after is the task.

## 2. The five findings, and the ONE diff each earns

| Finding | Detected as | The only diff it justifies |
|---|---|---|
| **Never read** | zero reads while sessions in its domain ran | rewrite the `description:` as the ASK ("Use when…"), touch no body line |
| **Re-read in the same turn** | a second `doc(name)` within the window | hoist the fact that was hunted to the top; do not add it twice |
| **Dependency gap** | a `doc(other)`/`apropos` right after the read | name that contract in one line at the step that needs it |
| **Dead command** | a literal the readers never ran | delete it, or demote to prose — a dead line is prompt tax on every future read |
| **Post-read repair** | error/retry/refused anchor in the window | correct that step, cite the session in the line |

Normalize before matching literals: strip `$VAR`, `<id>`, quoting and flag
order, then compare command SHAPES. A raw string match reports a skill whose
every example uses `"$SESSION"` as 0% exercised, which is a bug in the audit,
not a finding about the skill.

## 3. What makes a diff mergeable

- **Every proposed line carries its evidence** inline in the report: session id,
  scope (`t4/i17`), and the observed behavior. A line without a citation is an
  opinion; drop it.
- **No net growth.** A skill may only grow when the same diff deletes at least as
  many lines as it adds. Skills fail by accreting, never by omission — the
  system prompt pays for the description, and every read pays for the body.
- **One diff per finding**, applied with `patch(path, edits)` in a single call
  per file, quoting the anchor and never restating the file.
- **Never invent a step nobody took.** If the evidence is one session, say one
  session; a skill rewritten from a single trajectory overfits it.
- Frontmatter `description` and body are separate instruments: discovery misses
  are fixed ONLY in the description, comprehension misses ONLY in the body.

## 4. Report

Ordered worst-first, one block per skill: reads · sessions · dead-line ratio ·
findings with citations · the patch call. State the skills with zero evidence
separately and propose nothing for them beyond a description rewrite — with no
trajectory there is nothing to diff against.

## 5. Verify

Discovery is live, so a rewritten skill is checked in-session:
`apropos("<the phrasing a user would actually type>")` must rank it, and
`doc(name)` must print the edited body. Then `run_tests({"paths":
["test/com/blockether/vis/private_deployment_hygiene_test.clj"]})`, since a
SKILL.md is scanned like any other tree file.
