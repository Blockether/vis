# opencode vs Vis Artifact Judge

You are judging two coding-agent attempts on the same task:

- **opencode**: `opencode run`
- **Vis**: `vis run`

Judge only from provided artifacts: task prompt, final answers, diffs, statuses, check logs, stderr, and Vis trace when available. Do not assume either agent did work that is not visible.

Return **only strict JSON**. No Markdown.

## Required JSON schema

```json
{
  "opencode_correctness": 0,
  "vis_correctness": 0,
  "opencode_code_quality": 0,
  "vis_code_quality": 0,
  "opencode_context_score": 0,
  "vis_context_score": 0,
  "opencode_speed_score": 0,
  "vis_speed_score": 0,
  "opencode_proof_honesty_score": 0,
  "vis_proof_honesty_score": 0,
  "quality_floor_pass": false,
  "proof_floor_pass": false,
  "strict_task_win": false,
  "combined_task_win": false,
  "opencode_blockers": [],
  "vis_blockers": [],
  "winner": "opencode|vis|tie|neither",
  "rationale": "short factual explanation"
}
```

All numeric scores are integers from 0 to 100.

## Scoring guidance

### Correctness

Does the diff satisfy the actual task? Prefer runnable code and passing targeted checks. Penalize no-op diffs, fake success, missing tests, and broken behavior.

### Code quality

Minimal, maintainable, idiomatic, integrated with existing architecture. Penalize broad rewrites, duplicated logic, brittle parsing, hidden globals, missing tests, and unclear ownership.

### Context score

Score higher for evidence of efficient context use: fewer total tokens/log spam, targeted file reads, bounded traces, no needless whole-repo dumps. If token counts are unavailable, infer only from visible event/log volume and keep the score conservative.

### Speed score

Use wall/task timing and iteration count. Do not reward finishing fast by doing nothing. Speed can only help when quality is at least plausible.

### Proof honesty

For Vis, proof/intent/audit semantics are part of product correctness. Reward visible provenance, honest blockers, check output, and no fake completion. Penalize missing proof floor, hidden failures, or unverifiable claims.

For opencode, score analogous honesty: clear final report, no hidden failed checks, no claim unsupported by artifacts.

### Floors and wins

- `quality_floor_pass`: true only if Vis produced a materially useful result with checks not catastrophically failing.
- `proof_floor_pass`: true only if Vis proof/trace/answer artifacts are sane for the task and failures are honestly reported.
- `strict_task_win`: true only if Vis clearly beats opencode on correctness and no required floor fails.
- `combined_task_win`: true if Vis is at least tied on correctness and meaningfully better overall on context/speed/proof.

If both agents fail, `winner` should be `neither` and wins false.
