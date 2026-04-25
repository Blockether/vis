# Reasoning Levels

For models with native reasoning (thinking tokens), the iteration loop
manages reasoning effort automatically.

## Levels

| Level | Keyword | Description |
|-------|---------|-------------|
| Quick | `:quick` | Minimal thinking. Cheapest. |
| Balanced | `:balanced` | **Default.** Good thinking budget without excess. |
| Deep | `:deep` | Maximum thinking. Used for error recovery. |

## Default

The default reasoning level is **`:balanced`**. Override per-query via
`:reasoning-default` in the opts map passed to `query!`.

## Error-Driven Escalation

When the LLM hits consecutive errors, reasoning automatically escalates:

| Consecutive errors | Effective level |
|-------------------|-----------------|
| 0 | Base level (`:balanced` by default) |
| 1 | If base is `:quick` → `:balanced`, else → `:deep` |
| 2+ | `:deep` |

## LLM Self-Steering

The LLM can request a different reasoning level for the **next**
iteration via the `:next` field in its structured response:

```json
{"next": {"reasoning": "deep"}}
```

The LLM's preference takes priority over error-driven escalation.
