# State ownership

Vis separates durable conversation identity from branch-local and run-local state.

| State | Owner | Scope |
| --- | --- | --- |
| `conversation_soul` | conversation identity | Cross-branch conversation |
| `conversation_state` | branch/fork snapshot | One branch |
| `conversation_turn_soul` | user request identity | Branch-local turn |
| `conversation_turn_state` | one run/retry | Run-local |
| `iteration` | model round-trip | Run-local |
| `conversation_intent` | user objective as Vis understands it | Conversation soul |
| `conversation_intent_focus` | currently pursued intent set | Turn-state run |

## Intent ownership

Intents are conversation-scoped so related work and evidence can be linked across forked branches. Focus is turn-state-scoped so one branch/run can pursue a different intent without silently changing another branch.

A normal continuation must not silently switch to unrelated work while the current focused intent is unresolved. The user should finish or abandon the current intent, or fork for parallel unrelated work.

## Evidence ownership

Evidence is addressed by canonical provenance refs into observed iteration blocks. Writer APIs reject compact aliases and unresolved refs. Display labels may be shown in channels, but copy/export and persistence keep canonical refs.
