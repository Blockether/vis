# Review a specification and start implementation

Turn on **Plan before coding** when a change needs decisions before implementation.
Vis helps you settle those decisions, writes a versioned specification and breaks
it into small end-to-end tasks. You review the document before any implementation
starts. Simple corrections and read-only questions do not need this ceremony.

Your specification stays in an ordinary Markdown attachment and is the main place
where you review the work. It holds decisions, an implementation plan, comments
and progress. TUI and Companion read and annotate the same Markdown. Specifications
allow comments; implementation records are read-only reports that Vis updates as work progresses.

## Turn planning on

Open **Settings** in Companion or the TUI and enable **Plan before coding**. The
setting is off by default and is shared by interactive clients of that gateway.
It applies to subsequent prompts, not a turn already running. One-shot CLI
requests do not include the planning prompt.

Then describe your goal normally, for example:

> Add session search. Help me settle the behavior and prepare a specification first.

Vis inspects facts it can find itself, then asks about decisions that need your
input, with a recommendation and its trade-off. It waits before asking questions
that depend on your answer. The result is an attachment such as
`PLAN-session-search.md`, not a new repository file. Revisions keep that filename
and receive new version numbers.

## Review and revise

Open the specification from the conversation or **Artifacts** in Companion. Tap
a passage or select text to comment on it. The comment icon adds a whole-document
remark. Collect your comments before sending them; adding a comment alone does
not ask Vis to revise the document.

The document has one workflow action at a time. With pending comments, choose
**Send for revision**. It saves your comments first, then asks Vis to review that
exact saved version. Vis addresses every remark, records its resolution and
attaches a new revision. Neither commenting nor requesting revision authorizes
implementation. If saving fails, nothing is sent. If sending fails after a save,
retry uses that saved version instead of creating it again.

You can also review from chat. For example, say “Revise `PLAN-session-search.md`
v2 using its comments.” Name the version you reviewed. The document is the main
workspace; chat remains available for discussion and explicit instructions.

In the TUI, press **C-x i** to open the attachment inspector. With planning enabled,
specifications appear together under **Specifications**. Select a text document and press
**Enter** to read it inside the terminal; **o** still opens it externally. The keys
below apply to documents that allow comments. Read-only attachments have no comment,
save or review actions.

| Key in the document | Action |
| --- | --- |
| Up / Down | Select a document block or comment |
| Page Up / Page Down | Scroll the document |
| Enter | Comment on the block, or edit the selected comment |
| Tab | Switch between document and comments |
| w | Add a whole-document comment |
| Delete | Remove the selected comment |
| s | Save a new version without sending, for ordinary documents |
| r | Send pending comments for revision |
| a | Approve the specification and start implementation |
| ? | Show all document keys |
| Esc | Close and keep added, unsaved comments locally |

Lists and tables offer individual items or cells for quoting. If a TUI draft
belongs to different document contents, you choose whether to keep it and close
or discard it before opening the other version. It is not silently merged.

## Approve and start

A specification is **in-review** while decisions remain, and **ready** when its
implementation plan has no open questions. When no comments or unsaved changes
remain, choose **Approve and start**. This approves the reviewed specification
**and immediately requests implementation**; there is no second start button.
The same action is available on an **accepted** specification.

The request names the exact version you reviewed. If a newer revision exists,
Vis asks you to review it rather than approve or start from stale content. A
successful send is shown once; reopen the next revision to continue reviewing.

Approval in chat also starts implementation unless you explicitly limit it. To
record agreement without starting work, say “Approve `PLAN-session-search.md`
v3, without implementing it.” A document status by itself never authorizes edits.

During implementation, `IMPLEMENTATION-session-search.md` records completed tasks,
changed files, actual verification results, deviations and remaining work. It has no
comment controls. Vis can still publish a new version of the report.

A new product decision sends the affected scope back through review. Repository rules
and permissions for commits, pushes and other external actions still apply. Tickets
are not published to a tracker unless you ask.

## Review implementation diffs

The implementation record links to a separate diff after each completed task and a
final diff of the whole change. Open a diff to see the actual file changes and their
source, then comment on a passage or the whole diff. **Send for revision** submits the
complete round of comments for that exact version. It does not approve a new scope or
grant permission to commit or push.

Diff comments are separate from the patch: saving a review never changes the patch
itself. In a [draft](drafts.md), Vis captures changes against the draft's saved
starting point or a previous checkpoint, not against a moving checkout. Outside a
draft, the diff must exclude unrelated and pre-existing work. If Vis cannot separate
the changes safely, it reports that limitation instead of presenting someone else's
work as part of your task.

## Document format

Specifications use `PLAN-<kebab-case-feature>.md`; execution records use
`IMPLEMENTATION-<kebab-case-feature>.md`. Both have an unambiguous header:

```markdown
# Session search

**Feature:** session-search
**Status:** ready

## Spec
Goal, behavior, non-goals, decisions and rejected alternatives.

## Implementation plan
1. One narrow end-to-end behavior, with testable acceptance criteria.
   Blocked by: None.

## Open questions
None.

## Plan state
Next: review this version, then approve and start implementation.

## Resolved comments
```

Statuses are `draft`, `in-review`, `ready`, `accepted`, `implementing` and `done`.
The feature in the header must match the filename. Only a specification can offer
approval and start implementation. Execution records are not review requests.
Comments use the existing `## Comments` section and are moved into
`## Resolved comments` when addressed.

Commenting is an explicit attachment property, not a filename convention. The
producer publishes specifications with `commentable=True` and implementation
records with `commentable=False`. Ordinary attachments, including ones without that
property, are read-only. To request a review of an existing read-only document, ask
Vis to publish a reviewable version. The API also refuses comment saves to a
read-only attachment; hiding the controls is not the only protection.

Planning is an instruction to the agent, not an operating-system write barrier.
Turning it off removes the extra prompt and workflow controls; it does not delete
specifications or change which attachments allow comments. You can also explicitly
ask Vis to work without a plan for a particular task.

## See also

- [Configuration](configuration.md) — save settings for your gateway or project.
- [Project instructions](context-and-prompts.md) — describe project rules and reusable prompts.
- [Controlling a session](queue-and-cancel.md) — send follow-ups or cancel a request.
