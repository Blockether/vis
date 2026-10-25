# Swarm Forge

Swarm Forge runs a small team of agent sessions on your tasks. Each session has one role, such as
coder or reviewer, and works in its own draft. A task moves from role to role. You approve the
result on one board before it lands.

## When to use

- **You want a second agent to review each change before it lands.** Start the
  [two-pack](#choose-a-pack). The cleaner reviews and tidies the work of the coder.
- **You have a list of tasks and want them done in order, with checks.** [Add the
  tasks](#add-tasks) and follow them on the [board](#follow-the-board).
- **A role needs your decision while it works.** Answer it in the [Attention
  section](#answer-questions-and-approve) of the board.
- **You want to change the roles or their instructions.** [Write your own
  pack](#write-a-pack).

For one change in one session, use [Drafts](drafts.md). To ask another session for help without a
pipeline, use [Council](council.md).

## Before you start

- Work in a Git repository. Each role works in a `worktree` draft of that repository.
- Get the extension. It is in the Vis repository as `.vis/extensions/swarm_forge.py`, with its packs
  in `.vis/extensions/swarm_forge/packs/`. To use it in another project, copy both into that
  project's `.vis/extensions/`. Then run `/reload`.
- The extension uses the `pyyaml` and `pydantic` packages. If Vis reports a missing package, run
  `vis-agent python -m pip install pyyaml pydantic`.

Each role turn costs model tokens, and a six-pack runs six sessions. Approve lands the draft of the
last role on the default branch of your repository. Read the change before you approve it.

## Start a swarm

Type this command in a session of your project:

```text
/swarm start two-pack
```

Vis creates one session for each role of the pack. Drafts are on in these sessions only. The draft
setting of your project does not change. The session that runs the command keeps the board.

To work in another folder, add `--root`:

```text
/swarm start two-pack --root ~/work/api
```

## Choose a pack

| Pack | Roles |
| --- | --- |
| `two-pack` | Coder, then cleaner. |
| `four-pack` | Specifier, coder, refactorer, then architect. |
| `six-pack` | The four-pack roles, then hardening and QA. |

## Add tasks

Add a task in one of these ways:

- On the board, fill in **New task** and select **Add task**.
- Type `/swarm task Fix the login timeout`.
- Ask the agent of the board session. It calls `swarm.add_task(title, text)`.

A new task waits at the first role. Each role merges the commit of the previous role, does its own
work and commits it in its draft. Then the task moves to the next role.

## Follow the board

The board opens in the side panel of the session that started the swarm. In the terminal, press
Ctrl+X V. In the Companion app, select the panel icon next to Settings.

| Section | What it shows |
| --- | --- |
| Summary | Counters for open, working, attention and landed tasks. |
| Attention | Questions, forms, approvals and blocked work that need you. |
| Lanes | One lane for each role, with a card for each task. |
| Work queue | Every task and its state. |
| New task | A form to add a task. |
| Roles | The provider and model of each role. |

Change the provider or model of a role in **Roles**. The next turn of that role uses it. `/swarm
status` shows the state as text and opens the board again if it was closed.

## Answer questions and approve

The Attention section shows one item for each thing that needs you:

- **A question.** Write your answer and select **Send answer**. The role continues.
- **A form of a role session.** Fill it in and send it, like a form in that session.
- **An approval.** The last role finished. Select **Approve** to land the draft, **Reject** with a
  comment to send the task back, or **Drop** to stop the task.
- **Blocked work.** Select **Retry** to run the role again, or **Drop**.

A task that a role sends back three times stops as blocked. So two roles cannot pass a task
between them forever.

## Stop a swarm

Type `/swarm stop`, or select **Teardown** on the board and confirm. Running turns stop and the
board closes. The role sessions and their drafts stay, so you can read or continue the work.

The board stays open across `/reload`. After a gateway restart, the board shows its last state
until the extension loads again.

## Write a pack

A pack is a YAML file in `.vis/extensions/swarm_forge/packs/`. Its file name, without `.yml`, is the
name for `/swarm start`.

```yaml
name: fix-and-review
description: A coder fixes each task. A reviewer checks it.
roles:
- name: coder
  title: Coder
  provider: null
  model: null
  receive: task
  propagation: forward-only
  prompt: |
    Fix the task with the smallest correct change. Run the tests.
- name: reviewer
  title: Reviewer
  provider: null
  model: null
  receive: batch
  propagation: back-one
  prompt: |
    Review the change of the previous role. Fix each problem that you find.
```

| Key | Values |
| --- | --- |
| `provider`, `model` | A model that Vis serves, or `null` for the session default. Give both or neither. |
| `receive` | `task` takes one task at a time. `batch` takes all waiting tasks in one turn. |
| `propagation` | Where a rejected task goes: `forward-only` stays at this role, `back-one` goes to the previous role and `back-all` goes to the first role. |
| `prompt` | The work of this role. Vis adds the rules for drafts, commits and the final answer. |

A change to a pack applies at the next `/swarm start`.

## See also

- [Drafts](drafts.md) — how each role works in its own copy of the repository.
- [Live views](live-views.md#choose-a-placement) — how an extension opens a panel and a header
  pin, like the board.
- [Council](council.md) — ask another session for help without a pipeline.
