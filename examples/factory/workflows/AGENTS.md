# The workflows of the factory

This Vis project holds the code of the workflows. Each workflow is one file `<name>.py`
in this directory. The function with `@workflow` has the name of the file.

## How a change becomes a version

- Change a file here, or add a file. Do not change the code in another place.
- When no session of this project has a turn in progress, the factory reads each file.
  A changed file that loads becomes the next version in the database of the factory.
- A run and a trigger use the newest version. A run that started keeps its version.
- A file that does not load makes no version. The Workflows page shows the error, and
  the runs continue with the last version.
- The database is the store. Do not delete a file to remove a workflow: the factory
  writes the file again. A person removes a workflow, or restores an old version, on the
  Workflows page.

## Before you end a turn

Check each file that you changed:

```python
source = (project_root_path / "review.py").read_text()
print((await factory.check("review", source))["workflow"])
```

The result shows the title, the inputs, the activities and the triggers that the
factory reads. An error names the fault. Correct the file and check again.

## The workflow code

```python
from factory.models import Model
from factory.workflows import Context, Decision, Document, activity, cron, on, workflow


class Work(Model, sig="example.work@1"):
    summary: str
    tests_passed: bool


@activity(title="Make the change")
def make(ctx: Context, task: str) -> Work:
    return ctx.ask(f"Do this work and run the tests: {task}", Work)


@workflow(title="Nightly check", triggers=[cron("0 3 * * *"), on("webhook.check")])
def nightly(ctx: Context, task: str = "Find and fix one failing test") -> Work:
    return make(ctx, task)
```

- `ctx.ask(request)` gives the answer of the session as text. `ctx.ask(request, Work)`
  gives a validated model.
- `ctx.document(name, request)` gives a document that the session attached.
- `ctx.delegate(tasks)` gives the answer of one subagent for each task.
- `ctx.gate(key, title)` waits for the decision of a person.
- `ctx.note(text)` writes a line in the log of the run.
- `@workflow(model="claude-haiku-5-5")` names the default model of the runs, and
  `provider="..."` its provider. A person can select another model on the Workflows
  page. `ctx.ask(request, llm="...")` names the model of one request.
- The arguments of `@workflow`, `@activity`, `cron` and `on` must be constants. The
  factory reads them from the source and does not run the file for that.
- An input without a default is necessary. The fields of an event fill the inputs that
  have the same name.
- A run works in the project of the factory, not in this directory.
