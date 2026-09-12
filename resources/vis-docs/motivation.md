# Why I built Vis

I started Vis because I wanted a coding agent that fits the way I work: one that
uses my environment's tools, shows me what it is doing and lets me follow the
same work from my terminal, desktop or phone. Better model answers were only
part of what I needed.

## Fewer reminders, better tools

I kept seeing behavioral rules accumulate in skills: remember this check, use
that command, follow these steps before moving on. Skills are useful for
explaining an approach. But when a check matters, I do not want its execution
to depend only on whether the agent remembers the explanation.

In my experience, newer models need less instruction for ordinary coding work.
They are better at understanding a request, using tools and working with code.
That does not mean instructions have become unnecessary, or that a model knows
your organization. It changes where I want to spend the effort.

A capable model still cannot infer why you use Gerrit instead of GitHub, Jenkins
instead of GitHub Actions, or a particular internal review process. Those are
facts about your environment, not general programming knowledge. Repeating them
in a prompt is one option. Giving the agent functions that already follow the
process is another.

That is the role of Vis extensions. You can write an operation that selects the
right tests, validates a change or retrieves the relevant build result. Gerrit
and Jenkins are examples of tools you could integrate, not built-in integrations.
You supply the domain knowledge; the model works with operations that express it.

I still use `AGENTS.md` and skills for context, explanations and judgment. I put
checks in code when I want them to run at a defined point. The
[code-complexity example](extension-design.md#check-code-complexity-after-edits)
shows the distinction: a registered hook measures nesting after an edit and
reports findings. It is not a reminder to run a check, and it does not undo the
edit or guarantee that the agent will fix every finding.

## Let Python connect the steps

I wanted to improve the path from a request to useful work, rather than describe
every small step as another agent or a node in a workflow graph. In Vis, the
model can compose operations in Python: read data, branch on the result, call the
next function and print the evidence that matters.

This is a practical choice, not a claim that one language makes every model
better. In my own use, Astra 6 has been particularly good at orchestrating work
in Python. That experience helped shape Vis; it is not a benchmark or a guarantee
about every model or task.

Vis still has a model/tool loop. The difference is that repeatable procedures
can live in code you can inspect and test. Independent async operations can
overlap when their tools support it; dependent steps stay sequential. You do
not need a separate agent for every function call.

## Understand the work, not just the answer

A stream of shell commands can be enough when I am experimenting at home. At
work, I also want to know what was checked, what failed and how the result was
reached. I do not want to reconstruct all of that from commands and raw output.

Activities are the human-facing part of an operation. Your extension chooses a
clear label, a useful summary and the evidence worth opening. The function can
still return structured data for the agent to use in its next step.

For example, a local CI report reader can present:

```text
Read CI report
42 passed · 1 failed
Source: /workspace/ci-report.json
```

The read succeeded, but the test report contains a failure. Those are different
outcomes, and the presentation should not blur them. An empty report should say
that no tests were reported; a missing file should show an error. The
[complete Activity example](extension-design.md#show-a-ci-report-without-hiding-failures)
implements these cases with the public Python SDK.

This visibility does not prove that a solution is correct. It lets you inspect
the operations and evidence behind the answer, rather than accept a final
summary without knowing what happened. You can change the presentation in your
own extension instead of accepting a generic dump of its return value.

## One view from the terminal, desktop and phone

I also wanted to leave my desk without losing track of a session. I use the
terminal for development, but I want to check progress or respond from my phone
and use the desktop app when that is more convenient.

Vis clients connect to the same gateway and sessions. The phone is not running
a second agent with a copied conversation. You configure access to your gateway,
then use the client that suits where you are. The [gateway guide](gateway.md)
explains that setup.

These choices serve the same goal: give you control over how work runs and a
clear view of what happened. Better models help, but they do not replace your
knowledge of the environment, your checks or your judgment.

## See also

- [Getting started](index.md) — install Vis and start a session.
- [Build an extension](extending.md) — add operations for your environment.
- [Design an extension API](extension-design.md) — write useful tools, checks and Activities.
- [Gateway and companion](gateway.md) — connect clients to the same sessions.
