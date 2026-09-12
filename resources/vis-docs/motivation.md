# Why I built Vis

I wanted a coding agent that understood more than the source code. It needed to
work with my tools and processes, show me how it reached a result and let me
follow the same work from my terminal, desktop or phone. That is why I started
Vis.

## Fewer reminders, better tools

I kept adding rules to skills: remember this check, use that command, follow
these steps. As models improved, they needed less help with ordinary coding.
But the instructions about my environment kept growing. A better model still
did not know why a team used Gerrit instead of GitHub, or Jenkins instead of
GitHub Actions.

That is knowledge you have as the person working on the project. You know what
makes a change safe, which tests are relevant and how your review process works.
I wanted a way to give that knowledge to the agent that went beyond asking it
to remember another paragraph.

Suppose every change needs a particular set of tests. You can describe the
selection rules in a skill, or write a function that selects and runs those
tests. The function gives you something you can test, improve and reuse. If a
check needs to happen after an edit, a hook can run it at that point without
waiting for the model to ask.

Vis extensions let you define those operations in Python. Gerrit and Jenkins
are examples of integrations you could build, not services bundled with Vis.
Start with one task you repeat, give it clear inputs and useful results, and
build from there. Keep `AGENTS.md` and skills for context, explanations and work
that needs judgment.

The goal is to make the repeatable parts of the work predictable. It does not
make the model deterministic. You still decide which checks matter and review
the result. For a concrete example, the
[code-check hook](extension-design.md#check-code-complexity-after-edits) measures
Python nesting after edits and reports findings; it does not undo the edit.

## Let Python connect the steps

Models already write Python to work with code and data. I wanted Vis to use that
ability, rather than make every small step a separate tool exchange or a node
in a workflow graph. In my own work, Astra 6 has been particularly good at
orchestrating tasks this way.

Vis gives the model access to its tools and engine through Python functions.
It can search, inspect the results, decide what to do next and combine operations
in one program. Independent operations can overlap when the tools support it;
steps that depend on a result wait for it. The conversation still has a
model/tool loop, but a repeatable procedure can be ordinary code you can read
and test.

Useful helper definitions can be reused when you return to the same session,
even after restarting Vis. That saves rebuilding the same procedure each time;
it preserves the helper's source, not every object in memory. Vis also gives
the agent information about its workspace, permissions and available context.
It can keep the evidence that matters and summarize completed work as the
conversation grows, while the full history remains stored.

Once your own tools cover a workflow, you can disable shell access and have the
agent use those tools instead. That makes it easier to control the operations
it can perform. Model-written Python runs in a [sandbox](python-sandbox.md),
but installed extensions are trusted Python code with access to the host.
Only install extensions you trust.

## Understand the work, not just the answer

When experimenting at home, a stream of shell commands was often enough. At
work, I also needed to understand how a change was made: what was checked,
what failed and what evidence supported the answer. Reconstructing that from
commands and raw output took too much effort.

That is why Vis has **Activities**. A test run can show its passed and failed
counts. A file change can show its diff. You can open the details when you need
them, rather than work backward from the agent's final summary.

When you write an extension, you also choose how its work appears. The agent
gets structured data for its next step; you get an explanation suited to the
task. Reading a report successfully should still leave its failed tests visible.
The [Activity example](extension-design.md#show-a-ci-report-without-hiding-failures)
shows how to do that, including empty results and errors.

## One view from the terminal, desktop and phone

I wanted to leave my desk without losing track of a session. I use the terminal
for development, the desktop app when I want a separate window, and my phone to
check progress or respond while I am away.

All of these connect to the same gateway, the service running your sessions.
The work stays on that computer. Switching devices does not start a second
agent or copy a conversation: you return to the same work. The
[desktop and mobile guide](gateway.md) has the downloads and connection steps.

For me, these choices belong together. Your tools express how the work should
happen. Python lets the model combine them. Activities let you see what happened,
and the different apps let you stay involved wherever you are.

## See also

- [Getting started](index.md) — install Vis and try your first task.
- [Desktop and mobile apps](gateway.md) — download an app and connect to your sessions.
- [Extending Vis](extending.md) — turn a repeated task into a tool.
- [Extension design](extension-design.md) — working examples of checks and Activities.
- [How Vis manages context](token-optimization.md) — how long sessions retain useful work.
