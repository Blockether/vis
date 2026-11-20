# Rationale

I started Vis because a coding agent that reads only the source code was not enough for my work.
The agent had to use my tools and follow my processes. It had to show me how it reached a result.
And I wanted to follow the same work from my terminal, my desktop, and my phone.

## Tools replace reminders

I kept adding rules to skills: remember this check, use that command, follow these steps. Models
improved and needed less help with ordinary coding. The instructions about my environment kept
growing anyway. A better model still does not know why a team uses Gerrit instead of GitHub, or
Jenkins instead of GitHub Actions.

You work on the project, so you know what makes a change safe, which tests apply, and how review
works. A paragraph that asks the model to remember is a weak way to pass that knowledge on.

Suppose every change needs a particular set of tests. You can describe the selection rules in a
skill, or you can write a function that selects the tests and runs them. You can test the function,
improve it, and reuse it. If a check must run after each edit, a hook runs it then. The model does
not have to ask.

A Vis extension defines those operations in Python. Gerrit and Jenkins are examples of integrations
that you can build. Vis does not include them. Start with one task that you repeat, and give it
clear inputs and useful results. Keep `AGENTS.md` and skills for context, explanations, and work
that needs judgment.

Extensions make the repeatable parts of the work predictable. They do not make the model
deterministic. You still decide which checks matter, and you still review the result. For an
example, read the [code-check hook](extension-design.md#check-code-complexity-after-edits). It
measures Python nesting after each edit and reports its findings. It does not undo the edit.

## Python connects the steps

Models already write Python to work with code and data. Vis uses that ability. A small step does
not need its own tool exchange or its own node in a workflow graph. In my own work, Astra 6
orchestrates tasks this way well.

Vis gives the model its tools and its engine as Python functions. In one program, the model can
search, inspect the results, decide what to do next, and combine operations. Independent operations
overlap when the tools support it. A step that depends on a result waits for that result. The
conversation still has a loop between the model and the tools. But a repeatable procedure can be
ordinary code that you read and test.

Vis keeps the helper functions that the model defines in a session. When you return to the session,
the model can reuse them, also after a restart of Vis. Vis keeps the source of each helper, not the
objects in memory.

Vis also tells the agent about its workspace, its permissions, and the context that is available.
As a conversation grows, the agent keeps the evidence that matters and summarizes completed work.
The full history stays stored.

When your own tools cover a workflow, you can turn off shell access. The agent then uses your tools,
and you control more of what it can do. Python that the model writes runs in a
[sandbox](python-sandbox.md). An installed extension is trusted Python code with access to the
host. Install only extensions that you trust.

## Activities show how the agent reached a result

At home, a stream of shell commands was enough for most of my experiments. At work, I also had to
know how a change was made: what the agent checked, what failed, and what evidence supported the
answer. To reconstruct that from commands and raw output took too much effort.

Vis has **Activities** for that reason. A test run shows its passed and failed counts. A file
change shows its diff. Open the details when you need them. You do not have to work backward from
the final summary of the agent.

An extension that you write also chooses how its work appears. The agent gets structured data for
its next step, and you get an explanation that fits the task. A report that loads without an error
must still show its failed tests. The
[Activity example](extension-design.md#show-a-ci-report-without-hiding-failures) shows how, with
empty results and errors included.

## One gateway serves the terminal, the desktop, and the phone

I wanted to leave my desk and still follow a session. I develop in the terminal, and I use the
desktop app when I want a separate window. On my phone, I check progress or reply while I am away.

Each of these apps connects to the same gateway, the service that runs your sessions. The work
stays on the computer that runs the gateway. A change of device does not start a second agent or
copy a conversation. You return to the same work. The [Quickstart](quickstart.md#install)
has the downloads and the connection steps.

## See also

- To install Vis, connect an app, and try a first task, read [Quickstart](quickstart.md).
- To turn a repeated task into a tool, read [Extending Vis](extending.md).
- For working examples of checks and Activities, read [Extension design](extension-design.md).
- To learn how a long session keeps useful work, read [Context management](context-management.md).
