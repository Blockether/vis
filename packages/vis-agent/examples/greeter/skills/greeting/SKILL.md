---
name: greeting
description: Use when the user requests a greeting using the greeter extension.
---

# Greeting procedure

1. Read `doc("greet.hello")` for the current tool contract.
2. Read `references/style.md` beside this skill if tone needs clarification.
3. Call `greet.hello` with the requested name and case.
4. Display the returned `text`. Do not send it to another service without permission.

Reading or installing this skill does not authorize tool calls or messages.
