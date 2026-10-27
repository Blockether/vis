---
name: vis-docs
description: Write or change the Vis user manual in resources/vis-docs/ or the README. Gives the page rules, the module order and the checks.
---
# Documentation

The README and `resources/vis-docs/` are for people, also when `doc()` serves them; they are not
instructions for agents. User guides address people who use Vis. Extension guides and the API
reference address developers who build with Vis.

- Start with the reader's goal: what the feature does, when to use it and how to get a useful result. If a chat or UI workflow exists, show it before internal calls.
- Start each page outside the Intro module with a `When to use` section. State the readers' problems in their words, link each to the section that solves it and name the better page for nearby problems. Describe the reader's situation, not the feature's capabilities.
- Address the reader as "you", in a clear, conversational and professional tone. Be direct and literal: no unexplained jargon, metaphors, slogans, filler or forced friendliness.
- The page contract in `docs/core.clj` enforces the sentence, paragraph and semicolon limits.
- Do not write apologies, defensive text, AI or generated-content disclaimers, or notes on how text or screenshots were made. State facts, actions and limitations directly. Give provenance only when it changes how the reader uses or verifies the information.
- Organize guides around tasks, with realistic examples and expected results. Before the reader acts, explain prerequisites, costs, permissions and destructive effects.
- Keep tutorials, task guides, explanations and API reference separate. Put low-level protocol details in labeled reference sections, not at the start. Do not copy agent prompts or operating checklists into user guides; explain their user-visible effects.
- Review headings, introductions, navigation labels and page descriptions, not only body text. A new reader must understand the purpose and the next step without knowledge of Vis internals.
- Also follow [Google: tone and style](https://developers.google.com/style/tone), [Microsoft: simple and human](https://learn.microsoft.com/en-us/style-guide/brand-voice-above-all-simple-human) and [Diátaxis: how-to guides](https://diataxis.fr/how-to-guides/) within these rules.

## Manual structure

Keep the modules of `resources/vis-docs/site.edn` in this order:

1. **Intro**: Rationale, Getting started, Running a gateway, then Reporting a bug.
2. **Concepts**: one page for each feature, such as sessions, drafts, council, automations, forms, live views and decision models.
3. **Programmatic access**: the **Basics** group (Python SDK, HTTP API and Extension API), then the **Feature APIs** group.
4. **Extensions**, then **Reference**.

- Give each concept `X` that a program can use one page `X-api.md` in **Feature APIs**.
- On an `X-api` page, give each example as a Python block, then as an HTTP block. Follow the `PAIRED VARIANTS` rule in `docs/core.clj`.
- Name gateway routes only in HTTP blocks and on the `http-api` page.
- If a feature has an `X-api` page, put its SDK calls and gateway routes there, not on the concept page.
- Link the `X-api` page from the concept page's `When to use` and `See also`.
- Inside a group, give a page a sidebar `:label` without the group name, for example `Sessions` under **Feature APIs**. Keep the full name in `:title`.
- On the public site, show the Extension Center link at the end of the **Extensions** module.
- In the README, call the introduction `Rationale` and list the manual in module order.
- `docs-modules-test` and `packages/vis-agent/tests/test_api_guides.py` check these rules.
