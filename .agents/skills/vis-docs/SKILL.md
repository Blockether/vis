---
name: vis-docs
description: Write or change the Vis user manual in resources/vis-docs/ or the README. Gives the page rules, the module order and the checks.
---
# Documentation

The README and `resources/vis-docs/` are for people, also when `doc()` serves them. They are not
instructions for agents. User guides address people who use Vis. Extension guides and the API
reference address developers who build with Vis.

- Start with the reader's goal: what the feature does, when to use it and how to get a useful result. If a chat or UI workflow exists, show it before internal calls.
- Start each page outside the Intro module with a `When to use` section. State the readers' problems in their words, link each to the section that solves it and name the better page for nearby problems. Describe the reader's situation, not the feature's capabilities.
- Address the reader as "you", in a clear, conversational and professional tone. Be direct and literal: no unexplained jargon, metaphors, slogans, filler or forced friendliness.
- The page contract in `docs/core.clj` enforces the sentence, paragraph and semicolon limits.
- Do not write apologies, defensive text, AI or generated-content disclaimers, or notes on how text or screenshots were made. State facts, actions and limitations directly. Give provenance only when it changes how the reader uses or verifies the information.
- Organize guides around tasks, with realistic examples and expected results. Before the reader acts, explain prerequisites, costs, permissions and destructive effects.
- Keep tutorials, task guides, explanations and API reference separate. Put low-level protocol details in labeled reference sections, not at the start. Do not copy agent prompts or operating checklists into user guides. Explain their user-visible effects.
- Review headings, introductions, navigation labels and page descriptions, not only body text. A new reader must understand the purpose and the next step without knowledge of Vis internals.
- Also follow [Google: tone and style](https://developers.google.com/style/tone), [Microsoft: simple and human](https://learn.microsoft.com/en-us/style-guide/brand-voice-above-all-simple-human) and [Diátaxis: how-to guides](https://diataxis.fr/how-to-guides/) within these rules.

## Writing rules

These rules come from the `technical-writing` and `unslop` skills of
[cursor/plugins](https://github.com/cursor/plugins/tree/main/pstack/skills). Apply them to each
sentence that you write or change.

Keep a page minimal and complete:

- Keep each fact. A fact is an option, a default, a limit, a path, a command, or a key. An error with its fix, a prerequisite, a cost, and a permission are facts too.
- Cut a sentence that restates its heading, announces what follows, or repeats the code block below it.
- Give each fact one owner page. Other pages link to the owner page.
- Cut a sentence that can appear unchanged in the documentation of another project.

Choose one mode for each section:

- A tutorial section says what the reader builds. Each step has a result that the reader can see.
- A how-to section has the condition, the steps, and the expected result. It has no explanation.
- A reference section only describes. Use a table or a list, in the order of the thing that it describes.
- An explanation section covers one topic and gives the reasons. Only this mode can have an opinion.

Write each sentence for one reading:

- Write the real name of the symbol, file, flag, or command, and use one name for each thing.
- Name the actor and use the active voice. Write an instruction as a command, with its condition or warning first.
- Write one instruction or one thought in each sentence. Give the number, not an adverb.
- Make each "it", "they", and "this" point at one thing. Keep "that", "the", and "a".
- Use serial commas. Put code in code font and UI elements in bold. Write headings in sentence case, and let a heading state its point.
- Use numbered lists for sequences and bullets for other lists. Introduce a list with a complete sentence, and keep its items parallel.
- Write each `See also` item as one sentence with the goal first: "To turn a repeated task into a tool, read [Extending Vis](extending.md)."

Do not write these:

- Dashes between clauses, semicolons, slashes between words, "(s)", arrows, or Latin abbreviations. Use a full stop, a comma, "and", or "or".
- A colon in the middle of a sentence. A colon goes before a list or an example.
- Parentheses that are not a full grammatical unit, curly quotes, or decorative emojis.
- "Simply", "easy", "quickly", "just", "please", or more than one hedge. The one hedge is "may".
- "Not only X but Y", "serves as", or "from X to Y" without a real scale.
- Ideas forced into groups of three, or a trailing "-ing" phrase without a fact.
- Words such as "crucial", "enhance", "seamless", "robust", and "powerful". Name the mechanism or the number.
- A metaphor, an idiom, or an abstract noun such as "surface", "primitive", or "harness" in prose. A real API name stays, in code font.
- A bold label with a colon that restates its line, or a generic conclusion.

## Manual structure

Keep the modules of `resources/vis-docs/site.edn` in this order:

1. **Get started**: Getting started, Rationale, and Reporting a bug.
2. **Guides**: four groups that the sidebar shows as collapsed lists.
   - **Use Vis**: app workflows and shared session features.
   - **Run Vis**: installation, updates, the gateway, and access policy.
   - **Customize Vis**: settings, project instructions, skills, extension packages, and the brand.
   - **Build on Vis**: extension guides, the Python SDK, and the HTTP API.
3. **Reference**: lookup pages, then the **Feature APIs** group.

- Keep each shared workflow on one page. Link to it from the other client section.
- Give each concept `X` that a program can use one page `X-api.md` in **Feature APIs**.
- On an `X-api` page, give each example as a Python block, then as an HTTP block. Follow the `PAIRED VARIANTS` rule in `docs/core.clj`.
- Name gateway routes only in HTTP blocks and on the `http-api` page.
- If a feature has an `X-api` page, put its SDK calls and gateway routes there, not on the concept page.
- Link the `X-api` page from the concept page's `When to use` and `See also`.
- Inside a group, give a page a sidebar `:label` without the group name, for example `Sessions` under **Feature APIs**. Keep the full name in `:title`.
- On the public site, show the Extension Center link at the end of the **Guides** module.
- In the README, link to the manual and Curated Terminal Benchmark. Keep the manual contents on the documentation site.
- `docs-modules-test` and `packages/vis-agent/tests/test_api_guides.py` check these rules.
