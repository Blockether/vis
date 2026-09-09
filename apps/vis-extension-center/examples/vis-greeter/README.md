# Greeting tools

A small, runnable Extension Center example. It registers `greeter.greet(name)`.

From the Vis repository root, install this monorepo project:

```sh
vis-agent extension install . --subdirectory apps/vis-extension-center/examples/vis-greeter --project --trust
```

To list a public copy, submit its GitHub repository URL and set **Project folder** to
the directory containing this `pyproject.toml` and `extension.py`. If you move this
example to a repository root, leave the folder empty. The catalog links to GitHub;
no separate package upload is needed.

After installation, start Vis or use `/reload`. Ask the agent to call
`greeter.greet("Ada")`; the result is `Hello, Ada!`.

The package has no dependencies beyond the Vis SDK. Review `extension.py` before
installing. Editing a linked source checkout takes effect after `/reload`.
