# vis sandbox MCP shim: one name for each MCP server.
#
# The host gives each server a `python_name` in `session["env"]["mcp"]["servers"]`:
# its name in lower-case snake form. Before each block this shim binds that name
# to a handle, so a block says `await linear.save_issue(title="...")`. A name
# that Python, Vis or the session already has stays theirs: such a server is a
# child of `mcp`. The handles hold no catalog: a call goes through `mcp_call`,
# the one verb that the host binds, and `doc` asks the host for the page of a
# handle. So a tool that a server adds while the session runs is there at once.


def __vis_install_mcp__():
    import builtins as _b
    import types as _types

    g = globals()
    # The server names that are bound now. `_bind` is their only writer.
    bound = set()

    def _verb():
        verb = g.get("mcp_call")
        if not callable(verb):
            raise RuntimeError(
                "mcp: this session has no MCP server. Add one under `mcp.servers` "
                "in config.yml, then use /reload."
            )
        return verb

    def _servers():
        try:
            servers = g["session"]["env"]["mcp"]["servers"]
        except Exception:
            return {}
        return servers if isinstance(servers, dict) else {}

    def _identifier(name):
        return "".join(
            c if (c.isascii() and c.isalnum()) or c == "_" else "_" for c in str(name)
        )

    def _arguments(handle, args, kwargs):
        if len(args) > 1 or (args and not isinstance(args[0], dict)):
            raise TypeError(
                repr(handle) + " takes keyword arguments, or one dict of arguments"
            )
        merged = dict(args[0]) if args else {}
        merged.update(kwargs)
        return merged

    def _public(name):
        # A private name is never a server or a tool: `hasattr` probes of the
        # runtime (`__wrapped__`, `_ipython_display_`) must get a plain miss.
        if name.startswith("_"):
            raise AttributeError(name)
        return name

    class _Handle:
        __slots__ = ("_parts",)
        # Without this, `for x in handle` would index it for ever.
        __iter__ = None
        # `_bind` knows a handle of an older install of this shim by this mark.
        __vis_mcp_handle__ = True

        def __init__(self, *parts):
            object.__setattr__(self, "_parts", tuple(str(p) for p in parts))

        def __setattr__(self, name, value):
            raise AttributeError("an mcp handle is read-only")

        def __vis_doc_name__(self):
            name = self._parts[0].replace("-", "_")
            root = name if name in bound else "mcp." + _identifier(self._parts[0])
            return ".".join((root,) + self._parts[1:])

        def __repr__(self):
            name = self.__vis_doc_name__()
            return "<" + name + ": doc(" + name + ") prints its page>"

    class _Prompt(_Handle):
        __slots__ = ()

        def __call__(self, *args, **kwargs):
            server, _, name = self._parts
            params = {"name": name}
            arguments = _arguments(self, args, kwargs)
            if arguments:
                params["arguments"] = arguments
            return _verb()(server, "prompts/get", params)

    class _Prompts(_Handle):
        __slots__ = ()

        def __getattr__(self, name):
            return _Prompt(*self._parts, _public(name))

        def __getitem__(self, name):
            return _Prompt(*self._parts, name)

        def __call__(self):
            return _verb()(self._parts[0], "prompts/list")

    class _Tool(_Handle):
        __slots__ = ()

        def __call__(self, *args, **kwargs):
            server, tool = self._parts
            return _verb()(server, tool, _arguments(self, args, kwargs))

    class _Server(_Handle):
        __slots__ = ()

        def __getattr__(self, name):
            if name == "prompts":
                return _Prompts(*self._parts, "prompts")
            return _Tool(*self._parts, _public(name))

        def __getitem__(self, name):
            return _Tool(*self._parts, name)

        def __call__(self):
            return _verb()(self._parts[0])

        def __dir__(self):
            return ["prompts"]

    def _is_handle(value):
        return _b.getattr(_b.type(value), "__vis_mcp_handle__", False) is True

    def _bind(g, data):
        # Runs before each block. It reads only `_b`: a block global named `set`
        # or `str` must not stop the next block.
        try:
            rows = data["env"]["mcp"]["servers"]
        except _b.Exception:
            rows = {}
        wanted = {}
        for server, row in rows.items() if _b.isinstance(rows, _b.dict) else ():
            name = row.get("python_name") if _b.isinstance(row, _b.dict) else None
            if _b.isinstance(name, _b.str) and name.isidentifier():
                wanted[name] = _b.str(server)
        mine = _b.set(g.get("__vis_mcp_names__") or ())
        for name in mine - _b.set(wanted):
            if _is_handle(g.get(name)):
                g.pop(name, None)
        now = _b.set()
        for name, server in wanted.items():
            # A value of the session keeps its name: `mcp.<name>` is the server.
            if name not in g or _is_handle(g[name]):
                g[name] = _Server(server)
                now.add(name)
        protected = _b.set(g.get("__vis_protected_names__") or ())
        g["__vis_protected_names__"] = _b.sorted((protected - mine) | now)
        g["__vis_mcp_names__"] = _b.sorted(now)
        bound.clear()
        bound.update(now)

    g.setdefault("__vis_ctx_hooks__", {})["mcp"] = _bind

    # A module, so that `doc`, `apropos` and `help` read it as a namespace.
    class _Mcp(_types.ModuleType):
        def __getattr__(self, name):
            return _Server(_public(name))

        def __getitem__(self, name):
            return _Server(name)

        def __iter__(self):
            return iter(sorted(_servers()))

        def __contains__(self, name):
            return str(name).replace("_", "-") in _servers()

        def __dir__(self):
            return sorted(_identifier(name) for name in _servers())

        def __vis_doc_name__(self):
            return "mcp"

        def __repr__(self):
            return "<mcp: doc(mcp) prints its page>"

    docs = g.setdefault("__vis_docs__", {})
    calls = g.setdefault("__vis_calls__", {})
    calls["mcp"] = "await <python_name>.<tool>(**arguments)"
    docs["mcp"] = (
        "Each MCP server is a name of the sandbox, and each tool of the server is a "
        "function of that name. The name is the `python_name` of the server in "
        '`session["env"]["mcp"]["servers"]`: the server name in lower case, with `_` '
        "for each other character. So `await figma_dev.get_code(node=...)` calls the "
        "tool `get-code` of the server `figma-dev`. A tool takes keyword arguments, "
        "or one dict. The answer is {server, tool, content, is_error}, and the text "
        'of a content block is at content[i]["text"]. A tool with an output schema '
        "also gives `structured_content`."
        "\n\n`apropos(pattern)` finds the tools and the prompts of each server by "
        "name and by description. `doc(<python_name>.<tool>)` prints the description "
        "and the input schema of one tool, and `doc(<python_name>)` prints the tools "
        "and the prompts of the server. Each connects the server when that is "
        "necessary. `await <python_name>()` gives the tools with their input schemas "
        "as data."
        "\n\nWrite a tool name as an identifier: each character that an identifier "
        'cannot have becomes `_`. `figma_dev["get-code"]` takes the exact name.'
        "\n\nA server whose name Python or Vis already has, such as `time`, is a "
        "child of `mcp`: its `python_name` is `mcp.time`. A name that the session "
        "gave to a value before the server came stays with that value. `mcp.<name>` "
        'is the server then, and `mcp["<server>"]` takes the exact name of a server.'
        "\n\nPrompts: `doc(<python_name>.prompts.<name>)` prints one prompt, with "
        "its text when the prompt needs no argument. "
        "`await <python_name>.prompts.<name>(**arguments)` gets its messages, and "
        "`await <python_name>.prompts()` lists the prompts."
        "\n\nResources are attachments. `list_attachments()` also lists the resources "
        'of the connected servers, each with `source` "mcp". `get_attachment`, '
        "`read_attachment` and `show_attachment` take the `id` of such a row, or the "
        "URI of the resource."
    )

    # ONE text for one handle: `help(mcp)` and `doc(mcp)` read the same string.
    g["mcp"] = _Mcp("mcp", docs["mcp"])


__vis_install_mcp__()
del __vis_install_mcp__
