# vis sandbox MCP shim: mcp.
#
# `mcp` gives each MCP server as a name and each of its tools as a function, so a
# block says `await mcp.linear.save_issue(title="...")`. The handles hold no
# catalog: a call goes through `mcp_call`, the one verb that the host binds, and
# `doc` asks the host for the page of a handle. So a tool that a server adds
# while the session runs is there at once.


def __vis_install_mcp__():
    import types as _types

    g = globals()

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

        def __init__(self, *parts):
            object.__setattr__(self, "_parts", tuple(str(p) for p in parts))

        def __setattr__(self, name, value):
            raise AttributeError("an mcp handle is read-only")

        def __vis_doc_name__(self):
            return ".".join(("mcp",) + self._parts)

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
            entry = _servers().get(self._parts[0].replace("_", "-"))
            tools = entry.get("tools", []) if isinstance(entry, dict) else []
            return sorted({_identifier(t) for t in tools} | {"prompts"})

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
    calls["mcp"] = "await mcp.<server>.<tool>(**arguments)"
    docs["mcp"] = (
        "`mcp` gives each MCP server as a name and each tool of the server as a "
        "function. `await mcp.<server>.<tool>(**arguments)` calls the tool with "
        "keyword arguments, or with one dict. The answer is that of `mcp_call`: "
        "{server, tool, content, is_error}. The text of a content block is at "
        'content[i]["text"].'
        "\n\nWrite a server name or a tool name as an identifier: each character "
        "that an identifier cannot have becomes `_`. So `mcp.figma_dev.get_code` "
        "calls the tool `get-code` of the server `figma-dev`. "
        '`mcp["figma-dev"]["get-code"]` takes the exact names.'
        "\n\n`doc(mcp.<server>.<tool>)` prints the description and the input schema "
        "of one tool. `doc(mcp.<server>)` prints the tools and the prompts of the "
        "server. `doc` connects the server when that is necessary. "
        "`await mcp.<server>()` gives the tools with their input schemas as data. "
        "`apropos(pattern)` finds the tools and the prompts of the connected servers."
        "\n\nPrompts: `doc(mcp.<server>.prompts.<name>)` prints one prompt, with its "
        "text when the prompt needs no argument. "
        "`await mcp.<server>.prompts.<name>(**arguments)` gets its messages, and "
        "`await mcp.<server>.prompts()` lists the prompts."
        "\n\nResources are attachments. `list_attachments()` also lists the resources "
        'of the connected servers, each with `source` "mcp". `get_attachment`, '
        "`read_attachment` and `show_attachment` take the `id` of such a row, or the "
        "URI of the resource."
        '\n\nThe servers and their status are in `session["env"]["mcp"]["servers"]`. '
        "`mcp_call(server, tool, args)` is the same call with the names as strings."
    )

    # ONE text for one handle: `help(mcp)` and `doc(mcp)` read the same string.
    g["mcp"] = _Mcp("mcp", docs["mcp"])


__vis_install_mcp__()
del __vis_install_mcp__
