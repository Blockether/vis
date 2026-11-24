# vis sandbox MCP shim: one name for each MCP server.
#
# The host gives each server a `python_name` in `session["env"]["mcp"]["servers"]`:
# its name in lower-case snake form, with the prefix `mcp_` when Python or Vis has
# that name. Before each block this shim binds that name to a handle, so a block
# says `await linear.save_issue(title="...")`. There is no `mcp` name. The handles
# hold no catalog: a call goes through `__vis_mcp_call__`, the verb that the host
# binds for this shim, and `doc` and `inspect.signature` ask the host. So a tool
# that a server adds while the session runs is there at once.


def __vis_install_mcp__():
    import builtins as _b

    g = globals()
    # The sandbox name of each server that is bound now. `_bind` is its only writer.
    bound = {}

    def _door(name):
        door = g.get(name)
        if not callable(door):
            raise RuntimeError(
                "mcp: this session has no MCP server. Add one under `mcp.servers` "
                "in config.yml, then use /reload."
            )
        return door

    def _call(*args):
        return _door("__vis_mcp_call__")(*args)

    def _arguments(handle, args, kwargs):
        if len(args) > 1 or (args and not isinstance(args[0], dict)):
            raise TypeError(
                repr(handle) + " takes keyword arguments, or one dict of arguments"
            )
        merged = dict(args[0]) if args else {}
        merged.update(kwargs)
        return merged

    def _public(name):
        # A private name is never a tool: `hasattr` probes of the runtime
        # (`__wrapped__`, `_ipython_display_`) must get a plain miss.
        if name.startswith("_"):
            raise AttributeError(name)
        return name

    class _Unset:
        # The default of an optional argument in a signature.
        __slots__ = ()

        def __repr__(self):
            return "..."

    unset = _Unset()
    plain = {"string": str, "integer": int, "boolean": bool, "null": type(None)}

    def _union(members, unknown):
        # `A | B` of the members. A member without a type gives no type.
        if unknown in members or not members:
            return unknown
        whole = members[0]
        for member in members[1:]:
            whole = whole | member
        return whole

    def _annotation(schema, defs, depth=0):
        # The Python type of JSON schema `schema`, or `typing.Any` when it names none.
        import typing

        unknown = typing.Any
        if not isinstance(schema, dict) or depth > 8:
            return unknown

        def inner(part):
            return _annotation(part, defs, depth + 1)

        ref = schema.get("$ref")
        if isinstance(ref, str):
            return inner(defs.get(ref.rsplit("/", 1)[-1]))
        values = [schema["const"]] if "const" in schema else schema.get("enum")
        if isinstance(values, list) and values:
            # `Literal` takes no float and no container.
            if all(v is None or isinstance(v, (str, bool, int)) for v in values):
                return typing.Literal[tuple(values)]
        for key in ("anyOf", "oneOf"):
            parts = schema.get(key)
            if isinstance(parts, list) and parts:
                return _union([inner(part) for part in parts], unknown)
        parts = schema.get("allOf")
        if isinstance(parts, list) and len(parts) == 1:
            return inner(parts[0])
        word = schema.get("type")
        if isinstance(word, list):
            return _union([inner(dict(schema, type=one)) for one in word], unknown)
        if word == "number":
            return int | float
        if word == "array":
            items = inner(schema.get("items"))
            return list if items is unknown else list[items]
        if word == "object":
            values = inner(schema.get("additionalProperties"))
            return dict if values is unknown else dict[str, values]
        return plain.get(word, unknown) if isinstance(word, str) else unknown

    def _signature(handle):
        # The host reads the list of the server: a handle holds no schema.
        import inspect
        import json
        import keyword

        answer = json.loads(
            _door("__vis_mcp_parameters__")(json.dumps(list(handle._parts)))
        )
        if "error" in answer:
            raise ValueError(answer["error"])
        import typing

        empty = inspect.Parameter.empty
        defs = answer.get("defs") or {}
        named = []
        for row in answer["parameters"]:
            name = row["name"]
            if name.isidentifier() and not keyword.iskeyword(name):
                annotation = _annotation(row.get("schema"), defs)
                named.append(
                    inspect.Parameter(
                        name,
                        inspect.Parameter.KEYWORD_ONLY,
                        default=empty if row["required"] else unset,
                        annotation=empty if annotation is typing.Any else annotation,
                    )
                )
        if len(named) < len(answer["parameters"]):
            # A name that cannot be a parameter goes in one dict of arguments.
            taken = {parameter.name for parameter in named}
            rest = "arguments" if "arguments" not in taken else "more_arguments"
            named.append(inspect.Parameter(rest, inspect.Parameter.VAR_KEYWORD))
        named.sort(key=lambda p: (p.kind, p.default is not empty, p.name))
        return inspect.Signature(named, return_annotation=dict)

    class _Handle:
        __slots__ = ("_parts",)
        # Without this, `for x in handle` would index it for ever.
        __iter__ = None
        # `_bind` knows a handle of an older install of this shim by this mark.
        __vis_mcp_handle__ = True

        def __init__(self, *parts):
            object.__setattr__(self, "_parts", tuple(str(p) for p in parts))

        def __setattr__(self, name, value):
            raise AttributeError("an MCP handle is read-only")

        def __vis_doc_name__(self):
            server = self._parts[0]
            names = [name for name in bound if bound[name] == server]
            root = names[0] if names else server.replace("-", "_")
            return ".".join((root,) + self._parts[1:])

        def __repr__(self):
            name = self.__vis_doc_name__()
            return "<" + name + ": doc(" + name + ") prints its page>"

    class _Prompt(_Handle):
        __slots__ = ()
        __signature__ = property(_signature)

        def __call__(self, *args, **kwargs):
            server, _, name = self._parts
            params = {"name": name}
            arguments = _arguments(self, args, kwargs)
            if arguments:
                params["arguments"] = arguments
            return _call(server, "prompts/get", params)

    class _Prompts(_Handle):
        __slots__ = ()

        def __getattr__(self, name):
            return _Prompt(*self._parts, _public(name))

        def __getitem__(self, name):
            return _Prompt(*self._parts, name)

        def __call__(self):
            return _call(self._parts[0], "prompts/list")

    class _Tool(_Handle):
        __slots__ = ()
        __signature__ = property(_signature)

        def __call__(self, *args, **kwargs):
            server, tool = self._parts
            return _call(server, tool, _arguments(self, args, kwargs))

    class _Server(_Handle):
        __slots__ = ()

        def __getattr__(self, name):
            if name == "prompts":
                return _Prompts(*self._parts, "prompts")
            return _Tool(*self._parts, _public(name))

        def __getitem__(self, name):
            return _Tool(*self._parts, name)

        def __call__(self):
            return _call(self._parts[0])

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
        now = {}
        for name, server in wanted.items():
            # A value of the session keeps its name until the block deletes it.
            if name not in g or _is_handle(g[name]):
                g[name] = _Server(server)
                now[name] = server
        protected = _b.set(g.get("__vis_protected_names__") or ())
        g["__vis_protected_names__"] = _b.sorted((protected - mine) | _b.set(now))
        g["__vis_mcp_names__"] = _b.sorted(now)
        bound.clear()
        bound.update(now)

    g.setdefault("__vis_ctx_hooks__", {})["mcp"] = _bind

    docs = g.setdefault("__vis_docs__", {})
    calls = g.setdefault("__vis_calls__", {})
    calls["mcp"] = "await <python_name>.<tool>(**arguments)"
    # The page of the subject: `doc("mcp")` prints it. No name of the sandbox is `mcp`.
    docs["mcp"] = (
        "Each MCP server is a name of the sandbox, and each tool of the server is a "
        "function of that name. The name is the `python_name` of the server in "
        '`session["env"]["mcp"]["servers"]`: the server name in lower case, with `_` '
        "for each other character. So `await figma_dev.get_code(node=...)` calls the "
        "tool `get-code` of the server `figma-dev`. A tool takes keyword arguments, "
        "or one dict. The answer is {server, tool, content, is_error}, and the text "
        'of a content block is at content[i]["text"]. A tool with an output schema '
        "also gives `structured_content`."
        "\n\n`apropos(pattern)` finds the tools, the prompts and the skills of each server by "
        "name and by description. `doc(<python_name>.<tool>)` prints the description "
        "and the input schema of one tool, and `doc(<python_name>)` prints the tools "
        "and the prompts of the server. `inspect.signature(<python_name>.<tool>)` "
        "gives the parameters of one tool. Each connects the server when that is "
        "necessary. `await <python_name>()` gives the tools with their input schemas "
        "as data."
        "\n\nWrite a tool name as an identifier: each character that an identifier "
        'cannot have becomes `_`. `figma_dev["get-code"]` takes the exact name.'
        "\n\nA server whose name Python or Vis already has, such as `time`, gets "
        "the prefix `mcp_`: its `python_name` is `mcp_time`. A name that starts with "
        "a digit gets the prefix too. A name that the session gave to a value before "
        "the server came stays with that value: `del <name>` gives the name to the "
        "server at the next block."
        "\n\nPrompts: `doc(<python_name>.prompts.<name>)` prints one prompt, with "
        "its text when the prompt needs no argument. "
        "`await <python_name>.prompts.<name>(**arguments)` gets its messages, and "
        "`await <python_name>.prompts()` lists the prompts."
        '\n\nSkills: `doc("<python_name>.skills.<name>")` loads one skill: its verified '
        "`SKILL.md` and the list of its other files. "
        '`doc("<python_name>.skills.<name>/<path>")` loads one of those files.'
        "\n\nResources are attachments. `list_attachments()` also lists the resources "
        'of the connected servers, each with `source` "mcp". `get_attachment`, '
        "`read_attachment` and `show_attachment` take the `id` of such a row, or the "
        "URI of the resource."
    )


__vis_install_mcp__()
del __vis_install_mcp__
