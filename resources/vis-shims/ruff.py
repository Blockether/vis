# vis sandbox `ruff` shim.
#
# The agent sandbox ships no `ruff` wheel and there is no `ruff` binary on PATH,
# but the host already links ruff in-process (the `com.blockether/ruff` cdylib
# behind `format_code`/`lint_code` for Python). This shim exposes exactly that
# engine to sandbox Python:
#
#     import ruff                      -> format_str / check_str / format_file /
#                                         check_file / config_for / version
#     vis python -m ruff check  PATHS  -> lint report, exit 1 on findings
#     vis python -m ruff format PATHS  -> rewrite files (--check / --diff too)
#
# `-m` works because sandbox shims are synthesised modules the host marks with
# `__vis_shim__`: the module runner calls `console_main(sys.argv[1:])`, not runpy.
#
# Ruff's OWN configuration discovery is authoritative -- for every file the host
# walks up for `.ruff.toml`, `ruff.toml` or a `pyproject.toml` with
# `[tool.ruff]`. With no configuration anywhere, ruff's built-in defaults apply
# (E4, E7, E9, F; line length 88) and `check` says so on stderr.
#
# Not supported: --fix, --watch, --output-format, --show-settings, and config
# `exclude` walking (this walk skips the usual noise directories instead).


def __vis_install_ruff__():
    import builtins as _bi
    import os
    import sys
    import types

    _host_format = __vis_ruff_format__
    _host_lint = __vis_ruff_lint__
    _host_fix = __vis_ruff_fix__
    _host_config = __vis_ruff_config__
    _host_version = __vis_ruff_version__

    _NL = chr(10)

    class RuffError(Exception):
        """A ruff failure: unknown selector, bad configuration, unparsable source."""

    def _realize(value):
        """Turn a foreign host list/map into plain Python data."""
        is_foreign = globals().get("__vis_is_foreign__")
        if is_foreign is None or not is_foreign(value):
            return value
        if hasattr(value, "keys"):
            try:
                return {key: _realize(value[key]) for key in value.keys()}
            except Exception:
                return value
        try:
            return [_realize(item) for item in value]
        except Exception:
            return value

    def _unwrap(envelope):
        envelope = _realize(envelope)
        try:
            ok = envelope[0]
            payload = envelope[1]
        except Exception:
            raise RuffError("ruff: malformed host response")
        if not ok:
            raise RuffError(str(payload))
        return _realize(payload)

    # -- configuration -------------------------------------------------------

    _NO_CONFIG_HINT = (
        "warning: no ruff configuration found for {0} -- using ruff's built-in "
        "defaults (E4, E7, E9, F; line-length 88). Add a `ruff.toml`, "
        "`.ruff.toml`, or a `[tool.ruff]` table in `pyproject.toml` to pin the "
        "rules for this project."
    )

    def config_for(path):
        """Nearest ruff configuration file governing `path`, or None."""
        return _unwrap(_host_config(str(path)))

    def version():
        """`ruff X.Y.Z (vis shim, clj-ruff A.B.C)` -- ruff's own version first,
        the way the real CLI reports it, with the embedding cdylib after it."""
        raw = str(_unwrap(_host_version()))
        # The cdylib reports `<clj-ruff version> (ruff <ruff version>)`.
        head, _, tail = raw.partition("(ruff ")
        if tail:
            return (
                "ruff "
                + tail.rstrip(")")
                + " (vis shim, clj-ruff "
                + head.strip()
                + ")"
            )
        return raw

    # -- one source ----------------------------------------------------------

    def format_str(source, path=None, config=None, line_length=0):
        """Format Python `source`, returning the reformatted text."""
        return _unwrap(
            _host_format(source, path or "", config or "", int(line_length or 0))
        )

    def check_str(
        source,
        path=None,
        config=None,
        line_length=0,
        select=None,
        ignore=None,
        preview=False,
    ):
        """Lint Python `source`, returning a list of diagnostic dicts."""
        return _unwrap(
            _host_lint(
                source,
                path or "",
                config or "",
                int(line_length or 0),
                select or "",
                ignore or "",
                bool(preview),
            )
        )

    def fix_str(
        source,
        path=None,
        config=None,
        line_length=0,
        select=None,
        ignore=None,
        preview=False,
        unsafe_fixes=False,
    ):
        """Apply Ruff's safe fixes, plus unsafe fixes when explicitly enabled."""
        return _unwrap(
            _host_fix(
                source,
                path or "",
                config or "",
                int(line_length or 0),
                select or "",
                ignore or "",
                bool(preview),
                bool(unsafe_fixes),
            )
        )

    # -- files ---------------------------------------------------------------

    _SKIP_DIRS = {
        ".bzr",
        ".direnv",
        ".eggs",
        ".git",
        ".hg",
        ".mypy_cache",
        ".nox",
        ".pytype",
        ".ruff_cache",
        ".svn",
        ".tox",
        ".venv",
        "__pycache__",
        "__pypackages__",
        "build",
        "dist",
        "node_modules",
        "site-packages",
        "venv",
    }

    def _is_python(name):
        return name.endswith(".py") or name.endswith(".pyi")

    def _config_settings(config_path):
        """Read only the directory-walking options from Ruff's TOML config."""
        if not config_path:
            return {}
        try:
            import tomllib

            with open(config_path, "rb") as handle:
                data = tomllib.load(handle)
            if os.path.basename(config_path) == "pyproject.toml":
                data = data.get("tool", {}).get("ruff", {})
            return data if isinstance(data, dict) else {}
        except Exception:
            # The native engine remains authoritative and reports malformed TOML.
            return {}

    def _patterns(settings):
        patterns = []
        for key in ("exclude", "extend-exclude"):
            value = settings.get(key, [])
            if isinstance(value, str):
                value = [value]
            if isinstance(value, list):
                patterns.extend(str(item) for item in value)
        return patterns

    def _excluded(path, config_path, settings):
        if not config_path:
            return False
        root = os.path.dirname(config_path)
        relative = os.path.relpath(path, root).replace(os.sep, "/")
        import fnmatch

        for pattern in _patterns(settings):
            pattern = pattern.replace(os.sep, "/").lstrip("./")
            candidates = (relative, os.path.basename(relative), "**/" + relative)
            if any(fnmatch.fnmatch(candidate, pattern) for candidate in candidates):
                return True
            if relative == pattern or relative.startswith(pattern.rstrip("/") + "/"):
                return True
        return False

    def iter_files(paths, explicit_config=None):
        """Every Python file under `paths`, honoring Ruff excludes for directory targets."""
        out = []
        seen = set()
        for target in paths:
            target = os.path.abspath(target)
            if os.path.isfile(target):
                if target not in seen:
                    seen.add(target)
                    out.append(target)
                continue
            for root, dirs, files in os.walk(target):
                dirs[:] = sorted(d for d in dirs if d not in _SKIP_DIRS)
                config_path = explicit_config or config_for(root)
                settings = _config_settings(config_path)
                dirs[:] = [
                    d
                    for d in dirs
                    if not _excluded(os.path.join(root, d), config_path, settings)
                ]
                for name in sorted(files):
                    if not _is_python(name):
                        continue
                    full = os.path.join(root, name)
                    if _excluded(full, config_path, settings):
                        continue
                    if full not in seen:
                        seen.add(full)
                        out.append(full)
        return out

    def _read(path):
        with open(path, encoding="utf-8") as handle:
            return handle.read()

    def check_file(path, config=None, **kwargs):
        """Lint one file on disk with its own discovered configuration."""
        path = os.path.abspath(path)
        return check_str(
            _read(path), path=path, config=config or config_for(path), **kwargs
        )

    def fix_file(path, config=None, write=True, **kwargs):
        """Apply Ruff fixes to one file. Returns (changed, fixed_source)."""
        path = os.path.abspath(path)
        source = _read(path)
        fixed = fix_str(source, path=path, config=config or config_for(path), **kwargs)
        changed = fixed != source
        if changed and write:
            with open(path, "w", encoding="utf-8") as handle:
                handle.write(fixed)
        return changed, fixed

    def format_file(path, config=None, write=True, line_length=0):
        """Format one file on disk. Returns (changed, formatted_source)."""
        path = os.path.abspath(path)
        source = _read(path)
        formatted = format_str(
            source,
            path=path,
            config=config or config_for(path),
            line_length=line_length,
        )
        changed = formatted != source
        if changed and write:
            with open(path, "w", encoding="utf-8") as handle:
                handle.write(formatted)
        return changed, formatted

    # -- CLI -----------------------------------------------------------------

    _USAGE = _NL.join(
        [
            "usage: python -m ruff <command> [paths] [options]",
            "",
            "commands:",
            "  check    lint the given files/directories (default: .)",
            "  format   format the given files/directories (default: .)",
            "  version  print the bundled ruff version",
            "",
            "options:",
            "  --config PATH     ruff configuration file to use for every file",
            "  --fix             apply safe lint fixes",
            "  --unsafe-fixes    also apply fixes Ruff marks unsafe",
            "  --line-length N   override the configured line length",
            "  --select RULES    check: replace the selected rule set (e.g. E,F)",
            "  --ignore RULES    check: disable these rules on top of the selection",
            "  --preview         check: also run preview rules",
            "  --statistics      check: counts per rule instead of one line per finding",
            "  --check           format: do not write, exit 1 if a file would change",
            "  --diff            format: print a unified diff instead of writing",
            "  -q, --quiet       suppress the summary line",
            "  -h, --help        this message",
            "",
            "This is Vis's in-process Ruff shim; it uses the bundled Ruff engine, not a wheel.",
        ]
    )

    _VALUE_FLAGS = {"--config", "--line-length", "--select", "--ignore"}

    def _parse(argv):
        opts = {
            "config": None,
            "line_length": 0,
            "select": None,
            "ignore": None,
            "preview": False,
            "statistics": False,
            "fix": False,
            "unsafe_fixes": False,
            "check": False,
            "diff": False,
            "quiet": False,
            "help": False,
            "version": False,
        }
        paths = []
        rest = list(argv)
        while rest:
            arg = rest.pop(0)
            if arg in ("-h", "--help"):
                opts["help"] = True
            elif arg in ("-V", "--version"):
                opts["version"] = True
            elif arg in ("-q", "--quiet", "-s", "--silent"):
                opts["quiet"] = True
            elif arg == "--preview":
                opts["preview"] = True
            elif arg == "--fix":
                opts["fix"] = True
            elif arg == "--unsafe-fixes":
                opts["unsafe_fixes"] = True
            elif arg == "--statistics":
                opts["statistics"] = True
            elif arg == "--check":
                opts["check"] = True
            elif arg == "--diff":
                opts["diff"] = True
            elif arg in _VALUE_FLAGS:
                if not rest:
                    raise RuffError("ruff: " + arg + " requires a value")
                value = rest.pop(0)
                if arg == "--config":
                    opts["config"] = os.path.abspath(value)
                elif arg == "--line-length":
                    opts["line_length"] = int(value)
                elif arg == "--select":
                    opts["select"] = value
                else:
                    opts["ignore"] = value
            elif arg.startswith("--") and "=" in arg:
                head, _, value = arg.partition("=")
                rest.insert(0, value)
                rest.insert(0, head)
            elif arg.startswith("-") and arg != "-":
                raise RuffError("ruff: unsupported option " + arg)
            else:
                paths.append(arg)
        return opts, paths

    def _rel(path):
        """`path` relative to the CWD, but ABSOLUTE when it lives outside it --
        `../../../tmp/x.py` is noise, not a location."""
        try:
            rel = os.path.relpath(path)
        except ValueError:
            return path
        return path if rel.startswith("..") else rel

    def _warn_missing_config(files, quiet):
        """Tell the user, once, that this tree has no ruff configuration."""
        if quiet or not files:
            return
        without = [f for f in files if not config_for(f)]
        if not without:
            return
        where = os.path.dirname(without[0]) or "."
        sys.stderr.write(_NO_CONFIG_HINT.format(where) + _NL)

    def _check_main(paths, opts):
        files = iter_files(paths or ["."], opts["config"])
        _warn_missing_config(files, opts["quiet"])
        found = 0
        counts = {}
        for path in files:
            try:
                config = opts["config"] or config_for(path)
                if opts["fix"]:
                    settings = _config_settings(config)
                    fix_file(
                        path,
                        config=config,
                        select=opts["select"],
                        ignore=opts["ignore"],
                        preview=opts["preview"],
                        unsafe_fixes=opts["unsafe_fixes"]
                        or bool(settings.get("unsafe-fixes", False)),
                    )
                diagnostics = check_file(
                    path,
                    config=config,
                    line_length=opts["line_length"],
                    select=opts["select"],
                    ignore=opts["ignore"],
                    preview=opts["preview"],
                )
            except RuffError as error:
                sys.stderr.write(path + ": " + str(error) + _NL)
                found += 1
                continue
            for diagnostic in diagnostics:
                found += 1
                code = diagnostic.get("code") or "?"
                counts[code] = counts.get(code, 0) + 1
                if not opts["statistics"]:
                    sys.stdout.write(
                        "{0}:{1}:{2}: {3} {4}".format(
                            _rel(path),
                            diagnostic.get("row"),
                            diagnostic.get("col"),
                            code,
                            diagnostic.get("message"),
                        )
                        + _NL
                    )
        if opts["statistics"]:
            for code in sorted(counts, key=lambda c: (-counts[c], c)):
                sys.stdout.write("{0:>7}  {1}".format(counts[code], code) + _NL)
        if not opts["quiet"]:
            if found:
                sys.stdout.write(
                    "Found {0} error{1} in {2} file{3}.".format(
                        found,
                        "" if found == 1 else "s",
                        len(files),
                        "" if len(files) == 1 else "s",
                    )
                    + _NL
                )
            else:
                sys.stdout.write(
                    "All checks passed ({0} file{1}).".format(
                        len(files), "" if len(files) == 1 else "s"
                    )
                    + _NL
                )
        return 1 if found else 0

    def _format_main(paths, opts):
        import difflib

        files = iter_files(paths or ["."])
        changed = []
        failed = 0
        write = not (opts["check"] or opts["diff"])
        for path in files:
            try:
                did_change, formatted = format_file(
                    path,
                    config=opts["config"],
                    write=write,
                    line_length=opts["line_length"],
                )
            except RuffError as error:
                sys.stderr.write(path + ": " + str(error) + _NL)
                failed += 1
                continue
            if not did_change:
                continue
            changed.append(path)
            rel = _rel(path)
            if opts["diff"]:
                sys.stdout.write(
                    "".join(
                        difflib.unified_diff(
                            _read(path).splitlines(True),
                            formatted.splitlines(True),
                            fromfile=rel,
                            tofile=rel,
                        )
                    )
                )
            elif not write:
                sys.stdout.write("Would reformat: " + rel + _NL)
        if not opts["quiet"]:
            unchanged = len(files) - len(changed) - failed
            verb = "reformatted" if write else "would be reformatted"
            sys.stdout.write(
                "{0} file{1} {2}, {3} file{4} left unchanged.".format(
                    len(changed),
                    "" if len(changed) == 1 else "s",
                    verb,
                    unchanged,
                    "" if unchanged == 1 else "s",
                )
                + _NL
            )
        if failed:
            return 2
        return 1 if (changed and not write) else 0

    def console_main(argv=None):
        """`vis-agent python -m ruff ...` entry point. Returns a process exit code."""
        if argv is None:
            argv = sys.argv[1:]
        argv = list(argv)
        try:
            opts, paths = _parse(argv)
        except RuffError as error:
            sys.stderr.write(str(error) + _NL)
            return 2
        command = None
        if paths and paths[0] in ("check", "format", "version", "lint", "fmt"):
            command = paths.pop(0)
        if opts["help"] or (command is None and not paths and not opts["version"]):
            sys.stdout.write(_USAGE + _NL)
            return 0
        if opts["version"] or command == "version":
            sys.stdout.write(version() + _NL)
            return 0
        try:
            if command in ("format", "fmt"):
                return _format_main(paths, opts)
            return _check_main(paths, opts)
        except RuffError as error:
            sys.stderr.write(str(error) + _NL)
            return 2

    module = types.ModuleType("ruff")
    module.RuffError = RuffError
    module.config_for = config_for
    module.version = version
    try:
        module.__version__ = version()
    except Exception:
        # The host binding is the only source of truth; never fail the import
        # over a version string (the host stamps a fallback anyway).
        pass
    module.format_str = format_str
    module.check_str = check_str
    module.fix_str = fix_str
    module.format_file = format_file
    module.check_file = check_file
    module.fix_file = fix_file
    module.iter_files = iter_files
    module.console_main = console_main
    module.main = console_main
    module.__doc__ = (
        "In-process Ruff, no pip or PATH: `python -m ruff check|format <paths>` (`--fix`, "
        "`--check`, `--diff`) and the `format_/check_/fix_{str,file}` import API, honouring "
        "the nearest config per file. A reduced in-process shim, not a full Ruff wheel."
    )
    sys.modules["ruff"] = module
    try:
        _bi.ruff = module
    except Exception:
        pass


__vis_install_ruff__()
del __vis_install_ruff__
