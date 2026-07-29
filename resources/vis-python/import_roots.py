# Import roots a Python project DECLARES, read for `vis python`.
#
# Parsed with Python's own parsers -- `tomllib` for `pyproject.toml`,
# `configparser` for `setup.cfg` / `pytest.ini` / `tox.ini` -- never a regex
# over the file text. Returns the RAW paths, in declaration order, exactly as
# written; existence checks, `~` expansion and canonicalisation stay on the vis
# side. Unreadable or absent metadata yields an empty list, never an error.


def __vis_declared_import_roots__(project_dir):
    import configparser
    import os
    import tomllib

    roots = []

    def add(value):
        if isinstance(value, str) and value.strip():
            roots.append(value.strip())

    def add_each(value):
        if isinstance(value, str):
            add(value)
        elif isinstance(value, (list, tuple)):
            for item in value:
                add(item)

    def table(data, *path):
        for key in path:
            if not isinstance(data, dict):
                return {}
            data = data.get(key, {})
        return data if isinstance(data, dict) else {}

    pyproject = os.path.join(project_dir, "pyproject.toml")
    if os.path.isfile(pyproject):
        try:
            with open(pyproject, "rb") as fh:
                data = tomllib.load(fh)
        except Exception:
            data = {}

        tool = table(data, "tool")

        # [tool.setuptools.packages.find]  where = ["src"]
        add_each(table(tool, "setuptools", "packages", "find").get("where"))

        # [tool.setuptools]  package-dir = {"" = "src"}  (or a bare string)
        package_dir = table(tool, "setuptools").get("package-dir")
        if isinstance(package_dir, dict):
            for value in package_dir.values():
                add(value)
        else:
            add_each(package_dir)

        # [tool.pdm.build]  package-dir = "src"
        add_each(table(tool, "pdm", "build").get("package-dir"))

        # [tool.pytest.ini_options]  pythonpath = ["src"] -- pytest's own option.
        add_each(table(tool, "pytest", "ini_options").get("pythonpath"))

        # [tool.poetry]  packages = [{include = "pkg", from = "src"}]
        packages = table(tool, "poetry").get("packages")
        if isinstance(packages, (list, tuple)):
            for entry in packages:
                if isinstance(entry, dict):
                    add(entry.get("from"))

        # [tool.hatch.build.targets.wheel]  packages = ["src/pkg"] -- the PARENT
        # directory is the import root.
        hatch = table(tool, "hatch", "build", "targets", "wheel").get("packages")
        if isinstance(hatch, str):
            hatch = [hatch]
        if isinstance(hatch, (list, tuple)):
            for entry in hatch:
                if isinstance(entry, str):
                    add(os.path.dirname(entry.strip()))

    def sections(name):
        path = os.path.join(project_dir, name)
        if not os.path.isfile(path):
            return None
        # Raw: setup.cfg/tox.ini values legitimately carry `%` and `{}` that no
        # interpolation should touch, and duplicates must not abort the read.
        parser = configparser.RawConfigParser(strict=False)
        try:
            parser.read(path, encoding="utf-8")
        except Exception:
            return None
        return parser

    def option(parser, section, key):
        if parser is None or not parser.has_option(section, key):
            return ""
        try:
            return parser.get(section, key) or ""
        except Exception:
            return ""

    # setup.cfg [options]  package_dir =\n    =src   (also `pkg = src`)
    setup_cfg = sections("setup.cfg")
    for line in option(setup_cfg, "options", "package_dir").splitlines():
        if line.strip():
            add(line.rsplit("=", 1)[-1])

    # pytest's `pythonpath` wherever pytest accepts it outside pyproject.toml.
    for parser, section in (
        (setup_cfg, "tool:pytest"),
        (sections("pytest.ini"), "pytest"),
        (sections("tox.ini"), "pytest"),
    ):
        for entry in option(parser, section, "pythonpath").replace(",", " ").split():
            add(entry)

    return roots
