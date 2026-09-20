"""GitHub and local project admission share one inert manifest contract."""

import io
import json
import subprocess
from types import SimpleNamespace

import pytest
from blockether.vis import extension_package as package

MANIFEST = """[project]
name = "vis-greeter"
version = "1.0.0"
description = "Small greeting tools."
requires-python = ">=3.11"
dependencies = ["vis-agent>=0.1.0"]
[tool.vis]
category = "tools"
source_paths = ["src"]
"""
REPOSITORY = "https://github.com/example/extensions"


def project(directory, manifest=MANIFEST):
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "pyproject.toml").write_text(manifest)
    (directory / "extension.py").write_text("raise RuntimeError('not executed')")
    (directory / "src").mkdir()
    (directory / "src/greeter.py").write_text("VALUE = 1")
    return directory


@pytest.mark.parametrize("suffix", ["", "/", ".git"])
@pytest.mark.parametrize(
    "source",
    [
        REPOSITORY,
        "example/extensions",
        "Example/Extensions",
        "https://github.com/Example/Extensions",
    ],
)
def test_normalize_github_repository(source, suffix):
    assert package.github_repository(source + suffix) == REPOSITORY


@pytest.mark.parametrize(
    "url",
    [
        "http://github.com/example/extensions",
        "https://github.com.evil.test/a/b",
        "https://user@github.com/a/b",
        "https://github.com:443/a/b",
        "https://127.0.0.1/a/b",
        "https://github.com/a/b/tree/main/src",
        "https://github.com/a/b?url=elsewhere",
        "git@github.com:a/b",
        "https://github.com/a/..",
        "https://github.com/a/%2e%2e",
        "vis-greeter",
        "/example/extensions",
        "example/extensions/extra",
        "example/..",
        "example//extensions",
        "example/extensions?unsafe",
        "example/extensions#main",
        "example\\extensions",
    ],
)
def test_only_public_github_repository_slugs_and_urls_are_accepted(url):
    with pytest.raises(ValueError, match="GitHub"):
        package.github_repository(url)


@pytest.mark.parametrize(
    "directory",
    ["../outside", "/absolute", "src/../other", "a\\b", ".git", "src//tools"],
)
def test_subdirectory_cannot_escape_the_repository(directory):
    with pytest.raises(ValueError):
        package.project_subdirectory(directory)


@pytest.mark.parametrize(
    "source",
    [
        "example/extensions/plugins/greeting",
        "Example/Extensions/plugins/greeting",
        "https://github.com/example/extensions/plugins/greeting",
        "https://github.com/Example/Extensions/plugins/greeting/",
        "https://vis.blockether.com/extensions/example/extensions/plugins/greeting",
    ],
)
def test_catalog_identifier_carries_the_project_folder(source):
    assert package.github_source(source) == (REPOSITORY, "plugins/greeting")
    assert package.github_source(source, "plugins/greeting") == (
        REPOSITORY,
        "plugins/greeting",
    )
    with pytest.raises(ValueError, match="once"):
        package.github_source(source, "plugins/other")


@pytest.mark.parametrize(
    "source",
    ["example/extensions", REPOSITORY, REPOSITORY + "/", "Example/Extensions.git"],
)
def test_identifier_without_a_folder_selects_the_repository_root(source):
    assert package.github_source(source) == (REPOSITORY, "")
    assert package.github_source(source, "plugins/greeting") == (
        REPOSITORY,
        "plugins/greeting",
    )


@pytest.mark.parametrize(
    "source",
    [
        "vis-greeter",
        "example",
        "https://vis.blockether.com/extensions/example",
        "https://example.test/example/extensions/plugins/greeting",
        "example//extensions/plugins",
    ],
)
def test_catalog_identifier_needs_an_owner_and_repository(source):
    with pytest.raises(ValueError, match="GitHub"):
        package.github_source(source)


@pytest.mark.parametrize("folder", ["../outside", "plugins/../other", ".git/hooks"])
def test_catalog_identifier_folder_cannot_escape_the_repository(folder):
    with pytest.raises(ValueError):
        package.github_source("example/extensions/" + folder)


def test_manifest_inspection_never_imports_code(tmp_path):
    source = project(tmp_path / "source")
    with (source / "pyproject.toml").open("a") as manifest:
        manifest.write(
            '\n[project.urls]\nRepository = "https://github.com/other/project"\n'
        )
    (source.parent / "receipt.json").write_text(
        json.dumps({"repository_url": "https://github.com/other/project"})
    )
    metadata = package.inspect_source(source)
    assert metadata["name"] == "vis-greeter"
    assert metadata["category"] == "tools"
    assert "repository" not in metadata


@pytest.mark.parametrize(
    "manifest",
    [
        MANIFEST.replace('"vis-agent>=0.1.0"', '"httpx"'),
        MANIFEST.replace('category = "tools"', 'category = "unknown"'),
        MANIFEST.replace('version = "1.0.0"', 'version = "invalid"'),
        MANIFEST.replace('source_paths = ["src"]', 'source_paths = ["../outside"]'),
    ],
)
def test_invalid_metadata_is_rejected(manifest):
    with pytest.raises(ValueError):
        package.manifest_metadata(manifest)


def test_local_subdirectory_links_source_and_never_overwrites(tmp_path):
    repository = tmp_path / "source"
    source = project(repository / "plugins/greeting")
    target = tmp_path / "installed"
    with pytest.raises(ValueError, match="trust"):
        package.install(str(repository), target, subdirectory="plugins/greeting")
    assert not target.exists()
    result = package.install(
        str(repository), target, trust=True, subdirectory="plugins/greeting"
    )
    assert result["mode"] == "source"
    assert (target / "vis-greeter/current").is_symlink()
    (source / "extension.py").write_text("VALUE = 2")
    assert (target / "vis-greeter/current/extension.py").read_text() == "VALUE = 2"
    with pytest.raises(FileExistsError):
        package.install(str(source / "pyproject.toml"), target, trust=True)


def test_incompatible_vis_and_escaping_entry_are_rejected(tmp_path):
    source = project(
        tmp_path / "source", MANIFEST.replace("vis-agent>=0.1.0", "vis-agent>=9999")
    )
    with pytest.raises(ValueError, match="Vis"):
        package.install(
            str(source), tmp_path / "installed", trust=True, vis_version="0.1.45"
        )
    (source / "extension.py").unlink()
    (source / "extension.py").symlink_to(tmp_path / "outside.py")
    (tmp_path / "outside.py").write_text("VALUE = 1")
    with pytest.raises(ValueError, match="inside"):
        package.inspect_source(source)


def test_removed_archive_and_registry_inputs_are_not_supported(tmp_path):
    source = tmp_path / "old.zip"
    source.write_bytes(b"not a project")
    for value in [str(source), "vis-greeter@1.0.0"]:
        with pytest.raises(ValueError, match="GitHub|directory"):
            package.install(value, tmp_path / "installed", trust=True)
    assert not hasattr(package, "inspect_archive")
    assert not hasattr(package, "registry_base")


def test_real_git_checkout_is_pinned_and_installs_only_selected_project(
    tmp_path, monkeypatch
):
    repository = tmp_path / "repository"
    project(repository / "plugins/greeting")
    (repository / "unrelated.txt").write_text("not installed")

    def git(*args):
        return subprocess.run(
            ["git", "-C", str(repository), *args],
            check=True,
            capture_output=True,
            text=True,
        ).stdout.strip()

    git("init")
    git("add", ".")
    git(
        "-c",
        "user.name=Test Author",
        "-c",
        "user.email=test@example.com",
        "commit",
        "-m",
        "fixture",
    )
    revision = git("rev-parse", "HEAD")
    (repository / "plugins/greeting/src/greeter.py").write_text("VALUE = 2")
    git("add", ".")
    git(
        "-c",
        "user.name=Test Author",
        "-c",
        "user.email=test@example.com",
        "commit",
        "-m",
        "second fixture",
    )
    original = subprocess.run
    commands = []

    def local_transport(args, **kwargs):
        commands.append(args)
        args = [
            str(repository)
            if arg == REPOSITORY
            else "protocol.file.allow=always"
            if arg == "protocol.file.allow=never"
            else arg
            for arg in args
        ]
        return original(args, **kwargs)

    monkeypatch.setattr(package.subprocess, "run", local_transport)
    target = tmp_path / "installed"
    result = package.install(
        REPOSITORY,
        target,
        trust=True,
        subdirectory="plugins/greeting",
        revision=revision,
    )
    assert result["mode"] == "github"
    assert result["revision"] == revision
    assert (target / "vis-greeter/current/src/greeter.py").read_text() == "VALUE = 1"
    assert not (target / "vis-greeter/current/.git").exists()
    assert not (target / "vis-greeter/current/unrelated.txt").exists()
    assert commands
    assert sorted(p.name for p in target.iterdir()) == [".gitignore", "vis-greeter"]


def test_remote_failure_and_invalid_revision_leave_no_installation(
    tmp_path, monkeypatch
):
    target = tmp_path / "installed"

    def fail(*args):
        raise ValueError("Could not fetch repository")

    monkeypatch.setattr(package, "_checkout", fail)
    with pytest.raises(ValueError, match="fetch"):
        package.install(REPOSITORY, target, trust=True, revision="a" * 40)
    assert sorted(p.name for p in target.iterdir()) == [".gitignore"]
    with pytest.raises(ValueError, match="revision"):
        package.install(REPOSITORY, target, trust=True, revision="--upload-pack=bad")


@pytest.mark.parametrize("skills", ['["skills/greeting"]', "[]"])
def test_manifest_declares_skills_without_executing_code(tmp_path, skills):
    source = project(tmp_path / "source", MANIFEST + f"skills = {skills}\n")
    skill = source / "skills/greeting"
    skill.mkdir(parents=True)
    (skill / "SKILL.md").write_text(
        "---\nname: greeting\ndescription: Greet a person.\n---\nRead the tool contract."
    )
    metadata = package.inspect_source(source)
    assert metadata["skills"] == (["skills/greeting"] if skills != "[]" else [])


@pytest.mark.parametrize(
    "skills",
    [
        '"skills/greeting"',
        '["../outside"]',
        '["skills/a", "skills/a"]',
        '["/absolute"]',
    ],
)
def test_invalid_skill_declarations_are_rejected(skills):
    with pytest.raises(ValueError, match="skills|path"):
        package.manifest_metadata(MANIFEST + f"skills = {skills}\n")


def test_skill_paths_and_resources_stay_inside_package(tmp_path):
    source = project(tmp_path / "source", MANIFEST + 'skills = ["skills/greeting"]\n')
    with pytest.raises(ValueError, match="SKILL.md"):
        package.inspect_source(source)
    skill = source / "skills/greeting"
    skill.mkdir(parents=True)
    (skill / "SKILL.md").write_text("Greet a person.")
    (tmp_path / "private.txt").write_text("must not be read")
    (skill / "reference.txt").symlink_to(tmp_path / "private.txt")
    with pytest.raises(ValueError, match="inside"):
        package.inspect_source(source)


@pytest.fixture
def releases(tmp_path, monkeypatch):
    repository = project(tmp_path / "repository" / "plugins/greeting").parents[1]
    original = subprocess.run

    def git(*args):
        return original(
            ["git", "-C", str(repository), *args],
            check=True,
            capture_output=True,
            text=True,
        ).stdout.strip()

    git("init")
    metadata = []
    for version in ("1.0.0", "1.1.0", "2.0.0rc1"):
        (repository / "plugins/greeting/pyproject.toml").write_text(
            MANIFEST.replace('version = "1.0.0"', f'version = "{version}"')
        )
        (repository / "plugins/greeting/src/greeter.py").write_text(
            f'VERSION = "{version}"'
        )
        git("add", ".")
        git(
            "-c",
            "user.name=Test Author",
            "-c",
            "user.email=test@example.com",
            "commit",
            "-m",
            version,
        )
        metadata.append(
            {
                "name": "vis-greeter",
                "version": version,
                "revision": git("rev-parse", "HEAD"),
                "repository_url": REPOSITORY,
                "subdirectory": "plugins/greeting",
                "release_tag": "v" + version,
                "prerelease": "rc" in version,
            }
        )

    commands = []

    def transport(args, **kwargs):
        commands.append(args)
        return original(
            [
                str(repository)
                if arg == REPOSITORY
                else "protocol.file.allow=always"
                if arg == "protocol.file.allow=never"
                else arg
                for arg in args
            ],
            **kwargs,
        )

    monkeypatch.setattr(package.subprocess, "run", transport)
    monkeypatch.setattr(package, "_catalog", lambda *_: {"releases": metadata})
    return metadata, tmp_path / "installed", commands


@pytest.mark.parametrize("scope", ["workspace/.vis/extensions", "home/.vis/extensions"])
def test_installed_sources_use_name_and_version_without_staging_layers(
    releases, tmp_path, scope
):
    _, _, _ = releases
    target = tmp_path / scope
    result = package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    version = target / "vis-greeter/1.0.0"
    assert result["path"] == str(version)
    assert (version / "extension.py").is_file()
    assert (version / "receipt.json").is_file()
    assert (target / "vis-greeter/current").readlink().as_posix() == "1.0.0"
    assert sorted(p.name for p in target.iterdir()) == [".gitignore", "vis-greeter"]
    assert sorted(p.name for p in version.parent.iterdir()) == ["1.0.0", "current"]


def test_installer_state_is_ignored_without_hiding_authored_scripts(releases, tmp_path):
    _, _, _ = releases
    workspace = tmp_path / "workspace"
    workspace.mkdir()
    subprocess.run(["git", "init", str(workspace)], check=True, capture_output=True)
    target = workspace / ".vis/extensions"
    package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting", save=True
    )
    (target / "custom.py").write_text("# Authored extension\n")
    status = subprocess.run(
        ["git", "-C", str(workspace), "status", "--porcelain", "--untracked-files=all"],
        check=True,
        capture_output=True,
        text=True,
    )
    assert status.stdout.strip() == "?? .vis/extensions/custom.py"


@pytest.mark.parametrize("source", ["example/extensions", REPOSITORY])
def test_version_install_update_and_rollback_use_real_pinned_checkouts(
    releases, source
):
    metadata, target, commands = releases
    first = package.install(
        source, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    active = target / "vis-greeter/current"
    initial = active.resolve()
    assert first["revision"] == metadata[0]["revision"]
    inspected = package.inspect_source(active)
    assert inspected["name"] == "vis-greeter"
    assert (
        inspected["repository"]
        == REPOSITORY.removeprefix("https://github.com/").lower()
    )
    assert active.is_symlink()
    assert '"1.0.0"' in (active / "src/greeter.py").read_text()
    status = package.versions(source, directory=target)
    assert status["installed"] == "1.0.0"
    assert status["latest"] == "1.1.0"
    assert status["update_available"]
    assert [r["version"] for r in status["releases"]] == ["2.0.0rc1", "1.1.0", "1.0.0"]
    updated = package.update(source, target, trust=True)
    assert updated["version"] == "1.1.0"
    assert initial.exists()
    assert active.resolve() != initial
    assert not package.versions(source, directory=target)["update_available"]
    restored = package.rollback(source, target, trust=True)
    assert restored["revision"] == first["revision"]
    assert package.inspect_source(active)["repository"] == inspected["repository"]
    assert '"1.0.0"' in (active / "src/greeter.py").read_text()
    assert all(args[-1] != "HEAD" for args in commands if "fetch" in args)
    assert len([args for args in commands if "fetch" in args]) == 2
    assert updated["repository"] == "example/extensions"
    assert restored["repository"] == "example/extensions"


@pytest.mark.parametrize("source", ["example/extensions", REPOSITORY])
def test_versions_before_install_preserve_the_selected_folder(releases, source):
    _, target, commands = releases
    status = package.versions(source, subdirectory="plugins/greeting", directory=target)
    assert status["installed"] is None
    assert status["latest"] == "1.1.0"
    assert status["subdirectory"] == "plugins/greeting"
    assert not target.exists()
    assert not commands


@pytest.mark.parametrize(
    "operation", [package.versions, package.update, package.rollback]
)
def test_package_names_are_not_repository_selectors(releases, operation):
    _, target, _ = releases
    package.install(REPOSITORY, target, trust=True, subdirectory="plugins/greeting")
    options = {} if operation is package.versions else {"trust": True}
    with pytest.raises(ValueError, match="GitHub"):
        operation("vis-greeter", directory=target, **options)


@pytest.mark.parametrize("operation", [package.update, package.rollback])
def test_repository_resolution_stays_in_selected_scope_and_folder(releases, operation):
    _, target, _ = releases
    package.install(REPOSITORY, target, trust=True, subdirectory="plugins/greeting")
    for directory, folder in [
        (target, "plugins/other"),
        (target.parent / "other-scope", None),
    ]:
        with pytest.raises(ValueError, match="No managed GitHub installation"):
            operation("example/extensions", directory, trust=True, subdirectory=folder)


@pytest.mark.parametrize(
    "operation", [package.versions, package.update, package.rollback]
)
@pytest.mark.parametrize("other_folder", ["", "Plugins/Other"])
def test_monorepo_selectors_require_an_unambiguous_installed_folder(
    releases, operation, other_folder, monkeypatch
):
    # A repository slug must not silently select one of two installed projects.
    _, target, _ = releases
    package.install(REPOSITORY, target, trust=True, subdirectory="plugins/greeting")
    other = target / "another-package/1.1.0"
    project(
        other,
        MANIFEST.replace("vis-greeter", "another-package").replace(
            '"1.0.0"', '"1.1.0"'
        ),
    )
    receipt = json.loads((target / "vis-greeter/1.1.0/receipt.json").read_text())
    receipt.update(name="another-package", subdirectory=other_folder)
    (other / "receipt.json").write_text(json.dumps(receipt))
    (target / "another-package/current").symlink_to("1.1.0")
    options = {} if operation is package.versions else {"trust": True}
    with pytest.raises(ValueError, match="--subdirectory"):
        operation("example/extensions", directory=target, **options)
    if operation is package.rollback:
        with pytest.raises(ValueError, match="No previous"):
            operation(
                "example/extensions",
                directory=target,
                subdirectory="plugins/greeting",
                **options,
            )
    else:
        result = operation(
            "example/extensions",
            directory=target,
            subdirectory="plugins/greeting",
            **options,
        )
        assert result.get("installed", result.get("version")) == "1.1.0"
    assert (target / "another-package/current").resolve() == other
    monkeypatch.setattr(package, "_catalog", lambda *_: {"releases": [receipt]})
    status = package.versions(
        "Example/Extensions", directory=target, subdirectory=other_folder or "."
    )
    assert status["installed"] == "1.1.0"
    assert status["subdirectory"] == other_folder
    if other_folder:
        with pytest.raises(ValueError, match="No managed GitHub installation"):
            package.update(
                "example/extensions",
                target,
                trust=True,
                subdirectory=other_folder.lower(),
            )


def test_repository_slug_does_not_silently_select_a_local_directory(
    releases, tmp_path, monkeypatch
):
    _, target, _ = releases
    local = project(tmp_path / "example/extensions")
    monkeypatch.chdir(tmp_path)
    installed = package.install(
        "example/extensions",
        target,
        trust=True,
        subdirectory="plugins/greeting",
        version="1.0.0",
    )
    assert installed["mode"] == "github"
    linked = package.install("./example/extensions", tmp_path / "linked", trust=True)
    assert linked["mode"] == "source"
    assert (tmp_path / "linked/vis-greeter/current").resolve() == local


def test_catalog_identifier_selects_the_folder_for_every_lifecycle_command(releases):
    _, target, _ = releases
    installed = package.install(
        "example/extensions/plugins/greeting", target, trust=True, version="1.0.0"
    )
    assert installed["mode"] == "github"
    assert installed["subdirectory"] == "plugins/greeting"
    status = package.versions("example/extensions/plugins/greeting", directory=target)
    assert (status["subdirectory"], status["installed"]) == (
        "plugins/greeting",
        "1.0.0",
    )
    updated = package.update(
        "https://vis.blockether.com/extensions/example/extensions/plugins/greeting",
        target,
        trust=True,
    )
    assert updated["version"] == "1.1.0"
    restored = package.rollback(
        "https://github.com/example/extensions/plugins/greeting", target, trust=True
    )
    assert restored["version"] == "1.0.0"


def test_local_project_keeps_priority_over_a_catalog_folder(
    releases, tmp_path, monkeypatch
):
    _, target, _ = releases
    local = project(tmp_path / "example/extensions/plugins/greeting")
    monkeypatch.chdir(tmp_path)
    linked = package.install(
        "example/extensions/plugins/greeting", tmp_path / "linked", trust=True
    )
    assert linked["mode"] == "source"
    assert (tmp_path / "linked/vis-greeter/current").resolve() == local
    # An explicit release selector names the catalog, not the local folder.
    pinned = package.install(
        "example/extensions/plugins/greeting", target, trust=True, version="1.0.0"
    )
    assert (pinned["mode"], pinned["subdirectory"]) == ("github", "plugins/greeting")


def test_default_install_selects_stable_and_prerelease_is_explicit(releases):
    metadata, target, _ = releases
    installed = package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting"
    )
    assert installed["revision"] == metadata[1]["revision"]
    pre = package.update("example/extensions", target, trust=True, version="2.0.0rc1")
    assert pre["revision"] == metadata[2]["revision"]
    restored = package.rollback(
        "example/extensions", target, trust=True, version="1.0.0"
    )
    assert restored["version"] == "1.0.0"


def test_rollback_reuses_the_version_and_preserves_local_edits(releases):
    _, target, _ = releases
    package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    saved = (target / "vis-greeter/current").resolve()
    (saved / "src/greeter.py").write_text("LOCAL_EDIT = True")
    package.update("example/extensions", target, trust=True)
    package.rollback("example/extensions", target, trust=True)
    assert (saved / "src/greeter.py").read_text() == "LOCAL_EDIT = True"
    assert (target / "vis-greeter/current").resolve() == saved
    assert (
        target / "vis-greeter/current/src/greeter.py"
    ).read_text() == "LOCAL_EDIT = True"


@pytest.mark.parametrize(
    "failure", ["fetch", "manifest", "copy", "activate", "catalog"]
)
def test_failed_updates_leave_the_current_package_and_receipt_intact(
    releases, monkeypatch, failure
):
    metadata, target, _ = releases
    package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    active = (target / "vis-greeter/current").resolve()
    receipt = (active / "receipt.json").read_bytes()

    def fail(*_args, **_kwargs):
        raise ValueError("Fixture failure")

    if failure == "manifest":
        metadata[1]["name"] = "another-name"
    else:
        owner, attribute = (
            (package.os, "replace")
            if failure == "activate"
            else (
                package,
                {"fetch": "_checkout", "copy": "_copy_project", "catalog": "_catalog"}[
                    failure
                ],
            )
        )
        monkeypatch.setattr(owner, attribute, fail)
    with pytest.raises(ValueError):
        package.update("example/extensions", target, trust=True)
    assert (target / "vis-greeter/current").resolve() == active
    assert (active / "receipt.json").read_bytes() == receipt
    assert sorted(p.name for p in active.parent.iterdir()) == ["1.0.0", "current"]
    assert not list(target.glob("*.install-lock"))


def test_version_lookup_and_lifecycle_fail_closed(releases, monkeypatch):
    _, target, _ = releases
    with pytest.raises(ValueError, match="not approved"):
        package.install(
            REPOSITORY,
            target,
            trust=True,
            subdirectory="plugins/greeting",
            version="99.0.0",
        )
    assert not target.exists()
    package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    active = (target / "vis-greeter/current").resolve()
    with pytest.raises(ValueError, match="No previous"):
        package.rollback("example/extensions", target, trust=True)
    for operation in (package.update, package.rollback):
        with pytest.raises(ValueError, match="trust"):
            operation("example/extensions", target)
    with pytest.raises(ValueError, match="older"):
        package.rollback("example/extensions", target, trust=True, version="1.1.0")
    package.update("example/extensions", target, trust=True)
    with pytest.raises(ValueError, match="rollback"):
        package.update("example/extensions", target, trust=True, version="1.0.0")
    unchanged = package.update("example/extensions", target, trust=True)
    assert "Already installed" in unchanged["next"]
    monkeypatch.setattr(
        package, "_catalog", lambda *_: (_ for _ in ()).throw(ValueError("offline"))
    )
    assert (
        package.rollback("example/extensions", target, trust=True)["version"] == "1.0.0"
    )
    assert active.exists()


def test_local_source_links_and_unmanaged_directories_are_never_replaced(tmp_path):
    source = project(tmp_path / "source")
    target = tmp_path / "installed"
    package.install(str(source), target, trust=True)
    for operation in (package.update, package.rollback):
        with pytest.raises(ValueError, match="No managed GitHub installation"):
            operation("example/extensions", target, trust=True)
    with pytest.raises(ValueError, match="version"):
        package.install(str(source), target, trust=True, version="1.0.0")
    assert (target / "vis-greeter/current").resolve() == source


def test_catalog_version_identity_is_checked_before_fetch(releases):
    metadata, target, commands = releases
    metadata[0]["repository_url"] = "https://github.com/other/repository"
    with pytest.raises(ValueError, match="Invalid release"):
        package.install(
            REPOSITORY,
            target,
            trust=True,
            subdirectory="plugins/greeting",
            version="1.0.0",
        )
    assert not commands
    assert not target.exists()


def test_conflicting_selectors_do_not_access_network(tmp_path):
    with pytest.raises(ValueError, match="not both"):
        package.install(
            REPOSITORY, tmp_path, trust=True, version="1.0.0", revision="a" * 40
        )


def test_changed_installation_is_not_overwritten_after_network_preparation(
    releases, monkeypatch
):
    _, target, _ = releases
    package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    original = package._checkout

    def replace_during_fetch(*args):
        revision = original(*args)
        active = target / "vis-greeter/current"
        active.unlink()
        active.symlink_to(target / "user-source")
        project(target / "user-source")
        return revision

    monkeypatch.setattr(package, "_checkout", replace_during_fetch)
    with pytest.raises(ValueError, match="version"):
        package.update("example/extensions", target, trust=True)
    assert (target / "vis-greeter/current").resolve() == target / "user-source"


def test_catalog_uses_one_fixed_https_origin_and_refuses_redirects(monkeypatch):
    requests = []

    def open_request(request, timeout):
        requests.append((request.full_url, timeout))
        return io.BytesIO(b'{"releases": []}')

    def opener(handler):
        assert (
            handler.redirect_request(
                None, None, 302, None, None, "https://other.example.com"
            )
            is None
        )
        return SimpleNamespace(open=open_request)

    monkeypatch.setattr(package, "build_opener", opener)
    assert package._catalog(REPOSITORY, "plugins/greeting") == {"releases": []}
    expected = package.hashlib.sha256(
        (REPOSITORY.lower() + "\nplugins/greeting").encode()
    ).hexdigest()[:24]
    assert requests == [("https://vis.blockether.com/api/extensions/" + expected, 20)]


@pytest.mark.parametrize("body", [b"not json", b" " * (4 * 1024 * 1024 + 1)])
def test_malformed_or_oversized_catalog_responses_fail_closed(monkeypatch, body):
    monkeypatch.setattr(
        package,
        "build_opener",
        lambda *_: SimpleNamespace(open=lambda *_a, **_k: io.BytesIO(body)),
    )
    with pytest.raises(ValueError):
        package._catalog(REPOSITORY, "")


def test_prerelease_only_needs_explicit_selection_and_update_never_downgrades(releases):
    metadata, target, _ = releases
    metadata[:] = [metadata[2]]
    with pytest.raises(ValueError, match="No approved stable"):
        package.install(REPOSITORY, target, trust=True, subdirectory="plugins/greeting")
    installed = package.install(
        REPOSITORY,
        target,
        trust=True,
        subdirectory="plugins/greeting",
        version="2.0.0rc1",
    )
    metadata.append(
        {**metadata[0], "version": "1.1.0", "revision": "a" * 40, "prerelease": False}
    )
    result = package.update("example/extensions", target, trust=True)
    assert result["revision"] == installed["revision"]
    assert "no changes" in result["next"]


# Declarative sync owns its receipt and link, never a development checkout.
def test_sync_local_install_cache_and_explicit_prune(tmp_path):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"
    configured = {"vis-greeter": {"source": str(source)}}
    with pytest.raises(ValueError, match="trust"):
        package.sync(configured, target)
    assert not target.exists()
    first = package.sync(configured, target, trust=True)
    assert first[0]["status"] == "installed"
    second = package.sync(configured, target, trust=True)
    assert second[0]["status"] == "cached"
    assert package.sync({}, target, trust=True)[0]["status"] == "orphaned"
    assert (target / "vis-greeter/current").is_symlink()
    assert package.sync({}, target, trust=True, prune=True)[0]["status"] == "removed"
    assert source.is_dir()
    assert not (target / "vis-greeter/current").exists()


def test_sync_github_warm_path_does_not_fetch_or_check_catalog(releases, monkeypatch):
    _, target, commands = releases
    configured = {
        "vis-greeter": {
            "source": REPOSITORY,
            "subdirectory": "plugins/greeting",
            "version": "1.0.0",
        }
    }
    assert package.sync(configured, target, trust=True)[0]["status"] == "installed"
    fetched = list(commands)
    monkeypatch.setattr(
        package, "_catalog", lambda *_: pytest.fail("warm sync consulted catalog")
    )
    assert package.sync(configured, target, trust=True)[0]["status"] == "cached"
    assert commands == fetched


def test_sync_pins_updates_refresh_and_rollback(releases):
    metadata, target, commands = releases
    spec = {
        "source": REPOSITORY,
        "subdirectory": "plugins/greeting",
        "version": "1.0.0",
    }
    first = package.sync({"vis-greeter": spec}, target, trust=True)[0]
    old_source = (target / "vis-greeter/current").resolve()
    (old_source / "note.txt").write_text("keep edits")
    assert (
        package.sync({"vis-greeter": {**spec, "version": "1.1.0"}}, target, trust=True)[
            0
        ]["status"]
        == "updated"
    )
    assert (
        package.sync({"vis-greeter": spec}, target, trust=True)[0]["revision"]
        == first["revision"]
    )
    assert (old_source / "note.txt").read_text() == "keep edits"
    newer = metadata.pop(1)
    latest = {"vis-greeter": {k: v for k, v in spec.items() if k != "version"}}
    package.sync(latest, target, trust=True)
    fetched = len(commands)
    assert (
        package.sync(latest, target, trust=True, refresh=True)[0]["status"] == "cached"
    )
    assert len(commands) == fetched
    metadata.append(newer)
    assert package.sync(latest, target, trust=True)[0]["version"] == "1.0.0"
    assert (
        package.sync(latest, target, trust=True, refresh=True)[0]["version"] == "1.1.0"
    )


def test_sync_dry_run_and_invalid_inputs_are_inert(tmp_path, monkeypatch):
    target = tmp_path / "extensions"
    monkeypatch.setattr(
        package, "_catalog", lambda *_: pytest.fail("dry run used network")
    )
    assert (
        package.sync({"vis-greeter": {"source": REPOSITORY}}, target, dry_run=True)[0][
            "status"
        ]
        == "would-sync"
    )
    assert not target.exists()
    for config in (
        {"../escape": {"source": REPOSITORY}},
        {"ok": {"source": REPOSITORY, "trust": True}},
        {"ok": {"source": REPOSITORY, "revision": "main"}},
        {"ok": {"source": "https://user:secret@github.com/a/b"}},
    ):
        with pytest.raises(ValueError):
            package.sync(config, target, trust=True)
        assert not target.exists()


def test_sync_does_not_claim_manual_or_replaced_links(tmp_path):
    target = tmp_path / "extensions"
    original = project(tmp_path / "original")
    alternate = project(tmp_path / "alternate")
    package.install(str(original), target, trust=True)
    configured = {"vis-greeter": {"source": str(original)}}
    assert package.sync(configured, target, trust=True)[0]["status"] == "failed"
    assert package.sync({}, target, trust=True, prune=True) == []
    (target / "vis-greeter/current").unlink()
    package.sync(configured, target, trust=True)
    (target / "vis-greeter/current").unlink()
    (target / "vis-greeter/current").symlink_to(alternate, target_is_directory=True)
    assert package.sync({}, target, trust=True, prune=True)[0]["status"] == "failed"
    assert (target / "vis-greeter/current").resolve() == alternate
    assert original.exists()


def test_sync_changed_local_and_remote_sources_preserve_source(releases, tmp_path):
    _, target, _ = releases
    local = project(tmp_path / "local", MANIFEST.replace('"1.0.0"', '"1.0.0.dev1"'))
    remote = {
        "source": REPOSITORY,
        "subdirectory": "plugins/greeting",
        "version": "1.0.0",
    }
    assert (
        package.sync({"vis-greeter": {"source": str(local)}}, target, trust=True)[0][
            "status"
        ]
        == "installed"
    )
    assert (
        package.sync({"vis-greeter": remote}, target, trust=True)[0]["status"]
        == "updated"
    )
    snapshot = (target / "vis-greeter/current").resolve()
    assert (
        package.sync({"vis-greeter": {"source": str(local)}}, target, trust=True)[0][
            "status"
        ]
        == "updated"
    )
    assert snapshot.exists() and local.exists()
    assert (target / "vis-greeter/current").resolve() == local


def test_sync_failure_keeps_last_source_and_name_mismatch_installs_nothing(
    releases, monkeypatch
):
    _, target, _ = releases
    spec = {
        "source": REPOSITORY,
        "subdirectory": "plugins/greeting",
        "version": "1.0.0",
    }
    package.sync({"vis-greeter": spec}, target, trust=True)
    previous = (target / "vis-greeter/current").resolve()
    receipt = (target / ".sync.json").read_bytes()

    def fail(*_):
        raise ValueError("catalog unavailable")

    monkeypatch.setattr(package, "_catalog", fail)
    assert (
        package.sync({"vis-greeter": {**spec, "version": "1.1.0"}}, target, trust=True)[
            0
        ]["status"]
        == "failed"
    )
    assert (target / "vis-greeter/current").resolve() == previous
    assert (target / ".sync.json").read_bytes() == receipt
    local = project(target.parent / "mismatch")
    assert (
        package.sync({"wrong-name": {"source": str(local)}}, target, trust=True)[1][
            "status"
        ]
        == "failed"
    )
    assert not (target / "wrong-name").exists()


def test_sync_lock_and_corrupt_receipts_fail_without_touching_source(tmp_path):
    target = tmp_path / "extensions"
    target.mkdir()
    source = project(tmp_path / "source")
    config = {"vis-greeter": {"source": str(source)}}
    (target / ".sync-lock").write_text("")
    with pytest.raises(FileExistsError):
        package.sync(config, target, trust=True)
    (target / ".sync-lock").unlink()
    (target / ".sync.json").write_text('{"vis-greeter": {}}')
    with pytest.raises(ValueError):
        package.sync(config, target, trust=True)
    assert not (target / "vis-greeter/current").exists()
    assert not (target / ".sync-lock").exists()


def test_saved_local_install_is_sync_owned_and_repeatable(tmp_path):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"
    result = package.install(
        str(source / "pyproject.toml"), target, trust=True, save=True
    )
    assert result["name"] == "vis-greeter"
    assert result["declaration"] == {"source": str(source)}
    configured = {result["name"]: result["declaration"]}
    assert package.sync(configured, target, trust=True)[0]["status"] == "cached"
    again = package.install(str(source), target, trust=True, save=True)
    assert again["declaration"] == result["declaration"]
    assert (target / "vis-greeter/current").resolve() == source


@pytest.mark.parametrize("selector_kind", ["latest", "version", "revision"])
def test_saved_remote_install_pins_the_selected_release_or_revision(
    releases, selector_kind
):
    metadata, target, _ = releases
    selector = (
        {"revision": metadata[0]["revision"]}
        if selector_kind == "revision"
        else {"version": "1.0.0"}
        if selector_kind == "version"
        else {}
    )
    result = package.install(
        REPOSITORY,
        target,
        trust=True,
        save=True,
        subdirectory="plugins/greeting",
        **selector,
    )
    expected = {"source": REPOSITORY, "subdirectory": "plugins/greeting"}
    if "revision" in selector:
        expected["revision"] = selector["revision"]
    else:
        expected["version"] = result["version"]
    assert result["declaration"] == expected
    assert (
        package.sync({result["name"]: expected}, target, trust=True)[0]["status"]
        == "cached"
    )


def test_save_does_not_adopt_an_unrelated_or_externally_changed_link(tmp_path):
    source = project(tmp_path / "source")
    alternate = project(tmp_path / "alternate")
    target = tmp_path / "extensions"
    package.install(str(source), target, trust=True)
    with pytest.raises(ValueError, match="owned by sync"):
        package.install(str(alternate), target, trust=True, save=True)
    assert (target / "vis-greeter/current").resolve() == source
    (target / "vis-greeter/current").unlink()
    package.install(str(source), target, trust=True, save=True)
    (target / "vis-greeter/current").unlink()
    (target / "vis-greeter/current").symlink_to(alternate)
    with pytest.raises(ValueError, match="owned by sync"):
        package.install(str(source), target, trust=True, save=True)
    assert (target / "vis-greeter/current").resolve() == alternate


@pytest.mark.parametrize("remote", [False, True])
def test_save_adopts_the_same_manual_install_and_rollback_preserves_it(
    releases, tmp_path, remote
):
    _, target, _ = releases
    source = REPOSITORY if remote else str(project(tmp_path / "source"))
    options = {"subdirectory": "plugins/greeting", "version": "1.0.0"} if remote else {}
    package.install(source, target, trust=True, **options)
    previous = (target / "vis-greeter/current").resolve()
    result = package.install(source, target, trust=True, save=True, **options)
    assert result["save_state"]["previous"] is None
    package.rollback_saved_install(target, result["name"], result["save_state"])
    assert (target / "vis-greeter/current").resolve() == previous
    assert package._sync_records(target) == {}
    result = package.install(source, target, trust=True, save=True, **options)
    assert (
        package.sync({result["name"]: result["declaration"]}, target, trust=True)[0][
            "status"
        ]
        == "cached"
    )


def test_save_refuses_a_different_manual_release(releases):
    _, target, _ = releases
    options = {"subdirectory": "plugins/greeting"}
    package.install(REPOSITORY, target, trust=True, version="1.0.0", **options)
    previous = (target / "vis-greeter/current").resolve()
    with pytest.raises(ValueError):
        package.install(
            REPOSITORY, target, trust=True, save=True, version="1.1.0", **options
        )
    assert (target / "vis-greeter/current").resolve() == previous
    assert package._sync_records(target) == {}


def test_failed_configuration_save_can_remove_only_the_new_saved_link(tmp_path):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"
    result = package.install(str(source), target, trust=True, save=True)
    package.rollback_saved_install(target, result["name"], result["save_state"])
    assert not (target / "vis-greeter/current").exists()
    assert package._sync_records(target) == {}
    assert source.is_dir()


def test_failed_configuration_save_restores_previous_saved_source(releases):
    _, target, _ = releases
    first = package.install(
        REPOSITORY,
        target,
        trust=True,
        save=True,
        version="1.0.0",
        subdirectory="plugins/greeting",
    )
    previous = (target / "vis-greeter/current").resolve()
    record = package._sync_records(target)["vis-greeter"]
    result = package.install(
        REPOSITORY,
        target,
        trust=True,
        save=True,
        version="1.1.0",
        subdirectory="plugins/greeting",
    )
    newer = (target / "vis-greeter/current").resolve()
    package.rollback_saved_install(target, result["name"], result["save_state"])
    assert (target / "vis-greeter/current").resolve() == previous
    assert package._sync_records(target)["vis-greeter"] == record
    assert (
        package.sync({first["name"]: first["declaration"]}, target, trust=True)[0][
            "status"
        ]
        == "cached"
    )
    assert newer.is_dir()


@pytest.mark.parametrize("change", ["saved", "external", "same"])
def test_save_rollback_refuses_a_concurrently_changed_install(tmp_path, change):
    source = project(tmp_path / "source")
    alternate = project(tmp_path / "alternate")
    target = tmp_path / "extensions"
    result = package.install(str(source), target, trust=True, save=True)
    expected = source if change == "same" else alternate
    if change == "external":
        (target / "vis-greeter/current").unlink()
        (target / "vis-greeter/current").symlink_to(alternate)
    else:
        package.install(str(expected), target, trust=True, save=True)
    with pytest.raises(ValueError, match="changed"):
        package.rollback_saved_install(target, result["name"], result["save_state"])
    assert (target / "vis-greeter/current").resolve() == expected


def test_save_receipt_failure_restores_the_previously_active_link(
    tmp_path, monkeypatch
):
    source = project(tmp_path / "source")
    alternate = project(tmp_path / "alternate")
    target = tmp_path / "extensions"
    package.install(str(source), target, trust=True, save=True)
    receipt = (target / ".sync.json").read_bytes()

    def fail(*_):
        raise OSError("receipt is not writable")

    monkeypatch.setattr(package, "_save_sync_records", fail)
    with pytest.raises(OSError, match="not writable"):
        package.install(str(alternate), target, trust=True, save=True)
    assert (target / "vis-greeter/current").resolve() == source
    assert (target / ".sync.json").read_bytes() == receipt


def test_new_save_receipt_failure_removes_only_its_link(tmp_path, monkeypatch):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"

    def fail(*_):
        raise OSError("receipt is not writable")

    monkeypatch.setattr(package, "_save_sync_records", fail)
    with pytest.raises(OSError, match="not writable"):
        package.install(str(source), target, trust=True, save=True)
    assert not (target / "vis-greeter/current").exists()
    assert not (target / ".sync.json").exists()
    assert not (target / ".sync-lock").exists()
    assert source.is_dir()


def test_save_rollback_preserves_unrelated_sync_records(tmp_path):
    source = project(tmp_path / "source")
    other = project(tmp_path / "other", MANIFEST.replace("vis-greeter", "another-tool"))
    target = tmp_path / "extensions"
    result = package.install(str(source), target, trust=True, save=True)
    package.install(str(other), target, trust=True, save=True)
    record = package._sync_records(target)["another-tool"]
    package.rollback_saved_install(target, result["name"], result["save_state"])
    assert package._sync_records(target) == {"another-tool": record}
    assert (target / "another-tool/current").resolve() == other


def test_save_rollback_receipt_failure_restores_the_current_pointer(
    tmp_path, monkeypatch
):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"
    result = package.install(str(source), target, trust=True, save=True)
    receipt = (target / ".sync.json").read_bytes()

    def fail(*_):
        raise OSError("receipt is not writable")

    monkeypatch.setattr(package, "_save_sync_records", fail)
    with pytest.raises(OSError, match="not writable"):
        package.rollback_saved_install(target, result["name"], result["save_state"])
    assert (target / "vis-greeter/current").resolve() == source
    assert (target / ".sync.json").read_bytes() == receipt


def test_saved_install_requires_trust_and_respects_the_sync_lock(tmp_path):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"
    with pytest.raises(ValueError, match="trust"):
        package.install(str(source), target, save=True)
    assert not target.exists()
    target.mkdir()
    (target / ".sync-lock").write_text("")
    with pytest.raises(FileExistsError):
        package.install(str(source), target, trust=True, save=True)
    assert not (target / "vis-greeter/current").exists()


def test_saved_install_enforces_sync_receipt_package_limit(tmp_path):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"
    target.mkdir()
    records = {
        f"tool-{number}": {"spec": {}, "target": str(source), "result": {}}
        for number in range(128)
    }
    package._save_sync_records(target, records)
    with pytest.raises(ValueError, match="at most 128"):
        package.install(str(source), target, trust=True, save=True)
    assert not (target / "vis-greeter/current").exists()
    assert package._sync_records(target) == records


def test_repeated_update_and_rollback_keep_only_named_versions(releases):
    _, target, commands = releases
    package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    for _ in range(3):
        package.update(REPOSITORY, target, trust=True)
        package.rollback(REPOSITORY, target, trust=True)
    container = target / "vis-greeter"
    assert sorted(p.name for p in container.iterdir()) == ["1.0.0", "1.1.0", "current"]
    assert len([args for args in commands if "fetch" in args]) == 2
    assert package.rollback(REPOSITORY, target, trust=True)["version"] == "1.1.0"


@pytest.mark.parametrize("failure", ["receipt", "activate"])
def test_failed_reactivation_preserves_both_versions_and_receipts(
    releases, monkeypatch, failure
):
    _, target, _ = releases
    package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    package.update(REPOSITORY, target, trust=True)
    container = target / "vis-greeter"
    receipts = {
        p: (container / p / "receipt.json").read_bytes() for p in ("1.0.0", "1.1.0")
    }
    original = package.os.replace
    failing_path = container / (
        "1.0.0/receipt.json" if failure == "receipt" else "current"
    )

    def fail_once(source, destination):
        if destination == failing_path and source.name != "receipt-backup.json":
            raise OSError("activation unavailable")
        return original(source, destination)

    monkeypatch.setattr(package.os, "replace", fail_once)
    with pytest.raises(OSError, match="unavailable"):
        package.rollback(REPOSITORY, target, trust=True)
    assert (container / "current").readlink().as_posix() == "1.1.0"
    assert {
        p: (container / p / "receipt.json").read_bytes() for p in receipts
    } == receipts
    assert sorted(p.name for p in target.iterdir()) == [".gitignore", "vis-greeter"]


def test_an_existing_version_cannot_be_rebound_to_another_revision(
    releases, monkeypatch
):
    _, target, _ = releases
    package.install(
        REPOSITORY,
        target,
        trust=True,
        subdirectory="plugins/greeting",
        version="1.0.0",
        save=True,
    )
    installed = target / "vis-greeter/1.0.0"
    receipt = (installed / "receipt.json").read_bytes()

    def checkout(_repository, path, revision):
        project(path / "plugins/greeting")
        return revision

    monkeypatch.setattr(package, "_checkout", checkout)
    with pytest.raises(ValueError, match="cannot change"):
        package.install(
            REPOSITORY,
            target,
            trust=True,
            subdirectory="plugins/greeting",
            revision="b" * 40,
            save=True,
        )
    assert (installed / "receipt.json").read_bytes() == receipt
    assert (target / "vis-greeter/current").resolve() == installed


def test_failed_local_switch_restores_the_version_link(tmp_path, monkeypatch):
    source = project(tmp_path / "source")
    alternate = project(tmp_path / "alternate")
    target = tmp_path / "extensions"
    package.install(str(source), target, trust=True, save=True)
    original = package.os.replace

    def fail_activation(source, destination):
        if destination == target / "vis-greeter/current":
            raise OSError("activation unavailable")
        return original(source, destination)

    monkeypatch.setattr(package.os, "replace", fail_activation)
    with pytest.raises(OSError, match="unavailable"):
        package.install(str(alternate), target, trust=True, save=True)
    assert (target / "vis-greeter/current").resolve() == source
    assert (target / "vis-greeter/1.0.0").resolve() == source
    assert alternate.exists()


def test_warm_local_sync_tracks_the_manifest_version(tmp_path):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"
    config = {"vis-greeter": {"source": str(source)}}
    package.sync(config, target, trust=True)
    (source / "pyproject.toml").write_text(MANIFEST.replace('"1.0.0"', '"1.1.0"'))
    result = package.sync(config, target, trust=True)[0]
    assert result["status"] == "updated"
    assert result["version"] == "1.1.0"
    assert result["path"] == str(target / "vis-greeter/1.1.0")
    assert (target / "vis-greeter/current").readlink().as_posix() == "1.1.0"
    assert (target / "vis-greeter/1.1.0").resolve() == source


@pytest.mark.parametrize("local_first", [True, False])
def test_local_and_managed_sources_cannot_share_a_version(
    releases, tmp_path, local_first
):
    _, target, _ = releases
    source = project(tmp_path / "source")
    local = {"source": str(source)}
    remote = {
        "source": REPOSITORY,
        "subdirectory": "plugins/greeting",
        "version": "1.0.0",
    }
    first, second = (local, remote) if local_first else (remote, local)
    package.sync({"vis-greeter": first}, target, trust=True)
    active = (target / "vis-greeter/current").resolve()
    assert (
        package.sync({"vis-greeter": second}, target, trust=True)[0]["status"]
        == "failed"
    )
    assert (target / "vis-greeter/current").resolve() == active
    assert source.exists()


@pytest.mark.parametrize("linked", [True, False])
def test_obsolete_layout_is_not_migrated_or_overwritten(tmp_path, linked):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"
    target.mkdir()
    old = target / "vis-greeter"
    if linked:
        old.symlink_to(source)
    else:
        project(old)
    original = (old / "extension.py").read_bytes()
    with pytest.raises((ValueError, FileExistsError)):
        package.install(str(source), target, trust=True)
    assert (old / "extension.py").read_bytes() == original
    assert not (old / "current").exists()


def test_existing_gitignore_rules_are_preserved(tmp_path):
    source = project(tmp_path / "source")
    target = tmp_path / "extensions"
    target.mkdir()
    (target / ".gitignore").write_text("private.py")
    package.install(str(source), target, trust=True, save=True)
    expected = (target / ".gitignore").read_bytes()
    package.install(str(source), target, trust=True, save=True)
    assert (target / ".gitignore").read_bytes() == expected
    assert expected.startswith(b"private.py\n")
