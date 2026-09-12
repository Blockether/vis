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
        "vis-spel",
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
    assert (target / "vis-greeter").is_symlink()
    (source / "extension.py").write_text("VALUE = 2")
    assert (target / "vis-greeter/extension.py").read_text() == "VALUE = 2"
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
    assert (target / "vis-greeter/src/greeter.py").read_text() == "VALUE = 1"
    assert not (target / "vis-greeter/.git").exists()
    assert not (target / "vis-greeter/unrelated.txt").exists()
    assert commands
    assert sorted(p.name for p in target.iterdir()) == [".versions", "vis-greeter"]


def test_remote_failure_and_invalid_revision_leave_no_installation(
    tmp_path, monkeypatch
):
    target = tmp_path / "installed"

    def fail(*args):
        raise ValueError("Could not fetch repository")

    monkeypatch.setattr(package, "_checkout", fail)
    with pytest.raises(ValueError, match="fetch"):
        package.install(REPOSITORY, target, trust=True, revision="a" * 40)
    assert not target.exists() or not list(target.iterdir())
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


@pytest.mark.parametrize("source", ["example/extensions", REPOSITORY])
def test_version_install_update_and_rollback_use_real_pinned_checkouts(
    releases, source
):
    metadata, target, commands = releases
    first = package.install(
        source, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    active = target / "vis-greeter"
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
    assert len([args for args in commands if "fetch" in args]) == 3
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
    other = target / ".versions/another-package/.install-other/project"
    project(other, MANIFEST.replace("vis-greeter", "another-package"))
    receipt = json.loads(
        ((target / "vis-greeter").resolve().parent / "receipt.json").read_text()
    )
    receipt.update(name="another-package", subdirectory=other_folder)
    (other.parent / "receipt.json").write_text(json.dumps(receipt))
    (target / "another-package").symlink_to(other)
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
    assert (target / "another-package").resolve() == other
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
    assert (tmp_path / "linked/vis-greeter").resolve() == local


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


def test_rollback_keeps_local_edits_but_refetches_original_source(releases):
    _, target, _ = releases
    package.install(
        REPOSITORY, target, trust=True, subdirectory="plugins/greeting", version="1.0.0"
    )
    saved = (target / "vis-greeter").resolve()
    (saved / "src/greeter.py").write_text("LOCAL_EDIT = True")
    package.update("example/extensions", target, trust=True)
    package.rollback("example/extensions", target, trust=True)
    assert (saved / "src/greeter.py").read_text() == "LOCAL_EDIT = True"
    assert '"1.0.0"' in (target / "vis-greeter/src/greeter.py").read_text()


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
    active = (target / "vis-greeter").resolve()
    receipt = (active.parent / "receipt.json").read_bytes()

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
    assert (target / "vis-greeter").resolve() == active
    assert (active.parent / "receipt.json").read_bytes() == receipt
    assert len(list(active.parent.parent.iterdir())) == 1
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
    active = (target / "vis-greeter").resolve()
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
    assert (target / "vis-greeter").resolve() == source


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
        active = target / "vis-greeter"
        active.unlink()
        active.symlink_to(target / "user-source")
        project(target / "user-source")
        return revision

    monkeypatch.setattr(package, "_checkout", replace_during_fetch)
    with pytest.raises(ValueError, match="local source"):
        package.update("example/extensions", target, trust=True)
    assert (target / "vis-greeter").resolve() == target / "user-source"


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
    assert (target / "vis-greeter").is_symlink()
    assert package.sync({}, target, trust=True, prune=True)[0]["status"] == "removed"
    assert source.is_dir()
    assert not (target / "vis-greeter").exists()


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
    old_source = (target / "vis-greeter").resolve()
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
    (target / "vis-greeter").unlink()
    package.sync(configured, target, trust=True)
    (target / "vis-greeter").unlink()
    (target / "vis-greeter").symlink_to(alternate, target_is_directory=True)
    assert package.sync({}, target, trust=True, prune=True)[0]["status"] == "failed"
    assert (target / "vis-greeter").resolve() == alternate
    assert original.exists()


def test_sync_changed_local_and_remote_sources_preserve_source(releases, tmp_path):
    _, target, _ = releases
    local = project(tmp_path / "local")
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
    snapshot = (target / "vis-greeter").resolve()
    assert (
        package.sync({"vis-greeter": {"source": str(local)}}, target, trust=True)[0][
            "status"
        ]
        == "updated"
    )
    assert snapshot.exists() and local.exists()
    assert (target / "vis-greeter").resolve() == local


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
    previous = (target / "vis-greeter").resolve()
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
    assert (target / "vis-greeter").resolve() == previous
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
    assert not (target / "vis-greeter").exists()
    assert not (target / ".sync-lock").exists()
