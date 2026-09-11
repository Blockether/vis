#!/usr/bin/env python3
"""Refuse stable publication until every supported product artifact is uploaded."""

import argparse
import json
import os
import re
import subprocess
from pathlib import Path


def required_assets(tag: str) -> set[str]:
    if not re.fullmatch(r"v[0-9]+\.[0-9]+\.[0-9]+", tag):
        raise ValueError("expected an immutable vX.Y.Z release tag")
    version = tag[1:]
    names = {"install-vis-agent", "vis-agent"}
    for platform in ("linux-x64", "linux-arm64", "macos-arm64"):
        names.update(
            f"{product}-{platform}.tar.gz" for product in ("vis-agent", "vis-tui")
        )
    names.add(f"vis-companion-{version}-macos-universal.dmg")
    for arch in ("x64", "arm64"):
        names.update(
            f"vis-companion-{version}-linux-{arch}.{ext}" for ext in ("deb", "AppImage")
        )
    names.update(
        (f"vis-companion-{version}-ios.ipa", f"vis-companion-{version}-android.aab")
    )
    return names


def verify_release(release: dict, tag: str, *, draft: bool = True) -> set[str]:
    expected = required_assets(tag)
    if release.get("tag_name") != tag:
        raise ValueError("release tag does not match the verified source")
    if release.get("draft") is not draft or release.get("prerelease") is not False:
        state = "draft" if draft else "published release"
        raise ValueError(
            f"expected a stable {state}; published versions must not be modified"
        )
    uploaded = {
        asset["name"]
        for asset in release.get("assets", [])
        if asset.get("state") == "uploaded" and asset.get("size", 0) > 0
    }
    missing = expected - uploaded
    if missing:
        raise ValueError(
            "missing or incomplete release assets: " + ", ".join(sorted(missing))
        )
    return expected


def required_recovery_checks() -> set[str]:
    """Checks that must exist in the original complete-product release run."""
    checks = {
        "prepare",
        "Verify complete assets, deploy libraries and publish stable",
        "native / native / vis-agent-macos-arm64.tar.gz",
        "native / native / macOS builder pickup",
        "mobile / ios",
        "mobile / android",
        "mobile / Android publish freeze",
        "desktop / Package Linux x64",
        "desktop / Package Linux ARM64",
        "desktop / Package macOS universal",
    }
    checks.update(
        f"Verify release source / {name}"
        for name in (
            "lint / clj-kondo",
            "classpath / .",
            "classpath / apps/vis-tui",
            "tests / linux",
            "tests / macos",
            "build / aot uberjar",
        )
    )
    for system in ("ubuntu-latest", "macos-26"):
        checks.add(
            f"Verify release source / python-package / real SDK engine / {system}"
        )
        checks.update(
            f"Verify release source / python-package / {system} / {version}"
            for version in ("3.11", "3.12", "3.13", "3.14", "pypy3.11")
        )
    checks.update(
        f"native / native / vis-agent-linux-{arch}.tar.gz" for arch in ("x64", "arm64")
    )
    return checks


def verify_recovery(
    repository: str, tag: str, sha: str, source: dict, native: dict
) -> None:
    """Reuse green product checks only after both repaired Linux jobs test the same tag."""
    required_assets(tag)
    if not re.fullmatch(r"[0-9a-f]{40}", sha):
        raise ValueError("expected the complete immutable release commit")
    for run, workflow, event in (
        (source, "release.yml", "push"),
        (native, "native-release.yml", "workflow_dispatch"),
    ):
        if (
            run.get("status") != "completed"
            or run.get("event") != event
            or run.get("path") != f".github/workflows/{workflow}"
            or run.get("repository", {}).get("full_name") != repository
            or run.get("head_repository", {}).get("full_name") != repository
        ):
            raise ValueError(f"expected a completed trusted {workflow} run")
        names = [job["name"] for job in run["jobs"]]
        if len(names) != len(set(names)):
            raise ValueError("duplicate release job names")
    if (
        source.get("head_sha") != sha
        or source.get("head_branch") != tag
        or source.get("conclusion") not in ("success", "failure")
    ):
        raise ValueError(
            "original release does not verify the requested tag and commit"
        )
    source_jobs = {job["name"]: job for job in source["jobs"]}
    missing = required_recovery_checks() - source_jobs.keys()
    if missing:
        raise ValueError(
            "missing original release checks: " + ", ".join(sorted(missing))
        )
    replaced = {
        f"native / native / vis-agent-linux-{arch}.tar.gz" for arch in ("x64", "arm64")
    }
    publisher = "Verify complete assets, deploy libraries and publish stable"
    for name, job in source_jobs.items():
        if job.get("status") != "completed":
            raise ValueError(f"unfinished original release check: {name}")
        expected = (
            "skipped"
            if name == "Verify original product gates and repaired Linux checks"
            else "success"
        )
        if name not in replaced | {publisher} and job.get("conclusion") != expected:
            raise ValueError(f"unexpected original release check outcome: {name}")
    if native.get("conclusion") != "success":
        raise ValueError("repaired native workflow did not pass")
    native_jobs = {job["name"]: job for job in native["jobs"]}
    for arch in ("x64", "arm64"):
        name = f"native / vis-agent-linux-{arch}.tar.gz"
        job = native_jobs.get(name, {})
        if job.get("status") != "completed" or job.get("conclusion") != "success":
            raise ValueError(f"repaired native check is not green: {name}")
        if set(job.get("checkout_shas", [])) != {sha}:
            raise ValueError(f"repaired native checkout does not match the tag: {name}")
        steps = {step["name"]: step.get("conclusion") for step in job.get("steps", [])}
        for step in (
            "Test the native binaries",
            "Run ./.github/actions/test-native-python-sdk",
        ):
            if steps.get(step) != "success":
                raise ValueError(
                    f"repaired native verification is missing or not green: {name}: {step}"
                )


def load_recovery_run(repository: str, run_id: int, *, native: bool = False) -> dict:
    """Read all latest-attempt jobs and native checkout provenance with the GitHub CLI."""

    def gh(*args: str) -> str:
        return subprocess.check_output(["gh", "api", *args], text=True, timeout=120)

    endpoint = f"repos/{repository}/actions/runs/{run_id}"
    run = json.loads(gh(endpoint))
    pages = json.loads(
        gh("--paginate", "--slurp", endpoint + "/jobs?per_page=100&filter=latest")
    )
    run["jobs"] = [job for page in pages for job in page["jobs"]]
    if native:
        names = {f"native / vis-agent-linux-{arch}.tar.gz" for arch in ("x64", "arm64")}
        for job in run["jobs"]:
            if job["name"] in names:
                log = gh(
                    "--allow-escape-sequences",
                    f"repos/{repository}/actions/jobs/{job['id']}/logs",
                )
                log = re.sub(r"\x1b\[[0-?]*[ -/]*[@-~]", "", log)
                job["checkout_shas"] = re.findall(
                    r"\bgit log -1 --format=%H\r?\n[^\n]*?\b([0-9a-f]{40})\b", log
                )
    return run


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("metadata", type=Path, help="GitHub release API JSON")
    parser.add_argument("tag")
    parser.add_argument(
        "--published",
        action="store_true",
        help="verify published installer source, not draft promotion",
    )
    parser.add_argument(
        "--source-run", type=int, help="original complete-product release run"
    )
    parser.add_argument(
        "--native-run", type=int, help="successful repaired Linux native run"
    )
    parser.add_argument("--sha", help="immutable source commit for draft recovery")
    args = parser.parse_args()
    try:
        recovery = (args.source_run, args.native_run, args.sha)
        requested = any(value is not None for value in recovery)
        if requested and (
            not all(recovery)
            or args.published
            or args.source_run < 1
            or args.native_run < 1
        ):
            raise ValueError(
                "recovery requires a draft, positive source/native run IDs and its commit"
            )
        with args.metadata.open() as source:
            count = len(
                verify_release(json.load(source), args.tag, draft=not args.published)
            )
        if requested:
            repository = os.environ["GITHUB_REPOSITORY"]
            verify_recovery(
                repository,
                args.tag,
                args.sha,
                load_recovery_run(repository, args.source_run),
                load_recovery_run(repository, args.native_run, native=True),
            )
            print(
                f"Original product checks and repaired Linux checks verify {args.sha}."
            )
    except (
        ValueError,
        TypeError,
        KeyError,
        AttributeError,
        OSError,
        subprocess.CalledProcessError,
        subprocess.TimeoutExpired,
    ) as error:
        parser.exit(1, f"Release refused: {error}\n")
    print(f"Complete stable release {args.tag}: {count} required assets uploaded.")


if __name__ == "__main__":
    main()
