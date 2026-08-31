"""A working `os.link` for the confined sandbox filesystem.

GraalPy 25.1.3 never delivers the destination. Its emulated POSIX backend
converts the new path and then resolves the OLD path a second time
(`EmulatedPosixSupport.linkat` loads the source string for both ends), so the
host filesystem is asked to link the source onto itself: every `os.link` and
every `Path.hardlink_to` died with `FileExistsError` naming the SOURCE,
whatever destination was asked for. No host-side repair is possible — by the
time a `FileSystem` is called, the destination is already gone.

So the call is re-routed around the broken backend. `__vis_hard_link__` is the
host callback that runs `createLink` on the SAME confined filesystem every
other write goes through, which is what keeps a hard link inside the session's
roots, under the path gate and the syntax guard, and reported to Activity as a
`linked` row.

Only what that callback can honour is accepted: `dir_fd` and
`follow_symlinks=False` raise `NotImplementedError`, exactly as CPython does on
a platform that lacks them, rather than silently linking a different file.
"""


def __vis_install_hard_link():
    import errno
    import os

    def link(src, dst, *, src_dir_fd=None, dst_dir_fd=None, follow_symlinks=True):
        """Create a hard link `dst` pointing at the existing file `src`."""
        if src_dir_fd is not None or dst_dir_fd is not None:
            raise NotImplementedError("os.link: dir_fd is not supported in this sandbox")
        if not follow_symlinks:
            raise NotImplementedError(
                "os.link: follow_symlinks=False is not supported in this sandbox"
            )
        existing = os.path.abspath(os.fsdecode(src))
        new_link = os.path.abspath(os.fsdecode(dst))
        # Ask about the source through the sandbox itself, so a missing file still raises
        # FileNotFoundError and one outside the roots still raises the very refusal `open`
        # raises — the host boundary below can only answer one generic OSError.
        os.lstat(existing)
        if os.path.lexists(new_link):
            raise FileExistsError(errno.EEXIST, "File exists", os.fspath(dst))
        try:
            __vis_hard_link__(existing, new_link)
        except OSError:
            raise
        except Exception as exc:  # a host refusal or IO fault is an OSError out here
            raise OSError(errno.EIO, str(exc), os.fspath(src), None, os.fspath(dst)) from None

    os.link = link


__vis_install_hard_link()
del __vis_install_hard_link
