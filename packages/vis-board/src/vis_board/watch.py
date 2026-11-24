"""Wake a thread when the kernel reports a change in a folder. Nothing polls."""

import ctypes
import os
import select
import sys
import threading

# The inotify events of a file that a process adds, replaces, renames or removes.
_IN_CHANGE = 0x8 | 0x40 | 0x80 | 0x100 | 0x200


class FolderWatch:
    """Block in `wait()` until a file in `folder` changes or `close()` is called.

    macOS and BSD use kqueue, Linux uses inotify. Each change of the folder entries
    wakes the waiter, also a save through a temporary file and a rename.
    """

    def __init__(self, folder):
        self._lock = threading.Lock()
        self._is_closed = False
        self._wake, self._waker = os.pipe()
        self._fds = [self._wake, self._waker]
        try:
            if hasattr(select, "kqueue"):
                self._open_kqueue(folder)
            elif sys.platform.startswith("linux"):
                self._open_inotify(folder)
            else:
                raise OSError(f"No file events on {sys.platform}")
        except BaseException:
            self.release()
            raise

    def _open_kqueue(self, folder):
        flags = os.O_RDONLY | getattr(os, "O_EVTONLY", 0)
        folder_fd = os.open(folder, flags)
        self._fds.append(folder_fd)
        self._queue = select.kqueue()
        self._queue.control(
            [
                select.kevent(
                    folder_fd,
                    select.KQ_FILTER_VNODE,
                    select.KQ_EV_ADD | select.KQ_EV_CLEAR,
                    select.KQ_NOTE_WRITE | select.KQ_NOTE_DELETE | select.KQ_NOTE_RENAME,
                ),
                select.kevent(self._wake, select.KQ_FILTER_READ, select.KQ_EV_ADD),
            ],
            0,
        )
        self._ready = self._kqueue_ready

    def _kqueue_ready(self):
        events = self._queue.control(None, 4)
        return [event.ident for event in events]

    def _open_inotify(self, folder):
        libc = ctypes.CDLL(None, use_errno=True)
        fd = libc.inotify_init1(os.O_CLOEXEC)
        if fd < 0:
            raise OSError(ctypes.get_errno(), "inotify_init1 failed")
        self._fds.append(fd)
        if libc.inotify_add_watch(fd, os.fsencode(folder), _IN_CHANGE) < 0:
            error = ctypes.get_errno()
            raise OSError(error, os.strerror(error), str(folder))
        self._inotify = fd
        self._ready = self._inotify_ready

    def _inotify_ready(self):
        ready, _, _ = select.select([self._inotify, self._wake], [], [])
        if self._inotify in ready:
            os.read(self._inotify, 65536)
        return ready

    def wait(self):
        """True after a change; False after `close()`, which also frees the watch."""
        if self._wake in self._ready():
            self.release()
            return False
        return True

    def close(self):
        """Wake the waiter and end the watch. A repeated close does nothing."""
        with self._lock:
            if not self._is_closed:
                self._is_closed = True
                os.write(self._waker, b"x")

    def release(self):
        """Free the watch. The thread that waits calls it when it ends."""
        with self._lock:
            self._is_closed = True
            fds, self._fds = self._fds, []
            queue = getattr(self, "_queue", None)
            if queue is not None:
                queue.close()
            for fd in fds:
                os.close(fd)
