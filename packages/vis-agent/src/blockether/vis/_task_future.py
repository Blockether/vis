"""Private Task mechanics for the extension SDK and engine client."""

from __future__ import annotations

import asyncio
import logging
import threading
from asyncio import CancelledError as _CancelledError
from concurrent.futures import CancelledError as _FutureCancelledError
from concurrent.futures import Future, InvalidStateError
from contextvars import copy_context
from typing import Any, TypeVar, cast


class _HostFutureCancelledError(_FutureCancelledError, _CancelledError):
    pass


_FutureValue = TypeVar("_FutureValue")


class _HostFuture(Future[_FutureValue]):
    """Host-owned work that both Python wait functions can observe."""

    _vis_cancel_on_await = False

    @property
    def _asyncio_future_blocking(self):
        # CPython's wait_for(..., 0) cancels an asyncio Future directly. For a
        # handle we only observe, let it cancel its own await wrapper instead.
        return False if self._vis_cancel_on_await or self.done() else None

    def __init__(self):
        super().__init__()
        # Notify aggregate waiters before any user cancellation callback.
        super().add_done_callback(self._notify_cancelled)
        self._vis_async_callbacks = []
        super().add_done_callback(self._publish_async_callbacks)

    def cancel(self, msg=None):
        return super().cancel()

    @staticmethod
    def _notify_cancelled(future):
        if future.cancelled():
            future.set_running_or_notify_cancel()

    def get_loop(self):
        # The host work belongs to none of its callers' event loops.
        return asyncio.get_running_loop()

    def add_done_callback(self, fn, *, context=None):
        try:
            loop = self.get_loop()
        except RuntimeError:
            return super().add_done_callback(fn)
        context = copy_context() if context is None else context
        with self._condition:
            if not self.done():
                self._vis_async_callbacks.append((fn, loop, context))
                return
        self._schedule_async_callback(fn, loop, context)

    def remove_done_callback(self, fn):
        with self._condition:
            callbacks = self._vis_async_callbacks
            self._vis_async_callbacks = [entry for entry in callbacks if entry[0] != fn]
            return len(callbacks) - len(self._vis_async_callbacks)

    def _schedule_async_callback(self, fn, loop, context):
        try:
            loop.call_soon_threadsafe(fn, self, context=context)
        except RuntimeError:
            if not loop.is_closed():
                raise

    def _publish_async_callbacks(self, _future):
        with self._condition:
            callbacks = self._vis_async_callbacks
            self._vis_async_callbacks = []
        for fn, loop, context in callbacks:
            self._schedule_async_callback(fn, loop, context)

    def __await__(self):
        return self._await_result().__await__()

    async def _await_result(self):
        # asyncio.wait accepts this Future by its asyncio surface; typeshed asks
        # for an asyncio.Future, so the type says what the runtime already does.
        waitable = cast("asyncio.Future[Any]", self)
        try:
            if not self.done():
                await asyncio.wait([waitable])
        except _CancelledError:
            if self._vis_cancel_on_await and not self.done():
                threading.Thread(
                    target=copy_context().run,
                    args=(self._request_await_cancel,),
                    name="vis-task-interrupt",
                    daemon=True,
                ).start()
            raise
        return self.result()

    def _request_await_cancel(self):
        try:
            self.cancel()
        except Exception:
            logging.getLogger(__name__).debug("Task interrupt request failed")

    # A bare call never blocks. Released extensions pass `timeout=` to wait.
    def result(self, timeout=None) -> _FutureValue:
        if timeout is None and not self.done():
            raise InvalidStateError("Result is not ready.")
        try:
            return super().result(0 if timeout is None else timeout)
        except _FutureCancelledError:
            raise _HostFutureCancelledError() from None

    def exception(self, timeout=None) -> BaseException | None:
        if timeout is None and not self.done():
            raise InvalidStateError("Exception is not set.")
        try:
            return super().exception(0 if timeout is None else timeout)
        except _FutureCancelledError:
            raise _HostFutureCancelledError() from None
