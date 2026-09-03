(ns com.blockether.vis.tui.tty
  "The process's real terminal, and the log file everything else goes to.

   Lanterna paints through `tty-in` / `tty-out` — `/dev/tty` directly, never
   `System/out` — because the app redirects its own stdout and stderr into
   `~/.vis/logs/` the moment it starts: a stray library print on stdout would
   otherwise land mid-frame and corrupt the screen. `original-stdout` is that
   real stdout, captured at load, and is the ONLY way to write a line a human
   reads after the screen releases the terminal.

   `tty-out` is frame-buffered on purpose; [[frame-buffered-tty-out]] carries
   why a raw stream flickers."
  (:require [com.blockether.vis.tui.util :as util])
  (:import (java.io FileInputStream FileOutputStream OutputStream)))

(def tty-in (delay (FileInputStream. "/dev/tty")))

(def ^:private ^"[B" sync-update-begin
  ;; DEC private mode 2026 "synchronized update" - the terminal HOLDS
  ;; rendering from `h` to `l`, so everything between paints as ONE frame.
  ;; Terminals without 2026 support ignore both marks (unknown private
  ;; modes are no-ops), so emitting them unconditionally is safe.
  (util/utf8 "\u001b[?2026h"))

(def ^:private ^"[B" sync-update-end (util/utf8 "\u001b[?2026l"))

(defn- cursor-report-query?
  "Is this chunk Lanterna's CSI 6n cursor-position query? `reportPosition`
   writes it as ONE 4-byte chunk and then BLOCKS (up to 5s) for the
   terminal's reply WITHOUT flushing - the raw unbuffered FileOutputStream
   used to smuggle it out immediately. The frame buffer must flush it
   through on sight or every resize/size-probe stalls to the 5s timeout."
  [^bytes b]
  (and (= 4 (alength b))
       (= (aget b 0) (byte 0x1b))
       (= (aget b 1) (byte 0x5b)) ;; [
       (= (aget b 2) (byte 0x36)) ;; 6
       (= (aget b 3) (byte 0x6e)))) ;; n

(defn frame-buffered-tty-out
  "Wrap the raw tty stream so a whole repaint reaches the terminal as ONE
   atomic write instead of one write(2) syscall PER CELL.

   Lanterna's `refreshByDelta` calls `putString`/`setCursorPosition` per
   changed cell and only `flush`es once at the end of `refresh`. On a raw
   `FileOutputStream` every one of those calls is its own syscall straight
   to the tty, so the terminal renders PARTIAL frames mid-repaint: a fold
   toggle that shifts the transcript reads as a whole-screen flicker and a
   transient content jump. Buffering until `flush` collapses the frame to
   one write, and the DEC 2026 bracket makes the terminal hold rendering
   until the frame is complete even when the kernel chunks the write.

   Everything the app writes to the tty outside Lanterna (SGR-mouse /
   bracketed-paste toggles, OSC 11 background, the `:bell` fx, the panic
   PrintStream) already flushes explicitly, so nothing can sit in the
   buffer across frames."
  ^OutputStream [^OutputStream raw]
  (let [initial-capacity
        (* 64 1024)

        ;; `ByteArrayOutputStream/reset` keeps the grown backing array forever,
        ;; so one outsized frame (full repaint on a huge terminal) would pin
        ;; megabytes. Over the retention cap the buffer is REPLACED after the
        ;; flush instead of reset. Mutable holder because the swap needs a new
        ;; instance; all access goes through `lock`.
        retain-capacity
        (* 512 1024)

        lock
        (Object.)

        buf-holder
        (java.util.concurrent.atomic.AtomicReference. (java.io.ByteArrayOutputStream.
                                                        initial-capacity))]

    (proxy [OutputStream] []
      (write
        ([b]
         (if (bytes? b)
           (do (locking lock
                 (.write ^java.io.ByteArrayOutputStream (.get buf-holder)
                         ^bytes b
                         0
                         (alength ^bytes b)))
               (when (cursor-report-query? b) (.flush ^OutputStream this)))
           (locking lock (.write ^java.io.ByteArrayOutputStream (.get buf-holder) (int b)))))
        ([b off len]
         (locking lock
           (.write ^java.io.ByteArrayOutputStream (.get buf-holder) ^bytes b (int off) (int len)))))
      (flush []
        (locking lock
          (let [^java.io.ByteArrayOutputStream buf
                (.get buf-holder)

                n
                (.size buf)]

            (when (pos? n)
              (.write raw sync-update-begin)
              (.writeTo buf raw)
              (.write raw sync-update-end)
              (if (> n retain-capacity)
                (.set buf-holder (java.io.ByteArrayOutputStream. initial-capacity))
                (.reset buf)))
            (.flush raw))))
      (close [] (.flush ^OutputStream this) (.close raw)))))

(def tty-out (delay ^OutputStream (frame-buffered-tty-out (FileOutputStream. "/dev/tty"))))

(def ^java.io.PrintStream original-stdout System/out)

(def ^java.io.PrintStream original-stderr
  "The process's REAL stderr, captured at load - before the app points
   `System/err` at its log file."
  System/err)
