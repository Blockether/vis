(ns com.blockether.vis.internal.persistance-test
  "Tests for persistence bootstrap error normalization.

   The regression: Flyway migration checksum mismatches were surfaced as
   fatal stack traces in channel entry points. The fix belongs in the
   persistence facade so every channel gets the same user-facing error."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.manifest :as manifest]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private migration-checksum-mismatch?
  (deref #'persistance/migration-checksum-mismatch?))

(def ^:private maybe-wrap-db-open-error
  (deref #'persistance/maybe-wrap-db-open-error))

(def ^:private migration-checksum-mismatch-user-message
  (deref #'persistance/migration-checksum-mismatch-user-message))

(defdescribe migration-checksum-detection-test
  (it "matches Flyway checksum text at top level"
    (expect (true? (migration-checksum-mismatch?
                     (ex-info "Validate failed: Migrations have failed validation\nMigration checksum mismatch for migration version 1"
                       {})))))

  (it "matches Flyway checksum text in a nested cause"
    (let [cause (ex-info "Migration checksum mismatch for migration version 1" {})
          e     (ex-info "wrapper" {} cause)]
      (expect (true? (migration-checksum-mismatch? e)))))

  (it "returns false for unrelated failures"
    (expect (false? (migration-checksum-mismatch? (ex-info "boom" {}))))))

(defdescribe db-open-error-normalization-test
  (it "wraps checksum mismatch as :vis/user-error with actionable guidance"
    (let [root (ex-info "Migration checksum mismatch for migration version 1" {})
          e    (try
                 (with-redefs [manifest/scan-extensions! (fn [] nil)
                               persistance/pick-backend-id (fn [_] :sqlite)
                               persistance/resolve-impl
                               (fn [_ fn-name]
                                 (expect (= 'db-open! fn-name))
                                 (delay (fn [_] (throw root))))]
                   (persistance/db-create-connection! :memory)
                   nil)
                 (catch Throwable t t))]
      (expect (instance? clojure.lang.ExceptionInfo e))
      (expect (true? (:vis/user-error (ex-data e))))
      (expect (= :vis/db-migration-checksum-mismatch (:type (ex-data e))))
      (expect (= root (.getCause ^Throwable e)))
      (expect (str/includes? (.getMessage ^Throwable e) "~/.vis/vis.mdb"))
      (expect (str/includes? (.getMessage ^Throwable e) "packaged migration resources"))
      (expect (not (str/includes? (.getMessage ^Throwable e) "Flyway repair")))))

  (it "passes non-migration bootstrap failures through unchanged"
    (let [root (IllegalStateException. "totally unrelated failure")
          e    (try
                 (with-redefs [manifest/scan-extensions! (fn [] nil)
                               persistance/pick-backend-id (fn [_] :sqlite)
                               persistance/resolve-impl
                               (fn [_ _]
                                 (delay (fn [_] (throw root))))]
                   (persistance/db-create-connection! :memory)
                   nil)
                 (catch Throwable t t))]
      (expect (= IllegalStateException (class e)))
      (expect (= "totally unrelated failure" (.getMessage ^Throwable e)))
      (expect (nil? (ex-data e))))))

(defdescribe wrapped-message-contents-test
  (it "mentions reset path and does not suggest repair"
    (expect (str/includes? migration-checksum-mismatch-user-message "schema mismatch"))
    (expect (str/includes? migration-checksum-mismatch-user-message "~/.vis/vis.mdb"))
    (expect (str/includes? migration-checksum-mismatch-user-message "packaged migration resources"))
    (expect (not (str/includes? migration-checksum-mismatch-user-message "Flyway repair"))))

  (it "helper leaves unrelated errors untouched"
    (let [e (ex-info "x" {})]
      (expect (identical? e (maybe-wrap-db-open-error e))))))
