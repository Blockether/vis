(ns com.blockether.vis.internal.foundation.editing.structural-matrix-test
  "End-to-end per-LANGUAGE coverage for structural editing (real tree-sitter,
   real edits — no LLM). For every language we exercise the full lifecycle the
   model relies on: outline FINDS the def by name, replace/insert_after BY NAME,
   and rename. This is the spec + regression guard for `struct_patch`.

   `working-langs` MUST stay green. `fork-broken-langs` documents the languages
   whose tree-sitter def queries are incomplete in the pack fork (kotlin/cpp =
   name not captured, dart/zig = no def match) — when the fork is fixed, MOVE
   them into `working-langs` and the suite stays green. See
   project_structural_editing_audit."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.foundation.editing.outline :as outline]
   [com.blockether.vis.internal.foundation.editing.structural :as structural]
   [com.blockether.tree-sitter-language-pack]
   [lazytest.core :refer [defdescribe expect it describe]])
  (:import [dev.kreuzberg.treesitterlanguagepack StructuralApi]))

;; {:lang :path :src :target :replace :insert :rename}
;; `replace` swaps the body to `return 424242` so a successful edit is detectable
;; by the marker; `insert` adds a sibling def; `rename` foo -> renamed_fn.
(def ^:private cases
  [{:lang "go"         :path "f.go"   :target "Foo"
    :src "package m\nfunc Foo(a int) int { return 0 }\n"
    :replace "func Foo(a int) int { return 424242 }"
    :insert "func Sib() {}" :rename "Renamed"}
   {:lang "rust"       :path "f.rs"   :target "foo"
    :src "fn foo(a: i32) -> i32 { 0 }\n"
    :replace "fn foo(a: i32) -> i32 { 424242 }"
    :insert "fn sib() {}" :rename "renamed_fn"}
   {:lang "typescript" :path "f.ts"   :target "foo"
    :src "function foo(a: number): number { return 0; }\n"
    :replace "function foo(a: number): number { return 424242; }"
    :insert "function sib() {}" :rename "renamed_fn"}
   {:lang "tsx"        :path "f.tsx"  :target "foo"
    :src "function foo(a: number): number { return 0; }\n"
    :replace "function foo(a: number): number { return 424242; }"
    :insert "function sib() {}" :rename "renamed_fn"}
   {:lang "python"     :path "f.py"   :target "foo"
    :src "def foo(a):\n    return 0\n"
    :replace "def foo(a):\n    return 424242"
    :insert "def sib():\n    pass" :rename "renamed_fn"}
   {:lang "java"       :path "f.java" :target "foo"
    :src "class C { int foo(int a){ return 0; } }\n"
    :replace "int foo(int a){ return 424242; }"
    :insert "int sib(){ return 0; }" :rename "renamed_fn"}
   {:lang "ruby"       :path "f.rb"   :target "foo"
    :src "def foo(a)\n  0\nend\n"
    :replace "def foo(a)\n  424242\nend"
    :insert "def sib\nend" :rename "renamed_fn"}
   {:lang "clojure"    :path "f.clj"  :target "foo"
    :src "(ns m)\n(defn foo [a] 0)\n"
    :replace "(defn foo [a] 424242)"
    :insert "(defn sib [] nil)" :rename "renamed_fn"}])

;; FORK-PENDING: kotlin/cpp/dart/zig replace-by-name is FIXED in the pack fork
;; (crates/ts-pack-core/src/intel/intelligence.rs — simple_identifier + IDENTIFIER
;; in resolve_structure_name, C/C++ declarator descent, dart/zig signature+body
;; span extension) and VERIFIED locally (blockether.25-local: outline/replace/
;; insert/rename all green for these 4). The published .23 dep doesn't carry it
;; yet, so here we assert only rename (identifier-based, works on .23). Once the
;; fork is released + deps bumped, move these into `cases`. See
;; project_structural_editing_audit.
(def ^:private fork-pending-cases
  [{:lang "kotlin" :path "f.kt"   :target "foo" :src "fun foo(a: Int): Int { return 0 }\n"}
   {:lang "cpp"    :path "f.cpp"  :target "foo" :src "int foo(int a){ return 0; }\n"}
   {:lang "dart"   :path "f.dart" :target "foo" :src "int foo(int a){ return 0; }\n"}
   {:lang "zig"    :path "f.zig"  :target "foo" :src "fn foo(a: i32) i32 { return 0; }\n"}])

(defn- outline-name-found?
  "Does the tree-sitter outline expose a def whose name == target?"
  [path src target]
  (let [lang (outline/detect-language path)]
    (boolean (some #(= target (.name ^dev.kreuzberg.treesitterlanguagepack.StructuralApi$Target %))
               (StructuralApi/outline src lang)))))

(defn- edit-ok? [path src m marker]
  (try (str/includes? (structural/edit-source path src m) marker)
    (catch Throwable _ false)))

(defdescribe structural-language-matrix-test
  (describe "every supported language: full struct_patch lifecycle"
    (doseq [{:keys [lang path src target replace insert rename]} cases]
      (it (str lang " — outline finds the def by name")
        (expect (outline-name-found? path src target)))
      (it (str lang " — replace by name")
        (expect (edit-ok? path src {:op :replace :target target :code replace} "424242")))
      (it (str lang " — insert_after by name")
        (expect (edit-ok? path src {:op :insert-after :target target :code insert} (subs insert 0 6))))
      (it (str lang " — rename identifier")
        (expect (edit-ok? path src {:op :rename :target target :code rename} rename)))))

  ;; Fork-pending languages: replace-by-name is fixed in the pack fork (verified
  ;; locally) but not in the PUBLISHED dep yet, so assert only rename here
  ;; (identifier-based — works on .23). Flip into `cases` once the fork releases.
  (describe "fork-pending languages (replace fixed in fork, awaiting release)"
    (doseq [{:keys [lang path src target]} fork-pending-cases]
      (it (str lang " — rename works on the published dep")
        (expect (edit-ok? path src {:op :rename :target target :code "renamed_fn"} "renamed_fn"))))))
