(ns com.blockether.vis.ext.language-clojure.reflection-test
  "Tests for the `:general` lint provider (reflection + boxed-math compiler
   warnings) in `reflection/compile-warnings`."
  (:require [com.blockether.vis.ext.language-clojure.reflection :as reflection]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- types [findings] (set (map #(get % "type") findings)))

(defdescribe
  compile-warnings-test
  (it "flags an unresolved interop call as a reflection warning"
      (let
        [fs
         (reflection/compile-warnings "(defn r [x] (.length x))" "<stdin>")

         refl
         (first (filter #(= "reflection" (get % "type")) fs))]

        (expect (contains? (types fs) "reflection"))
        (expect (= "warning" (get refl "level")))
        (expect (= "general" (get refl "provider")))
        (expect (= "<stdin>" (get refl "file")))
        (expect (number? (get refl "row")))
        (expect (number? (get refl "col")))
        (expect (string? (get refl "message")))))
  (it "flags boxed numeric ops as a boxed-math warning"
      (let
        [fs
         (reflection/compile-warnings "(defn add [a b] (+ a b))" "<stdin>")

         boxed
         (first (filter #(= "boxed-math" (get % "type")) fs))]

        (expect (contains? (types fs) "boxed-math"))
        (expect (= "general" (get boxed "provider")))
        (expect (= "warning" (get boxed "level")))))
  (it "reports clean, primitive-typed code with no findings"
      (expect (empty? (reflection/compile-warnings
                        "(defn add ^long [^long a ^long b] (unchecked-add a b))"
                        "<stdin>"))))
  (it "returns [] for blank code"
      (expect (= [] (reflection/compile-warnings "" "<stdin>")))
      (expect (= [] (reflection/compile-warnings nil "<stdin>"))))
  (it "never throws on a hard compile error"
      (expect (vector? (reflection/compile-warnings "(this is (not balanced" "<stdin>"))))
  (it "keeps the compiler-reported source when no file is given"
      (let [fs (reflection/compile-warnings "(defn r [x] (.length x))")]
        (expect (contains? (types fs) "reflection"))))
  (it "does not leak the throwaway namespace it compiles in"
      (let [before (set (map ns-name (all-ns)))]
        (reflection/compile-warnings "(ns leaky.probe) (defn r [x] (.length x))" "<stdin>")
        (expect (not (contains? (set (map ns-name (all-ns))) 'leaky.probe)))
        ;; no namespaces leaked at all
        (expect (= before (set (map ns-name (all-ns))))))))
