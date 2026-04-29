(ns com.blockether.vis.internal.spec
  "Iteration response spec.

   The agent's response shape is tiny:

     { :thinking? str    ;; non-reasoning models only — chain-of-thought
       :code      str    ;; required — ONE Clojure source blob, parsed by the
                         ;;            runtime into top-level forms; each form
                         ;;            becomes its own expression_state row.
                         ;;            Empty string is allowed (no-op iteration).
       :answer?   str    ;; emit when done; terminates the turn }

   Public surface:
     iteration-spec                   composer; picks thinking / no-thinking
     ITERATION_SPEC_NON_REASONING     prebuilt with :thinking
     ITERATION_SPEC_REASONING         prebuilt without :thinking
     ITERATION_SPEC_BASE              alias for ITERATION_SPEC_REASONING"
  (:require
   [com.blockether.svar.internal.spec :as svar-spec]))

(defn- make-iteration-spec
  [{:keys [include-thinking?]}]
  (let [base-fields
        [(svar-spec/field {::svar-spec/name        :code
                           ::svar-spec/type        :spec.type/string
                           ::svar-spec/cardinality :spec.cardinality/one
                           ::svar-spec/required    true
                           ::svar-spec/description "ONE Clojure source string for this iteration. The runtime parses it into top-level forms and evaluates each in order. `(def name val)` and `(defn name [args] body)` create vars visible in <var_index>. Pass an empty string `\"\"` when emitting :answer with no further code."})
         (svar-spec/field {::svar-spec/name        :answer
                           ::svar-spec/type        :spec.type/string
                           ::svar-spec/cardinality :spec.cardinality/one
                           ::svar-spec/required    false
                           ::svar-spec/description "Final answer for the user. Emit when the task is satisfied. Plain text or markdown."})]

        fields
        (if include-thinking?
          (into [(svar-spec/field {::svar-spec/name        :thinking
                                   ::svar-spec/type        :spec.type/string
                                   ::svar-spec/cardinality :spec.cardinality/one
                                   ::svar-spec/required    false
                                   ::svar-spec/description "Short chain-of-thought for this iteration. Non-reasoning providers only."})]
            base-fields)
          base-fields)]
    (apply svar-spec/spec fields)))

(defn iteration-spec
  "Compose the iteration response spec for the current environment state.
   `:has-reasoning?` selects the thinking/non-thinking variant."
  [{:keys [has-reasoning?]}]
  (make-iteration-spec {:include-thinking? (not has-reasoning?)}))

(def ITERATION_SPEC_BASE
  "Reasoning variant — no :thinking field."
  (make-iteration-spec {:include-thinking? false}))

(def ITERATION_SPEC_NON_REASONING
  "Non-reasoning variant — includes :thinking."
  (make-iteration-spec {:include-thinking? true}))

(def ITERATION_SPEC_REASONING
  "Alias for ITERATION_SPEC_BASE."
  ITERATION_SPEC_BASE)
