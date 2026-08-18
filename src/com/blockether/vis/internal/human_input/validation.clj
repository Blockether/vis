(ns com.blockether.vis.internal.human-input.validation
  "Field validation for human-input requests: a validator is a FUNCTION.

   A field carries `:validate` — one function or a list of them. Each one is
   called with the COERCED value, or with the value and the whole coerced
   `field id -> value` map when it asks for a second argument, and answers:

   - `nil` or `true` — the value is fine.
   - a string — that string IS the field's error message.
   - `false` — \"is not valid\", the message of last resort. Say something better.
   - a throw — \"could not be validated: …\", because swallowing it would ACCEPT
     the value the validator was written to refuse.

   Validators run in the order they were written and the FIRST message wins, so
   a field's `:validate` list reads like a checklist.

   Validation is code, not data, and it never crosses the wire. The ENGINE is
   the only authority: it runs every validator when a form is CONFIRMED
   ([[com.blockether.vis.internal.human-input/submit!]]) and hands the surfaces
   the errors it found, keyed by field. Neither the TUI band nor the companion
   app owns a validation rule or re-checks anything as the operator types —
   they show the engine's verdict, and drop a field's error the moment that
   field is touched again, so nobody is scolded mid-word about a value they are
   still typing.

   A validator NEVER fires on a blank value. Emptiness is `:is-required`'s
   single job; a check that also rejected \"\" would quietly make every optional
   field mandatory."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- declared-arities
  "Every argument count `f` ITSELF declares an `invoke` method for. Declared, not
   inherited: `AFn` implements `invoke` for every arity there is (each of them a
   thrown `ArityException`), so asking a class for all its methods would claim a
   two-argument function is happy with one."
  [f]
  (into #{}
        (comp (filter #(= "invoke" (.getName ^java.lang.reflect.Method %)))
              (map #(alength (.getParameterTypes ^java.lang.reflect.Method %))))
        (.getDeclaredMethods ^Class (class f))))

(defn- arity-of
  "How a validator wants to be called: 1 for `(f value)`, 2 for
   `(f value values)`, or nil when it can take neither. A function that accepts
   both is called with the value alone — the simplest call it agreed to."
  [f]
  (if (instance? clojure.lang.RestFn f)
    ;; A variadic fn declares no fixed `invoke` at all; what it can take is
    ;; "its required count, or more".
    (let [required (.getRequiredArity ^clojure.lang.RestFn f)]
      (cond (<= required 1) 1
            (= required 2) 2
            :else nil))
    (let [arities (declared-arities f)]
      (cond (contains? arities 1) 1
            (contains? arities 2) 2
            :else nil))))

(defn normalize-validators
  "`raw` — one validator or a list of them — as the field's vector of
   two-argument functions, or nil when the field declares none. `fail!` reports
   a bad validator the way the surrounding spec reports every other mistake.

   The arity question is asked HERE, once, while the spec is being read: a
   validator that takes only the value is wrapped into the two-argument shape
   [[check]] calls, and one that can take neither shape is refused before a
   human is ever shown the form it would have crashed."
  [raw fail!]
  (let [normalize (fn [validator]
                    (if-not (fn? validator)
                      (fail! (str ":validate takes a FUNCTION, not "
                                  (pr-str validator)
                                  " — it receives the value (and, if it asks for a second"
                                  " argument, every value) and answers nil when the value"
                                  " is fine or the error message as a string"))
                      (let [arity (arity-of validator)]
                        (cond (= 1 arity) (fn [value _values]
                                            (validator value))
                              (= 2 arity) validator
                              :else (fail! (str ":validate function takes the value, or the value"
                                                " and every value — this one takes neither"))))))]
    (cond (nil? raw) nil
          (sequential? raw) (not-empty (mapv normalize raw))
          :else [(normalize raw)])))

(defn- blank-value?
  "Nothing to check. A validator describes the SHAPE of an answer; whether there
   is one at all is `:is-required`'s single job."
  [value]
  (cond (nil? value) true
        (string? value) (str/blank? value)
        (coll? value) (empty? value)
        :else false))

(defn- verdict->message
  [verdict]
  (cond (or (nil? verdict) (true? verdict)) nil
        (false? verdict) "is not valid"
        (string? verdict) (not-empty (str/trim verdict))
        :else (str verdict)))

(defn check
  "The first message a validator has against `value`, or nil when every one of
   them accepts it. `values` is the whole coerced `field id -> value` map, which
   is how a confirmation field compares itself with the field it confirms."
  [validators value values]
  (when-not (blank-value? value)
    (some (fn [validator]
            (try (verdict->message (validator value values))
                 (catch Throwable t (str "could not be validated: " (or (ex-message t) (str t))))))
          validators)))
