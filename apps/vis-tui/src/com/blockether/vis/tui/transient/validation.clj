(ns com.blockether.vis.tui.transient.validation
  "TUI-local validation for a transient popup's data, paint region and host adapter.

   `item-types` is the ONE vocabulary table. The renderer derives flag, value and
   command behavior from its traits; producers are checked once at `transient/run!`,
   never per keystroke or painted frame.

   These shapes are private to this channel, so ordinary predicates keep them beside
   their sole consumer instead of presenting a cross-owner `clojure.spec` contract.
   Every `*-error` returns nil or one short line that `transient/run!` may put directly
   in its typed exception."
  (:require [clojure.string :as str]))

;; The vocabulary — one table, every surface reads it

(def item-types
  "Every kind of row a transient offers, and what that kind MEANS:

     `:is-flag`     wears the leading `-`, toggles IN PLACE and keeps the
                    popup open
     `:is-valued`   carries a value the human is asked for, stored under
                    `[:options id]`
     `:is-command`  fires ONCE and ends the run, reported as `:action`

   Traits — never the type keyword — drive the component's branches."
  {:switch {:is-flag true :is-valued false :is-command false}
   :option {:is-flag true :is-valued true :is-command false}
   :action {:is-flag false :is-valued false :is-command true}})

(defn- types-with
  "The item types whose [[item-types]] entry carries `trait`."
  [trait]
  (into #{}
        (keep (fn [[type traits]]
                (when (get traits trait) type)))
        item-types))

(def flag-types
  "Types drawn with a leading `-` and toggled in place: `#{:switch :option}`."
  (types-with :is-flag))

(def valued-types
  "Types that carry a value read from the human: `#{:option}`."
  (types-with :is-valued))

(def command-types
  "Types that fire once and close the popup: `#{:action}`."
  (types-with :is-command))

;; The keys — one table per closed data shape

(def value-keys
  "Keys only a valued item may carry: how its value is asked for and echoed."
  #{:prompt :mask :secret?})

(def item-keys "Every key one item may carry." (into #{:key :type :id :label :arg} value-keys))

(def group-keys "Every key one group may carry." #{:title :items})

(def spec-keys "Every key one transient declaration may carry." #{:title :groups :read-option})

(def state-keys "Every key in a transient run state." #{:switches :options})

;; Small predicates

(defn- closed? [allowed value] (and (map? value) (every? allowed (keys value))))

(defn- non-blank-string? [value] (and (string? value) (not (str/blank? value))))

(defn- one-key? [value] (and (string? value) (= 1 (count value))))

(defn- optional-valid? [value key pred] (or (not (contains? value key)) (pred (get value key))))

(defn- items-of [spec] (mapcat :items (:groups spec)))

(defn- duplicate-values? [values] (not= (count values) (count (distinct values))))

(defn- child-error
  [label explain values]
  (some (fn [[index value]]
          (when-let [why (explain value)]
            (str label " " index ": " why)))
        (map-indexed vector values)))

;; Explainers

(defn item-error
  "nil when `item` is a legal transient row, else one short reason."
  [item]
  (cond (not (map? item)) "item must be a map"
        (not (closed? item-keys item)) "item carries an unsupported key"
        (not (contains? item :key)) "item needs :key"
        (not (one-key? (:key item))) "item :key must be one character"
        (not (contains? item-types (:type item))) "item :type is not supported"
        (nil? (:id item)) "item needs a non-nil :id"
        (not (string? (:label item))) "item :label must be a string"
        (not (optional-valid? item :arg non-blank-string?)) "item :arg must be non-blank text"
        (not (optional-valid? item :prompt non-blank-string?)) "item :prompt must be non-blank text"
        (not (optional-valid? item :mask char?)) "item :mask must be one character"
        (not (optional-valid? item :secret? boolean?)) "item :secret? must be boolean"
        (and (contains? item :arg) (not (contains? flag-types (:type item))))
        "only a flag item may carry :arg"
        (and (not (contains? valued-types (:type item))) (some value-keys (keys item)))
        "only a valued item may carry value settings"
        :else nil))

(defn group-error
  "nil when `group` is a legal non-empty group, else one short reason."
  [group]
  (let [items (:items group)]
    (cond (not (map? group)) "group must be a map"
          (not (closed? group-keys group)) "group carries an unsupported key"
          (not (contains? group :items)) "group needs :items"
          (not (sequential? items)) "group :items must be sequential"
          (not (seq items)) "group :items must not be empty"
          (not (optional-valid? group :title string?)) "group :title must be a string"
          :else (child-error "item" item-error items))))

(defn spec-error
  "nil when `spec` is a legal transient declaration, else one short reason."
  [spec]
  (let [groups (:groups spec)]
    (cond (not (map? spec)) "transient declaration must be a map"
          (not (closed? spec-keys spec)) "transient declaration carries an unsupported key"
          (not (contains? spec :groups)) "transient declaration needs :groups"
          (not (sequential? groups)) "transient :groups must be sequential"
          (not (seq groups)) "transient :groups must not be empty"
          (not (optional-valid? spec :title string?)) "transient :title must be a string"
          (not (optional-valid? spec :read-option ifn?)) "transient :read-option must be callable"
          :else
          (or (child-error "group" group-error groups)
              (let [items (items-of spec)]
                (cond (duplicate-values? (map :key items)) "transient item keys must be distinct"
                      (duplicate-values? (map :id items)) "transient item ids must be distinct"
                      :else nil))))))

(defn region-error
  "nil when `region` is a rectangle the component can paint into, else one short reason."
  [region]
  (cond (not (map? region)) "region must be a map"
        (not (nat-int? (:left region))) "region :left must be a natural integer"
        (not (pos-int? (:inner-w region))) "region :inner-w must be a positive integer"
        (not (pos-int? (:text-w region))) "region :text-w must be a positive integer"
        (not (nat-int? (:hint-row region))) "region :hint-row must be a natural integer"
        (not (optional-valid? region :min-row nat-int?)) "region :min-row must be a natural integer"
        (not (optional-valid? region :cols pos-int?)) "region :cols must be a positive integer"
        (not (optional-valid? region :is-sideless boolean?)) "region :is-sideless must be boolean"
        (not (optional-valid? region :restore! ifn?)) "region :restore! must be callable"
        (and (:is-sideless region) (nil? (:cols region))) "a sideless region needs :cols"
        :else nil))

(defn host-error
  "nil when `host` can paint, flush and answer keystrokes, else one short reason."
  [host]
  (cond (not (map? host)) "host must be a map"
        (nil? (:g host)) "host needs :g"
        (not (fn? (:hint-bar! host))) "host :hint-bar! must be a function"
        (not (fn? (:refresh! host))) "host :refresh! must be a function"
        (not (fn? (:read-key! host))) "host :read-key! must be a function"
        :else nil))

(defn state-error
  "nil when `state` is a legal run state, else one short reason."
  [state]
  (let [switches
        (:switches state)

        options
        (:options state)]

    (cond (not (map? state)) "state must be a map"
          (not (closed? state-keys state)) "state carries an unsupported key"
          (not (set? switches)) "state :switches must be a set"
          (not (every? some? switches)) "state :switches must not contain nil"
          (not (map? options)) "state :options must be a map"
          (not (every? some? (keys options))) "state :options must not use nil keys"
          (not (every? non-blank-string? (vals options)))
          "state option values must be non-blank text"
          :else nil)))

(defn option-value-error
  "nil when `value` is text an option may paint and return, else one short reason."
  [value]
  (when-not (non-blank-string? value) "option value must be non-blank text"))
