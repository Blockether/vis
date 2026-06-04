#!/usr/bin/env bash
# W6 (hard) — multi-turn measurement on a BIG, multi-function file.
#
# The easy calc.clj scenario was too small to force re-location or warrant a
# task. This seeds a ~160-line service.clj with many functions, then runs 4
# turns each editing a DIFFERENT function — so the model must re-locate inside
# a big file every turn (stresses locate-waste) and each turn is genuinely
# multi-step (stresses the PLAN/REMEMBER gates).
#
# Prints the session UUID on the last line for scoring with benches.ctx-metrics.
#   VIS_PROVIDER / VIS_MODEL override the model (default zai-coding-plan/glm-5.1).
set -euo pipefail

VIS_BIN="${VIS_BIN:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)/bin/vis}"
PROVIDER="${VIS_PROVIDER:-zai-coding-plan}"
MODEL="${VIS_MODEL:-glm-5.1}"
TURN_TIMEOUT="${W6_TURN_TIMEOUT:-360}"

work="$(mktemp -d)"
echo "[w6-hard] workdir=$work model=$PROVIDER/$MODEL" >&2

cat > "$work/service.clj" <<'CLJ'
(ns service
  "Tiny in-memory order service used as a benchmark fixture.")

(def ^:private orders (atom {}))

(defn validate-order
  "Return the order if it has :id and a non-empty :items vec, else throw."
  [order]
  (when-not (:id order)
    (throw (ex-info "order missing :id" {:order order})))
  (when-not (seq (:items order))
    (throw (ex-info "order has no :items" {:order order})))
  order)

(defn item-subtotal
  "Price for a single line item: :qty * :unit-price."
  [{:keys [qty unit-price]}]
  (* (or qty 0) (or unit-price 0)))

(defn price-order
  "Sum every line item's subtotal into the order's :subtotal."
  [order]
  (assoc order :subtotal (reduce + 0 (map item-subtotal (:items order)))))

(defn apply-discount
  "Multiply :subtotal by (1 - rate) into :total."
  [order rate]
  (assoc order :total (* (:subtotal order) (- 1 rate))))

(defn format-receipt
  "Human-readable multi-line receipt string for a priced order."
  [order]
  (str "Order " (:id order) "\n"
    "  items:    " (count (:items order)) "\n"
    "  subtotal: " (:subtotal order) "\n"
    "  total:    " (:total order)))

(defn save-order
  "Persist a validated order into the in-memory store."
  [order]
  (let [o (validate-order order)]
    (swap! orders assoc (:id o) o)
    o))

(defn load-order
  "Fetch a stored order by id, or nil."
  [id]
  (get @orders id))

(defn list-orders
  "All stored orders as a vector, in insertion-independent order."
  []
  (vec (vals @orders)))

(defn summarize-orders
  "Count + summed totals across all stored orders."
  []
  (let [os (list-orders)]
    {:count (count os)
     :total (reduce + 0 (keep :total os))}))
CLJ

echo "[w6-hard] seeded service.clj ($(wc -l < "$work/service.clj") lines)" >&2

run_turn() { local prompt="$1" sid="${2:-}"
  ( cd "$work"
    if [[ -n "$sid" ]]; then
      timeout "$TURN_TIMEOUT" "$VIS_BIN" --json --session-id "$sid" --provider "$PROVIDER" --model "$MODEL" "$prompt"
    else
      timeout "$TURN_TIMEOUT" "$VIS_BIN" --json --persist --provider "$PROVIDER" --model "$MODEL" "$prompt"
    fi ); }
sid_of() { python3 -c 'import json,sys; print(json.load(sys.stdin).get("session-id") or "")'; }

echo "[w6-hard] turn 1/4 — price-order nil guard" >&2
o1="$(run_turn 'In service.clj, make price-order throw a clear ex-info when passed nil instead of NPE-ing. Load the ns to verify it compiles.')"
SID="$(printf '%s' "$o1" | sid_of)"
echo "[w6-hard] session=$SID" >&2
[[ -z "$SID" ]] && { echo "[w6-hard] FAILED: no session id"; echo "$o1" >&2; exit 1; }

echo "[w6-hard] turn 2/4 — apply-discount clamp" >&2
run_turn 'In service.clj, change apply-discount so rate is clamped between 0 and 1 before use. Verify the ns still loads.' "$SID" >/dev/null

echo "[w6-hard] turn 3/4 — list-orders :status filter" >&2
run_turn 'In service.clj, add an optional status arg to list-orders that, when given, keeps only orders whose :status matches. Verify it loads.' "$SID" >/dev/null

echo "[w6-hard] turn 4/4 — add total-revenue" >&2
run_turn 'In service.clj, add a function total-revenue that sums :total across all stored orders (reuse summarize-orders if helpful). Verify it loads.' "$SID" >/dev/null

echo "[w6-hard] DONE session=$SID" >&2
echo "$SID"
