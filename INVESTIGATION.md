# 4Clojure Benchmark Investigation — GLM-5-Turbo via RLM

**Date:** 2026-04-13
**Model:** glm-5-turbo
**Total:** 145/151 correct (96.0%)
**Cost:** $1.45

## Failures

### Timeouts (4 — hit 10 min limit)

#### #82 Word Chains
- **Status:** TIMEOUT (10 min)
- **Iterations:** exhausted or timed out
- **Root cause:** Model couldn't solve within iteration/time budget

#### #87 Create an Equation
- **Status:** TIMEOUT (10 min)
- **Iterations:** exhausted or timed out
- **Root cause:** Model couldn't solve within iteration/time budget

#### #127 Love Triangle
- **Status:** TIMEOUT (10 min)
- **Iterations:** exhausted or timed out
- **Root cause:** Model couldn't solve within iteration/time budget

#### #140 Veitch, Please!
- **Status:** TIMEOUT (10 min)
- **Iterations:** exhausted or timed out
- **Root cause:** Model couldn't solve within iteration/time budget

### Incorrect (2 — model solved, bb verification failed)

#### #150 Palindromic Numbers
- **Status:** INCORRECT (iter=2, 401.9s)
- **Answer:** `...`
- **bb error:** `Nested fn literals not allowed.`
- **Root cause:** TODO — investigate

#### #177 Balancing Brackets
- **Status:** INCORRECT (iter=5, 94.1s)
- **Answer:** `...`
- **bb error:** `Unable to resolve symbol: catch`
- **Root cause:** TODO — investigate

## Slowest Passes (top 15)

| # | Problem | Iter | Time | Notes |
|---|---------|------|------|-------|
| 93 | Partially Flatten a Sequence | 20 | 557.6s | |
| 103 | Generating k-combinations | 13 | 334.3s | |
| 65 | Black Box Testing | 5 | 231.9s | |
| 116 | Prime Sandwich | 7 | 214.2s | |
| 132 | Intervals | 4 | 160.1s | |
| 117 | For Science! | 6 | 157.2s | |
| 111 | Crossword puzzle | 3 | 139.8s | |
| 125 | Gus' Quinundrum | 2 | 137.1s | |
| 100 | Least Common Multiple | 7 | 117.9s | |
| 153 | Pairwise Disjoint Sets | 2 | 101.9s | |
| 131 | Sum Some Set Subsets | 3 | 100.0s | |
| 83 | A Half-Truth | 7 | 96.6s | |
| 60 | Sequence Reductions | 3 | 90.1s | |
| 173 | Intro to Destructuring 2 | 1 | 85.5s | |
| 112 | Sequs Horribilis | 1 | 79.8s | |

## All Results (sorted by duration desc)

| # | Problem | Status | Iter | Time |
|---|---------|--------|------|------|
| 93 | Partially Flatten a Sequence | ✅ | 20 | 557.6s |
| 150 | Palindromic Numbers | ❌ FAIL | 2 | 401.9s |
| 103 | Generating k-combinations | ✅ | 13 | 334.3s |
| 65 | Black Box Testing | ✅ | 5 | 231.9s |
| 116 | Prime Sandwich | ✅ | 7 | 214.2s |
| 132 | Intervals | ✅ | 4 | 160.1s |
| 117 | For Science! | ✅ | 6 | 157.2s |
| 111 | Crossword puzzle | ✅ | 3 | 139.8s |
| 125 | Gus' Quinundrum | ✅ | 2 | 137.1s |
| 100 | Least Common Multiple | ✅ | 7 | 117.9s |
| 153 | Pairwise Disjoint Sets | ✅ | 2 | 101.9s |
| 131 | Sum Some Set Subsets | ✅ | 3 | 100.0s |
| 83 | A Half-Truth | ✅ | 7 | 96.6s |
| 177 | Balancing Brackets | ❌ FAIL | 5 | 94.1s |
| 60 | Sequence Reductions | ✅ | 3 | 90.1s |
| 173 | Intro to Destructuring 2 | ✅ | 1 | 85.5s |
| 112 | Sequs Horribilis | ✅ | 1 | 79.8s |
| 101 | Levenshtein Distance | ✅ | 7 | 77.1s |
| 73 | Analyze a Tic-Tac-Toe Board | ✅ | 5 | 73.3s |
| 124 | Analyze Reversi | ✅ | 2 | 70.6s |
| 168 | Infinite Matrix | ✅ | 1 | 58.0s |
| 89 | Graph Tour | ✅ | 1 | 56.9s |
| 85 | Power Set | ✅ | 4 | 56.1s |
| 53 | Longest Increasing Sub-Seq | ✅ | 2 | 51.8s |
| 126 | Through the Looking Class | ✅ | 8 | 51.6s |
| 171 | Intervals | ✅ | 3 | 48.1s |
| 108 | Lazy Searching | ✅ | 2 | 46.9s |
| 105 | Identify keys and values | ✅ | 2 | 42.3s |
| 75 | Euler's Totient Function | ✅ | 3 | 42.2s |
| 58 | Function Composition | ✅ | 3 | 41.3s |
| 144 | Oscilrate | ✅ | 1 | 39.6s |
| 110 | Sequence of pronunciations | ✅ | 3 | 36.2s |
| 71 | Rearranging Code: -> | ✅ | 2 | 36.1s |
| 148 | The Big Divide | ✅ | 3 | 35.1s |
| 56 | Find Distinct Items | ✅ | 3 | 34.6s |
| 106 | Number Maze | ✅ | 3 | 34.3s |
| 121 | Universal Computation Engine | ✅ | 2 | 34.2s |
| 84 | Transitive Closure | ✅ | 2 | 31.9s |
| 120 | Sum of square of digits | ✅ | 1 | 30.2s |
| 54 | Partition a Sequence | ✅ | 2 | 29.3s |
| 114 | Global take-while | ✅ | 1 | 28.7s |
| 107 | Simple closures | ✅ | 2 | 25.8s |
| 195 | Parentheses... Again | ✅ | 2 | 25.6s |
| 119 | Win at Tic-Tac-Toe | ✅ | 2 | 24.7s |
| 69 | Merge with a Function | ✅ | 1 | 24.0s |
| 94 | Game of Life | ✅ | 2 | 23.9s |
| 98 | Equivalence Classes | ✅ | 2 | 23.8s |
| 62 | Re-implement Iteration | ✅ | 2 | 21.7s |
| 95 | To Tree, or not to Tree | ✅ | 1 | 20.8s |
| 157 | Indexing Sequences | ✅ | 1 | 19.8s |
| 115 | The Balance of N | ✅ | 1 | 18.3s |
| 76 | Intro to Trampoline | ✅ | 1 | 18.2s |
| 78 | Reimplement Trampoline | ✅ | 2 | 16.8s |
| 128 | Recognize Playing Cards | ✅ | 1 | 15.6s |
| 145 | For the win | ✅ | 2 | 15.2s |
| 141 | Tricky card games | ✅ | 1 | 14.9s |
| 96 | Beauty is Symmetry | ✅ | 1 | 14.3s |
| 147 | Pascal's Trapezoid | ✅ | 1 | 14.2s |
| 72 | Rearranging Code: ->> | ✅ | 1 | 14.0s |
| 118 | Re-implement Map | ✅ | 1 | 13.1s |
| 91 | Graph Connectivity | ✅ | 1 | 12.8s |
| 137 | Digits and bases | ✅ | 1 | 12.7s |
| 104 | Write Roman Numerals | ✅ | 1 | 12.7s |
| 63 | Group a Sequence | ✅ | 1 | 12.5s |
| 97 | Pascal's Triangle | ✅ | 1 | 12.5s |
| 135 | Infix Calculator | ✅ | 1 | 12.5s |
| 92 | Read Roman numerals | ✅ | 1 | 12.0s |
| 74 | Filter Perfect Squares | ✅ | 1 | 11.6s |
| 61 | Map Construction | ✅ | 1 | 11.5s |
| 158 | Decurry | ✅ | 1 | 11.4s |
| 19 | Last Element | ✅ | 1 | 11.2s |
| 40 | Interpose a Seq | ✅ | 1 | 11.1s |
| 102 | intoCamelCase | ✅ | 1 | 11.1s |
| 122 | Read a binary number | ✅ | 1 | 10.9s |
| 46 | Flipping out | ✅ | 1 | 10.8s |
| 79 | Triangle Minimal Path | ✅ | 1 | 10.5s |
| 134 | A nil key | ✅ | 1 | 10.4s |
| 70 | Word Sorting | ✅ | 1 | 10.4s |
| 20 | Penultimate Element | ✅ | 1 | 10.2s |
| 57 | Simple Recursion | ✅ | 2 | 10.1s |
| 47 | Contain Yourself | ✅ | 1 | 10.0s |
| 17 | map | ✅ | 1 | 9.9s |
| 86 | Happy numbers | ✅ | 1 | 9.8s |
| 162 | Logical falsity and truth | ✅ | 1 | 9.6s |
| 44 | Rotate Sequence | ✅ | 1 | 9.5s |
| 34 | Implement range | ✅ | 1 | 9.4s |
| 13 | rest | ✅ | 2 | 9.3s |
| 64 | Intro to Reduce | ✅ | 1 | 9.3s |
| 43 | Reverse Interleave | ✅ | 1 | 9.3s |
| 166 | Comparisons | ✅ | 1 | 9.3s |
| 18 | filter | ✅ | 1 | 9.2s |
| 27 | Palindrome Detector | ✅ | 1 | 9.1s |
| 28 | Flatten a Sequence | ✅ | 1 | 9.1s |
| 45 | Intro to Iterate | ✅ | 2 | 8.8s |
| 90 | Cartesian Product | ✅ | 1 | 8.8s |
| 67 | Prime Numbers | ✅ | 1 | 8.7s |
| 68 | Recurring Theme | ✅ | 1 | 8.7s |
| 161 | Subset and Superset | ✅ | 1 | 8.6s |
| 39 | Interleave Two Seqs | ✅ | 1 | 8.5s |
| 37 | Regular Expressions | ✅ | 1 | 8.5s |
| 81 | Set Intersection | ✅ | 1 | 8.2s |
| 146 | Trees into tables | ✅ | 1 | 8.2s |
| 32 | Duplicate a Sequence | ✅ | 1 | 8.1s |
| 31 | Pack a Sequence | ✅ | 1 | 8.1s |
| 41 | Drop Every Nth Item | ✅ | 1 | 7.8s |
| 88 | Symmetric Difference | ✅ | 1 | 7.7s |
| 26 | Fibonacci Sequence | ✅ | 1 | 7.6s |
| 55 | Count Occurences | ✅ | 1 | 7.4s |
| 30 | Compress a Sequence | ✅ | 1 | 7.3s |
| 33 | Replicate a Sequence | ✅ | 1 | 7.3s |
| 25 | Find the odd numbers | ✅ | 1 | 7.3s |
| 48 | Intro to some | ✅ | 1 | 7.0s |
| 38 | Maximum value | ✅ | 1 | 7.0s |
| 77 | Anagram Finder | ✅ | 1 | 7.0s |
| 66 | Greatest Common Divisor | ✅ | 1 | 7.0s |
| 23 | Reverse a Sequence | ✅ | 1 | 6.9s |
| 156 | Map Defaults | ✅ | 1 | 6.7s |
| 10 | Maps | ✅ | 1 | 6.7s |
| 12 | Sequences | ✅ | 1 | 6.7s |
| 59 | Juxtaposition | ✅ | 1 | 6.6s |
| 80 | Perfect Numbers | ✅ | 1 | 6.6s |
| 22 | Count a Sequence | ✅ | 1 | 6.6s |
| 5 | conj on lists | ✅ | 1 | 6.4s |
| 143 | dot product | ✅ | 1 | 6.4s |
| 36 | Let it Be | ✅ | 1 | 6.4s |
| 16 | Hello World | ✅ | 1 | 6.4s |
| 6 | Vectors | ✅ | 1 | 6.2s |
| 24 | Sum It All Up | ✅ | 1 | 6.2s |
| 29 | Get the Caps | ✅ | 1 | 6.2s |
| 35 | Local bindings | ✅ | 1 | 6.2s |
| 49 | Split a sequence | ✅ | 1 | 6.2s |
| 11 | conj on maps | ✅ | 1 | 6.2s |
| 21 | Nth Element | ✅ | 1 | 6.1s |
| 99 | Product Digits | ✅ | 1 | 6.1s |
| 7 | conj on vectors | ✅ | 1 | 5.9s |
| 4 | Lists | ✅ | 1 | 5.8s |
| 51 | Advanced Destructuring | ✅ | 1 | 5.7s |
| 42 | Factorial Fun | ✅ | 1 | 5.7s |
| 8 | Sets | ✅ | 1 | 5.7s |
| 50 | Split by Type | ✅ | 1 | 5.5s |
| 9 | conj on sets | ✅ | 1 | 5.2s |
| 52 | Intro to Destructuring | ✅ | 1 | 5.1s |
| 15 | Double Down | ✅ | 1 | 5.1s |
| 2 | Simple Math | ✅ | 1 | 4.8s |
| 14 | Functions | ✅ | 1 | 4.7s |
| 3 | Strings | ✅ | 1 | 4.7s |
| 1 | Nothing but the Truth | ✅ | 1 | 4.6s |
| 82 | Word Chains | ⏰ TIMEOUT | 0 | 0.0s |
| 87 | Create an Equation | ⏰ TIMEOUT | 0 | 0.0s |
| 127 | Love Triangle | ⏰ TIMEOUT | 0 | 0.0s |
| 140 | Veitch, Please! | ⏰ TIMEOUT | 0 | 0.0s |
