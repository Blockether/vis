# vis-playground

The project that the [Vis playground](https://github.com/Blockether/vis/tree/main/examples/aws-playground) works on.

A Vis session on AWS Lambda gets a clone of the `main` branch. It can read, change and test the
code, and publish its commits back to this repository.

- `src/shop/` is a small Python package. Run its tests with `python3 -m pytest`.
- `src/shop/pricing.clj` is a small Clojure namespace with the same price rules.
