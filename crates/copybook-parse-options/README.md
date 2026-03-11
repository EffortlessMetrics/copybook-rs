# copybook-parse-options

Shared `ParseOptions` contract for copybook parsing APIs.

This microcrate exists to keep parsing option definitions in a single SRP-focused
location, enabling parser- and lexer-adjacent crates to reuse the same type
without depending on heavier parsing implementations.
