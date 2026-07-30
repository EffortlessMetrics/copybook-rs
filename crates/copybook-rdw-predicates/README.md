# copybook-rdw-predicates

Single-responsibility microcrate for the RDW ASCII-digit header heuristic.

This 0.5 compatibility crate forwards the byte-level predicate owned by
`copybook_rdw::diagnostics`. It intentionally stays narrow:

- `rdw_is_suspect_ascii_corruption` checks whether RDW length bytes look like
  ASCII digits.
- `rdw_is_suspect_ascii_corruption_slice` applies the same predicate to a
  variable-length byte slice with bounds guarding.
