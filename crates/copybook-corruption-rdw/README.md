# copybook-corruption-rdw

Small 0.5 compatibility crate forwarding RDW ASCII-transfer corruption
heuristics to `copybook_rdw::diagnostics`.

The canonical implementation lives with the RDW framing owner; this package
remains available for existing 0.5 call sites while primary consumers migrate.
