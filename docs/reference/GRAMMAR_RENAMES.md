# RENAMES (Level-66) — Grammar (Slice-1: parse only)

```
RENAMES-DECL ::= '66' IDENT 'RENAMES' QNAME ('THRU' | 'THROUGH') QNAME '.'
QNAME        ::= IDENT ( 'OF' IDENT )*
```

**Status:** Parse only (Issue #122, Slice-1). Resolver/projection is deferred to Slice-2.
