<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-audit

Experimental enterprise audit system for copybook-rs: regulatory compliance
tracking (SOX, HIPAA, GDPR, PCI DSS), security event auditing, performance
baselines, data lineage, and tamper-evident audit trails.

This crate lives **outside the stable core** (`copybook-core` has no audit
surface since #656 Phase E). Depend on it explicitly to opt in:

```toml
[dependencies]
copybook-audit = "0.5"
```

See `docs/audit-api-reference.md` for the API contracts. Experimental APIs may
change between minor versions; validate before production use.
