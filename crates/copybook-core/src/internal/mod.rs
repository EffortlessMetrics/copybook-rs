// SPDX-License-Identifier: AGPL-3.0-or-later
//! Crate-internal semantic helpers owned by `copybook-core`.
//!
//! Checked operations live with the layer that assigns the stable product
//! error (#655): these helpers return core-typed failures instead of
//! delegating to a context-free utility package. The module is `pub(crate)`
//! so the convergence work adds no public API surface.

pub(crate) mod bounds;
pub(crate) mod collections;
