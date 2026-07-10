<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Example Guidance

This file extends the root [`AGENTS.md`](../AGENTS.md). Examples teach supported
public workflows; they must not depend on private APIs, fabricated types, or
undocumented behavior.

Several examples are standalone Cargo projects, and Kafka examples can require
external services. Inspect the nearest manifest and map before choosing a
command. Compile examples against the intended public feature set, keep inputs
sanitized, and make prerequisites and unsupported environments explicit.
Example prose must agree with the library API and CLI reference.
