#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

cargo run --quiet --package copybook-scripts -- soak-aggregate
