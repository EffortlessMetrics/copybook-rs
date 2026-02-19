#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

branch="roadmap/phase6-done-$(date +%Y%m%d%H%M%S)"
git checkout -b "$branch"

python - <<'PY'
from pathlib import Path
import re

path = Path("docs/ROADMAP.md")
text = path.read_text()

status_pattern = re.compile(r"(\*\*Status:\*\*)[^\n]*")
replacement = r"\1 Done — staging exporter live; dashboards + alerts active."
text, count = status_pattern.subn(replacement, text, count=1)
rendered_status = "**Status:** Done — staging exporter live; dashboards + alerts active."
if count == 0 and rendered_status not in text:
    raise SystemExit("Failed to update status line in docs/ROADMAP.md")

checklist_map = {
    "- [ ] Staging rollout (enable flag; Prom scrape)": "- [x] Staging rollout (enable flag; Prom scrape)",
    "- [ ] Dashboards (QPS, bytes/s, error rate, MiB/s)": "- [x] Dashboards (QPS, bytes/s, error rate, MiB/s)",
    "- [ ] Alerts (error burst, throughput drop)": "- [x] Alerts (error burst, throughput drop)",
    "- [ ] Perf sanity on staging (MiB/s, RSS in-band)": "- [x] Perf sanity on staging (MiB/s, RSS in-band)",
}

for old, new in checklist_map.items():
    if old in text:
        text = text.replace(old, new, 1)
    elif new not in text:
        raise SystemExit(f"Did not find checklist item: {old}")

path.write_text(text)
PY

git add docs/ROADMAP.md
git commit -m "docs(roadmap): mark Telemetry Phase 6 as done"
git push -u origin "$branch"

echo "Open a PR for ${branch} with dashboard + release links."
