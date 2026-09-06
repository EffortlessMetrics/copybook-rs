# SPDX-License-Identifier: AGPL-3.0-or-later
from pathlib import Path

path = Path("crates/copybook-cli/src/commands/verify.rs")
text = path.read_text(encoding="utf-8")
old = """            Err(error) => {
                records_total += 1;
                record_verification_error(&mut verify_report, records_total - 1, &error, None);
                if record_iter.is_eof() {
                    break;
                }
            }
"""
new = """            Err(error) => {
                records_total += 1;
                record_verification_error(&mut verify_report, records_total - 1, &error, None);
                // A framing error leaves no reliable record boundary from which
                // verification can resume.
                break;
            }
"""
if text.count(old) != 1:
    raise SystemExit("expected verify framing-error loop was not found exactly once")
path.write_text(text.replace(old, new, 1), encoding="utf-8")
