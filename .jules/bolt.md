## 2026-02-24 - Zero-Allocation Encoding Pattern
**Learning:** For high-throughput encoding of small records (e.g., 15 bytes), `Vec::new` allocation per record can consume ~5% of CPU time.
**Action:** Use `encode_record_into` pattern with a reused `Vec<u8>` buffer for batch processing loops (like `encode_jsonl_to_file`) to eliminate this overhead.
