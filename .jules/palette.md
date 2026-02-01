## 2026-03-05 - [CLI Tree Visualization]
**Learning:** Flat lists of hierarchical data (like COBOL paths) create high cognitive load for users trying to understand the structure. Replacing full dot-separated paths with indented tree structures using box-drawing characters (│, ├──, └──) dramatically improves scanability and mental model alignment.
**Action:** When displaying nested structures in CLI tools (like schemas or file trees), prefer tree visualizations over flat lists of paths. Ensure proper handling of `is_last` for clean connectors.
