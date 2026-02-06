## 2026-02-25 - Tree View for Nested Structures
**Learning:** For deeply nested data structures like COBOL copybooks, a flat list with dot-notation paths is difficult to scan. Replacing it with a tree view using box-drawing characters (├──, └──) significantly improves readability and user understanding of the structure.
**Action:** When displaying hierarchical data in CLI tools, prefer recursive tree visualizations over flat lists, even if it requires slightly more complex printing logic.
