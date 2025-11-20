# RENAMES Implementation - Fix Checklist & Code Locations

## Quick Reference: What Needs Fixing

### 1. Field Struct Initialization (BLOCKING)

**Issue**: Direct Field struct instantiation missing `resolved_renames: None`

**Files to Fix**:

#### copybook-codec/src/roundtrip.rs
- [ ] Line 256: `create_simple_alphanum_schema()` - add `resolved_renames: None,`
- [ ] Line 278: `create_zoned_decimal_schema()` - add `resolved_renames: None,`
- [ ] Line 300: `create_packed_decimal_schema()` - add `resolved_renames: None,`
- [ ] Line 322: `create_binary_int_schema()` - add `resolved_renames: None,`
- [ ] Line 344: `create_redefines_schema()` - primary_field - add `resolved_renames: None,`
- [ ] Line 359: `create_redefines_schema()` - redefining_field - add `resolved_renames: None,`

**Pattern to add** (insert before `children: Vec::new(),`):
```rust
resolved_renames: None,
```

#### copybook-codec/src/odo_redefines.rs
- [ ] Line 604: `test_odo_with_redefines()` - counter - add `resolved_renames: None,`
- [ ] Line 624: `test_odo_with_redefines()` - array_field - add `resolved_renames: None,`
- [ ] Line 756: `test_redefines_and_odo_in_group()` - field_a - add `resolved_renames: None,`
- [ ] Line 772: `test_redefines_and_odo_in_group()` - field_b - add `resolved_renames: None,`

#### copybook-codec/src/json.rs
- [ ] Line 2397: `test_redefines_cluster_validation()` - field_a - add `resolved_renames: None,`
- [ ] Line 2412: `test_redefines_cluster_validation()` - field_b - add `resolved_renames: None,`
- [ ] Line 2427: `test_redefines_cluster_validation()` - field_c - add `resolved_renames: None,`
- [ ] Line 2442: `test_redefines_cluster_validation()` - group_child - add `resolved_renames: None,`
- [ ] Line 2457: `test_redefines_cluster_validation()` - group - add `resolved_renames: None,`

#### copybook-codec/tests/metadata_fingerprint.rs
- [ ] Line 69: `test_field_fingerprint_changed()` - field - add `resolved_renames: None,`

#### copybook-codec/tests/integration_memory_management.rs
- [ ] Line 13: `test_memory_bounded_roundtrip()` - field - add `resolved_renames: None,`

**Validation Command**:
```bash
cargo build --workspace
```

---

### 2. Remove Debug Output (CODE QUALITY)

**Issue**: Heavy eprintln!() statements in resolver

**File**: copybook-core/src/layout.rs

**Function**: `fn resolve_renames_aliases()` (lines 614-735)

**Lines to Remove/Comment**:
- [ ] Line 626: `eprintln!("[RESOLVER] resolve_renames_aliases called with {} fields", fields.len());`
- [ ] Lines 627-630: Entire debug loop printing field info
- [ ] Line 635: `eprintln!("[RESOLVER] Recursing into children of '{}'", field.name);`
- [ ] Lines 654-660: Debug output during RENAMES resolution

**Option 1: Remove entirely** (cleanest)
```rust
// Remove all eprintln! calls
```

**Option 2: Convert to tracing** (better for debugging)
```rust
use tracing::debug;

debug!("[RESOLVER] resolve_renames_aliases called with {} fields", fields.len());
debug!("[RESOLVER]   field[{i}]: level={} name='{}' children={}", f.level, f.name, f.children.len());
// etc.
```

**Validation Command**:
```bash
cargo build --workspace 2>&1 | grep -c eprintln
```
(Should return 0 if removed/converted)

---

### 3. Verify Hierarchy Building (TESTING)

**File**: copybook-core/src/parser.rs

**Issue**: Verify that level-66 fields are placed at correct hierarchy level

**Test Locations**:
- [ ] copybook-core/tests/renames_parser_tests.rs - Add test for hierarchy structure

**What to Test**:
```rust
#[test]
fn test_renames_hierarchy_placement() {
    let copybook = r"
       01 RECORD.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC 9(5).
       66 ALIAS-A RENAMES FIELD-1 THRU FIELD-2.
    ";
    
    let schema = parse_copybook(copybook).unwrap();
    
    // RENAMES should be ROOT level, not nested under RECORD
    let renames_field = schema.fields.iter()
        .find(|f| f.name == "ALIAS-A")
        .expect("RENAMES should be at root level");
    
    assert!(renames_field.level == 66);
    assert!(renames_field.resolved_renames.is_some(), 
            "RENAMES should have resolved_renames populated");
}
```

**Expected Behavior**:
- Level-66 fields should be in `schema.fields` (root level)
- Should NOT be in `Record.children`
- Should have `resolved_renames` populated by layout resolver

---

### 4. Add Resolver Output Verification Tests (TESTING)

**File**: copybook-core/tests/renames_parser_tests.rs

**New Tests to Add**:

```rust
#[test]
fn test_renames_resolved_output() {
    let copybook = r"
       01 RECORD.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC 9(5).
          05 FIELD-3 PIC X(20).
       66 ALIAS-A RENAMES FIELD-1 THRU FIELD-3.
    ";
    
    let schema = parse_copybook(copybook).unwrap();
    let alias = schema.all_fields().iter()
        .find(|f| f.name == "ALIAS-A")
        .unwrap();
    
    assert!(alias.resolved_renames.is_some());
    let resolved = alias.resolved_renames.as_ref().unwrap();
    
    // Validate offset: should start at FIELD-1
    assert_eq!(resolved.offset, 0);
    
    // Validate length: sum of FIELD-1 + FIELD-2 + FIELD-3
    assert_eq!(resolved.length, 10 + 5 + 20);
    
    // Validate members: should list all 3 fields
    assert_eq!(resolved.members.len(), 3);
}

#[test]
fn test_renames_partial_range() {
    let copybook = r"
       01 RECORD.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC 9(5).
          05 FIELD-3 PIC X(20).
       66 PARTIAL RENAMES FIELD-1 THRU FIELD-2.
    ";
    
    let schema = parse_copybook(copybook).unwrap();
    let partial = schema.all_fields().iter()
        .find(|f| f.name == "PARTIAL")
        .unwrap();
    
    let resolved = partial.resolved_renames.as_ref().unwrap();
    assert_eq!(resolved.length, 10 + 5);
    assert_eq!(resolved.members.len(), 2);
}
```

---

### 5. Documentation Updates (OPTIONAL)

**Files to Update**:

#### docs/reference/GRAMMAR_RENAMES.md
- [ ] Add section on resolved_renames field
- [ ] Document resolver algorithm
- [ ] Clarify scope limitations (Slice-2+)

#### CLAUDE.md (Project Instructions)
- [ ] Add note about resolved_renames field
- [ ] Document that level-66 fields should be at ROOT level
- [ ] Update Library API section with example usage

---

## Validation Commands

### Build Check
```bash
cargo build --workspace --all-targets
```

### Test All Renames Tests
```bash
cargo test --package copybook-core --test renames_parser_tests
```

### Test All With Resolved Field
```bash
cargo test --workspace
```

### Check No eprintln
```bash
grep -r "eprintln!" copybook-core/src/layout.rs
# Should return 0 results
```

### Verify Field Struct Changes
```bash
grep -n "resolved_renames: None" copybook-codec/src/roundtrip.rs
# Should show all 6 additions
```

---

## Priority Execution Order

1. **BLOCKING**: Fix all Field struct initializations (section 1)
   - Without this, compilation fails
   
2. **CODE QUALITY**: Remove debug output (section 2)
   - Improves code cleanliness
   
3. **TESTING**: Add resolver verification tests (section 4)
   - Validates the implementation works
   
4. **VERIFICATION**: Check hierarchy building (section 3)
   - Ensures correct structure
   
5. **DOCUMENTATION**: Update docs (section 5)
   - Helps future maintainers

---

## Expected Test Results After Fixes

```
Test Summary:
- renames_parser_tests: 14+ passing (current 11 + new tests)
- All Field struct initializations compile
- No eprintln! statements in resolver
- resolved_renames field populated correctly in all RENAMES fields
- Hierarchy correctly places level-66 at ROOT level
```

---

## References

**Full Exploration Report**: See `RENAMES_EXPLORATION_REPORT.md`

**Key Files**:
- `/home/steven/code/Rust/copybook-rs/copybook-core/src/schema.rs` - Field struct definition
- `/home/steven/code/Rust/copybook-rs/copybook-core/src/layout.rs` - Resolver implementation
- `/home/steven/code/Rust/copybook-rs/copybook-core/tests/renames_parser_tests.rs` - Parser tests
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
