# RENAMES (Level-66) Implementation Exploration Report

## Overview

This report documents a comprehensive exploration of the copybook-rs codebase to understand the RENAMES (level-66 field) implementation and identify areas requiring fixes.

**Current Status**: RENAMES parsing is implemented (Slice-1, commit 1daa2e8), but the resolver and Field construction across test files are incomplete.

---

## 1. Current RENAMES Parsing in copybook-core/src/parser.rs

### Level-66 Parsing Flow

**Location**: Lines 441-449, 553-559, 1048-1124

1. **Token Recognition** (lines 441-449):
```rust
Some(TokenPos {
    token: Token::Level66,
    ..
}) => {
    let level = 66;
    self.advance();
    level
}
```

2. **Field Name Extraction** (lines 515-530):
   - Standard field name extraction after level number
   - Used by `Field::new(level, name)` constructor

3. **RENAMES Clause Parsing** (lines 1048-1124):
   - **Function**: `fn parse_renames(&mut self, field: &mut Field) -> Result<()>`
   - **Syntax Validation**: `66 NAME RENAMES from-field THROUGH|THRU thru-field.`
   - **Field Kind Assignment**: Sets `field.kind = FieldKind::Renames { from_field, thru_field }`
   - **Validation** (lines 553-559): Level-66 fields MUST have RENAMES clause or error

4. **Validation** (line 553-559):
```rust
if level == 66 && !matches!(field.kind, FieldKind::Renames { .. }) {
    return Err(Error::new(
        ErrorCode::CBKP001_SYNTAX,
        format!("Level-66 field '{}' must have RENAMES clause", field.name),
    ));
}
```

### Hierarchy Building Issues

**Location**: `build_hierarchy()` function, lines 122-189

**Current Behavior**:
- Stack-based approach processes all fields by level comparison
- No special handling for level-66 or level-88 fields
- Both level-66 and level-88 fields are treated like storage fields in hierarchy building
- Level-66 and level-88 fields get pushed to the stack and assigned as children normally

**Problem**: 
- Level-66 fields should be placed at **ROOT level ONLY** (siblings to their source record)
- Level-88 fields should be placed as **children of their parent field**, NOT as siblings
- Current code treats them like normal storage fields, causing incorrect hierarchy placement

**Example Issue**:
```cobol
01 RECORD.
   05 FIELD-1 PIC X(10).
   05 FIELD-2 PIC 9(5).
66 ALIAS-A RENAMES FIELD-1 THRU FIELD-2.  <- Should be ROOT level, not child of RECORD
88 STATUS VALUE 'A'.                      <- Should be child of last parent, not ROOT
```

---

## 2. Layout Resolution in copybook-core/src/layout.rs

### resolve_renames_aliases() Function

**Location**: Lines 614-735

**Current Implementation**:

1. **Bottom-up Recursion** (lines 632-638):
   - Recursively processes children first
   - Then resolves RENAMES at each level

2. **Field Resolution** (lines 644-723):
   - **Two-pass approach**:
     - **Pass 1** (lines 644-723): Collect resolution information
     - **Pass 2** (lines 725-732): Apply resolutions

3. **Name Matching** (lines 666-679):
```rust
for (i, sibling) in fields.iter().enumerate() {
    // Skip level-66 and level-88 fields (they don't have storage)
    if sibling.level == 66 || sibling.level == 88 {
        continue;  // <- CORRECT: Skips non-storage fields
    }
    
    // Match by name (not path, since we're looking for simple names)
    if &sibling.name == from_name && from_idx.is_none() {
        from_idx = Some(i);
    }
    if &sibling.name == thru_name {
        thru_idx = Some(i);
    }
}
```

4. **Member Collection** (lines 708-719):
```rust
let offset = fields[from_i].offset;
let end_offset = fields[thru_i].offset + fields[thru_i].len;
let length = end_offset - offset;

for i in from_i..=thru_i {
    // Include storage fields only (skip level 66 and 88)
    if fields[i].level != 66 && fields[i].level != 88 {
        members.push(fields[i].path.clone());
    }
}
```

5. **Resolution Storage** (lines 726-732):
```rust
fields[idx].resolved_renames = Some(ResolvedRenames {
    offset,
    length,
    members,
});
```

### Issues Identified

1. **Debug Output**: Heavy `eprintln!()` statements (lines 626-660) should be removed or converted to proper logging
2. **Same-scope Validation**: The resolver correctly enforces that from/thru fields are siblings, but doesn't validate:
   - That fields are contiguous (may have gaps via FILLER)
   - That from/thru are in the same parent group (implicit via sibling search)

---

## 3. Field Structure Construction Sites

### Schema.rs Field Constructors

**Location**: Lines 362-401

Two helper constructors exist:

```rust
pub fn new(level: u8, name: String) -> Self {
    Self {
        path: name.clone(),
        name,
        level,
        kind: FieldKind::Group,  // Default
        offset: 0,
        len: 0,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,  // <- CORRECTLY SET
        children: Vec::new(),
    }
}

pub fn with_kind(level: u8, name: String, kind: FieldKind) -> Self {
    Self {
        // ... same as above with explicit kind parameter
        resolved_renames: None,  // <- CORRECTLY SET
    }
}
```

### Test Files with Missing `resolved_renames`

Files requiring fixes (all have direct Field struct initialization without `resolved_renames`):

1. **copybook-codec/src/roundtrip.rs** (lines 256, 278, 300, 322, 344, 359)
   - `create_simple_alphanum_schema()`
   - `create_zoned_decimal_schema()`
   - `create_packed_decimal_schema()`
   - `create_binary_int_schema()`
   - `create_redefines_schema()`

2. **copybook-codec/src/odo_redefines.rs** (lines 604, 624, 756, 772)
   - Test field constructions for ODO/REDEFINES scenarios

3. **copybook-codec/src/json.rs** (lines 2397, 2412, 2427, 2442, 2457)
   - Test field constructions in JSON codec tests

4. **copybook-codec/tests/metadata_fingerprint.rs** (line 69)
   - Field construction in fingerprint tests

5. **copybook-codec/tests/integration_memory_management.rs** (line 13)
   - Memory management test field construction

---

## 4. Name Resolution and QNAME Handling

### Current Name Matching Strategy

**Location**: `resolve_renames_aliases()`, lines 672-678

```rust
// Match by name (not path, since we're looking for simple names)
if &sibling.name == from_name && from_idx.is_none() {
    from_idx = Some(i);
}
if &sibling.name == thru_name {
    thru_idx = Some(i);
}
```

**Key Points**:
- Matches by **simple name only** (not qualified path like "PARENT.CHILD")
- Sibling-based search (searches same parent's children)
- **No QNAME support** currently - qualified name resolution not implemented
- RENAMES refers to fields by simple name in source copybook

**Implications**:
- From/thru field names in RENAMES clause are simple names (not paths)
- Need to match by name within the same parent level
- No support for cross-group RENAMES (from field in one group, thru in another)

### QNAME Support Gap

COBOL allows qualified names for disambiguation:
```cobol
66 ALIAS RENAMES PARENT1.FIELD1 THRU PARENT2.FIELD2.
```

**Current Status**: NOT SUPPORTED
- Parser only accepts simple identifiers after RENAMES keyword
- Would need lexer/parser enhancement to support qualified names
- Layout resolver currently assumes same parent

---

## 5. Level-88 Field Handling Comparison

### How Level-88 Fields Are Handled

**Location**: Multiple locations in parser.rs and layout.rs

1. **Parser Recognition** (lines 458-465):
```rust
Some(TokenPos {
    token: Token::Level88,
    ..
}) => {
    let level = 88;
    self.advance();
    level
}
```

2. **VALUE Clause Parsing** (lines 622-625):
```rust
if field.level == 88 {
    self.advance();
    self.parse_level88_value_clause(field)?;
}
```

3. **Hierarchy Building - NO SPECIAL HANDLING**:
   - Level-88 fields are processed like any other field by level comparison
   - They get pushed to stack and become children of parent
   - This is CORRECT because level-88 fields are attached to their parent

4. **Storage Field Validation** (lines 376-396):
```rust
fn is_storage_field(&self, field: &Field) -> bool {
    if field.level == 88 {
        return false;  // <- Level-88 has no storage
    }
    match &field.kind {
        FieldKind::Condition { .. } => false,  // <- Explicitly non-storage
        FieldKind::Renames { .. } => false,    // <- Explicitly non-storage
        // ...
    }
}
```

5. **ODO Tail Validation** (lines 338-340):
```rust
// Check if there are any storage fields after this ODO field
!siblings
    .iter()
    .skip(odo_index + 1)
    .any(|sibling| self.is_storage_field(sibling))
```

### Key Difference from RENAMES

| Aspect | Level-88 | Level-66 |
|--------|----------|---------|
| **Storage** | No (Condition) | No (Alias) |
| **Parent** | Attached to parent field | Should be at ROOT level |
| **Scope** | Applies to parent | Aliases range across parent |
| **Hierarchy** | Child of parent | Sibling to parent group |

---

## 6. FieldKind Enum Coverage

### Current FieldKind Variants

**Location**: schema.rs, lines 68-113

```rust
pub enum FieldKind {
    Alphanum { len: u32 },
    ZonedDecimal { digits: u16, scale: i16, signed: bool },
    BinaryInt { bits: u16, signed: bool },
    PackedDecimal { digits: u16, scale: i16, signed: bool },
    Group,
    Condition { values: Vec<String> },      // Level-88
    Renames { from_field: String, thru_field: String },  // Level-66
}
```

**Coverage**:
- ✅ All variants defined
- ✅ RENAMES variant created with from/thru fields
- ✅ Non-storage indicator (Condition, Renames)

---

## 7. ResolvedRenames Structure

### Definition

**Location**: schema.rs, lines 115-124

```rust
pub struct ResolvedRenames {
    /// Byte offset of the aliased range
    pub offset: u32,
    /// Total byte length of the aliased range
    pub length: u32,
    /// Paths of fields covered by this alias (in document order)
    pub members: Vec<String>,
}
```

### Computation

**Location**: layout.rs, lines 708-721

```rust
let offset = fields[from_i].offset;
let end_offset = fields[thru_i].offset + fields[thru_i].len;
let length = end_offset - offset;

for i in from_i..=thru_i {
    if fields[i].level != 66 && fields[i].level != 88 {
        members.push(fields[i].path.clone());
    }
}
```

### Validation

Checks performed:
1. ✅ from/thru fields exist (within sibling range)
2. ✅ from field comes before thru field in document order
3. ✅ Non-storage fields skipped in member list
4. ❌ Contiguity NOT checked (FILLER fields create gaps)
5. ❌ Cross-group aliases NOT prevented
6. ❌ Circular/recursive aliases NOT prevented

---

## File Reference Summary

### Key Files and Line Numbers

| File | Location | Purpose |
|------|----------|---------|
| **copybook-core/src/schema.rs** | 37-64 | Field struct definition with `resolved_renames` field |
| | 106-112 | FieldKind::Renames variant |
| | 115-124 | ResolvedRenames struct |
| | 362-401 | Field constructors (with_kind, new) |
| **copybook-core/src/parser.rs** | 122-189 | build_hierarchy() - stack-based hierarchy construction |
| | 375-396 | is_storage_field() - includes level-66/88 checks |
| | 441-449 | Level-66 token recognition |
| | 553-559 | Level-66 RENAMES validation |
| | 1048-1124 | parse_renames() - RENAMES syntax parsing |
| **copybook-core/src/layout.rs** | 65-99 | resolve_layout() - calls resolve_renames_aliases |
| | 614-735 | resolve_renames_aliases() - RENAMES resolver |
| **copybook-codec/src/roundtrip.rs** | 256, 278, 300, 322, 344, 359 | Field constructions needing resolved_renames |
| **copybook-codec/src/odo_redefines.rs** | 604, 624, 756, 772 | Field constructions in ODO tests |
| **copybook-codec/src/json.rs** | 2397, 2412, 2427, 2442, 2457 | Field constructions in JSON tests |
| **copybook-codec/tests/metadata_fingerprint.rs** | 69 | Field construction in fingerprint test |
| **copybook-codec/tests/integration_memory_management.rs** | 13 | Memory management test field |

---

## Issues Summary

### 1. **Hierarchy Building** (MEDIUM PRIORITY)
- **Problem**: Level-66 fields placed as children, should be at ROOT level
- **Affected**: `build_hierarchy()` in parser.rs
- **Fix**: Add special case for level-66 to prevent nesting under groups
- **Current**: Level-66 is pushed to stack like any field

### 2. **Field Struct Initialization** (HIGH PRIORITY)
- **Problem**: Direct Field struct initialization missing `resolved_renames: None`
- **Locations**: 5+ files with direct struct constructions
- **Impact**: Compilation will fail if all fields are required
- **Fix**: Add `resolved_renames: None` to all Field { ... } constructions

### 3. **Debug Output** (LOW PRIORITY - CODE QUALITY)
- **Problem**: Heavy eprintln!() statements in resolve_renames_aliases()
- **Location**: layout.rs, lines 626-660
- **Fix**: Remove or convert to proper tracing/debug macros

### 4. **Name Resolution** (MEDIUM PRIORITY - FUTURE)
- **Current**: Simple name matching only
- **Gap**: No QNAME (qualified name) support for cross-group RENAMES
- **Status**: Documented in Slice-2 roadmap

### 5. **Validation Gaps** (MEDIUM PRIORITY - FUTURE)
- Contiguity checking (gaps via FILLER)
- Cross-group prevention
- Circular/recursive alias detection

---

## Recommendations

### Immediate (This PR)
1. ✅ Add `resolved_renames: None` to all direct Field struct constructions
2. ✅ Remove debug eprintln!() from resolve_renames_aliases()
3. ✅ Add test cases for resolved_renames field population

### Short-term (Next PR)
1. ⚠️  Verify level-66 fields are placed at correct hierarchy level
2. ⚠️  Add comprehensive integration tests with actual RENAMES resolution
3. ⚠️  Document resolver algorithm and constraints

### Long-term (Slice-2+)
1. QNAME support for qualified name resolution
2. Additional validation (contiguity, circularity)
3. Codec support for RENAMES projection to output

---

## Test Coverage

Current tests in `copybook-core/tests/renames_parser_tests.rs`:
- ✅ Basic RENAMES with THRU/THROUGH
- ✅ Multiple RENAMES in one copybook
- ✅ RENAMES with different field types
- ✅ Error cases (missing clause, invalid syntax)
- ❌ Hierarchy placement validation
- ❌ Resolver output verification
- ❌ Cross-group RENAMES (error case)

