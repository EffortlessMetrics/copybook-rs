//! Comprehensive schema tests for copybook-rs
//!
//! This test suite validates schema structures, field types, and
//! schema-related functionality.

#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]

use copybook_core::schema::ResolvedRenames;
use copybook_core::{
    Field, FieldKind, Occurs, Schema, SignPlacement, SignSeparateInfo, TailODO, parse_copybook,
};
use serde_json::json;

#[test]
fn test_schema_new() {
    let schema = Schema::new();

    assert!(schema.fields.is_empty());
    assert!(schema.lrecl_fixed.is_none());
    assert!(schema.tail_odo.is_none());
    assert!(schema.fingerprint.is_empty());
}

#[test]
fn test_schema_from_fields() {
    let fields = vec![Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    }];

    let schema = Schema::from_fields(fields.clone());
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "FIELD1");
    assert!(!schema.fingerprint.is_empty());
}

#[test]
fn test_schema_calculate_fingerprint() {
    let mut schema = Schema::new();
    schema.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    schema.calculate_fingerprint();
    assert!(!schema.fingerprint.is_empty());
    assert_eq!(schema.fingerprint.len(), 64); // SHA-256 hex string
}

#[test]
fn test_schema_find_field() {
    let mut schema = Schema::new();
    schema.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    let field = schema.find_field("ROOT.FIELD1");
    assert!(field.is_some());
    assert_eq!(field.unwrap().name, "FIELD1");
}

#[test]
fn test_schema_find_field_not_found() {
    let schema = Schema::new();
    let field = schema.find_field("NONEXISTENT");
    assert!(field.is_none());
}

#[test]
fn test_schema_find_field_nested() {
    let mut schema = Schema::new();
    let child_field = Field {
        path: "ROOT.GROUP.CHILD".to_string(),
        name: "CHILD".to_string(),
        level: 10,
        kind: FieldKind::Alphanum { len: 5 },
        offset: 0,
        len: 5,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    };

    let group_field = Field {
        path: "ROOT.GROUP".to_string(),
        name: "GROUP".to_string(),
        level: 5,
        kind: FieldKind::Group,
        offset: 0,
        len: 5,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![child_field],
    };

    schema.fields.push(group_field);

    let field = schema.find_field("ROOT.GROUP.CHILD");
    assert!(field.is_some());
    assert_eq!(field.unwrap().name, "CHILD");
}

#[test]
fn test_schema_find_field_or_alias() {
    let mut schema = Schema::new();
    schema.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    // Test direct field lookup
    let field = schema.find_field_or_alias("ROOT.FIELD1");
    assert!(field.is_some());
    assert_eq!(field.unwrap().name, "FIELD1");

    // Test lookup by name
    let field = schema.find_field_or_alias("FIELD1");
    assert!(field.is_some());
    assert_eq!(field.unwrap().name, "FIELD1");
}

#[test]
fn test_schema_find_field_or_alias_not_found() {
    let schema = Schema::new();
    let field = schema.find_field_or_alias("NONEXISTENT");
    assert!(field.is_none());
}

#[test]
fn test_schema_create_canonical_json() {
    let mut schema = Schema::new();
    schema.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    let canonical = schema.create_canonical_json();
    assert!(canonical.contains("fields"));
    assert!(canonical.contains("FIELD1"));
}

#[test]
fn test_schema_create_canonical_json_with_lrecl() {
    let mut schema = Schema::new();
    schema.lrecl_fixed = Some(100);
    schema.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    let canonical = schema.create_canonical_json();
    assert!(canonical.contains("lrecl_fixed"));
    assert!(canonical.contains("100"));
}

#[test]
fn test_schema_create_canonical_json_with_tail_odo() {
    let mut schema = Schema::new();
    schema.tail_odo = Some(TailODO {
        counter_path: "ROOT.COUNTER".to_string(),
        min_count: 1,
        max_count: 10,
        array_path: "ROOT.ARRAY".to_string(),
    });
    schema.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    let canonical = schema.create_canonical_json();
    assert!(canonical.contains("tail_odo"));
    assert!(canonical.contains("counter_path"));
    assert!(canonical.contains("array_path"));
}

#[test]
fn test_field_kind_alphanum() {
    let kind = FieldKind::Alphanum { len: 10 };
    assert!(matches!(kind, FieldKind::Alphanum { len: 10 }));
}

#[test]
fn test_field_kind_zoned_decimal() {
    let kind = FieldKind::ZonedDecimal {
        digits: 5,
        scale: 2,
        signed: true,
        sign_separate: None,
    };
    assert!(matches!(kind, FieldKind::ZonedDecimal { digits: 5, .. }));
}

#[test]
fn test_field_kind_zoned_decimal_with_sign_separate() {
    let kind = FieldKind::ZonedDecimal {
        digits: 5,
        scale: 2,
        signed: true,
        sign_separate: Some(SignSeparateInfo {
            placement: SignPlacement::Leading,
        }),
    };
    assert!(matches!(
        kind,
        FieldKind::ZonedDecimal {
            sign_separate: Some(_),
            ..
        }
    ));
}

#[test]
fn test_field_kind_binary_int() {
    let kind = FieldKind::BinaryInt {
        bits: 32,
        signed: true,
    };
    assert!(matches!(kind, FieldKind::BinaryInt { bits: 32, .. }));
}

#[test]
fn test_field_kind_packed_decimal() {
    let kind = FieldKind::PackedDecimal {
        digits: 7,
        scale: 2,
        signed: true,
    };
    assert!(matches!(kind, FieldKind::PackedDecimal { digits: 7, .. }));
}

#[test]
fn test_field_kind_group() {
    let kind = FieldKind::Group;
    assert!(matches!(kind, FieldKind::Group));
}

#[test]
fn test_field_kind_condition() {
    let kind = FieldKind::Condition {
        values: vec!["A".to_string(), "B".to_string()],
    };
    assert!(matches!(kind, FieldKind::Condition { .. }));
}

#[test]
fn test_field_kind_renames() {
    let kind = FieldKind::Renames {
        from_field: "FIELD1".to_string(),
        thru_field: "FIELD5".to_string(),
    };
    assert!(matches!(kind, FieldKind::Renames { .. }));
}

#[test]
fn test_field_kind_edited_numeric() {
    let kind = FieldKind::EditedNumeric {
        pic_string: "ZZ,ZZ9.99".to_string(),
        width: 10,
        scale: 2,
        signed: false,
    };
    assert!(matches!(kind, FieldKind::EditedNumeric { .. }));
}

#[test]
fn test_occurs_fixed() {
    let occurs = Occurs::Fixed { count: 10 };
    assert!(matches!(occurs, Occurs::Fixed { count: 10 }));
}

#[test]
fn test_occurs_odo() {
    let occurs = Occurs::ODO {
        min: 1,
        max: 10,
        counter_path: "ROOT.COUNTER".to_string(),
    };
    assert!(matches!(
        occurs,
        Occurs::ODO {
            min: 1,
            max: 10,
            ..
        }
    ));
}

#[test]
fn test_sign_placement_leading() {
    let placement = SignPlacement::Leading;
    assert!(matches!(placement, SignPlacement::Leading));
}

#[test]
fn test_sign_placement_trailing() {
    let placement = SignPlacement::Trailing;
    assert!(matches!(placement, SignPlacement::Trailing));
}

#[test]
fn test_sign_separate_info() {
    let info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    assert!(matches!(info.placement, SignPlacement::Leading));
}

#[test]
fn test_resolved_renames() {
    let renames = ResolvedRenames {
        offset: 0,
        length: 20,
        members: vec!["ROOT.FIELD1".to_string(), "ROOT.FIELD2".to_string()],
    };
    assert_eq!(renames.offset, 0);
    assert_eq!(renames.length, 20);
    assert_eq!(renames.members.len(), 2);
}

#[test]
fn test_tail_odo() {
    let tail_odo = TailODO {
        counter_path: "ROOT.COUNTER".to_string(),
        min_count: 1,
        max_count: 10,
        array_path: "ROOT.ARRAY".to_string(),
    };
    assert_eq!(tail_odo.counter_path, "ROOT.COUNTER");
    assert_eq!(tail_odo.min_count, 1);
    assert_eq!(tail_odo.max_count, 10);
    assert_eq!(tail_odo.array_path, "ROOT.ARRAY");
}

#[test]
fn test_schema_serialization() {
    let mut schema = Schema::new();
    schema.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    let json = serde_json::to_value(&schema);
    assert!(json.is_ok());

    let value = json.unwrap();
    assert!(value.is_object());
    assert!(value["fields"].is_array());
}

#[test]
fn test_schema_deserialization() {
    let json = json!({
        "fields": [{
            "path": "ROOT.FIELD1",
            "name": "FIELD1",
            "level": 5,
            "kind": "Alphanum(10)",
            "offset": 0,
            "len": 10,
            "redefines_of": null,
            "occurs": null,
            "sync_padding": null,
            "synchronized": false,
            "blank_when_zero": false,
            "resolved_renames": null,
            "children": []
        }],
        "lrecl_fixed": null,
        "tail_odo": null,
        "fingerprint": ""
    });

    let schema: Result<Schema, _> = serde_json::from_value(json);
    assert!(schema.is_ok());
}

#[test]
fn test_field_with_redefines() {
    let field = Field {
        path: "ROOT.FIELD2".to_string(),
        name: "FIELD2".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: Some("ROOT.FIELD1".to_string()),
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    };

    assert_eq!(field.redefines_of, Some("ROOT.FIELD1".to_string()));
}

#[test]
fn test_field_with_occurs() {
    let field = Field {
        path: "ROOT.ARRAY".to_string(),
        name: "ARRAY".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 5 },
        offset: 0,
        len: 5,
        redefines_of: None,
        occurs: Some(Occurs::Fixed { count: 10 }),
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    };

    assert!(matches!(field.occurs, Some(Occurs::Fixed { count: 10 })));
}

#[test]
fn test_field_with_sync_padding() {
    let field = Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: Some(2),
        synchronized: true,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    };

    assert_eq!(field.sync_padding, Some(2));
    assert!(field.synchronized);
}

#[test]
fn test_field_with_blank_when_zero() {
    let field = Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::ZonedDecimal {
            digits: 5,
            scale: 0,
            signed: true,
            sign_separate: None,
        },
        offset: 0,
        len: 5,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: true,
        resolved_renames: None,
        children: vec![],
    };

    assert!(field.blank_when_zero);
}

#[test]
fn test_field_with_children() {
    let child = Field {
        path: "ROOT.GROUP.CHILD".to_string(),
        name: "CHILD".to_string(),
        level: 10,
        kind: FieldKind::Alphanum { len: 5 },
        offset: 0,
        len: 5,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    };

    let parent = Field {
        path: "ROOT.GROUP".to_string(),
        name: "GROUP".to_string(),
        level: 5,
        kind: FieldKind::Group,
        offset: 0,
        len: 5,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![child],
    };

    assert_eq!(parent.children.len(), 1);
    assert_eq!(parent.children[0].name, "CHILD");
}

#[test]
fn test_schema_multiple_fields() {
    let mut schema = Schema::new();
    for i in 0..5 {
        schema.fields.push(Field {
            path: format!("ROOT.FIELD{}", i),
            name: format!("FIELD{}", i),
            level: 5,
            kind: FieldKind::Alphanum { len: 10 },
            offset: (i * 10) as u32,
            len: 10,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            resolved_renames: None,
            children: vec![],
        });
    }

    assert_eq!(schema.fields.len(), 5);
    assert_eq!(schema.fields[0].name, "FIELD0");
    assert_eq!(schema.fields[4].name, "FIELD4");
}

#[test]
fn test_schema_with_parsed_copybook() {
    let copybook = "01 RECORD.\n   05 FIELD1 PIC X(10).\n   05 FIELD2 PIC 9(5).";
    let schema = parse_copybook(copybook).unwrap();

    assert!(!schema.fields.is_empty());
    assert!(schema.fields.len() >= 2);
}

#[test]
fn test_field_kind_serialization() {
    let kinds = vec![
        FieldKind::Alphanum { len: 10 },
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
            sign_separate: None,
        },
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 2,
            signed: true,
        },
        FieldKind::Group,
    ];

    for kind in kinds {
        let json = serde_json::to_value(&kind);
        assert!(json.is_ok());
    }
}

#[test]
fn test_field_with_resolved_renames() {
    let field = Field {
        path: "ROOT.ALIAS".to_string(),
        name: "ALIAS".to_string(),
        level: 66,
        kind: FieldKind::Renames {
            from_field: "ROOT.FIELD1".to_string(),
            thru_field: "ROOT.FIELD5".to_string(),
        },
        offset: 0,
        len: 20,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: Some(ResolvedRenames {
            offset: 0,
            length: 20,
            members: vec!["ROOT.FIELD1".to_string(), "ROOT.FIELD2".to_string()],
        }),
        children: vec![],
    };

    assert!(field.resolved_renames.is_some());
    let renames = field.resolved_renames.as_ref().unwrap();
    assert_eq!(renames.members.len(), 2);
}

#[test]
fn test_schema_fingerprint_stability() {
    let mut schema1 = Schema::new();
    schema1.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    let mut schema2 = Schema::new();
    schema2.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    schema1.calculate_fingerprint();
    schema2.calculate_fingerprint();

    assert_eq!(schema1.fingerprint, schema2.fingerprint);
}

#[test]
fn test_schema_fingerprint_uniqueness() {
    let mut schema1 = Schema::new();
    schema1.fields.push(Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    let mut schema2 = Schema::new();
    schema2.fields.push(Field {
        path: "ROOT.FIELD2".to_string(),
        name: "FIELD2".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    });

    schema1.calculate_fingerprint();
    schema2.calculate_fingerprint();

    assert_ne!(schema1.fingerprint, schema2.fingerprint);
}

#[test]
fn test_schema_deeply_nested_fields() {
    let mut schema = Schema::new();
    let level3 = Field {
        path: "ROOT.LEVEL1.LEVEL2.LEVEL3".to_string(),
        name: "LEVEL3".to_string(),
        level: 15,
        kind: FieldKind::Alphanum { len: 5 },
        offset: 0,
        len: 5,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    };

    let level2 = Field {
        path: "ROOT.LEVEL1.LEVEL2".to_string(),
        name: "LEVEL2".to_string(),
        level: 10,
        kind: FieldKind::Group,
        offset: 0,
        len: 5,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![level3],
    };

    let level1 = Field {
        path: "ROOT.LEVEL1".to_string(),
        name: "LEVEL1".to_string(),
        level: 5,
        kind: FieldKind::Group,
        offset: 0,
        len: 5,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![level2],
    };

    schema.fields.push(level1);

    let field = schema.find_field("ROOT.LEVEL1.LEVEL2.LEVEL3");
    assert!(field.is_some());
    assert_eq!(field.unwrap().name, "LEVEL3");
}

#[test]
fn test_occurs_odo_serialization() {
    let occurs = Occurs::ODO {
        min: 1,
        max: 10,
        counter_path: "ROOT.COUNTER".to_string(),
    };

    let json = serde_json::to_value(&occurs);
    assert!(json.is_ok());

    let value = json.unwrap();
    assert!(value.is_object());
}

#[test]
fn test_field_with_all_attributes() {
    let field = Field {
        path: "ROOT.FIELD1".to_string(),
        name: "FIELD1".to_string(),
        level: 5,
        kind: FieldKind::ZonedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
            sign_separate: Some(SignSeparateInfo {
                placement: SignPlacement::Trailing,
            }),
        },
        offset: 0,
        len: 5,
        redefines_of: Some("ROOT.OTHER".to_string()),
        occurs: Some(Occurs::Fixed { count: 10 }),
        sync_padding: Some(1),
        synchronized: true,
        blank_when_zero: true,
        resolved_renames: None,
        children: vec![],
    };

    assert_eq!(field.level, 5);
    assert_eq!(field.offset, 0);
    assert_eq!(field.len, 5);
    assert!(field.redefines_of.is_some());
    assert!(field.occurs.is_some());
    assert!(field.sync_padding.is_some());
    assert!(field.synchronized);
    assert!(field.blank_when_zero);
}

#[test]
fn test_schema_empty_fields() {
    let schema = Schema::new();
    assert_eq!(schema.fields.len(), 0);
    assert!(schema.find_field("ANYTHING").is_none());
}
