//! Tests for COMP-1 and COMP-2 floating-point support in copybook-core.
//!
//! Validates lexer tokenization, parser FieldKind creation, and layout sizing.

#[allow(clippy::unwrap_used)]
#[allow(clippy::expect_used)]
mod comp_float_parse {
    use copybook_core::feature_flags::{Feature, FeatureFlags};
    use copybook_core::parser::parse;
    use copybook_core::schema::FieldKind;

    fn enable_comp_flags_for_tests() {
        let mut flags = FeatureFlags::default();
        flags.enable(Feature::Comp1);
        flags.enable(Feature::Comp2);
        copybook_core::feature_flags::FeatureFlags::set_global(flags);
    }

    // =========================================================================
    // Lexer token tests
    // =========================================================================

    #[test]
    fn test_comp1_token_is_lexed() {
        use copybook_core::lexer::{Lexer, Token};

        let input = "COMP-1";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert!(
            tokens.iter().any(|t| t.token == Token::Comp1),
            "COMP-1 should be lexed as Token::Comp1"
        );
    }

    #[test]
    fn test_comp2_token_is_lexed() {
        use copybook_core::lexer::{Lexer, Token};

        let input = "COMP-2";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert!(
            tokens.iter().any(|t| t.token == Token::Comp2),
            "COMP-2 should be lexed as Token::Comp2"
        );
    }

    #[test]
    fn test_computational_1_token_is_lexed() {
        use copybook_core::lexer::{Lexer, Token};

        let input = "COMPUTATIONAL-1";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert!(
            tokens.iter().any(|t| t.token == Token::Comp1),
            "COMPUTATIONAL-1 should be lexed as Token::Comp1"
        );
    }

    #[test]
    fn test_computational_2_token_is_lexed() {
        use copybook_core::lexer::{Lexer, Token};

        let input = "COMPUTATIONAL-2";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert!(
            tokens.iter().any(|t| t.token == Token::Comp2),
            "COMPUTATIONAL-2 should be lexed as Token::Comp2"
        );
    }

    #[test]
    fn test_comp1_case_insensitive() {
        use copybook_core::lexer::{Lexer, Token};

        for variant in &["comp-1", "Comp-1", "COMP-1"] {
            let mut lexer = Lexer::new(variant);
            let tokens = lexer.tokenize();
            assert!(
                tokens.iter().any(|t| t.token == Token::Comp1),
                "'{}' should be lexed as Token::Comp1",
                variant
            );
        }
    }

    // =========================================================================
    // Parser FieldKind tests
    // =========================================================================

    #[test]
    fn test_parser_creates_float_single() {
        enable_comp_flags_for_tests();
        let input = "01 FIELD-A COMP-1.";
        let schema = parse(input).expect("Failed to parse COMP-1 field");
        assert_eq!(schema.fields.len(), 1);
        assert!(
            matches!(schema.fields[0].kind, FieldKind::FloatSingle),
            "Expected FloatSingle, got {:?}",
            schema.fields[0].kind
        );
    }

    #[test]
    fn test_parser_creates_float_double() {
        enable_comp_flags_for_tests();
        let input = "01 FIELD-B COMP-2.";
        let schema = parse(input).expect("Failed to parse COMP-2 field");
        assert_eq!(schema.fields.len(), 1);
        assert!(
            matches!(schema.fields[0].kind, FieldKind::FloatDouble),
            "Expected FloatDouble, got {:?}",
            schema.fields[0].kind
        );
    }

    #[test]
    fn test_parser_usage_comp1() {
        enable_comp_flags_for_tests();
        let input = "01 FIELD-A USAGE COMP-1.";
        let schema = parse(input).expect("Failed to parse USAGE COMP-1 field");
        assert_eq!(schema.fields.len(), 1);
        assert!(matches!(schema.fields[0].kind, FieldKind::FloatSingle));
    }

    #[test]
    fn test_parser_usage_comp2() {
        enable_comp_flags_for_tests();
        let input = "01 FIELD-A USAGE COMP-2.";
        let schema = parse(input).expect("Failed to parse USAGE COMP-2 field");
        assert_eq!(schema.fields.len(), 1);
        assert!(matches!(schema.fields[0].kind, FieldKind::FloatDouble));
    }

    #[test]
    fn test_parser_computational_1() {
        enable_comp_flags_for_tests();
        let input = "01 FIELD-A COMPUTATIONAL-1.";
        let schema = parse(input).expect("Failed to parse COMPUTATIONAL-1 field");
        assert_eq!(schema.fields.len(), 1);
        assert!(matches!(schema.fields[0].kind, FieldKind::FloatSingle));
    }

    #[test]
    fn test_parser_computational_2() {
        enable_comp_flags_for_tests();
        let input = "01 FIELD-B COMPUTATIONAL-2.";
        let schema = parse(input).expect("Failed to parse COMPUTATIONAL-2 field");
        assert_eq!(schema.fields.len(), 1);
        assert!(matches!(schema.fields[0].kind, FieldKind::FloatDouble));
    }

    #[test]
    fn test_comp1_no_pic_required() {
        enable_comp_flags_for_tests();
        // COMP-1/COMP-2 are self-defining, no PIC clause needed
        let input = "01 TEMP-VALUE COMP-1.";
        let schema = parse(input).expect("COMP-1 should not require PIC");
        assert!(matches!(schema.fields[0].kind, FieldKind::FloatSingle));
    }

    #[test]
    fn test_comp1_overrides_pic() {
        enable_comp_flags_for_tests();
        // If PIC is present before COMP-1, the COMP-1 should override it
        let input = "01 TEMP-VALUE PIC 9(5) COMP-1.";
        let schema = parse(input).expect("COMP-1 should override PIC");
        assert!(
            matches!(schema.fields[0].kind, FieldKind::FloatSingle),
            "COMP-1 should override PIC clause, got {:?}",
            schema.fields[0].kind
        );
    }

    // =========================================================================
    // Layout sizing tests
    // =========================================================================

    #[test]
    fn test_float_single_size_is_4_bytes() {
        enable_comp_flags_for_tests();
        let input = "01 FIELD-A COMP-1.";
        let schema = parse(input).expect("Failed to parse COMP-1");
        assert_eq!(schema.fields[0].len, 4, "COMP-1 should be 4 bytes");
    }

    #[test]
    fn test_float_double_size_is_8_bytes() {
        enable_comp_flags_for_tests();
        let input = "01 FIELD-B COMP-2.";
        let schema = parse(input).expect("Failed to parse COMP-2");
        assert_eq!(schema.fields[0].len, 8, "COMP-2 should be 8 bytes");
    }

    #[test]
    fn test_float_fields_in_record() {
        enable_comp_flags_for_tests();
        let input = r"
01 RECORD.
   05 ALPHA-FIELD PIC X(10).
   05 FLOAT-SINGLE COMP-1.
   05 FLOAT-DOUBLE COMP-2.
   05 TRAILER PIC X(5).
";
        let schema = parse(input).expect("Failed to parse record with floats");
        assert_eq!(schema.fields.len(), 1);
        let root = &schema.fields[0];
        assert_eq!(root.children.len(), 4);

        // ALPHA-FIELD at offset 0, len 10
        assert_eq!(root.children[0].offset, 0);
        assert_eq!(root.children[0].len, 10);

        // FLOAT-SINGLE at offset 10, len 4
        assert_eq!(root.children[1].offset, 10);
        assert_eq!(root.children[1].len, 4);
        assert!(matches!(root.children[1].kind, FieldKind::FloatSingle));

        // FLOAT-DOUBLE at offset 14, len 8
        assert_eq!(root.children[2].offset, 14);
        assert_eq!(root.children[2].len, 8);
        assert!(matches!(root.children[2].kind, FieldKind::FloatDouble));

        // TRAILER at offset 22, len 5
        assert_eq!(root.children[3].offset, 22);
        assert_eq!(root.children[3].len, 5);

        // Total record size: 10 + 4 + 8 + 5 = 27
        assert_eq!(schema.lrecl_fixed, Some(27));
    }

    #[test]
    fn test_float_single_synchronized_alignment() {
        enable_comp_flags_for_tests();
        let input = r"
01 RECORD.
   05 ONE-BYTE PIC X(1).
   05 ALIGNED-FLOAT COMP-1 SYNCHRONIZED.
";
        let schema = parse(input).expect("Failed to parse synchronized COMP-1");
        let root = &schema.fields[0];
        // ONE-BYTE at offset 0, len 1
        // ALIGNED-FLOAT should be aligned to 4-byte boundary -> offset 4
        assert_eq!(root.children[1].offset, 4, "COMP-1 SYNC should align to 4");
        assert_eq!(root.children[1].len, 4);
        assert_eq!(
            root.children[1].sync_padding,
            Some(3),
            "Should have 3 bytes of sync padding"
        );
    }

    #[test]
    fn test_float_double_synchronized_alignment() {
        enable_comp_flags_for_tests();
        let input = r"
01 RECORD.
   05 ONE-BYTE PIC X(1).
   05 ALIGNED-DOUBLE COMP-2 SYNCHRONIZED.
";
        let schema = parse(input).expect("Failed to parse synchronized COMP-2");
        let root = &schema.fields[0];
        // ONE-BYTE at offset 0, len 1
        // ALIGNED-DOUBLE should be aligned to 8-byte boundary -> offset 8
        assert_eq!(root.children[1].offset, 8, "COMP-2 SYNC should align to 8");
        assert_eq!(root.children[1].len, 8);
        assert_eq!(
            root.children[1].sync_padding,
            Some(7),
            "Should have 7 bytes of sync padding"
        );
    }

    // =========================================================================
    // Canonical JSON / Fingerprint tests
    // =========================================================================

    #[test]
    fn test_float_single_canonical_json() {
        let input = "01 FIELD-A COMP-1.";
        let schema = parse(input).expect("Failed to parse");
        let canonical = schema.create_canonical_json();
        assert!(
            canonical.contains("FloatSingle"),
            "Canonical JSON should contain FloatSingle"
        );
    }

    #[test]
    fn test_float_double_canonical_json() {
        let input = "01 FIELD-B COMP-2.";
        let schema = parse(input).expect("Failed to parse");
        let canonical = schema.create_canonical_json();
        assert!(
            canonical.contains("FloatDouble"),
            "Canonical JSON should contain FloatDouble"
        );
    }

    #[test]
    fn test_float_field_kind_serialization() {
        // FloatSingle
        let kind = FieldKind::FloatSingle;
        let serialized = serde_json::to_string(&kind).unwrap();
        let deserialized: FieldKind = serde_json::from_str(&serialized).unwrap();
        let re_serialized = serde_json::to_string(&deserialized).unwrap();
        assert_eq!(serialized, re_serialized);

        // FloatDouble
        let kind = FieldKind::FloatDouble;
        let serialized = serde_json::to_string(&kind).unwrap();
        let deserialized: FieldKind = serde_json::from_str(&serialized).unwrap();
        let re_serialized = serde_json::to_string(&deserialized).unwrap();
        assert_eq!(serialized, re_serialized);
    }
}
