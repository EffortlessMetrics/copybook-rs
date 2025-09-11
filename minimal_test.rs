#[cfg(test)]
mod tests {
    use copybook_codec::{decode_file_to_jsonl, Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
    use copybook_core::parse_copybook;
    use std::io::Cursor;

    #[test]
    fn test_raw_minimal() {
        let copybook = r#"
01 SIMPLE-RECORD.
   05 FIELD1 PIC X(8).
"#;
        let schema = parse_copybook(copybook).unwrap();
        let decode_options = DecodeOptions {
            format: RecordFormat::Fixed,
            codepage: Codepage::ASCII,
            json_number_mode: JsonNumberMode::Lossless,
            emit_filler: false,
            emit_meta: false,
            emit_raw: RawMode::Record,
            strict_mode: false,
            max_errors: None,
            on_decode_unmappable: UnmappablePolicy::Error,
            threads: 1,
        };
        
        let test_data = b"HELLO123";
        let input = Cursor::new(test_data);
        let mut output = Vec::new();
        
        decode_file_to_jsonl(&schema, input, &mut output, &decode_options).unwrap();
        let output_str = String::from_utf8(output).unwrap();
        println!("Output: {}", output_str);
        
        let json_record: serde_json::Value = serde_json::from_str(output_str.trim()).unwrap();
        println!("Parsed: {}", serde_json::to_string_pretty(&json_record).unwrap());
        
        assert!(json_record.get("__raw_b64").is_some(), "Raw data should be present");
        assert!(json_record.get("FIELD1").is_some(), "Field should be present");
    }
}
