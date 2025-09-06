#[cfg(test)]
mod temp_test {
    use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
    use copybook_core::parse_copybook;
    use std::io::Cursor;

    #[test]
    fn debug_odo_output() {
        let copybook = r#"
01 ODO-RDW-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

        let schema = parse_copybook(copybook).unwrap();
        let options = DecodeOptions {
            format: RecordFormat::RDW,
            codepage: Codepage::ASCII,
            json_number_mode: JsonNumberMode::Lossless,
            emit_filler: false,
            emit_meta: false,
            emit_raw: RawMode::Off,
            strict_mode: false,
            max_errors: None,
            on_decode_unmappable: UnmappablePolicy::Error,
            threads: 1,
        };

        let rdw_odo_data = b"\x00\x0B\x00\x0003ABCDEFGHI";
        let input = Cursor::new(rdw_odo_data);
        let mut output = Vec::new();

        let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
        assert!(result.is_ok());

        let output_str = String::from_utf8(output).unwrap();
        println!("JSON Output: {}", output_str);
    }
}
