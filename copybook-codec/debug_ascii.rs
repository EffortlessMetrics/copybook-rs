use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy};
use copybook_core::parse_copybook;
use std::io::Cursor;

fn main() {
    let copybook = "01 SIGNED-FIELD PIC S9(3).";
    let schema = parse_copybook(copybook).unwrap();
    let options = DecodeOptions {
        format: RecordFormat::Fixed,
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

    let test_data = b"12}";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    println!("Result: {:?}", result);
    println!("Output: {:?}", String::from_utf8_lossy(&output));
}
