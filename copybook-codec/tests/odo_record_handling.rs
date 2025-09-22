use copybook_codec::{Codepage, DecodeOptions, decode_record};
use copybook_core::{ErrorCode, parse_copybook};
use serde_json;

const ODO_COPYBOOK: &str = r#"
       01 RECORD.
          05 ARRAY-COUNT PIC 9(1).
          05 DYNAMIC-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON ARRAY-COUNT.
             10 ELEMENT PIC X(10).
"#;

fn create_ebcdic_record(count: u8, data: &[u8]) -> Vec<u8> {
    let mut record = Vec::new();

    // Convert decimal digit to EBCDIC zoned numeric (CP037)
    let ebcdic_digit = match count {
        0 => 0xF0,
        1 => 0xF1,
        2 => 0xF2,
        3 => 0xF3,
        4 => 0xF4,
        5 => 0xF5,
        9 => 0xF9,
        _ => 0xF0,
    };

    record.push(ebcdic_digit);
    record.extend_from_slice(data);
    record
}

#[test]
fn test_odo_array_valid_count() {
    let schema = parse_copybook(ODO_COPYBOOK).unwrap();
    let valid_record = create_ebcdic_record(
        3,
        &[
            // Element 1: 10 bytes
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
            // Element 2: 10 bytes
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
            // Element 3: 10 bytes
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
        ],
    );

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    match decode_record(&schema, &valid_record, &options) {
        Ok(json_value) => {
            println!(
                "Successfully decoded record: {}",
                serde_json::to_string_pretty(&json_value).unwrap()
            );
        }
        Err(e) => panic!("Unexpected decode error: {:?} - {}", e.code, e.to_string()),
    }
}

#[test]
fn test_odo_array_count_clipped() {
    let schema = parse_copybook(ODO_COPYBOOK).unwrap();
    let clipped_record = create_ebcdic_record(
        5,
        &[
            // Only provide data for 1 element, but count says 5 (insufficient data)
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
        ],
    );

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    match decode_record(&schema, &clipped_record, &options) {
        Ok(json_value) => {
            println!(
                "Unexpectedly decoded record: {}",
                serde_json::to_string_pretty(&json_value).unwrap()
            );
            panic!("Expected error for clipped record, but got success");
        }
        Err(e) => {
            println!("Clipped record error code: {:?}", e.code);
            println!("Clipped record error message: {}", e.to_string());
            assert_eq!(
                e.code,
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                "Wrong error code for insufficient data"
            );
        }
    }
}

#[test]
fn test_odo_array_record_too_short() {
    let schema = parse_copybook(ODO_COPYBOOK).unwrap();
    let too_short_record = vec![0xF3]; // Just count, no data

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    match decode_record(&schema, &too_short_record, &options) {
        Ok(json_value) => {
            println!(
                "Unexpectedly decoded record: {}",
                serde_json::to_string_pretty(&json_value).unwrap()
            );
            panic!("Expected error for too short record, but got success");
        }
        Err(e) => {
            println!("Too short record error code: {:?}", e.code);
            println!("Too short record error message: {}", e.to_string());
            assert_eq!(
                e.code,
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                "Wrong error code for too short record"
            );
        }
    }
}

#[test]
fn test_odo_array_count_raised() {
    let schema = parse_copybook(ODO_COPYBOOK).unwrap();
    let raised_count_record = create_ebcdic_record(
        9,
        &[
            // Provide some data (but count 9 exceeds max 5)
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
        ],
    );

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    match decode_record(&schema, &raised_count_record, &options) {
        Ok(json_value) => {
            println!(
                "Unexpectedly decoded record: {}",
                serde_json::to_string_pretty(&json_value).unwrap()
            );
            panic!("Expected error for raised ODO count, but got success");
        }
        Err(e) => {
            println!("Raised count record error code: {:?}", e.code);
            println!("Raised count record error message: {}", e.to_string());
            assert_eq!(
                e.code,
                ErrorCode::CBKS301_ODO_CLIPPED,
                "Wrong error code for ODO count exceeding maximum"
            );
        }
    }
}

#[test]
fn test_odo_array_count_below_min() {
    let schema = parse_copybook(ODO_COPYBOOK).unwrap();
    let below_min_record = create_ebcdic_record(
        0,
        &[
            // Provide some data (but count 0 is below min 1)
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0,
        ],
    );

    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    match decode_record(&schema, &below_min_record, &options) {
        Ok(json_value) => {
            println!(
                "Unexpectedly decoded record: {}",
                serde_json::to_string_pretty(&json_value).unwrap()
            );
            panic!("Expected error for ODO count below minimum, but got success");
        }
        Err(e) => {
            println!("Below min count record error code: {:?}", e.code);
            println!("Below min count record error message: {}", e.to_string());
            assert_eq!(
                e.code,
                ErrorCode::CBKS302_ODO_RAISED,
                "Wrong error code for ODO count below minimum"
            );
        }
    }
}
