
# [Task]: Incomplete Test Assertion in `test_write_rdw_record_with_padding`

**Issue Description**

The test function `test_write_rdw_record_with_padding` in `copybook-codec/src/record.rs` contains an assertion with a placeholder value: `assert_eq!(record.payload, b"testxxxx");`. The `xxxx` suggests that the test is incomplete and does not properly validate the padding behavior.

An incomplete test can pass even if the underlying logic is broken, providing a false sense of security.

**File and Location:**

`copybook-codec/src/record.rs:1150`

**Code Context:**

```rust
// copybook-codec/src/record.rs

    #[test]
    fn test_write_rdw_record_with_padding() {
        let mut output = Vec::new();
        let format = RecordFormat::RDW { lrecl: 8 };
        write_record(&mut output, b"test", format).unwrap();

        let mut reader = RdwRecordReader::new(Cursor::new(output), false);
        let record = reader.read_record().unwrap().unwrap();

        assert_eq!(record.lrecl, 8);
        assert_eq!(record.payload.len(), 8);
        assert_eq!(record.payload, b"testxxxx"); // FIXME: Incomplete assertion
    }
```

**Proposed Fix**

The test should be completed to assert the correct padding bytes. Assuming the padding byte is a space (`b' '`), the assertion should be updated.

```rust
// copybook-codec/src/record.rs

    #[test]
    fn test_write_rdw_record_with_padding() {
        let mut output = Vec::new();
        let format = RecordFormat::RDW { lrecl: 8 };
        write_record(&mut output, b"test", format).unwrap();

        let mut reader = RdwRecordReader::new(Cursor::new(output), false);
        let record = reader.read_record().unwrap().unwrap();

        assert_eq!(record.lrecl, 8);
        assert_eq!(record.payload.len(), 8);
        // Assert that the payload is the original content plus space padding
        assert_eq!(record.payload, b"test    ");
    }
```

Additionally, the padding character should be documented or made configurable if it isn't already. If the padding character is not a space, the test should be updated with the correct character.
