use copybook_core::{WorkloadType, parse_copybook};

#[test]
fn classify_display_heavy_schema() {
    let copybook = r#"
01 TEST.
   05 FIELD-A PIC X(5).
   05 FIELD-B PIC 9(3).
   05 PACKED PIC 9(3) COMP-3.
"#;
    let schema = parse_copybook(copybook).expect("parse");
    assert_eq!(schema.workload_type(), WorkloadType::DisplayHeavy);
}

#[test]
fn classify_comp3_heavy_schema() {
    let copybook = r#"
01 TEST.
   05 PACKED-1 PIC 9(3) COMP-3.
   05 PACKED-2 PIC 9(3) COMP-3.
   05 FIELD-A PIC X(5).
"#;
    let schema = parse_copybook(copybook).expect("parse");
    assert_eq!(schema.workload_type(), WorkloadType::Comp3Heavy);
}

#[test]
fn classify_mixed_schema() {
    let copybook = r#"
01 TEST.
   05 FIELD-A PIC X(5).
   05 PACKED PIC 9(3) COMP-3.
"#;
    let schema = parse_copybook(copybook).expect("parse");
    assert_eq!(schema.workload_type(), WorkloadType::Mixed);
}
