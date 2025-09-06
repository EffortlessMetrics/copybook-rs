#[cfg(test)]
mod tests {
    use crate::parse_copybook;

    #[test]
    fn test_simple_odo() {
        let input = "01 TEST-FIELD OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(5).";
        let result = parse_copybook(input);
        println!("Result: {:?}", result);
        assert!(result.is_ok());
    }
}
