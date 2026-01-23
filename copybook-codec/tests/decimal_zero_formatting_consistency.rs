
#[cfg(test)]
mod tests {
    use copybook_codec::numeric::SmallDecimal;

    #[test]
    fn test_zero_formatting_consistency() {
        // Create a zero decimal with scale 2
        let decimal = SmallDecimal::new(0, 2, false);

        // standard to_string()
        let s1 = decimal.to_string();

        // format_to_scratch_buffer (now wrapper around append_to_string)
        let mut scratch = String::new();
        decimal.format_to_scratch_buffer(2, &mut scratch);

        assert_eq!(s1, scratch, "Formatting should be consistent");
        assert_eq!(s1, "0", "Should be normalized to 0");
    }
}
