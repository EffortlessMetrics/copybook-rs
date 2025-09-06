// Simple test to verify the formatting logic
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmallDecimal {
    pub value: i64,
    pub scale: i16,
    pub negative: bool,
}

impl SmallDecimal {
    pub fn new(value: i64, scale: i16, negative: bool) -> Self {
        Self { value, scale, negative }
    }
    
    pub fn to_fixed_scale_string_with_width(&self, scale: i16, min_width: u16) -> String {
        let mut result = String::new();
        
        if self.negative && self.value != 0 {
            result.push('-');
        }

        if scale <= 0 {
            let scaled_value = if scale < 0 {
                self.value * 10_i64.pow((-scale) as u32)
            } else {
                self.value
            };
            
            // For scale=0 (integers), preserve the original field width with leading zeros
            if scale == 0 {
                write!(result, "{:0width$}", scaled_value, width = min_width as usize).unwrap();
            } else {
                write!(result, "{scaled_value}").unwrap();
            }
        } else {
            let divisor = 10_i64.pow(scale as u32);
            let integer_part = self.value / divisor;
            let fractional_part = (self.value % divisor).abs();
            write!(result, "{integer_part}.{:0width$}", fractional_part, width = scale as usize).unwrap();
        }

        result
    }
}

fn main() {
    // Test case: value=3, scale=0, digits=2 (like PIC 9(2))
    let decimal = SmallDecimal::new(3, 0, false);
    let formatted = decimal.to_fixed_scale_string_with_width(0, 2);
    println!("Formatted: '{}' (expected: '03')", formatted);
    
    // Test case: value=123, scale=0, digits=3
    let decimal2 = SmallDecimal::new(123, 0, false);
    let formatted2 = decimal2.to_fixed_scale_string_with_width(0, 3);
    println!("Formatted: '{}' (expected: '123')", formatted2);
}
