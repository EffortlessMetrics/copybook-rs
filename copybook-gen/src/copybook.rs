//! Synthetic copybook generation

use crate::GeneratorConfig;
use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;

/// Generate a synthetic COBOL copybook
pub fn generate_synthetic_copybook(config: &GeneratorConfig) -> String {
    let mut rng = StdRng::seed_from_u64(config.seed);
    
    // Placeholder implementation - will be expanded in task 10.2
    let mut copybook = String::new();
    copybook.push_str("      * Generated synthetic copybook\n");
    copybook.push_str("       01  RECORD-ROOT.\n");
    
    // Generate some basic fields
    for i in 1..=5 {
        let field_type = rng.gen_range(0..4);
        match field_type {
            0 => {
                let len = rng.gen_range(1..=20);
                copybook.push_str(&format!("           05  FIELD-{:02}     PIC X({}).\n", i, len));
            }
            1 => {
                let digits = rng.gen_range(1..=9);
                copybook.push_str(&format!("           05  NUM-{:02}       PIC 9({}).\n", i, digits));
            }
            2 => {
                let digits = rng.gen_range(1..=9);
                copybook.push_str(&format!("           05  COMP-{:02}      PIC 9({}) COMP.\n", i, digits));
            }
            3 => {
                let digits = rng.gen_range(1..=9);
                copybook.push_str(&format!("           05  PACKED-{:02}    PIC 9({}) COMP-3.\n", i, digits));
            }
            _ => unreachable!(),
        }
    }
    
    copybook
}