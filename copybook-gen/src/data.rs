//! Synthetic data generation

use copybook_core::Schema;
use crate::GeneratorConfig;
use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;

/// Generate synthetic binary data for a schema
pub fn generate_synthetic_data(schema: &Schema, config: &GeneratorConfig) -> Vec<Vec<u8>> {
    let mut rng = StdRng::seed_from_u64(config.seed);
    let mut records = Vec::new();
    
    // Placeholder implementation - will be expanded in task 10.2
    for _ in 0..config.record_count {
        let record_len = schema.lrecl_fixed.unwrap_or(100) as usize;
        let mut record = vec![0x40; record_len]; // EBCDIC spaces
        
        // Fill with some random data
        for byte in &mut record {
            if rng.gen_bool(0.3) {
                *byte = rng.gen_range(0xF0..=0xF9); // EBCDIC digits
            }
        }
        
        records.push(record);
    }
    
    records
}