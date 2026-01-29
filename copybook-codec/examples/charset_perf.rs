use copybook_codec::charset::utf8_to_ebcdic;
use copybook_codec::options::Codepage;
use std::time::Instant;

fn main() {
    let iterations = 100_000;
    let text = "Hello, World! This is a test string for benchmarking EBCDIC conversion performance. It contains enough characters to be meaningful.";

    // Warmup
    for _ in 0..100 {
        let _ = utf8_to_ebcdic(text, Codepage::CP037);
    }

    let start = Instant::now();
    for _ in 0..iterations {
        let _ = utf8_to_ebcdic(text, Codepage::CP037).unwrap();
    }
    let duration = start.elapsed();

    println!("Time for {} iterations: {:?}", iterations, duration);
    println!("Average time per iteration: {:?}", duration / iterations as u32);
    println!("Throughput: {:.2} MB/s", (text.len() as f64 * iterations as f64) / duration.as_secs_f64() / 1024.0 / 1024.0);
}
