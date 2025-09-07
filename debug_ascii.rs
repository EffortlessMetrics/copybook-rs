fn main() {
    let suspect_data = b"0010HELLO12345";
    println!("Data bytes: {:?}", suspect_data);
    
    let length_bytes = [suspect_data[0], suspect_data[1]];
    let length = u16::from_be_bytes(length_bytes);
    println!("Length from first 2 bytes: {}", length);
    
    let reserved_bytes = [suspect_data[2], suspect_data[3]];
    let reserved = u16::from_be_bytes(reserved_bytes);
    println!("Reserved from bytes 2-3: {}", reserved);
    
    // ASCII detection
    let is_ascii = length_bytes.iter().all(|&b| (0x30..=0x39).contains(&b));
    println!("Is ASCII digits: {}", is_ascii);
    
    println!("Total data length: {}", suspect_data.len());
}
