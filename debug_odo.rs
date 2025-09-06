fn main() {
    let rdw_odo_data = b"\x00\x0B\x00\x0003ABCDEFGHI";
    println!("RDW data bytes: {:?}", rdw_odo_data);
    
    let length = u16::from_be_bytes([rdw_odo_data[0], rdw_odo_data[1]]);
    println!("RDW length: {}", length);
    
    let payload = &rdw_odo_data[4..];
    println!("Payload bytes: {:?}", payload);
    println!("Payload as string: {:?}", std::str::from_utf8(payload));
    
    let counter_bytes = &payload[0..2];
    println!("Counter bytes: {:?}", counter_bytes);
    println!("Counter as string: {:?}", std::str::from_utf8(counter_bytes));
}
