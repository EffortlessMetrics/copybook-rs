// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_safe_ops::safe_string_char_at as safe_ops_char_at;
use copybook_safe_ops::parse_isize as safe_ops_parse_isize;
use copybook_safe_ops::parse_usize as safe_ops_parse_usize;
use copybook_safe_ops::safe_parse_u16 as safe_ops_parse_u16;
use copybook_safe_ops::safe_write as safe_ops_write;
use copybook_safe_ops::safe_write_str as safe_ops_write_str;

use copybook_safe_text::{safe_string_char_at, parse_isize, parse_usize, safe_parse_u16, safe_write, safe_write_str};

#[test]
fn safe_ops_and_safe_text_contract() {
    assert_eq!(parse_usize("2048", "contract").expect("usize parse"), safe_ops_parse_usize("2048", "contract").expect("usize parse"));
    assert_eq!(parse_isize("-42", "contract").expect("isize parse"), safe_ops_parse_isize("-42", "contract").expect("isize parse"));
    assert_eq!(safe_parse_u16("65535", "contract").expect("u16 parse"), safe_ops_parse_u16("65535", "contract").expect("u16 parse"));

    let text = "xyz";
    assert_eq!(safe_string_char_at(text, 1, "contract").expect("char"), safe_ops_char_at(text, 1, "contract").expect("char"));

    let mut buffer = String::new();
    safe_write(&mut buffer, format_args!("{}{}", "a", "b")).expect("write");
    let mut buffer_ops = String::new();
    safe_ops_write(&mut buffer_ops, format_args!("{}{}", "a", "b")).expect("write");
    assert_eq!(buffer, buffer_ops);

    buffer.clear();
    buffer_ops.clear();
    safe_write_str(&mut buffer, "value").expect("write_str");
    safe_ops_write_str(&mut buffer_ops, "value").expect("write_str");
    assert_eq!(buffer, buffer_ops);
}
