// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests that keep `copybook-corruption-rdw` aligned with
//! the public façade in `copybook-corruption`.

use copybook_corruption::detect_rdw_ascii_corruption as detect_via_facade;
use copybook_corruption_rdw::detect_rdw_ascii_corruption;

#[test]
fn facade_and_microcrate_agree_on_detection() {
    let cases: &[&[u8]] = &[
        &[b'1', b'2', 0x00, 0x00],
        &[0x00, 0x50, 0x00, 0x00],
        &[0x00, 0x50, b'A', b'B'],
        &[0x00, 0x00],
    ];

    for data in cases {
        let facade = detect_via_facade(data).is_some();
        let micro = detect_rdw_ascii_corruption(data).is_some();
        assert_eq!(
            facade, micro,
            "facade and microcrate should agree for {data:?}",
        );
    }
}
