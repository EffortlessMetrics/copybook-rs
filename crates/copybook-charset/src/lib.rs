// SPDX-License-Identifier: AGPL-3.0-or-later
//! Character set conversion utilities for EBCDIC ↔ UTF-8.
//!
//! Provides bidirectional conversion between five IBM EBCDIC codepages and
//! Unicode/UTF-8 using compile-time lookup tables for maximum throughput.
//!
//! ## Supported Codepages
//!
//! | Codepage | Region |
//! |----------|--------|
//! | CP 037 | US / Canada |
//! | CP 273 | Germany / Austria |
//! | CP 500 | International Latin-1 |
//! | CP 1047 | Latin-1 / Open Systems |
//! | CP 1140 | US / Canada with € |
//!
//! ## Key Functions
//!
//! - [`ebcdic_to_utf8`] — Decode an EBCDIC byte slice to a Rust `String`.
//! - [`utf8_to_ebcdic`] — Encode a UTF-8 string into an EBCDIC byte vector.
//!
//! Both functions accept an [`UnmappablePolicy`] that controls behaviour when a
//! character has no mapping in the target encoding (replace with a substitute
//! character or return an error).

pub use copybook_codepage::{Codepage, UnmappablePolicy, get_zoned_sign_table, space_byte};
use copybook_error::{Error, ErrorCode, Result};
use std::convert::TryFrom;
use tracing::warn;

// ============================================================================
// Charset conversion code (moved from copybook-codec/src/charset.rs)
// ============================================================================

// EBCDIC to Unicode lookup tables for supported code pages
// Each table maps EBCDIC byte values (0-255) to Unicode code points

/// EBCDIC Code Page 037 (US/Canada) to Unicode lookup table
static CP037_TO_UNICODE: [u32; 256] = [
    0x0000, 0x0001, 0x0002, 0x0003, 0x009C, 0x0009, 0x0086, 0x007F, // 00-07
    0x0097, 0x008D, 0x008E, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, // 08-0F
    0x0010, 0x0011, 0x0012, 0x0013, 0x009D, 0x0085, 0x0008, 0x0087, // 10-17
    0x0018, 0x0019, 0x0092, 0x008F, 0x001C, 0x001D, 0x001E, 0x001F, // 18-1F
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x000A, 0x0017, 0x001B, // 20-27
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x0005, 0x0006, 0x0007, // 28-2F
    0x0090, 0x0091, 0x0016, 0x0093, 0x0094, 0x0095, 0x0096, 0x0004, // 30-37
    0x0098, 0x0099, 0x009A, 0x009B, 0x0014, 0x0015, 0x009E, 0x001A, // 38-3F
    0x0020, 0x00A0, 0x00E2, 0x00E4, 0x00E0, 0x00E1, 0x00E3,
    0x00E5, // 40-47 (space, nbsp, â, ä, à, á, ã, å)
    0x00E7, 0x00F1, 0x00A2, 0x002E, 0x003C, 0x0028, 0x002B,
    0x007C, // 48-4F (ç, ñ, ¢, ., <, (, +, |)
    0x0026, 0x00E9, 0x00EA, 0x00EB, 0x00E8, 0x00ED, 0x00EE,
    0x00EF, // 50-57 (&, é, ê, ë, è, í, î, ï)
    0x00EC, 0x00DF, 0x0021, 0x0024, 0x002A, 0x0029, 0x003B,
    0x00AC, // 58-5F (ì, ß, !, $, *, ), ;, ¬)
    0x002D, 0x002F, 0x00C2, 0x00C4, 0x00C0, 0x00C1, 0x00C3,
    0x00C5, // 60-67 (-, /, Â, Ä, À, Á, Ã, Å)
    0x00C7, 0x00D1, 0x00A6, 0x002C, 0x0025, 0x005F, 0x003E,
    0x003F, // 68-6F (Ç, Ñ, ¦, ,, %, _, >, ?)
    0x00F8, 0x00C9, 0x00CA, 0x00CB, 0x00C8, 0x00CD, 0x00CE,
    0x00CF, // 70-77 (ø, É, Ê, Ë, È, Í, Î, Ï)
    0x00CC, 0x0060, 0x003A, 0x0023, 0x0040, 0x0027, 0x003D,
    0x0022, // 78-7F (Ì, `, :, #, @, ', =, ")
    0x00D8, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, // 80-87 (Ø, a-g)
    0x0068, 0x0069, 0x00AB, 0x00BB, 0x00F0, 0x00FD, 0x00FE,
    0x00B1, // 88-8F (h, i, «, », ð, ý, þ, ±)
    0x00B0, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F, 0x0070, // 90-97 (°, j-p)
    0x0071, 0x0072, 0x00AA, 0x00BA, 0x00E6, 0x00B8, 0x00C6,
    0x00A4, // 98-9F (q, r, ª, º, æ, ¸, Æ, ¤)
    0x00B5, 0x007E, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 0x0078, // A0-A7 (µ, ~, s-x)
    0x0079, 0x007A, 0x00A1, 0x00BF, 0x00D0, 0x00DD, 0x00DE,
    0x00AE, // A8-AF (y, z, ¡, ¿, Ð, Ý, Þ, ®)
    0x005E, 0x00A3, 0x00A5, 0x00B7, 0x00A9, 0x00A7, 0x00B6,
    0x00BC, // B0-B7 (^, £, ¥, ·, ©, §, ¶, ¼)
    0x00BD, 0x00BE, 0x005B, 0x005D, 0x00AF, 0x00A8, 0x00B4,
    0x00D7, // B8-BF (½, ¾, [, ], ¯, ¨, ´, ×)
    0x007B, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, // C0-C7 ({, A-G)
    0x0048, 0x0049, 0x00AD, 0x00F4, 0x00F6, 0x00F2, 0x00F3,
    0x00F5, // C8-CF (H, I, ­, ô, ö, ò, ó, õ)
    0x007D, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 0x0050, // D0-D7 (}, J-P)
    0x0051, 0x0052, 0x00B9, 0x00FB, 0x00FC, 0x00F9, 0x00FA,
    0x00FF, // D8-DF (Q, R, ¹, û, ü, ù, ú, ÿ)
    0x005C, 0x00F7, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058, // E0-E7 (\, ÷, S-X)
    0x0059, 0x005A, 0x00B2, 0x00D4, 0x00D6, 0x00D2, 0x00D3,
    0x00D5, // E8-EF (Y, Z, ², Ô, Ö, Ò, Ó, Õ)
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, // F0-F7 (0-7)
    0x0038, 0x0039, 0x00B3, 0x00DB, 0x00DC, 0x00D9, 0x00DA,
    0x009F, // F8-FF (8, 9, ³, Û, Ü, Ù, Ú, control)
];

/// EBCDIC Code Page 273 (Germany/Austria) to Unicode lookup table
static CP273_TO_UNICODE: [u32; 256] = [
    0x0000, 0x0001, 0x0002, 0x0003, 0x009C, 0x0009, 0x0086, 0x007F, // 00-07
    0x0097, 0x008D, 0x008E, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, // 08-0F
    0x0010, 0x0011, 0x0012, 0x0013, 0x009D, 0x0085, 0x0008, 0x0087, // 10-17
    0x0018, 0x0019, 0x0092, 0x008F, 0x001C, 0x001D, 0x001E, 0x001F, // 18-1F
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x000A, 0x0017, 0x001B, // 20-27
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x0005, 0x0006, 0x0007, // 28-2F
    0x0090, 0x0091, 0x0016, 0x0093, 0x0094, 0x0095, 0x0096, 0x0004, // 30-37
    0x0098, 0x0099, 0x009A, 0x009B, 0x0014, 0x0015, 0x009E, 0x001A, // 38-3F
    0x0020, 0x00A0, 0x00E2, 0x007B, 0x00E0, 0x00E1, 0x00E3,
    0x00E5, // 40-47 (space, nbsp, â, {, à, á, ã, å)
    0x00E7, 0x00F1, 0x00C4, 0x002E, 0x003C, 0x0028, 0x002B,
    0x0021, // 48-4F (ç, ñ, Ä, ., <, (, +, !)
    0x0026, 0x00E9, 0x00EA, 0x00EB, 0x00E8, 0x00ED, 0x00EE,
    0x00EF, // 50-57 (&, é, ê, ë, è, í, î, ï)
    0x00EC, 0x00DF, 0x00DC, 0x0024, 0x002A, 0x0029, 0x003B,
    0x005E, // 58-5F (ì, ß, Ü, $, *, ), ;, ^)
    0x002D, 0x002F, 0x00C2, 0x005B, 0x00C0, 0x00C1, 0x00C3,
    0x00C5, // 60-67 (-, /, Â, [, À, Á, Ã, Å)
    0x00C7, 0x00D1, 0x00F6, 0x002C, 0x0025, 0x005F, 0x003E,
    0x003F, // 68-6F (Ç, Ñ, ö, ,, %, _, >, ?)
    0x00F8, 0x00C9, 0x00CA, 0x00CB, 0x00C8, 0x00CD, 0x00CE,
    0x00CF, // 70-77 (ø, É, Ê, Ë, È, Í, Î, Ï)
    0x00CC, 0x0060, 0x003A, 0x0023, 0x00A7, 0x0027, 0x003D,
    0x0022, // 78-7F (Ì, `, :, #, §, ', =, ")
    0x00D8, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, // 80-87 (Ø, a-g)
    0x0068, 0x0069, 0x00AB, 0x00BB, 0x00F0, 0x00FD, 0x00FE,
    0x00B1, // 88-8F (h, i, «, », ð, ý, þ, ±)
    0x00B0, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F, 0x0070, // 90-97 (°, j-p)
    0x0071, 0x0072, 0x00AA, 0x00BA, 0x00E6, 0x00B8, 0x00C6,
    0x00A4, // 98-9F (q, r, ª, º, æ, ¸, Æ, ¤)
    0x00B5, 0x007E, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 0x0078, // A0-A7 (µ, ~, s-x)
    0x0079, 0x007A, 0x00A1, 0x00BF, 0x00D0, 0x00DD, 0x00DE,
    0x00AE, // A8-AF (y, z, ¡, ¿, Ð, Ý, Þ, ®)
    0x00A2, 0x00A3, 0x00A5, 0x00B7, 0x00A9, 0x0040, 0x00B6,
    0x00BC, // B0-B7 (¢, £, ¥, ·, ©, @, ¶, ¼)
    0x00BD, 0x00BE, 0x00AC, 0x007C, 0x00AF, 0x00A8, 0x00B4,
    0x00D7, // B8-BF (½, ¾, ¬, |, ¯, ¨, ´, ×)
    0x00E4, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, // C0-C7 (ä, A-G)
    0x0048, 0x0049, 0x00AD, 0x00F4, 0x00A6, 0x00F2, 0x00F3,
    0x00F5, // C8-CF (H, I, ­, ô, ¦, ò, ó, õ)
    0x00FC, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 0x0050, // D0-D7 (ü, J-P)
    0x0051, 0x0052, 0x00B9, 0x00FB, 0x007D, 0x00F9, 0x00FA,
    0x00FF, // D8-DF (Q, R, ¹, û, }, ù, ú, ÿ)
    0x00D6, 0x00F7, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058, // E0-E7 (Ö, ÷, S-X)
    0x0059, 0x005A, 0x00B2, 0x00D4, 0x005C, 0x00D2, 0x00D3,
    0x00D5, // E8-EF (Y, Z, ², Ô, \, Ò, Ó, Õ)
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, // F0-F7 (0-7)
    0x0038, 0x0039, 0x00B3, 0x00DB, 0x005D, 0x00D9, 0x00DA,
    0x009F, // F8-FF (8, 9, ³, Û, ], Ù, Ú, control)
];

/// EBCDIC Code Page 500 (International) to Unicode lookup table
static CP500_TO_UNICODE: [u32; 256] = [
    0x0000, 0x0001, 0x0002, 0x0003, 0x009C, 0x0009, 0x0086, 0x007F, // 00-07
    0x0097, 0x008D, 0x008E, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, // 08-0F
    0x0010, 0x0011, 0x0012, 0x0013, 0x009D, 0x0085, 0x0008, 0x0087, // 10-17
    0x0018, 0x0019, 0x0092, 0x008F, 0x001C, 0x001D, 0x001E, 0x001F, // 18-1F
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x000A, 0x0017, 0x001B, // 20-27
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x0005, 0x0006, 0x0007, // 28-2F
    0x0090, 0x0091, 0x0016, 0x0093, 0x0094, 0x0095, 0x0096, 0x0004, // 30-37
    0x0098, 0x0099, 0x009A, 0x009B, 0x0014, 0x0015, 0x009E, 0x001A, // 38-3F
    0x0020, 0x00A0, 0x00E2, 0x00E4, 0x00E0, 0x00E1, 0x00E3,
    0x00E5, // 40-47 (space, nbsp, â, ä, à, á, ã, å)
    0x00E7, 0x00F1, 0x005B, 0x002E, 0x003C, 0x0028, 0x002B,
    0x0021, // 48-4F (ç, ñ, [, ., <, (, +, !)
    0x0026, 0x00E9, 0x00EA, 0x00EB, 0x00E8, 0x00ED, 0x00EE,
    0x00EF, // 50-57 (&, é, ê, ë, è, í, î, ï)
    0x00EC, 0x00DF, 0x005D, 0x0024, 0x002A, 0x0029, 0x003B,
    0x005E, // 58-5F (ì, ß, ], $, *, ), ;, ^)
    0x002D, 0x002F, 0x00C2, 0x00C4, 0x00C0, 0x00C1, 0x00C3,
    0x00C5, // 60-67 (-, /, Â, Ä, À, Á, Ã, Å)
    0x00C7, 0x00D1, 0x00A6, 0x002C, 0x0025, 0x005F, 0x003E,
    0x003F, // 68-6F (Ç, Ñ, ¦, ,, %, _, >, ?)
    0x00F8, 0x00C9, 0x00CA, 0x00CB, 0x00C8, 0x00CD, 0x00CE,
    0x00CF, // 70-77 (ø, É, Ê, Ë, È, Í, Î, Ï)
    0x00CC, 0x0060, 0x003A, 0x0023, 0x0040, 0x0027, 0x003D,
    0x0022, // 78-7F (Ì, `, :, #, @, ', =, ")
    0x00D8, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, // 80-87 (Ø, a-g)
    0x0068, 0x0069, 0x00AB, 0x00BB, 0x00F0, 0x00FD, 0x00FE,
    0x00B1, // 88-8F (h, i, «, », ð, ý, þ, ±)
    0x00B0, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F, 0x0070, // 90-97 (°, j-p)
    0x0071, 0x0072, 0x00AA, 0x00BA, 0x00E6, 0x00B8, 0x00C6,
    0x00A4, // 98-9F (q, r, ª, º, æ, ¸, Æ, ¤)
    0x00B5, 0x007E, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 0x0078, // A0-A7 (µ, ~, s-x)
    0x0079, 0x007A, 0x00A1, 0x00BF, 0x00D0, 0x00DD, 0x00DE,
    0x00AE, // A8-AF (y, z, ¡, ¿, Ð, Ý, Þ, ®)
    0x00A2, 0x00A3, 0x00A5, 0x00B7, 0x00A9, 0x00A7, 0x00B6,
    0x00BC, // B0-B7 (¢, £, ¥, ·, ©, §, ¶, ¼)
    0x00BD, 0x00BE, 0x00AC, 0x007C, 0x00AF, 0x00A8, 0x00B4,
    0x00D7, // B8-BF (½, ¾, ¬, |, ¯, ¨, ´, ×)
    0x007B, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, // C0-C7 ({, A-G)
    0x0048, 0x0049, 0x00AD, 0x00F4, 0x00F6, 0x00F2, 0x00F3,
    0x00F5, // C8-CF (H, I, ­, ô, ö, ò, ó, õ)
    0x007D, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 0x0050, // D0-D7 (}, J-P)
    0x0051, 0x0052, 0x00B9, 0x00FB, 0x00FC, 0x00F9, 0x00FA,
    0x00FF, // D8-DF (Q, R, ¹, û, ü, ù, ú, ÿ)
    0x005C, 0x00F7, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058, // E0-E7 (\, ÷, S-X)
    0x0059, 0x005A, 0x00B2, 0x00D4, 0x00D6, 0x00D2, 0x00D3,
    0x00D5, // E8-EF (Y, Z, ², Ô, Ö, Ò, Ó, Õ)
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, // F0-F7 (0-7)
    0x0038, 0x0039, 0x00B3, 0x00DB, 0x00DC, 0x00D9, 0x00DA,
    0x009F, // F8-FF (8, 9, ³, Û, Ü, Ù, Ú, control)
];

/// EBCDIC Code Page 1047 (Open Systems) to Unicode lookup table
static CP1047_TO_UNICODE: [u32; 256] = [
    0x0000, 0x0001, 0x0002, 0x0003, 0x009C, 0x0009, 0x0086, 0x007F, // 00-07
    0x0097, 0x008D, 0x008E, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, // 08-0F
    0x0010, 0x0011, 0x0012, 0x0013, 0x009D, 0x0085, 0x0008, 0x0087, // 10-17
    0x0018, 0x0019, 0x0092, 0x008F, 0x001C, 0x001D, 0x001E, 0x001F, // 18-1F
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x000A, 0x0017, 0x001B, // 20-27
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x0005, 0x0006, 0x0007, // 28-2F
    0x0090, 0x0091, 0x0016, 0x0093, 0x0094, 0x0095, 0x0096, 0x0004, // 30-37
    0x0098, 0x0099, 0x009A, 0x009B, 0x0014, 0x0015, 0x009E, 0x001A, // 38-3F
    0x0020, 0x00A0, 0x00E2, 0x00E4, 0x00E0, 0x00E1, 0x00E3,
    0x00E5, // 40-47 (space, nbsp, â, ä, à, á, ã, å)
    0x00E7, 0x00F1, 0x00A2, 0x002E, 0x003C, 0x0028, 0x002B,
    0x007C, // 48-4F (ç, ñ, ¢, ., <, (, +, |)
    0x0026, 0x00E9, 0x00EA, 0x00EB, 0x00E8, 0x00ED, 0x00EE,
    0x00EF, // 50-57 (&, é, ê, ë, è, í, î, ï)
    0x00EC, 0x00DF, 0x0021, 0x0024, 0x002A, 0x0029, 0x003B,
    0x00AC, // 58-5F (ì, ß, !, $, *, ), ;, ¬)
    0x002D, 0x002F, 0x00C2, 0x00C4, 0x00C0, 0x00C1, 0x00C3,
    0x00C5, // 60-67 (-, /, Â, Ä, À, Á, Ã, Å)
    0x00C7, 0x00D1, 0x00A6, 0x002C, 0x0025, 0x005F, 0x003E,
    0x003F, // 68-6F (Ç, Ñ, ¦, ,, %, _, >, ?)
    0x00F8, 0x00C9, 0x00CA, 0x00CB, 0x00C8, 0x00CD, 0x00CE,
    0x00CF, // 70-77 (ø, É, Ê, Ë, È, Í, Î, Ï)
    0x00CC, 0x0060, 0x003A, 0x0023, 0x0040, 0x0027, 0x003D,
    0x0022, // 78-7F (Ì, `, :, #, @, ', =, ")
    0x00D8, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, // 80-87 (Ø, a-g)
    0x0068, 0x0069, 0x00AB, 0x00BB, 0x00F0, 0x00FD, 0x00FE,
    0x00B1, // 88-8F (h, i, «, », ð, ý, þ, ±)
    0x00B0, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F, 0x0070, // 90-97 (°, j-p)
    0x0071, 0x0072, 0x00AA, 0x00BA, 0x00E6, 0x00B8, 0x00C6,
    0x00A4, // 98-9F (q, r, ª, º, æ, ¸, Æ, ¤)
    0x00B5, 0x007E, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 0x0078, // A0-A7 (µ, ~, s-x)
    0x0079, 0x007A, 0x00A1, 0x00BF, 0x00D0, 0x005B, 0x00DE,
    0x00AE, // A8-AF (y, z, ¡, ¿, Ð, [, Þ, ®)
    0x005E, 0x00A3, 0x00A5, 0x00B7, 0x00A9, 0x00A7, 0x00B6,
    0x00BC, // B0-B7 (^, £, ¥, ·, ©, §, ¶, ¼)
    0x00BD, 0x00BE, 0x00DD, 0x00A8, 0x00AF, 0x005D, 0x00B4,
    0x00D7, // B8-BF (½, ¾, Ý, ¨, ¯, ], ´, ×)
    0x007B, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, // C0-C7 ({, A-G)
    0x0048, 0x0049, 0x00AD, 0x00F4, 0x00F6, 0x00F2, 0x00F3,
    0x00F5, // C8-CF (H, I, ­, ô, ö, ò, ó, õ)
    0x007D, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 0x0050, // D0-D7 (}, J-P)
    0x0051, 0x0052, 0x00B9, 0x00FB, 0x00FC, 0x00F9, 0x00FA,
    0x00FF, // D8-DF (Q, R, ¹, û, ü, ù, ú, ÿ)
    0x005C, 0x00F7, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058, // E0-E7 (\, ÷, S-X)
    0x0059, 0x005A, 0x00B2, 0x00D4, 0x00D6, 0x00D2, 0x00D3,
    0x00D5, // E8-EF (Y, Z, ², Ô, Ö, Ò, Ó, Õ)
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, // F0-F7 (0-7)
    0x0038, 0x0039, 0x00B3, 0x00DB, 0x00DC, 0x00D9, 0x00DA,
    0x009F, // F8-FF (8, 9, ³, Û, Ü, Ù, Ú, control)
];

/// EBCDIC Code Page 1140 (US/Canada with Euro) to Unicode lookup table
static CP1140_TO_UNICODE: [u32; 256] = [
    0x0000, 0x0001, 0x0002, 0x0003, 0x009C, 0x0009, 0x0086, 0x007F, // 00-07
    0x0097, 0x008D, 0x008E, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, // 08-0F
    0x0010, 0x0011, 0x0012, 0x0013, 0x009D, 0x0085, 0x0008, 0x0087, // 10-17
    0x0018, 0x0019, 0x0092, 0x008F, 0x001C, 0x001D, 0x001E, 0x001F, // 18-1F
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x000A, 0x0017, 0x001B, // 20-27
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x0005, 0x0006, 0x0007, // 28-2F
    0x0090, 0x0091, 0x0016, 0x0093, 0x0094, 0x0095, 0x0096, 0x0004, // 30-37
    0x0098, 0x0099, 0x009A, 0x009B, 0x0014, 0x0015, 0x009E, 0x001A, // 38-3F
    0x0020, 0x00A0, 0x00E2, 0x00E4, 0x00E0, 0x00E1, 0x00E3,
    0x00E5, // 40-47 (space, nbsp, â, ä, à, á, ã, å)
    0x00E7, 0x00F1, 0x00A2, 0x002E, 0x003C, 0x0028, 0x002B,
    0x007C, // 48-4F (ç, ñ, ¢, ., <, (, +, |)
    0x0026, 0x00E9, 0x00EA, 0x00EB, 0x00E8, 0x00ED, 0x00EE,
    0x00EF, // 50-57 (&, é, ê, ë, è, í, î, ï)
    0x00EC, 0x00DF, 0x0021, 0x0024, 0x002A, 0x0029, 0x003B,
    0x00AC, // 58-5F (ì, ß, !, $, *, ), ;, ¬)
    0x002D, 0x002F, 0x00C2, 0x00C4, 0x00C0, 0x00C1, 0x00C3,
    0x00C5, // 60-67 (-, /, Â, Ä, À, Á, Ã, Å)
    0x00C7, 0x00D1, 0x00A6, 0x002C, 0x0025, 0x005F, 0x003E,
    0x003F, // 68-6F (Ç, Ñ, ¦, ,, %, _, >, ?)
    0x00F8, 0x00C9, 0x00CA, 0x00CB, 0x00C8, 0x00CD, 0x00CE,
    0x00CF, // 70-77 (ø, É, Ê, Ë, È, Í, Î, Ï)
    0x00CC, 0x0060, 0x003A, 0x0023, 0x0040, 0x0027, 0x003D,
    0x0022, // 78-7F (Ì, `, :, #, @, ', =, ")
    0x00D8, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, // 80-87 (Ø, a-g)
    0x0068, 0x0069, 0x00AB, 0x00BB, 0x00F0, 0x00FD, 0x00FE,
    0x00B1, // 88-8F (h, i, «, », ð, ý, þ, ±)
    0x00B0, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F, 0x0070, // 90-97 (°, j-p)
    0x0071, 0x0072, 0x00AA, 0x00BA, 0x00E6, 0x00B8, 0x00C6,
    0x00A4, // 98-9F (q, r, ª, º, æ, ¸, Æ, ¤)
    0x00B5, 0x007E, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 0x0078, // A0-A7 (µ, ~, s-x)
    0x0079, 0x007A, 0x00A1, 0x00BF, 0x00D0, 0x00DD, 0x00DE,
    0x00AE, // A8-AF (y, z, ¡, ¿, Ð, Ý, Þ, ®)
    0x005E, 0x00A3, 0x00A5, 0x00B7, 0x00A9, 0x00A7, 0x00B6,
    0x00BC, // B0-B7 (^, £, ¥, ·, ©, §, ¶, ¼)
    0x00BD, 0x00BE, 0x005B, 0x005D, 0x00AF, 0x00A8, 0x00B4,
    0x00D7, // B8-BF (½, ¾, [, ], ¯, ¨, ´, ×)
    0x007B, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, // C0-C7 ({, A-G)
    0x0048, 0x0049, 0x00AD, 0x00F4, 0x00F6, 0x00F2, 0x00F3,
    0x00F5, // C8-CF (H, I, ­, ô, ö, ò, ó, õ)
    0x007D, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 0x0050, // D0-D7 (}, J-P)
    0x0051, 0x0052, 0x00B9, 0x00FB, 0x00FC, 0x00F9, 0x00FA,
    0x00FF, // D8-DF (Q, R, ¹, û, ü, ù, ú, ÿ)
    0x005C, 0x00F7, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058, // E0-E7 (\, ÷, S-X)
    0x0059, 0x005A, 0x00B2, 0x00D4, 0x00D6, 0x00D2, 0x00D3,
    0x00D5, // E8-EF (Y, Z, ², Ô, Ö, Ò, Ó, Õ)
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, // F0-F7 (0-7)
    0x0038, 0x0039, 0x00B3, 0x00DB, 0x00DC, 0x00D9, 0x00DA,
    0x20AC, // F8-FF (8, 9, ³, Û, Ü, Ù, Ú, €)
];

/// Get the appropriate lookup table for the given codepage
fn get_ebcdic_table(codepage: Codepage) -> Option<&'static [u32; 256]> {
    match codepage {
        Codepage::ASCII => None,
        Codepage::CP037 => Some(&CP037_TO_UNICODE),
        Codepage::CP273 => Some(&CP273_TO_UNICODE),
        Codepage::CP500 => Some(&CP500_TO_UNICODE),
        Codepage::CP1047 => Some(&CP1047_TO_UNICODE),
        Codepage::CP1140 => Some(&CP1140_TO_UNICODE),
    }
}

/// Convert EBCDIC bytes to UTF-8 string
///
/// # Errors
/// Returns an error if the EBCDIC data contains invalid bytes that cannot be converted.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn ebcdic_to_utf8(data: &[u8], codepage: Codepage, policy: UnmappablePolicy) -> Result<String> {
    // ASCII pass-through mode (transparent 8-bit, not Windows-1252)
    if codepage == Codepage::ASCII {
        return Ok(String::from_utf8_lossy(data).into_owned());
    }

    let table = get_ebcdic_table(codepage).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
            format!("Unsupported codepage: {codepage:?}"),
        )
    })?;

    let mut result = String::with_capacity(data.len());

    for &byte in data {
        let unicode_point = table[byte as usize];

        // Check for unmappable characters (control characters < 0x20 except tab, LF, CR)
        if unicode_point < 0x20
            && unicode_point != 0x09
            && unicode_point != 0x0A
            && unicode_point != 0x0D
        {
            match policy {
                UnmappablePolicy::Error => {
                    return Err(Error::new(
                        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                        format!("Unmappable EBCDIC byte: 0x{byte:02X} -> U+{unicode_point:04X}"),
                    ));
                }
                UnmappablePolicy::Replace => {
                    warn!(
                        "CBKC301_INVALID_EBCDIC_BYTE: Unmappable EBCDIC byte 0x{:02X}, replacing with U+FFFD",
                        byte
                    );
                    result.push('\u{FFFD}'); // Unicode replacement character
                    continue;
                }
                UnmappablePolicy::Skip => {
                    warn!(
                        "CBKC301_INVALID_EBCDIC_BYTE: Unmappable EBCDIC byte 0x{:02X}, skipping",
                        byte
                    );
                    continue;
                }
            }
        }

        // Convert Unicode code point to char
        if let Some(ch) = char::from_u32(unicode_point) {
            result.push(ch);
        } else {
            match policy {
                UnmappablePolicy::Error => {
                    return Err(Error::new(
                        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                        format!("Invalid Unicode code point: U+{unicode_point:04X}"),
                    ));
                }
                UnmappablePolicy::Replace => {
                    warn!(
                        "CBKC301_INVALID_EBCDIC_BYTE: Invalid Unicode code point U+{:04X}, replacing with U+FFFD",
                        unicode_point
                    );
                    result.push('\u{FFFD}');
                }
                UnmappablePolicy::Skip => {
                    warn!(
                        "CBKC301_INVALID_EBCDIC_BYTE: Invalid Unicode code point U+{:04X}, skipping",
                        unicode_point
                    );
                }
            }
        }
    }

    Ok(result)
}

/// Convert UTF-8 string to EBCDIC bytes
///
/// # Errors
/// Returns an error if the UTF-8 text contains characters that cannot be mapped to the target codepage.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn utf8_to_ebcdic(text: &str, codepage: Codepage) -> Result<Vec<u8>> {
    // ASCII pass-through mode (transparent 8-bit, not Windows-1252)
    if codepage == Codepage::ASCII {
        return Ok(text.as_bytes().to_vec());
    }

    let table = get_ebcdic_table(codepage).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
            format!("Unsupported codepage: {codepage:?}"),
        )
    })?;

    // Build reverse lookup table (Unicode -> EBCDIC)
    let mut reverse_table = std::collections::HashMap::new();
    for (ebcdic_index, &unicode_point) in table.iter().enumerate() {
        if let Some(ch) = char::from_u32(unicode_point) {
            let ebcdic_byte = u8::try_from(ebcdic_index).map_err(|_| {
                Error::new(
                    ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                    format!("EBCDIC byte index {ebcdic_index} exceeds u8 range"),
                )
            })?;
            reverse_table.insert(ch, ebcdic_byte);
        }
    }

    let mut result = Vec::with_capacity(text.len());

    for ch in text.chars() {
        if let Some(&ebcdic_byte) = reverse_table.get(&ch) {
            result.push(ebcdic_byte);
        } else {
            return Err(Error::new(
                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                format!("Character '{ch}' cannot be mapped to {codepage:?}"),
            ));
        }
    }

    Ok(result)
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;
    use copybook_error::ErrorCode;

    #[test]
    fn test_space_byte_ascii() {
        assert_eq!(space_byte(Codepage::ASCII), 0x20);
    }

    #[test]
    fn test_space_byte_ebcdic() {
        // All EBCDIC codepages use 0x40 for space
        assert_eq!(space_byte(Codepage::CP037), 0x40);
        assert_eq!(space_byte(Codepage::CP273), 0x40);
        assert_eq!(space_byte(Codepage::CP500), 0x40);
        assert_eq!(space_byte(Codepage::CP1047), 0x40);
        assert_eq!(space_byte(Codepage::CP1140), 0x40);
    }

    #[test]
    fn test_codepage_is_ascii() {
        assert!(Codepage::ASCII.is_ascii());
        assert!(!Codepage::CP037.is_ascii());
    }

    #[test]
    fn test_codepage_is_ebcdic() {
        assert!(!Codepage::ASCII.is_ebcdic());
        assert!(Codepage::CP037.is_ebcdic());
    }

    #[test]
    fn test_codepage_code_page_number() {
        assert_eq!(Codepage::ASCII.code_page_number(), None);
        assert_eq!(Codepage::CP037.code_page_number(), Some(37));
        assert_eq!(Codepage::CP1140.code_page_number(), Some(1140));
    }

    // --- ebcdic_to_utf8 tests ---

    #[test]
    fn test_ebcdic_to_utf8_empty_input() {
        let result = ebcdic_to_utf8(&[], Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "");
    }

    #[test]
    fn test_ebcdic_to_utf8_ascii_passthrough() {
        let data = b"Hello, World!";
        let result = ebcdic_to_utf8(data, Codepage::ASCII, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "Hello, World!");
    }

    #[test]
    fn test_ebcdic_to_utf8_ascii_passthrough_non_utf8() {
        // Non-UTF8 bytes get replaced with U+FFFD via from_utf8_lossy
        let data: &[u8] = &[0xFF, 0xFE];
        let result = ebcdic_to_utf8(data, Codepage::ASCII, UnmappablePolicy::Error).unwrap();
        assert!(result.contains('\u{FFFD}'));
    }

    #[test]
    fn test_ebcdic_to_utf8_cp037_space() {
        // EBCDIC 0x40 = space in CP037
        let data: &[u8] = &[0x40];
        let result = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, " ");
    }

    #[test]
    fn test_ebcdic_to_utf8_cp037_digits() {
        // EBCDIC 0xF0-0xF9 = digits 0-9 in CP037
        let data: Vec<u8> = (0xF0..=0xF9).collect();
        let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "0123456789");
    }

    #[test]
    fn test_ebcdic_to_utf8_cp037_uppercase() {
        // EBCDIC 0xC1-0xC9 = A-I in CP037
        let data: &[u8] = &[0xC1, 0xC2, 0xC3];
        let result = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "ABC");
    }

    #[test]
    fn test_ebcdic_to_utf8_cp037_lowercase() {
        // EBCDIC 0x81-0x89 = a-i in CP037
        let data: &[u8] = &[0x81, 0x82, 0x83];
        let result = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "abc");
    }

    #[test]
    fn test_ebcdic_to_utf8_unmappable_error_policy() {
        // EBCDIC 0x00 maps to U+0000 (NUL) which is < 0x20 and not tab/LF/CR
        let data: &[u8] = &[0x00];
        let err = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Error).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
    }

    #[test]
    fn test_ebcdic_to_utf8_unmappable_replace_policy() {
        let data: &[u8] = &[0x00];
        let result = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
        assert_eq!(result, "\u{FFFD}");
    }

    #[test]
    fn test_ebcdic_to_utf8_unmappable_skip_policy() {
        let data: &[u8] = &[0x00];
        let result = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Skip).unwrap();
        assert_eq!(result, "");
    }

    #[test]
    fn test_ebcdic_to_utf8_mixed_valid_and_unmappable_skip() {
        // 0x00 (unmappable), 0xC1 (A), 0x00 (unmappable), 0xC2 (B)
        let data: &[u8] = &[0x00, 0xC1, 0x00, 0xC2];
        let result = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Skip).unwrap();
        assert_eq!(result, "AB");
    }

    #[test]
    fn test_ebcdic_to_utf8_all_codepages_digits() {
        // Digits 0xF0-0xF9 should map to 0-9 on all EBCDIC codepages
        let data: Vec<u8> = (0xF0..=0xF9).collect();
        for cp in [
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
        ] {
            let result = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(result, "0123456789", "Failed for {cp:?}");
        }
    }

    #[test]
    fn test_ebcdic_to_utf8_cp1140_euro_sign() {
        // CP1140 maps 0x9F to € (U+20AC) — unlike CP037 which maps it to a control char
        let data: &[u8] = &[0xFF];
        let result = ebcdic_to_utf8(data, Codepage::CP1140, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "€");
    }

    #[test]
    fn test_ebcdic_to_utf8_cp037_tab_allowed() {
        // EBCDIC 0x05 -> U+0009 (tab) — should be allowed (not unmappable)
        let data: &[u8] = &[0x05];
        let result = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "\t");
    }

    #[test]
    fn test_ebcdic_to_utf8_cp037_lf_allowed() {
        // EBCDIC 0x25 -> U+000A (LF) — should be allowed
        let data: &[u8] = &[0x25];
        let result = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "\n");
    }

    #[test]
    fn test_ebcdic_to_utf8_cp037_cr_allowed() {
        // EBCDIC 0x0D -> U+000D (CR) — should be allowed
        let data: &[u8] = &[0x0D];
        let result = ebcdic_to_utf8(data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "\r");
    }

    // --- utf8_to_ebcdic tests ---

    #[test]
    fn test_utf8_to_ebcdic_empty_input() {
        let result = utf8_to_ebcdic("", Codepage::CP037).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn test_utf8_to_ebcdic_ascii_passthrough() {
        let result = utf8_to_ebcdic("Hello", Codepage::ASCII).unwrap();
        assert_eq!(result, b"Hello");
    }

    #[test]
    fn test_utf8_to_ebcdic_cp037_space() {
        let result = utf8_to_ebcdic(" ", Codepage::CP037).unwrap();
        assert_eq!(result, &[0x40]);
    }

    #[test]
    fn test_utf8_to_ebcdic_cp037_digits() {
        let result = utf8_to_ebcdic("0123456789", Codepage::CP037).unwrap();
        let expected: Vec<u8> = (0xF0..=0xF9).collect();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_utf8_to_ebcdic_cp037_uppercase() {
        let result = utf8_to_ebcdic("ABC", Codepage::CP037).unwrap();
        assert_eq!(result, &[0xC1, 0xC2, 0xC3]);
    }

    #[test]
    fn test_utf8_to_ebcdic_unmappable_character() {
        // Chinese character cannot be mapped to CP037
        let err = utf8_to_ebcdic("日", Codepage::CP037).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
    }

    #[test]
    fn test_ebcdic_roundtrip_cp037() {
        let original = "Hello World 123";
        let ebcdic = utf8_to_ebcdic(original, Codepage::CP037).unwrap();
        let roundtrip = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(roundtrip, original);
    }

    #[test]
    fn test_ebcdic_roundtrip_cp500() {
        let original = "Test 789";
        let ebcdic = utf8_to_ebcdic(original, Codepage::CP500).unwrap();
        let roundtrip = ebcdic_to_utf8(&ebcdic, Codepage::CP500, UnmappablePolicy::Error).unwrap();
        assert_eq!(roundtrip, original);
    }

    #[test]
    fn test_ebcdic_roundtrip_cp1047() {
        let original = "COBOL DATA";
        let ebcdic = utf8_to_ebcdic(original, Codepage::CP1047).unwrap();
        let roundtrip = ebcdic_to_utf8(&ebcdic, Codepage::CP1047, UnmappablePolicy::Error).unwrap();
        assert_eq!(roundtrip, original);
    }

    // ====================================================================
    // Exhaustive charset conversion tests
    // ====================================================================

    /// All EBCDIC codepages under test.
    const ALL_EBCDIC: [Codepage; 5] = [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];

    // --- 1. Full printable ASCII range (both directions) per codepage ---

    #[test]
    fn test_printable_ascii_roundtrip_cp037() {
        roundtrip_printable_ascii(Codepage::CP037);
    }

    #[test]
    fn test_printable_ascii_roundtrip_cp273() {
        roundtrip_printable_ascii(Codepage::CP273);
    }

    #[test]
    fn test_printable_ascii_roundtrip_cp500() {
        roundtrip_printable_ascii(Codepage::CP500);
    }

    #[test]
    fn test_printable_ascii_roundtrip_cp1047() {
        roundtrip_printable_ascii(Codepage::CP1047);
    }

    #[test]
    fn test_printable_ascii_roundtrip_cp1140() {
        roundtrip_printable_ascii(Codepage::CP1140);
    }

    /// Encode every printable ASCII char (0x20..=0x7E) to EBCDIC then back,
    /// asserting perfect round-trip for the given codepage.
    fn roundtrip_printable_ascii(cp: Codepage) {
        let printable: String = (0x20u8..=0x7Eu8).map(|b| b as char).collect();
        let ebcdic =
            utf8_to_ebcdic(&printable, cp).unwrap_or_else(|e| panic!("{cp:?} encode failed: {e}"));
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("{cp:?} decode failed: {e}"));
        assert_eq!(back, printable, "Round-trip mismatch for {cp:?}");
    }

    // --- 2. Special characters ---

    #[test]
    fn test_cp1140_euro_sign_roundtrip() {
        // CP1140 byte 0xFF maps to U+20AC (€)
        let decoded = ebcdic_to_utf8(&[0xFF], Codepage::CP1140, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "€");
        let encoded = utf8_to_ebcdic("€", Codepage::CP1140).unwrap();
        assert_eq!(encoded, &[0xFF]);
    }

    #[test]
    fn test_cp037_currency_sign_at_9f() {
        // CP037 0x9F maps to U+00A4 (¤) – the international currency sign
        let decoded = ebcdic_to_utf8(&[0x9F], Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "¤");
    }

    #[test]
    fn test_cp273_national_chars() {
        // CP273 has German national characters at different positions than CP037
        // 0x4A -> Ä (U+00C4), 0x6A -> ö (U+00F6), 0xC0 -> ä (U+00E4)
        let data: &[u8] = &[0x4A, 0x6A, 0xC0];
        let decoded = ebcdic_to_utf8(data, Codepage::CP273, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "Äöä");
        // Round-trip
        let encoded = utf8_to_ebcdic("Äöä", Codepage::CP273).unwrap();
        assert_eq!(encoded, data);
    }

    #[test]
    fn test_cp1140_vs_cp037_difference() {
        // CP1140 is identical to CP037 except at byte 0x9F:
        //   CP037  0x9F -> U+00A4 (¤)
        //   CP1140 byte 0x9F -> U+00A4 (¤) as well, but 0xFF differs:
        //   CP037  0xFF -> U+009F (control)
        //   CP1140 0xFF -> U+20AC (€)
        let cp037_ff = ebcdic_to_utf8(&[0xFF], Codepage::CP037, UnmappablePolicy::Replace).unwrap();
        let cp1140_ff = ebcdic_to_utf8(&[0xFF], Codepage::CP1140, UnmappablePolicy::Error).unwrap();
        assert_ne!(cp037_ff, cp1140_ff, "CP037 and CP1140 must differ at 0xFF");
        assert_eq!(cp1140_ff, "€");
    }

    // --- 3. Control characters ---

    #[test]
    fn test_control_chars_error_policy_all_codepages() {
        // EBCDIC 0x00 maps to U+0000 (NUL) on all codepages – a control char
        for cp in ALL_EBCDIC {
            let err = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Error).unwrap_err();
            assert_eq!(
                err.code,
                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                "Expected error for NUL on {cp:?}"
            );
        }
    }

    #[test]
    fn test_control_chars_replace_policy_all_codepages() {
        for cp in ALL_EBCDIC {
            let result = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Replace).unwrap();
            assert_eq!(result, "\u{FFFD}", "Replace policy failed for {cp:?}");
        }
    }

    #[test]
    fn test_control_chars_skip_policy_all_codepages() {
        for cp in ALL_EBCDIC {
            let result = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Skip).unwrap();
            assert_eq!(result, "", "Skip policy failed for {cp:?}");
        }
    }

    #[test]
    fn test_allowed_control_chars_tab_lf_cr_all_codepages() {
        // Tab (0x05), LF (0x25), CR (0x0D) should pass through on all EBCDIC codepages
        for cp in ALL_EBCDIC {
            let tab = ebcdic_to_utf8(&[0x05], cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(tab, "\t", "Tab failed for {cp:?}");

            let lf = ebcdic_to_utf8(&[0x25], cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(lf, "\n", "LF failed for {cp:?}");

            let cr = ebcdic_to_utf8(&[0x0D], cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(cr, "\r", "CR failed for {cp:?}");
        }
    }

    // --- 4. Unmappable character handling (utf8_to_ebcdic direction) ---

    #[test]
    fn test_utf8_to_ebcdic_unmappable_cjk_all_codepages() {
        // CJK character '日' cannot be mapped to any EBCDIC codepage
        for cp in ALL_EBCDIC {
            let err = utf8_to_ebcdic("日", cp).unwrap_err();
            assert_eq!(
                err.code,
                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                "Expected unmappable error for {cp:?}"
            );
        }
    }

    #[test]
    fn test_utf8_to_ebcdic_emoji_unmappable() {
        let err = utf8_to_ebcdic("😀", Codepage::CP037).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
    }

    // --- 5. Empty input ---

    #[test]
    fn test_empty_input_all_codepages_both_directions() {
        for cp in ALL_EBCDIC {
            let decoded = ebcdic_to_utf8(&[], cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(decoded, "", "Empty decode failed for {cp:?}");

            let encoded = utf8_to_ebcdic("", cp).unwrap();
            assert!(encoded.is_empty(), "Empty encode failed for {cp:?}");
        }
        // Also ASCII
        let decoded = ebcdic_to_utf8(&[], Codepage::ASCII, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "");
        let encoded = utf8_to_ebcdic("", Codepage::ASCII).unwrap();
        assert!(encoded.is_empty());
    }

    // --- 6. Full round-trip consistency (EBCDIC -> UTF-8 -> EBCDIC) per codepage ---

    #[test]
    fn test_full_byte_roundtrip_cp037() {
        full_byte_roundtrip(Codepage::CP037);
    }

    #[test]
    fn test_full_byte_roundtrip_cp273() {
        full_byte_roundtrip(Codepage::CP273);
    }

    #[test]
    fn test_full_byte_roundtrip_cp500() {
        full_byte_roundtrip(Codepage::CP500);
    }

    #[test]
    fn test_full_byte_roundtrip_cp1047() {
        full_byte_roundtrip(Codepage::CP1047);
    }

    #[test]
    fn test_full_byte_roundtrip_cp1140() {
        full_byte_roundtrip(Codepage::CP1140);
    }

    /// For every EBCDIC byte 0x00..=0xFF that decodes to a non-control Unicode
    /// character, verify EBCDIC → UTF-8 → EBCDIC produces the original byte.
    fn full_byte_roundtrip(cp: Codepage) {
        for byte in 0x00u8..=0xFF {
            let Ok(decoded) = ebcdic_to_utf8(&[byte], cp, UnmappablePolicy::Skip) else {
                continue;
            };
            if decoded.is_empty() {
                // Skipped control char – that's fine
                continue;
            }
            let Ok(re_encoded) = utf8_to_ebcdic(&decoded, cp) else {
                continue;
            };
            assert_eq!(
                re_encoded,
                &[byte],
                "{cp:?}: byte 0x{byte:02X} decoded to {decoded:?} but re-encoded to {re_encoded:?}"
            );
        }
    }

    // --- 7. Large buffer conversion ---

    #[test]
    fn test_large_buffer_decode_cp037() {
        // 10 000 EBCDIC spaces (0x40) should decode to 10 000 ASCII spaces
        let large_input = vec![0x40u8; 10_000];
        let result =
            ebcdic_to_utf8(&large_input, Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(result.len(), 10_000);
        assert!(result.chars().all(|c| c == ' '));
    }

    #[test]
    fn test_large_buffer_encode_cp037() {
        let large_text: String = std::iter::repeat_n('A', 10_000).collect();
        let encoded = utf8_to_ebcdic(&large_text, Codepage::CP037).unwrap();
        assert_eq!(encoded.len(), 10_000);
        assert!(encoded.iter().all(|&b| b == 0xC1)); // 'A' in CP037
    }

    #[test]
    fn test_large_buffer_roundtrip_all_codepages() {
        let pattern = "HELLO WORLD 12345 ";
        let large_text: String = pattern.repeat(500); // ~9 000 chars
        for cp in ALL_EBCDIC {
            let encoded = utf8_to_ebcdic(&large_text, cp)
                .unwrap_or_else(|e| panic!("{cp:?} large encode failed: {e}"));
            let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error)
                .unwrap_or_else(|e| panic!("{cp:?} large decode failed: {e}"));
            assert_eq!(decoded, large_text, "Large roundtrip failed for {cp:?}");
        }
    }

    // --- 8. Mixed content with unmappable bytes ---

    #[test]
    fn test_mixed_valid_and_control_replace_all_codepages() {
        // Byte sequence: NUL, 'A' (0xC1 on CP037/500/1047/1140, 0xC1 on CP273), NUL
        for cp in ALL_EBCDIC {
            let data: &[u8] = &[0x00, 0xC1, 0x00];
            let result = ebcdic_to_utf8(data, cp, UnmappablePolicy::Replace).unwrap();
            // Should have replacement chars around the letter
            assert_eq!(
                result.matches('\u{FFFD}').count(),
                2,
                "Replace count wrong for {cp:?}"
            );
            assert!(result.contains('A'), "Missing 'A' for {cp:?}");
        }
    }

    #[test]
    fn test_mixed_valid_and_control_skip_preserves_valid() {
        // NUL, space (0x40), digit-1 (0xF1), NUL
        for cp in ALL_EBCDIC {
            let data: &[u8] = &[0x00, 0x40, 0xF1, 0x00];
            let result = ebcdic_to_utf8(data, cp, UnmappablePolicy::Skip).unwrap();
            assert_eq!(result, " 1", "Skip mixed content wrong for {cp:?}");
        }
    }

    // --- 9. Codepage-specific letter position differences ---

    #[test]
    fn test_uppercase_letters_all_codepages() {
        // Verify A-I, J-R, S-Z positions are correct per codepage
        let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        for cp in ALL_EBCDIC {
            let encoded =
                utf8_to_ebcdic(alpha, cp).unwrap_or_else(|e| panic!("{cp:?} alpha encode: {e}"));
            let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error)
                .unwrap_or_else(|e| panic!("{cp:?} alpha decode: {e}"));
            assert_eq!(decoded, alpha, "Alphabet roundtrip failed for {cp:?}");
            // EBCDIC letters live in C1-C9 (A-I), D1-D9 (J-R), E2-E9 (S-Z)
            assert_eq!(encoded[0], 0xC1, "{cp:?}: 'A' should be 0xC1");
            assert_eq!(encoded[9], 0xD1, "{cp:?}: 'J' should be 0xD1");
            assert_eq!(encoded[18], 0xE2, "{cp:?}: 'S' should be 0xE2");
        }
    }

    #[test]
    fn test_lowercase_letters_all_codepages() {
        let alpha = "abcdefghijklmnopqrstuvwxyz";
        for cp in ALL_EBCDIC {
            let encoded =
                utf8_to_ebcdic(alpha, cp).unwrap_or_else(|e| panic!("{cp:?} lower encode: {e}"));
            let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error)
                .unwrap_or_else(|e| panic!("{cp:?} lower decode: {e}"));
            assert_eq!(decoded, alpha, "Lowercase roundtrip failed for {cp:?}");
            // EBCDIC lowercase: 81-89 (a-i), 91-99 (j-r), A2-A9 (s-z)
            assert_eq!(encoded[0], 0x81, "{cp:?}: 'a' should be 0x81");
            assert_eq!(encoded[9], 0x91, "{cp:?}: 'j' should be 0x91");
            assert_eq!(encoded[18], 0xA2, "{cp:?}: 's' should be 0xA2");
        }
    }

    #[test]
    fn test_digits_all_codepages() {
        let digits = "0123456789";
        for cp in ALL_EBCDIC {
            let encoded =
                utf8_to_ebcdic(digits, cp).unwrap_or_else(|e| panic!("{cp:?} digit encode: {e}"));
            assert_eq!(encoded.len(), 10);
            // Digits are always F0-F9 on all EBCDIC codepages
            for (i, &b) in encoded.iter().enumerate() {
                assert_eq!(
                    b,
                    0xF0 + u8::try_from(i).unwrap(),
                    "{cp:?}: digit {i} wrong"
                );
            }
        }
    }
}
