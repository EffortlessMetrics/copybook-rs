#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! JSON field-path traversal helpers.
//!
//! This crate owns tiny, reusable lookup routines used by codec encode validation.
//! It keeps JSON path behavior centralized and testable.

use serde_json::Value;

/// Look up a dot-delimited field path inside nested JSON objects.
///
/// Returns `None` if any path segment is missing, the traversal reaches a
/// non-object before the end of the path, or the path is empty.
#[inline]
#[must_use]
pub fn lookup_value<'a>(value: &'a Value, field_path: &str) -> Option<&'a Value> {
    if field_path.is_empty() {
        return None;
    }

    let mut current = value;
    for segment in field_path.split('.') {
        current = current.as_object()?.get(segment)?;
    }
    Some(current)
}

/// Look up an array at `field_path`.
///
/// Primarily returns arrays found directly at a dot-delimited path. For
/// backward-compatible behavior, if that fails and the root value is an object,
/// this also attempts to look up the leaf segment directly on the root object.
#[inline]
#[must_use]
pub fn lookup_array<'a>(value: &'a Value, field_path: &str) -> Option<&'a Vec<Value>> {
    let leaf = field_path.split('.').next_back().unwrap_or("");
    match lookup_value(value, field_path) {
        Some(Value::Array(array)) => Some(array),
        _ => {
            if let Value::Object(obj) = value {
                obj.get(leaf).and_then(Value::as_array)
            } else {
                None
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn lookup_value_resolves_nested_field() {
        let value = json!({ "header": { "count": 3 } });
        assert_eq!(lookup_value(&value, "header.count"), Some(&json!(3)));
    }

    #[test]
    fn lookup_value_returns_none_for_empty_path() {
        let value = json!({ "a": 1 });
        assert!(lookup_value(&value, "").is_none());
    }

    #[test]
    fn lookup_array_resolves_nested_array() {
        let value = json!({ "root": { "items": [1, 2, 3] } });
        let arr = lookup_array(&value, "root.items").expect("array should resolve");
        assert_eq!(arr.len(), 3);
    }

    #[test]
    fn lookup_array_falls_back_to_leaf_in_root_object() {
        let value = json!({ "items": ["a"], "root": { "other": 1 } });
        let arr = lookup_array(&value, "root.items").expect("leaf fallback should resolve");
        assert_eq!(arr.len(), 1);
    }

    #[test]
    fn lookup_array_returns_none_when_missing() {
        let value = json!({ "root": { "items": 5 } });
        assert!(lookup_array(&value, "root.items").is_none());
    }
}
