@safe-ops
Feature: Safe operations contract
  Scenario: parse a valid unsigned integer
    Given the safe-op input is "2048"
    When safe_ops parses the input as usize
    Then the parse result should be 2048

  Scenario: parse an invalid unsigned integer
    Given the safe-op input is "not-a-number"
    When safe_ops parses the input as usize
    Then safe_ops should report a syntax error

  Scenario: compute checked array bounds
    When safe_array_bound is called with base 10, count 3, and item size 4
    Then safe_array_bound should return 22

  Scenario: handle divide-by-zero without panic
    When safe_divide is called with numerator 10 and denominator 0
    Then safe_ops should report a syntax error

  Scenario: parse a valid signed integer via safe-text
    Given the safe-op input is "-42"
    When safe_text parses the input as isize
    Then the safe-text isize parse result should be -42

  Scenario: parse a valid u16 via safe-text
    Given the safe-op input is "65535"
    When safe_text parses the input as u16
    Then the safe-text u16 parse result should be 65535

  Scenario: read a string character in safe-text
    Given the safe-op input is "abc"
    When safe_text gets character at index 1
    Then the safe-text character should be "b"

  Scenario: safe-text reports char out-of-range error
    Given the safe-op input is "abc"
    When safe_text gets character at index 10
    Then safe_ops should report a syntax error

  Scenario: read an in-range index via safe-index
    When safe_index gets element at index 1
    Then safe_index should return 20

  Scenario: report error for out-of-range safe-index access
    When safe_index gets element at index 99
    Then safe_ops should report a syntax error
