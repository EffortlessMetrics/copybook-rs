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
