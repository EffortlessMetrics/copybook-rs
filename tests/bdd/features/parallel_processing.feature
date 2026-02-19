@parallel
Feature: Parallel Processing
  Test multi-threaded decoding behavior

  Background:
    Given a copybook with content:
      """
      01 PARALLEL-RECORD.
         05 RECORD-ID PIC X(10).
      """
    And ASCII codepage

  Scenario: Single-threaded decode
    Given thread count 1
    And multi-record binary data with 10 records
    When the data is decoded with 1 thread(s)
    Then decoding should succeed
    And 10 records should be decoded

  Scenario: Multi-threaded decode with 2 threads
    Given thread count 2
    And multi-record binary data with 10 records
    When the data is decoded with 2 thread(s)
    Then decoding should succeed
    And 10 records should be decoded

  Scenario: Multi-threaded decode with 4 threads
    Given thread count 4
    And multi-record binary data with 20 records
    When the data is decoded with 4 thread(s)
    Then decoding should succeed
    And 20 records should be decoded

  Scenario: Multi-threaded decode with 8 threads
    Given thread count 8
    And multi-record binary data with 100 records
    When the data is decoded with 8 thread(s)
    Then decoding should succeed
    And 100 records should be decoded

  Scenario: Output determinism across thread counts
    Given multi-record binary data with 10 records
    When the data is decoded with 1 thread(s)
    And the output is saved as baseline
    When the data is decoded with 2 thread(s)
    Then the multi-threaded output should match baseline

  Scenario: Single record with multiple threads
    Given thread count 4
    And multi-record binary data with 1 records
    When the data is decoded with 4 thread(s)
    Then decoding should succeed
    And 1 records should be decoded

  Scenario: Empty data with threads
    Given thread count 2
    And binary data: ""
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Thread count of 1 is single-threaded
    Given thread count 1
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Large record count with threads
    Given thread count 4
    And multi-record binary data with 50 records
    When the data is decoded with 4 thread(s)
    Then decoding should succeed
    And 50 records should be decoded

  Scenario: Thread count does not affect output validity
    Given thread count 2
    And multi-record binary data with 5 records
    When the data is decoded with 2 thread(s)
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Determinism with 4 threads
    Given multi-record binary data with 20 records
    When the data is decoded with 1 thread(s)
    And the output is saved as baseline
    When the data is decoded with 4 thread(s)
    Then the multi-threaded output should match baseline

  Scenario: Decode with default thread count
    Given binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
