@rdw-corruption
Feature: RDW corruption heuristic
  Scenario: detect an ASCII digit length header
    When the rdw ascii-corruption heuristic evaluates header "\x31\x32\x00\x00"
    Then the rdw ascii-corruption heuristic should report ASCII corruption

  Scenario: detect an ascii-corruption RDW header in detector
    When the rdw corruption detector evaluates header "\x31\x32\x00\x00"
    Then the rdw corruption detector should report corruption

  Scenario: ignore a non-ASCII length header
    When the rdw ascii-corruption heuristic evaluates header "\x00\x05\x00\x00"
    Then the rdw ascii-corruption heuristic should not report ASCII corruption

  Scenario: ignore a clean RDW header in detector
    When the rdw corruption detector evaluates header "\x00\x05\x00\x00"
    Then the rdw corruption detector should not report corruption

  Scenario: detect a corrupted EBCDIC control byte
    When the ebcdic corruption predicate evaluates bytes "\x00"
    Then the ebcdic corruption predicate should detect corruption

  Scenario: ignore a clean EBCDIC control-free byte sequence
    When the ebcdic corruption predicate evaluates bytes "\x41\x42\x43"
    Then the ebcdic corruption predicate should not detect corruption

  Scenario: detect invalid packed-decimal nibbles
    When the packed corruption predicate evaluates bytes "\xA2\x34\x5A"
    Then the packed corruption predicate should detect corruption

  Scenario: ignore valid packed-decimal data
    When the packed corruption predicate evaluates bytes "\x12\x34\x5C"
    Then the packed corruption predicate should not detect corruption

  Scenario: detect corrupted EBCDIC bytes in detector
    When the ebcdic corruption detector evaluates bytes "\x00"
    Then the ebcdic corruption detector should report corruption

  Scenario: ignore clean EBCDIC bytes in detector
    When the ebcdic corruption detector evaluates bytes "\x41\x42\x43"
    Then the ebcdic corruption detector should not report corruption

  Scenario: detect packed-decimal corruption in detector
    When the packed corruption detector evaluates bytes "\xA2\x34\x5A"
    Then the packed corruption detector should report corruption

  Scenario: ignore valid packed-decimal data in detector
    When the packed corruption detector evaluates bytes "\x12\x34\x5C"
    Then the packed corruption detector should not report corruption
