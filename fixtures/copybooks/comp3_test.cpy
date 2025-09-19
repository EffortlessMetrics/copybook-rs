      * COMP-3 encoding test copybook
       01  COMP3-RECORD.
           05  RECORD-ID           PIC 9(4).
           05  POSITIVE-AMOUNT     PIC 9(5) COMP-3.
           05  NEGATIVE-AMOUNT     PIC S9(5) COMP-3.
           05  DECIMAL-AMOUNT      PIC S9(7)V99 COMP-3.
           05  UNSIGNED-AMOUNT     PIC 9(3) COMP-3.
           05  DESCRIPTION         PIC X(20).