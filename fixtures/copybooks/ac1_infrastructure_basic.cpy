      * AC1 Infrastructure Basic - Basic COBOL structures
      * Tests fundamental field types and group structures
       01  INFRASTRUCTURE-RECORD.
           05  RECORD-ID           PIC X(10).
           05  PROCESSING-DATE     PIC 9(8).
           05  BASIC-GROUP.
               10  ITEM-COUNT      PIC 9(4).
               10  STATUS-FLAG     PIC X(1).
               10  AMOUNT-FIELD    PIC S9(7)V99 COMP-3.
           05  ARRAY-GROUP.
               10  SIMPLE-ARRAY OCCURS 10 TIMES.
                   15  ARRAY-ITEM      PIC X(5).
           05  FILLER              PIC X(20).