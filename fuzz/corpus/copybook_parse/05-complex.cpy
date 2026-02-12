       01  COMPLEX-RECORD.
           05  HEADER.
               10  RECORD-TYPE    PIC X(2).
               10  RECORD-ID       PIC 9(10).
           05  DATA-SECTION.
               10  FIELD-COUNT    PIC 9(3).
               10  FIELDS OCCURS 1 TO 50 TIMES
                       DEPENDING ON FIELD-COUNT.
                   15  FIELD-NAME  PIC X(20).
                   15  FIELD-VALUE PIC X(30).
           05  TRAILER.
               10  CHECKSUM       PIC 9(8).
               10  FILLER         PIC X(10).
