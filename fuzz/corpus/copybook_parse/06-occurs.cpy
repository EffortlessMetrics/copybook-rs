       01  ARRAY-RECORD.
           05  NUMBERS            PIC 9(5) OCCURS 10 TIMES.
           05  TEXT-FIELDS        PIC X(20) OCCURS 5 TIMES.
           05  NESTED.
               10  OUTER          OCCURS 3 TIMES.
                   15  INNER      OCCURS 4 TIMES.
                       20  ITEM   PIC 9(3).
