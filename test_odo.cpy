       01  ORDER-RECORD.
           05  ORDER-COUNT         PIC 9(2).
           05  ORDER-ITEMS         OCCURS 1 TO 10 TIMES
                                   DEPENDING ON ORDER-COUNT.
               10  ITEM-CODE       PIC X(5).
               10  QUANTITY        PIC 9(3).
           05  TOTAL-AMOUNT        PIC 9(7)V99.