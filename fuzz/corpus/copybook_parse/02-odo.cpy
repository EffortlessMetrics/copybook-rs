       01  CUSTOMER-RECORD.
           05  ORDER-COUNT         PIC 9(3).
           05  ORDERS OCCURS 1 TO 100 TIMES
                   DEPENDING ON ORDER-COUNT.
               10  ORDER-ID        PIC 9(8).
               10  ORDER-DATE     PIC 9(8).
               10  ORDER-AMOUNT   PIC S9(7)V99 COMP-3.
