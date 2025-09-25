      * Test OCCURS syntax
       01  TEST-RECORD.
           05  ITEM-COUNT      PIC 9(3).
           05  ITEMS OCCURS 10 TIMES.
               10  ITEM-CODE   PIC X(4).