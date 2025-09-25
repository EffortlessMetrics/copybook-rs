      * Test OCCURS syntax
       01  TEST-RECORD.
           05  TEST-GROUP.
               10  ITEM-COUNT  PIC 9(3).
               10  ITEMS OCCURS 5 TIMES.
                   15  ITEM-CODE   PIC X(4).