      * Test exact copy
       01  TEST-RECORD.
           05  TEST-GROUP.
               10  ITEM-COUNT  PIC 9(3).
                   15  ITEMS OCCURS 5 TIMES.
                       20  ITEM-CODE   PIC X(4).