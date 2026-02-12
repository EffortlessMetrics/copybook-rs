       01  ORDER-RECORD.
           05  ORDER-ID        PIC 9(8).
           05  LINE-ITEMS      OCCURS 10 TIMES.
               10  ITEM-CODE   PIC X(5).
               10  QUANTITY    PIC 9(3).
           66  ORDER-ITEMS RENAMES LINE-ITEMS THRU LINE-ITEMS.
