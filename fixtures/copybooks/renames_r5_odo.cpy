       01  ORDER-RECORD.
           05  ORDER-ID        PIC 9(8).
           05  ITEM-COUNT     PIC 9(3).
           05  LINE-ITEMS      OCCURS 1 TO 10 DEPENDING ON ITEM-COUNT.
               10  ITEM-CODE   PIC X(5).
               10  QUANTITY    PIC 9(3).
           66  ORDER-ITEMS RENAMES LINE-ITEMS.
