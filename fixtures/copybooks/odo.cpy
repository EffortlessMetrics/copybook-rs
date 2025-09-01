      * Copybook with OCCURS DEPENDING ON
       01  VARIABLE-RECORD.
           05  RECORD-LENGTH       PIC 9(4) COMP.
           05  ITEM-COUNT          PIC 9(3).
           05  HEADER-INFO         PIC X(20).
           05  ITEMS OCCURS 1 TO 100 TIMES
               DEPENDING ON ITEM-COUNT.
               10  ITEM-ID         PIC 9(6).
               10  ITEM-NAME       PIC X(15).
               10  ITEM-PRICE      PIC S9(5)V99 COMP-3.