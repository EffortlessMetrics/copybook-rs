      * AC2 Level-88 After ODO Basic - Variable-length records
      * Note: Level-88 not supported by parser, using regular fields
       01  VARIABLE-LENGTH-RECORD.
           05  RECORD-HEADER.
               10  RECORD-ID       PIC X(10).
               10  ITEM-COUNT      PIC 9(3).
           05  RECORD-TRAILER.
               10  CHECKSUM        PIC 9(8).
               10  RECORD-STATUS   PIC X(1).
           05  FILLER              PIC X(10).
           05  ITEMS OCCURS 1 TO 100 TIMES
               DEPENDING ON ITEM-COUNT.
               10  ITEM-CODE       PIC X(8).
               10  ITEM-STATUS     PIC X(1).
               10  ITEM-AMOUNT     PIC S9(7)V99 COMP-3.