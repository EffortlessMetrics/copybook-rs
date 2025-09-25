      * AC3 Child Inside ODO Basic - Nested structures within ODO arrays
      * Tests complex array element structures
       01  ORDER-PROCESSING-RECORD.
           05  ORDER-HEADER.
               10  ORDER-ID        PIC X(12).
               10  CUSTOMER-ID     PIC X(10).
               10  ORDER-DATE      PIC 9(8).
               10  LINE-COUNT      PIC 9(3).
           05  ORDER-TOTALS.
               10  SUBTOTAL        PIC 9(10)V99 COMP-3.
               10  TAX-TOTAL       PIC 9(8)V99 COMP-3.
               10  DISCOUNT-TOTAL  PIC 9(8)V99 COMP-3.
               10  GRAND-TOTAL     PIC 9(10)V99 COMP-3.
           05  FILLER              PIC X(50).
           05  ORDER-LINES OCCURS 1 TO 999 TIMES
               DEPENDING ON LINE-COUNT.
               10  LINE-DETAILS.
                   15  LINE-NUMBER PIC 9(3).
                   15  PRODUCT-CODE PIC X(12).
                   15  QUANTITY    PIC 9(6).
                   15  UNIT-PRICE  PIC 9(6)V99 COMP-3.
               10  LINE-CALCULATIONS.
                   15  LINE-TOTAL  PIC 9(8)V99 COMP-3.
                   15  TAX-AMOUNT  PIC 9(6)V99 COMP-3.
                   15  DISCOUNT-AMOUNT PIC 9(6)V99 COMP-3.
               10  LINE-STATUS.
                   15  STATUS-CODE PIC X(2).
                   15  SHIP-DATE   PIC 9(8).
                   15  TRACKING-NUM PIC X(20).