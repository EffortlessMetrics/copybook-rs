      * Complex copybook with REDEFINES and OCCURS
       01  TRANSACTION-RECORD.
           05  RECORD-TYPE         PIC X(2).
           05  TRANSACTION-DATA.
               10  COMMON-FIELDS.
                   15  TRANS-ID    PIC 9(10).
                   15  TRANS-DATE  PIC 9(8).
               10  TYPE-A-DATA REDEFINES COMMON-FIELDS.
                   15  ACCOUNT-NUM PIC 9(12).
                   15  AMOUNT      PIC S9(9)V99 COMP-3.
               10  TYPE-B-DATA REDEFINES COMMON-FIELDS.
                   15  CUSTOMER-ID PIC 9(8).
                   15  ITEM-COUNT  PIC 9(3).
                   15  ITEMS OCCURS 5 TIMES.
                       20  ITEM-CODE PIC X(4).
           05  FILLER              PIC X(10).