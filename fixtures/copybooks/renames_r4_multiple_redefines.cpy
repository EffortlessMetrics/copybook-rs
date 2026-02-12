       01  TRANSACTION-RECORD.
           05  TRANS-TYPE      PIC X(1).
           05  TRANS-DATA      PIC X(20).
               10  CHECK-DATA   REDEFINES TRANS-DATA.
                   15  CHECK-NUM  PIC 9(8).
                   15  CHECK-AMT  PIC 9(10).
               10  CARD-DATA    REDEFINES TRANS-DATA.
                   15  CARD-NUM   PIC 9(16).
                   15  CARD-EXP   PIC 9(4).
           66  PAYMENT-INFO RENAMES CHECK-DATA THRU CARD-DATA.
