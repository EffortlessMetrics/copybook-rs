      01  CUSTOMER-RECORD.
          05  CUSTOMER-ID         PIC 9(10).
          05  CUSTOMER-NAME       PIC X(30).
          05  ACCOUNT-BALANCE     PIC S9(13)V99 COMP-3.
          05  CUSTOMER-STATUS     PIC X(1).
              88  STATUS-ACTIVE   VALUE 'A'.
              88  STATUS-INACTIVE VALUE 'I'.
          05  TRANSACTION-COUNT   PIC 9(3).
          05  TRANSACTIONS OCCURS 1 TO 100 TIMES
                  DEPENDING ON TRANSACTION-COUNT.
              10  TRANSACTION-ID  PIC 9(8).
              10  AMOUNT          PIC S9(9)V99 COMP-3.