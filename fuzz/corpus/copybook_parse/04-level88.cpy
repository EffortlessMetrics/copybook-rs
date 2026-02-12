       01  CUSTOMER-RECORD.
           05  CUSTOMER-STATUS     PIC X(1).
               88  STATUS-ACTIVE   VALUE 'A'.
               88  STATUS-INACTIVE VALUE 'I'.
               88  STATUS-PENDING  VALUE 'P'.
           05  CUSTOMER-ID         PIC 9(6).
