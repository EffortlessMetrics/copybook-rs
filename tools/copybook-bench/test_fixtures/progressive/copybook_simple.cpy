      * Simple COBOL copybook for progressive testing
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID           PIC 9(8) COMP.
           05  CUSTOMER-NAME         PIC X(50).
           05  ACCOUNT-BALANCE       PIC S9(13)V99 COMP-3.
           05  STATUS-CODE           PIC X(1).