      *> SOX Compliance Test Fixture - Financial Transaction Records
      *> Tests enterprise financial data processing with audit requirements
       01 FINANCIAL-TRANSACTION-RECORD.
           05 TRANSACTION-HEADER.
               10 TRANSACTION-ID         PIC 9(12) COMP-3.
                   88 VALID-TRANSACTION  VALUE 100000000000 THRU 999999999999.
               10 ACCOUNT-NUMBER         PIC 9(16) COMP-3.
               10 TRANSACTION-TYPE       PIC X(3).
                   88 DEBIT-TRANSACTION  VALUE 'DBT'.
                   88 CREDIT-TRANSACTION VALUE 'CRT'.
                   88 TRANSFER-TRANS     VALUE 'TRF'.
               10 BRANCH-CODE            PIC 9(4) COMP.
               10 PROCESSING-DATE        PIC 9(8) COMP-3.
               10 POSTING-TIMESTAMP      PIC 9(15) COMP-3.
           05 TRANSACTION-AMOUNTS.
               10 BASE-AMOUNT            PIC S9(13)V99 COMP-3.
                   88 MATERIAL-AMOUNT    VALUE 100000.00 THRU 999999999999.99.
               10 FEES-TOTAL             PIC S9(7)V99 COMP-3.
               10 TAX-AMOUNT             PIC S9(7)V99 COMP-3.
               10 NET-AMOUNT             PIC S9(13)V99 COMP-3.
           05 AUDIT-CONTROL-FIELDS.
               10 ORIGINATOR-ID          PIC X(8).
               10 AUTHORIZER-ID          PIC X(8).
               10 AUDIT-TRAIL-REF        PIC X(32).
               10 REGULATORY-FLAG        PIC X(1).
                   88 SOX-REPORTABLE     VALUE 'S'.
                   88 CFPB-REPORTABLE    VALUE 'C'.
                   88 SEC-REPORTABLE     VALUE 'E'.
               10 ENCRYPTION-STATUS      PIC X(1).
                   88 DATA-ENCRYPTED     VALUE 'Y'.
                   88 DATA-UNENCRYPTED   VALUE 'N'.
           05 CUSTOMER-INFO.
               10 CUSTOMER-ID            PIC 9(12) COMP-3.
               10 CUSTOMER-CLASSIFICATION PIC X(2).
                   88 RETAIL-CUSTOMER    VALUE 'RT'.
                   88 BUSINESS-CUSTOMER  VALUE 'BZ'.
                   88 INSTITUTIONAL      VALUE 'IN'.
               10 RISK-PROFILE           PIC X(1).
                   88 LOW-RISK           VALUE 'L'.
                   88 MEDIUM-RISK        VALUE 'M'.
                   88 HIGH-RISK          VALUE 'H'.
                   88 RESTRICTED         VALUE 'R'.