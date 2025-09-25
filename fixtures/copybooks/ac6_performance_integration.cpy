      * AC6 Performance Integration - Large-scale performance testing
      * Tests parsing performance with complex nested structures
       01  PERFORMANCE-TEST-RECORD.
           05  HEADER-SECTION.
               10  RECORD-TYPE     PIC X(4).
               10  PROCESSING-TS   PIC 9(14).
               10  BATCH-ID        PIC X(20).
           05  ENTRY-COUNT         PIC 9(6).
           05  SUMMARY-SECTION.
               10  TOTAL-ENTRIES  PIC 9(8).
               10  PROCESSED-COUNT PIC 9(8).
               10  ERROR-COUNT     PIC 9(6).
               10  TOTAL-AMOUNT    PIC S9(15)V99 COMP-3.
           05  FILLER              PIC X(100).
           05  ENTRIES OCCURS 1 TO 10000 TIMES
               DEPENDING ON ENTRY-COUNT.
               10  ENTRY-HEADER.
                   15  ENTRY-ID    PIC X(16).
                   15  ENTRY-TYPE  PIC X(4).
                   15  CREATE-TS   PIC 9(14).
               10  CORE-DATA.
                   15  TRANSACTION-ID PIC X(20).
                   15  ACCOUNT-FROM PIC X(20).
                   15  ACCOUNT-TO  PIC X(20).
                   15  AMOUNT      PIC S9(13)V99 COMP-3.
               10  METADATA.
                   15  USER-ID     PIC X(8).
                   15  SESSION-ID  PIC X(16).
                   15  IP-ADDRESS  PIC X(15).
                   15  USER-AGENT  PIC X(100).
               10  AUDIT-DATA.
                   15  APPROVAL-STATUS PIC X(2).
                   15  APPROVED-BY PIC X(8).
                   15  APPROVAL-TS PIC 9(14).
               10  RISK-DATA.
                   15  RISK-SCORE  PIC 9(3).
                   15  AML-FLAG    PIC X(1).
                   15  FRAUD-FLAG  PIC X(1).
                   15  COMPLIANCE-CODE PIC X(6).
               10  PROCESSING-INFO.
                   15  QUEUE-TIME  PIC 9(6)V999 COMP-3.
                   15  PROCESS-TIME PIC 9(4)V999 COMP-3.
                   15  RETRY-COUNT PIC 9(2).