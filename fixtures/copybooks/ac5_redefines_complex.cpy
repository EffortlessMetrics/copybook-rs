      * AC5 REDEFINES Complex - Complex overlapping data structures
      * Tests multiple REDEFINES patterns and nested structures
       01  FINANCIAL-TRANSACTION-RECORD.
           05  TRANSACTION-HEADER.
               10  TRANS-ID        PIC X(16).
               10  TRANS-TYPE      PIC X(4).
               10  PROCESS-DATE    PIC 9(8).
           05  TRANSACTION-DATA    PIC X(200).
           05  WIRE-TRANSFER REDEFINES TRANSACTION-DATA.
               10  SENDER-INFO.
                   15  SENDER-ACCOUNT PIC X(20).
                   15  SENDER-ROUTING PIC X(9).
                   15  SENDER-NAME   PIC X(40).
               10  RECEIVER-INFO.
                   15  RECEIVER-ACCOUNT PIC X(20).
                   15  RECEIVER-ROUTING PIC X(9).
                   15  RECEIVER-NAME PIC X(40).
               10  TRANSFER-AMOUNT PIC S9(13)V99 COMP-3.
               10  FEES-AND-CHARGES.
                   15  WIRE-FEE    PIC S9(6)V99 COMP-3.
                   15  PROCESSING-FEE PIC S9(4)V99 COMP-3.
               10  REGULATORY-INFO.
                   15  SWIFT-CODE  PIC X(11).
                   15  REFERENCE-NUM PIC X(16).
               10  FILLER          PIC X(37).
           05  CHECK-PAYMENT REDEFINES TRANSACTION-DATA.
               10  PAYOR-INFO.
                   15  PAYOR-NAME  PIC X(50).
                   15  PAYOR-ADDRESS PIC X(60).
               10  PAYEE-INFO.
                   15  PAYEE-NAME  PIC X(50).
                   15  PAYEE-ADDRESS PIC X(60).
               10  CHECK-DETAILS.
                   15  CHECK-NUMBER PIC 9(8).
                   15  CHECK-DATE  PIC 9(8).
                   15  CHECK-AMOUNT PIC S9(11)V99 COMP-3.
               10  FILLER          PIC X(14).
           05  CARD-PAYMENT REDEFINES TRANSACTION-DATA.
               10  CARD-INFO.
                   15  CARD-NUMBER PIC X(16).
                   15  CARD-TYPE   PIC X(4).
                   15  EXPIRY-DATE PIC 9(4).
                   15  CVV-CODE    PIC 9(3).
               10  MERCHANT-INFO.
                   15  MERCHANT-ID PIC X(15).
                   15  TERMINAL-ID PIC X(8).
                   15  LOCATION-CODE PIC X(10).
               10  TRANSACTION-DETAILS.
                   15  AUTH-CODE   PIC X(6).
                   15  TRANS-AMOUNT PIC S9(9)V99 COMP-3.
                   15  TIP-AMOUNT  PIC S9(6)V99 COMP-3.
                   15  CASHBACK-AMOUNT PIC S9(6)V99 COMP-3.
               10  FILLER          PIC X(120).
           05  TRANSACTION-FOOTER.
               10  CHECKSUM        PIC 9(10).
               10  STATUS-CODE     PIC X(2).
               10  PROCESSED-BY    PIC X(8).
           05  FILLER              PIC X(30).