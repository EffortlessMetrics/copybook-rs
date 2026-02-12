       01  PAYLOAD-RECORD.
           05  RAW-PAYLOAD        PIC X(20).
           05  NUMERIC-PAYLOAD    REDEFINES RAW-PAYLOAD.
               10  NUM-1           PIC S9(9) COMP.
               10  NUM-2           PIC S9(9) COMP.
