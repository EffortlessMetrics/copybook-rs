      * AC1 Infrastructure Enterprise - Complex enterprise structures
      * Tests nested groups, multiple data types, and complex layouts
       01  ENTERPRISE-SYSTEM-RECORD.
           05  SYSTEM-HEADER.
               10  SYSTEM-ID       PIC X(8).
               10  TIMESTAMP       PIC 9(14).
               10  VERSION-INFO.
                   15  MAJOR-VER   PIC 9(2).
                   15  MINOR-VER   PIC 9(2).
                   15  PATCH-VER   PIC 9(3).
           05  MODULE-COUNT        PIC 9(4).
           05  MODULES OCCURS 1000 TIMES.
               10  MODULE-INFO.
                   15  MODULE-ID   PIC X(12).
                   15  MODULE-TYPE PIC X(4).
                   15  STATUS-CODE PIC X(2).
               10  PERFORMANCE-METRICS.
                   15  CPU-USAGE   PIC 9(3)V99 COMP-3.
                   15  MEMORY-USAGE PIC 9(6) COMP.
                   15  RESPONSE-TIME PIC 9(4)V999 COMP-3.
               10  ERROR-INFO.
                   15  ERROR-COUNT PIC 9(6).
                   15  WARNING-COUNT PIC 9(6).
                   15  LAST-ERROR  PIC X(100).
           05  SUMMARY-TOTALS.
               10  TOTAL-MODULES   PIC 9(6).
               10  ACTIVE-MODULES  PIC 9(6).
               10  FAILED-MODULES  PIC 9(6).
           05  FILLER              PIC X(50).