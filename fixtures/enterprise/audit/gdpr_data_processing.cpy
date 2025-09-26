      *> GDPR Compliance Test Fixture - Personal Data Processing
      *> Tests enterprise personal data processing with GDPR audit requirements
       01 PERSONAL-DATA-PROCESSING-RECORD.
           05 DATA-SUBJECT-INFO.
               10 DATA-SUBJECT-ID        PIC 9(12) COMP-3.
                   88 VALID-SUBJECT-ID   VALUE 100000000000 THRU 999999999999.
               10 CONSENT-REFERENCE      PIC X(16).
               10 LEGAL-BASIS            PIC X(2).
                   88 CONSENT            VALUE 'CN'.
                   88 CONTRACT           VALUE 'CT'.
                   88 LEGAL-OBLIGATION   VALUE 'LO'.
                   88 VITAL-INTERESTS    VALUE 'VI'.
                   88 PUBLIC-TASK        VALUE 'PT'.
                   88 LEGITIMATE-INTEREST VALUE 'LI'.
               10 PROCESSING-PURPOSE     PIC X(50).
           05 PERSONAL-DATA-CATEGORIES.
               10 DATA-CATEGORY-COUNT    PIC 9(2) COMP.
               10 DATA-CATEGORIES OCCURS 1 TO 15 TIMES
                       DEPENDING ON DATA-CATEGORY-COUNT.
                   15 CATEGORY-CODE       PIC X(3).
                       88 IDENTIFICATION   VALUE 'IDN'.
                       88 CONTACT-INFO     VALUE 'CON'.
                       88 FINANCIAL        VALUE 'FIN'.
                       88 HEALTH-DATA      VALUE 'HLT'.
                       88 BIOMETRIC        VALUE 'BIO'.
                       88 LOCATION         VALUE 'LOC'.
                       88 BEHAVIORAL       VALUE 'BEH'.
                   15 SENSITIVITY-LEVEL   PIC X(1).
                       88 BASIC-DATA       VALUE 'B'.
                       88 SENSITIVE        VALUE 'S'.
                       88 SPECIAL-CATEGORY VALUE 'C'.
                   15 DATA-MINIMIZATION   PIC X(1).
                       88 MINIMIZED        VALUE 'Y'.
                       88 NOT-MINIMIZED    VALUE 'N'.
           05 PROCESSING-CONTROLS.
               10 PROCESSING-START-DATE  PIC 9(8) COMP-3.
               10 RETENTION-PERIOD-DAYS  PIC 9(5) COMP.
               10 AUTO-DELETE-DATE       PIC 9(8) COMP-3.
               10 CROSS-BORDER-TRANSFER  PIC X(1).
                   88 DOMESTIC-ONLY      VALUE 'D'.
                   88 EU-EEA-ONLY        VALUE 'E'.
                   88 ADEQUACY-DECISION  VALUE 'A'.
                   88 SAFEGUARDS         VALUE 'S'.
               10 TRANSFER-COUNTRY-CODE  PIC X(3).
               10 DATA-CONTROLLER-ID     PIC X(8).
               10 DATA-PROCESSOR-ID      PIC X(8).
           05 GDPR-AUDIT-TRAIL.
               10 PROCESSING-ACTIVITY-ID PIC X(16).
               10 CONSENT-TIMESTAMP      PIC 9(15) COMP-3.
               10 CONSENT-VERSION        PIC X(8).
               10 RIGHT-EXERCISED        PIC X(2).
                   88 ACCESS-REQUEST     VALUE 'AC'.
                   88 RECTIFICATION      VALUE 'RC'.
                   88 ERASURE-REQUEST    VALUE 'ER'.
                   88 RESTRICT-PROCESS   VALUE 'RP'.
                   88 DATA-PORTABILITY   VALUE 'DP'.
                   88 OBJECTION          VALUE 'OB'.
               10 BREACH-INCIDENT-ID     PIC X(16).
               10 DPO-NOTIFICATION       PIC X(1).
                   88 DPO-NOTIFIED       VALUE 'Y'.
                   88 DPO-NOT-REQUIRED   VALUE 'N'.
               10 SUPERVISORY-AUTH-REF   PIC X(16).
           05 SECURITY-MEASURES.
               10 ENCRYPTION-AT-REST     PIC X(1).
                   88 ENCRYPTED          VALUE 'Y'.
                   88 NOT-ENCRYPTED      VALUE 'N'.
               10 ENCRYPTION-IN-TRANSIT  PIC X(1).
                   88 TRANSIT-ENCRYPTED  VALUE 'Y'.
                   88 TRANSIT-CLEAR      VALUE 'N'.
               10 ACCESS-CONTROL-LEVEL   PIC X(1).
                   88 ROLE-BASED         VALUE 'R'.
                   88 ATTRIBUTE-BASED    VALUE 'A'.
                   88 DISCRETIONARY      VALUE 'D'.
               10 PSEUDONYMIZATION       PIC X(1).
                   88 PSEUDONYMIZED      VALUE 'Y'.
                   88 NOT-PSEUDONYMIZED  VALUE 'N'.