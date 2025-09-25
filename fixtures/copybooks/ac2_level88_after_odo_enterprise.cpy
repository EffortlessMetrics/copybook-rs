      * AC2 Level-88 After ODO Enterprise - Healthcare enterprise pattern
      * Complex variable-length records with nested structures
       01  HEALTHCARE-ENTERPRISE-RECORD.
           05  PATIENT-HEADER.
               10  PATIENT-ID      PIC X(12).
               10  ADMISSION-DATE  PIC 9(8).
               10  DISCHARGE-DATE  PIC 9(8).
               10  PROVIDER-ID     PIC X(8).
           05  DIAGNOSIS-COUNT     PIC 9(3).
           05  MEDICATION-COUNT    PIC 9(3).
           05  SUMMARY-INFO.
               10  TOTAL-CHARGES   PIC S9(10)V99 COMP-3.
               10  INSURANCE-COVERAGE PIC S9(10)V99 COMP-3.
               10  PATIENT-RESPONSIBILITY PIC S9(8)V99 COMP-3.
           05  FILLER              PIC X(100).
           05  DIAGNOSES OCCURS 1 TO 200 TIMES
               DEPENDING ON DIAGNOSIS-COUNT.
               10  DIAGNOSIS-INFO.
                   15  ICD-CODE    PIC X(7).
                   15  SEVERITY    PIC X(1).
                   15  ONSET-DATE  PIC 9(8).
               10  TREATMENT-INFO.
                   15  TREATMENT-CODE PIC X(10).
                   15  PROVIDER-ID PIC X(8).
                   15  START-DATE  PIC 9(8).
                   15  END-DATE    PIC 9(8).
               10  BILLING-INFO.
                   15  PROCEDURE-COST PIC S9(8)V99 COMP-3.
                   15  INSURANCE-CODE PIC X(6).
                   15  COPAY-AMOUNT PIC S9(4)V99 COMP-3.
           05  MEDICATIONS OCCURS 1 TO 50 TIMES
               DEPENDING ON MEDICATION-COUNT.
               10  MEDICATION-CODE PIC X(12).
               10  DOSAGE-INFO.
                   15  DOSAGE-AMOUNT PIC 9(4)V99 COMP-3.
                   15  DOSAGE-UNIT PIC X(10).
                   15  FREQUENCY   PIC X(15).
               10  PRESCRIPTION-DATE PIC 9(8).