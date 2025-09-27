      *> HIPAA Compliance Test Fixture - Healthcare PHI Records
      *> Tests enterprise healthcare data processing with PHI audit requirements
       01 PATIENT-HEALTH-RECORD.
           05 PATIENT-IDENTIFICATION.
               10 PATIENT-ID             PIC 9(10) COMP-3.
                   88 VALID-PATIENT-ID   VALUE 1000000000 THRU 9999999999.
               10 MEDICAL-RECORD-NUMBER  PIC X(12).
               10 SSN-LAST-FOUR          PIC 9(4) COMP.
               10 DATE-OF-BIRTH          PIC 9(8) COMP-3.
           05 PHI-CLASSIFICATION.
               10 PHI-CATEGORY           PIC X(2).
                   88 DEMOGRAPHIC-INFO   VALUE 'DM'.
                   88 MEDICAL-HISTORY    VALUE 'MH'.
                   88 TREATMENT-PLAN     VALUE 'TP'.
                   88 BILLING-INFO       VALUE 'BI'.
                   88 INSURANCE-INFO     VALUE 'IN'.
               10 SENSITIVITY-LEVEL      PIC X(1).
                   88 PUBLIC-HEALTH      VALUE 'P'.
                   88 CONFIDENTIAL       VALUE 'C'.
                   88 RESTRICTED         VALUE 'R'.
                   88 HIGHLY-SENSITIVE   VALUE 'H'.
               10 MINIMUM-NECESSARY-FLAG PIC X(1).
                   88 MIN-NECESSARY      VALUE 'Y'.
                   88 NOT-MIN-NECESSARY  VALUE 'N'.
           05 TREATMENT-INFORMATION.
               10 DIAGNOSIS-COUNT        PIC 9(2) COMP.
               10 DIAGNOSES OCCURS 1 TO 20 TIMES
                       DEPENDING ON DIAGNOSIS-COUNT.
                   15 DIAGNOSIS-CODE      PIC X(7).
                   15 DIAGNOSIS-DESC      PIC X(100).
                   15 SEVERITY-LEVEL      PIC X(1).
                       88 MILD-CONDITION   VALUE 'M'.
                       88 MODERATE-COND    VALUE 'O'.
                       88 SEVERE-CONDITION VALUE 'S'.
                       88 CRITICAL-COND    VALUE 'C'.
           05 HIPAA-AUDIT-CONTROLS.
               10 ACCESS-PURPOSE         PIC X(3).
                   88 TREATMENT          VALUE 'TRT'.
                   88 PAYMENT            VALUE 'PAY'.
                   88 OPERATIONS         VALUE 'OPS'.
                   88 RESEARCH           VALUE 'RSH'.
                   88 QUALITY-ASSURANCE  VALUE 'QAL'.
               10 AUTHORIZED-USER        PIC X(8).
               10 ACCESS-TIMESTAMP       PIC 9(15) COMP-3.
               10 CONSENT-STATUS         PIC X(1).
                   88 CONSENT-OBTAINED   VALUE 'Y'.
                   88 CONSENT-DECLINED   VALUE 'N'.
                   88 CONSENT-REVOKED    VALUE 'R'.
               10 ENCRYPTION-METHOD      PIC X(3).
                   88 AES-256            VALUE 'AES'.
                   88 RSA-ENCRYPTION     VALUE 'RSA'.
                   88 NO-ENCRYPTION      VALUE 'NON'.
               10 AUDIT-LOG-ID           PIC X(32).
               10 BREACH-NOTIFICATION-ID PIC X(16).
           05 PROVIDER-INFORMATION.
               10 PROVIDER-ID            PIC 9(10) COMP-3.
               10 PROVIDER-NPI           PIC 9(10) COMP-3.
               10 FACILITY-ID            PIC 9(6) COMP-3.
               10 DEPARTMENT-CODE        PIC X(4).
                   88 EMERGENCY-DEPT     VALUE 'EMER'.
                   88 CARDIOLOGY         VALUE 'CARD'.
                   88 ONCOLOGY           VALUE 'ONCO'.
                   88 PEDIATRICS         VALUE 'PEDI'.
                   88 SURGERY            VALUE 'SURG'.