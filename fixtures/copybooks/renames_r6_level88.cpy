       01  STATUS-RECORD.
           05  STATUS-CODE     PIC X(1).
               88  STATUS-OK   VALUE 'A'.
               88  STATUS-ERR VALUE 'E'.
           66  STATUS-FLAG RENAMES STATUS-CODE THRU STATUS-CODE.
