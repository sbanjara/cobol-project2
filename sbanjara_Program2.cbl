      ******************* IDENTIFICATION-DIVISION *******************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SBNPROGRAM2.
       AUTHOR.    SABIN BANJARA.

      ****************************************************************
      * THIS PROGRAM CREATES A DETAILED EMPLOYEE SALARY REPORT OF 
      * DRAKEA INC, WHICH IS GROUPED BY WAREHOUSES.
      **********
      * INPUT:
      *  THE EMPLOYEE RECORD FILE WHICH CONTAINS FOLLOWING INFORMATION,
      *    1. WAREHOUSE ID
      *    2. EMPLOYEE ID
      *    3. EMPLOYEE POSITION
      *    4. EMPLOYEE NAME
      *    5. HIRE DATE
      *    6. STARTING SALARY
      *    7. DATE OF LAST PAY INCREASE
      *    8. CURRENT SALARY
      *    9. UNION DUES
      *    10. INSURANCE
      **********
      * OUTPUT: 
      *  THE DETAILED SALARY REPORT CONTAINING FOLLOWING INFORMATION,
      *    1. WAREHOUSE
      *    2. EMPLOYEE ID
      *    3. EMPLOYEE POSITION
      *    4. EMPLOYEE LAST NAME
      *    5. INCREASED CURRENT SALARY
      *    6. INCREASED UNION DUES
      *    7. INCREASED INSURANCE
      *****************
      * CALCULATIONS:
      *    1. INCREASED CURRENT SALARY = 
      *      EMPLOYEE CURRENT SALARY + (0.05 * EMPLOYEE CURRENT SALARY)               
      *    2. INCREASED UNION DUES = 
      *      CURRENT UNION DUES + (0.03 * CURRENT UNION DUES)
      *    3. INCREASED INSURANCE =
      *      CURRENT INSURANCE + (0.05 * CURRENT INSURANCE)
      *    4. TOTAL INCREASED CURRENT SALARY =
      *      THE SUM OF ALL INCREASED CURRENT SALARY
      *    5. TOTAL INCREASED UNION DUES = 
      *      THE SUM OF ALL INCREASED UNION DUES 
      *    6. TOTAL INCREASED INSURANCE =
      *      THE SUM OF ALL INCREASED INSURANCE
      **********
      * NOTE: 
      *    THE INCREASED PERCENTAGE OF CURRENT SALARY, UNIONS DUES, AND
      *    INSURANCE WHICH IS 5%, 3%, AND 5% RESPECTIVELY ARE STORED AS
      *    WORKING-STORAGE'S FIELDS (CURRENT-SALARY-INCREASE,
      *    UNION-DUES-INCREASE, AND INSURANCE-INCREASE RESPECTIVELY).
      *
      ****************************************************************

      ********************** ENVIRONMENT-DIVISON *********************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  LENEVO-PC.
       OBJECT-COMPUTER.  LENEVO-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-RECORD-FILE
               ASSIGN TO 'PR2FA19.TXT'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SALARY-REPORT-FILE
               ASSIGN TO PRINTER 'SALARY-REPORT'.

      ************************* DATA-DIVISION ************************

       DATA DIVISION.
       FILE SECTION.

       FD  EMPLOYEE-RECORD-FILE
           RECORD CONTAINS 83 CHARACTERS.

       01  EMPLOYEE-RECORD.
           05  WAREHOUSE-ID       PIC X(4).
           05  EMPLOYEE-ID        PIC X(5).
           05  EMPLOYEE-POSITION  PIC X(2).
           05  LAST-NAME          PIC X(10).
           05  FIRST-NAME         PIC X(10).
           05  FILLER             PIC X(3).
           05  HIRE-DATE          PIC 9(8).
           05  STARTING-SALARY    PIC 9(6)V99.
           05  FILLER             PIC X(4).
           05  PAY-INCREASE-DATE  PIC 9(8).
           05  CURRENT-SALARY     PIC 9(6)V99.
           05  FILLER             PIC X(5).
           05  UNION-DUES         PIC 9(3).
           05  INSURANCE          PIC 9(3)V99.

       FD  SALARY-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01  SALARY-REPORT          PIC X(80).

      ********************* WORKING-STORAGE **************************

       WORKING-STORAGE SECTION.

       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG          PIC X       VALUE ' '.
               88  NO-MORE-DATA              VALUE 'N'.

       01  DETAIL-FIELDS.
           05  WS-INCREASED-CURRENT              PIC 9(6)V99 VALUE 0.
           05  WS-INCREASED-UNION-DUES           PIC 9(5)V99 VALUE 0.
           05  WS-INCREASED-INSURANCE            PIC 9(5)V99 VALUE 0.
           05  CURRENT-SALARY-INCREASE           PIC 9V99  VALUE 0.05.
           05  UNION-DUES-INCREASE               PIC 9V99  VALUE 0.03.
           05  INSURANCE-INCREASE                PIC 9V99  VALUE 0.05.

       01  TOTAL-FIELDS.
           05  WS-INCREASED-CURRENT-SLRY-TL  PIC 9(7)V99.
           05  WS-INCREASED-UNION-DUES-TL    PIC 9(6)V99.
           05  WS-INCREASED-INSURANCE-TL     PIC 9(6)V99.
    
       01  CONTROL-FIELDS.
           05  WS-FIRST-RECORD    PIC X(3)      VALUE 'YES'.
           05  WS-HOLD-FIELD      PIC XXXX.
           05  WS-WAREHOUSE       PIC X(11).

       01  REPORT-FIELDS.
           05  PROPER-SPACING                   PIC 9 VALUE 0.
           05  COUNTER                          PIC 99 VALUE 00.
		   
       01  DATE-FIELDS.
		   05  WS-YEAR                          PIC 9999.
		   05  WS-MONTH                         PIC 99.
		   05  WS-DAY                           PIC 99.

      ************************* OUTPUT AREA ***************************
       
       01  HEADING-ONE.
           05  H1-MONTH           PIC 99.
           05                     PIC X       VALUE '/'.
           05  H1-DAY             PIC 99.
           05                     PIC X       VALUE '/'.
           05  H1-YEAR            PIC 9999.
           05                     PIC X(26)   VALUE '     SBN'.
           05                     PIC X(35)   VALUE 'DRAKEA, LTD'.
           05                     PIC X(5)    VALUE 'PAGE'.
           05  PAGE-NUM           PIC 99.

       01  HEADING-TWO.
           05                     PIC X(35)   VALUE SPACES.
           05                     PIC X(45)   VALUE 'SALARY REPORT'.

       01  HEADING-WAREHOUSE.
           05                     PIC X(12)   VALUE 'WAREHOUSE:'.
           05  WAREHOUSE-NAME     PIC X(11).
           05                     PIC X(57)   VALUE SPACES.

       01  HEADING-THREE.
           05                     PIC X(14)   VALUE '  EMPLOYEE'.
           05                     PIC X(13)   VALUE 'EMPLOYEE'.
           05                     PIC X(12)   VALUE 'EMPLOYEE'.
           05                     PIC X(14)   VALUE 'INCREASED'.
           05                     PIC X(13)   VALUE 'INCREASED'.
           05                     PIC X(13)   VALUE 'INCREASED'.

       01  HEADING-FOUR.
           05                     PIC X(5)    VALUE SPACES.
           05                     PIC X(9)    VALUE 'ID'.
           05                     PIC X(12)   VALUE 'POSITION'.
           05                     PIC X(14)   VALUE 'LAST NAME'.
           05                     PIC X(13)   VALUE 'CURRENT'.
           05                     PIC X(13)   VALUE 'UNION DUES'.
           05                     PIC X(13)   VALUE 'INSURANCE'.

       01  DETAIL-LINE.
           05                            PIC X(3) VALUE SPACES.
           05  EMPLOYEE-ID-OUT           PIC X(5).
		   05                            PIC X(5) VALUE SPACES.
           05  EMPLOYEE-POSITION-OUT     PIC X(10).
		   05                            PIC X(3) VALUE SPACES.
           05  LAST-NAME-OUT             PIC X(10).
		   05                            PIC X(3) VALUE SPACES.
           05  INCREASED-CURRENT-SALARY  PIC $***,***.99.
		   05                            PIC X(3) VALUE SPACES.
           05  INCREASED-UNION-DUES      PIC $**,***.99.
		   05                            PIC X(3) VALUE SPACES.
           05  INCREASED-INSURANCE       PIC $***,***.99.

       01  TOTAL-LINE.
           05                           PIC X(7)  VALUE SPACES.
           05  WAREHOUSE-OUT            PIC X(11).
           05                           PIC X(18) 
                                        VALUE ' WAREHOUSE TOTAL:'.
           05  INCREASED-CURRENT-TL     PIC $*,***,***.99.
           05                           PIC XX    VALUE SPACES.
           05  INCREASED-UN-DUES-TL     PIC $***,***.99.
           05                           PIC XX    VALUE SPACES.
           05  INCREASED-INSURANCE-TL   PIC $***,***.99.
           05                           PIC X(3)  VALUE SPACES.
      
      ************************ PROCEDURE DIVISION ********************
      
       PROCEDURE DIVISION.

       10-CONTROL-MODULE.

           PERFORM 20-HOUSEKEEPING-ROUTINE
           PERFORM 40-MAIN-ROUTINE
           PERFORM 110-EOF-ROUTINE
           .

       20-HOUSEKEEPING-ROUTINE.

           OPEN INPUT  EMPLOYEE-RECORD-FILE
               OUTPUT SALARY-REPORT-FILE
           PERFORM 30-HEADER-ROUTINE
           .

       30-HEADER-ROUTINE.

           WRITE SALARY-REPORT AFTER ADVANCING PAGE
           ADD 1 TO COUNTER
           MOVE COUNTER TO PAGE-NUM
		   ACCEPT DATE-FIELDS FROM DATE YYYYMMDD
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY   TO H1-DAY
           MOVE WS-YEAR  TO H1-YEAR
           WRITE SALARY-REPORT FROM HEADING-ONE
               AFTER ADVANCING 1 LINE
           WRITE SALARY-REPORT FROM HEADING-TWO
               AFTER ADVANCING 2 LINES
           MOVE 2 TO PROPER-SPACING
           . 

       40-MAIN-ROUTINE.
           
           PERFORM UNTIL NO-MORE-DATA
               READ EMPLOYEE-RECORD-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 50-READ-A-RECORD
               END-READ
           END-PERFORM
           .

       50-READ-A-RECORD.

           IF WS-FIRST-RECORD = 'YES'
               MOVE  WAREHOUSE-ID TO WS-HOLD-FIELD
               MOVE  'NO' TO WS-FIRST-RECORD
               PERFORM 60-PRINT-CLASS-HEADER
            
           ELSE 
               IF WAREHOUSE-ID NOT = WS-HOLD-FIELD
                   PERFORM 70-CONTROL-BREAK
               END-IF
           END-IF

           MOVE EMPLOYEE-ID TO EMPLOYEE-ID-OUT
           PERFORM  90-WRITE-EMPLOYEE-POSITION
           MOVE LAST-NAME TO LAST-NAME-OUT

           MULTIPLY CURRENT-SALARY BY CURRENT-SALARY-INCREASE 
               GIVING WS-INCREASED-CURRENT
		   ADD  CURRENT-SALARY TO WS-INCREASED-CURRENT
           MOVE WS-INCREASED-CURRENT TO INCREASED-CURRENT-SALARY
           ADD  WS-INCREASED-CURRENT TO WS-INCREASED-CURRENT-SLRY-TL
           MOVE 0 TO WS-INCREASED-CURRENT

           MULTIPLY UNION-DUES BY UNION-DUES-INCREASE
               GIVING WS-INCREASED-UNION-DUES
		   ADD  UNION-DUES TO WS-INCREASED-UNION-DUES
           MOVE WS-INCREASED-UNION-DUES TO INCREASED-UNION-DUES
           ADD  WS-INCREASED-UNION-DUES TO WS-INCREASED-UNION-DUES-TL
           MOVE 0 TO WS-INCREASED-UNION-DUES

           MULTIPLY INSURANCE BY INSURANCE-INCREASE 
               GIVING WS-INCREASED-INSURANCE
		   ADD  INSURANCE TO WS-INCREASED-INSURANCE
           MOVE WS-INCREASED-INSURANCE  TO INCREASED-INSURANCE
           ADD  WS-INCREASED-INSURANCE  TO WS-INCREASED-INSURANCE-TL
           MOVE 0 TO WS-INCREASED-INSURANCE

           MOVE DETAIL-LINE TO SALARY-REPORT
		   PERFORM 100-WRITE-LINE
		   MOVE 1 TO PROPER-SPACING
           .

       60-PRINT-CLASS-HEADER.

           PERFORM 80-WRITE-A-WAREHOUSE
           MOVE WS-WAREHOUSE TO WAREHOUSE-NAME
		   MOVE HEADING-WAREHOUSE TO SALARY-REPORT
           PERFORM 100-WRITE-LINE
           WRITE SALARY-REPORT FROM HEADING-THREE
               AFTER ADVANCING 3 LINES
           WRITE SALARY-REPORT FROM HEADING-FOUR
               AFTER ADVANCING 1 LINE
           MOVE 2 TO PROPER-SPACING
           . 

       70-CONTROL-BREAK.

           MOVE WS-WAREHOUSE TO WAREHOUSE-OUT
           MOVE WAREHOUSE-ID TO WS-HOLD-FIELD
           MOVE WS-INCREASED-CURRENT-SLRY-TL TO INCREASED-CURRENT-TL
           MOVE WS-INCREASED-INSURANCE-TL  TO INCREASED-INSURANCE-TL
           MOVE WS-INCREASED-UNION-DUES-TL TO INCREASED-UN-DUES-TL

           MOVE 3 TO PROPER-SPACING
           MOVE TOTAL-LINE TO SALARY-REPORT
		   PERFORM 100-WRITE-LINE
           PERFORM 60-PRINT-CLASS-HEADER

           MOVE 0 TO WS-INCREASED-CURRENT-SLRY-TL
           MOVE 0 TO WS-INCREASED-UNION-DUES-TL
           MOVE 0 TO WS-INCREASED-INSURANCE-TL
           .

       80-WRITE-A-WAREHOUSE.

           EVALUATE TRUE
               WHEN WAREHOUSE-ID = 'AL10'
                   MOVE 'ALABAMA' TO WS-WAREHOUSE
               WHEN WAREHOUSE-ID = 'GA11'
                   MOVE 'GEORGIA' TO WS-WAREHOUSE
               WHEN WAREHOUSE-ID = 'MS12'
                   MOVE 'MISSISSIPPI' TO WS-WAREHOUSE
           END-EVALUATE
           .

       90-WRITE-EMPLOYEE-POSITION.
    
           EVALUATE TRUE
               WHEN EMPLOYEE-POSITION = 'WM'
                   MOVE 'MANAGER' TO EMPLOYEE-POSITION-OUT
               WHEN EMPLOYEE-POSITION = 'DS'
                   MOVE 'SUPERVISOR' TO EMPLOYEE-POSITION-OUT
               WHEN EMPLOYEE-POSITION = 'OW'
                   MOVE 'OFFICE' TO EMPLOYEE-POSITION-OUT       
               WHEN EMPLOYEE-POSITION = 'WW'
                   MOVE 'WAREHOUSE' TO EMPLOYEE-POSITION-OUT
               WHEN EMPLOYEE-POSITION = 'WS'
                   MOVE 'SECURITY' TO EMPLOYEE-POSITION-OUT
           END-EVALUATE
           .

       100-WRITE-LINE.

           WRITE SALARY-REPORT AFTER ADVANCING PROPER-SPACING
           .

       110-EOF-ROUTINE.
               
           PERFORM 80-WRITE-A-WAREHOUSE
           MOVE WS-WAREHOUSE TO WAREHOUSE-OUT
           MOVE WS-INCREASED-CURRENT-SLRY-TL TO INCREASED-CURRENT-TL
           MOVE WS-INCREASED-INSURANCE-TL    TO INCREASED-INSURANCE-TL
           MOVE WS-INCREASED-UNION-DUES-TL   TO INCREASED-UN-DUES-TL

           MOVE 3 TO PROPER-SPACING
           MOVE TOTAL-LINE TO SALARY-REPORT
           PERFORM 100-WRITE-LINE

           CLOSE EMPLOYEE-RECORD-FILE
                  SALARY-REPORT-FILE
           STOP RUN
           .
       