C ****************************************************************
C *                  LUNAR LANDER SIMULATION                     *
C *              ANSI FORTRAN 77 Implementation                  *
C *          with Enhanced Character Graphics Display            *
C *                                                              *
C *             Mickey W. Lawless  Dec.21-2025	           *
C *          Compiler: Lahey Personal FORTRAN 77		  *
C *                                                              *
C ****************************************************************

      PROGRAM LUNARL
    
      IMPLICIT NONE
      INTEGER T, A, LANDST
      REAL H, V, V1, F, B, D
      
C     Clear screen and display title
    
      CALL CLRSCR
      WRITE(*,100)
100   FORMAT(/,
     >       '             LUNAR LANDING SIMULATION',/,
     >       '             ----- ------- ----------',/)
      WRITE(*,110)
110   FORMAT(' DO YOU WANT INSTRUCTIONS',/,
     >       ' (Type 0 for NO): ')
      READ(*,*) A
      
      IF (A .NE. 0) THEN
         CALL INSTRS
      END IF
      
C     Main game loop - restart point

200   CONTINUE
      
      WRITE(*,210)
210   FORMAT(//,' BEGINNING LANDING PROCEDURE.........',//,
     >       ' G O O D  L U C K ! ! !',///)
      
C     Initialize variables

      T = 0
      H = 500.0
      V = 50.0
      F = 120.0
      
C     Display header

      WRITE(*,220)
220   FORMAT(' SEC   FEET   SPEED   FUEL',5X,
     >       'PLOT OF DISTANCE',/)
      
C     Game loop - main landing sequence

300   CONTINUE
      
C     Display status with plot

      CALL DSPSTA(T, H, V, F)
      
C     Check if fuel remains

      IF (F .GT. 0.0) THEN
         WRITE(*,310) 
310      FORMAT(' ? ')
         READ(*,*) B
      ELSE
         B = 0.0
      END IF
      
C     Limit fuel burn rate

      IF (B .LT. 0.0) B = 0.0
      IF (B .GT. 30.0) B = 30.0
      IF (B .GT. F) B = F
      
C     Physics calculations

      V1 = V - B + 5.0
      F = F - B
      H = H - 0.5 * (V + V1)
      
C     Check for touchdown

      IF (H .LE. 0.0) THEN
         GOTO 400
      END IF
      
      T = T + 1
      V = V1
      
C     Check for out of fuel

      IF (F .LE. 0.0) THEN
         WRITE(*,320)
320      FORMAT(/,' ***OUT OF FUEL***',/)
         GOTO 350
      END IF
      
      GOTO 300
      
C     Out of fuel loop

350   CONTINUE
      CALL DSPSTA(T, H, V, F)
      B = 0.0
      V1 = V - B + 5.0
      F = F - B
      H = H - 0.5 * (V + V1)
      
      IF (H .LE. 0.0) THEN
         GOTO 400
      END IF
      
      T = T + 1
      V = V1
      GOTO 350
      
C     Touchdown sequence

400   CONTINUE
      
      WRITE(*,410)
410   FORMAT(/,' ****CONTACT****',/)
      
C     Calculate exact touchdown time and velocity

      H = H + 0.5 * (V + V1)
      
      IF (ABS(B - 5.0) .LT. 0.001) THEN
         D = H / V
      ELSE
         D = (-V + SQRT(V*V + H*(10.0 - 2.0*B))) / (5.0 - B)
      END IF
      
      V1 = V + (5.0 - B) * D
      
C     Display landing statistics

      WRITE(*,420) REAL(T) + D, V1, F
420   FORMAT('    TOUCHDOWN AT ',F6.2,' SECONDS.',/,
     >       '    LANDING VELOCITY = ',F6.1,' FT/SEC',/,
     >       '    ',F5.1,' UNITS OF FUEL REMAINING.',/)
      
C     Draw landing scene based on outcome

      LANDST = 0
      IF (ABS(V1) .LT. 0.01) THEN
         LANDST = 1
      ELSE IF (ABS(V1) .LT. 2.0) THEN
         LANDST = 2
      ELSE
         LANDST = 3
      END IF
      
      CALL DRAWLD(LANDST)
      
C     Display landing evaluation

      IF (LANDST .EQ. 1) THEN
         WRITE(*,430)
430      FORMAT(/,' CONGRATULATIONS!!!  A PERFECT LANDING!',/,
     >          ' YOUR LICENSE WILL BE RENEWED.........LATER.',/)
      ELSE IF (LANDST .EQ. 2) THEN
         WRITE(*,440)
440      FORMAT(/,' WELL......THAT WAS OK.  BUT NOT SPECTACULAR...',/)
      ELSE
         WRITE(*,450)
450      FORMAT(/,' ***** SORRY, BUT YOU BLEW IT KIDO!!!!!',/,
     >          ' THIS WAS OUR ONLY CHANCE TO MAKE CONTACT WITH',/,
     >          ' EXTRATERRESTRIAL BEINGS.  APPROPRIATE CONDOLENCES',/,
     >          ' MIGHT BE SENT TO YOUR NEXT OF KIN.',/)
      END IF
      
C     Ask for another mission

      WRITE(*,460)
460   FORMAT(/,' ANOTHER MISSION ANYONE',/,
     >       ' (Type 1 if YES): ')
      READ(*,*) A
      
      IF (A .EQ. 1) THEN
         CALL CLRSCR
         GOTO 200
      END IF
      
      WRITE(*,470)
470   FORMAT(/,' CONTROL OUT.',//)
      
      STOP
      END
      
      
C     ****************************************************************
C     *                    SUBROUTINE INSTRS                         *
C     *              Display game instructions                       *
C     ****************************************************************

      SUBROUTINE INSTRS

      IMPLICIT NONE
      CHARACTER*1 CH
      
      WRITE(*,100)
100   FORMAT(//,
     >       ' YOU ARE LANDING ON THE MOON AND HAVE TAKEN OVER MANUAL',
     >       /,
     >       ' CONTROL 500 FEET ABOVE A GOOD LANDING SPOT. YOU HAVE A',
     >       /,
     >       ' DOWNWARD VELOCITY OF 50 FT/SEC.  120 UNITS OF FUEL ',
     >       'REMAIN.',//,
     >       ' HERE ARE THE RULES THAT GOVERN YOUR SPACE VEHICLE:',/,
     >       ' (1) AFTER EACH SECOND, THE HEIGHT, VELOCITY, AND ',
     >       'REMAINING',/,
     >       '     FUEL WILL BE REPORTED.',/,
     >       ' (2) AFTER THE REPORT, A "?" WILL BE TYPED.  ENTER THE',
     >       /,
     >       '     NUMBER OF UNITS OF FUEL YOU WISH TO BURN DURING THE'
     >       ,/,
     >       '     NEXT SECOND.  EACH UNIT OF FUEL WILL SLOW YOUR ',
     >       'DESCENT',/,
     >       ' (3) THE MAXIMUM THRUST OF YOUR ENGINE IS 30 FT/SEC/SEC ',
     >       'OR',/,
     >       '     30 UNITS OF FUEL PER SECOND',/,
     >       ' (4) WHEN YOU CONTACT THE LUNAR SURFACE, YOUR DESCENT ',
     >       'ENGINE',/,
     >       '     WILL AUTOMATICALLY SHUT OFF AND YOU WILL BE GIVEN A'
     >       ,/,
     >       '     REPORT OF YOUR LANDING SPEED AND REMAINING FUEL',/,
     >       ' (5) IF YOU RUN OUT OF FUEL, THE "?" WILL NO LONGER ',
     >       'APPEAR',/,
     >       '     BUT YOUR SECOND BY SECOND REPORT WILL CONTINUE ',
     >       'UNTIL',/,
     >       '     YOU CONTACT LUNAR SURFACE.',//,
     >       ' HIT <RETURN> TO CONTINUE...')
      
      READ(*,'(A)') CH
      
      RETURN
      END
      
      
C     ****************************************************************
C     *                    SUBROUTINE DSPSTA                         *
C     *       Display status with distance plot indicator            *
C     ****************************************************************
      
      SUBROUTINE DSPSTA(T, H, V, F)
      
      IMPLICIT NONE
      INTEGER T, PLTPOS, I
      REAL H, V, F
      CHARACTER*80 LINE
      
C     Calculate plot position (29 to 80)
      
      PLTPOS = INT(H / 12.0) + 29
      IF (PLTPOS .GT. 80) PLTPOS = 80
      IF (PLTPOS .LT. 29) PLTPOS = 29
      
C     Build output line with plot indicator
      
      WRITE(LINE,100) T, H, V, F
100   FORMAT(I3,F8.0,F8.0,F8.0,'     I')
      
C     Add spaces and asterisk for plot
      
      DO 200 I = 29, PLTPOS-1
         LINE(I:I) = ' '
200   CONTINUE
      
      IF (PLTPOS .LE. 80) THEN
         LINE(PLTPOS:PLTPOS) = '*'
         WRITE(*,'(A)') LINE(1:PLTPOS)
      ELSE
         WRITE(*,'(A)') LINE(1:29)
      END IF
      
      RETURN
      
      END
      
      
C     ****************************************************************
C     *                    SUBROUTINE DRAWLD                         *
C     *    Draw ultra-enhanced lunar lander scene with animation     *
C     ****************************************************************
      
      SUBROUTINE DRAWLD(STATUS)
      
      IMPLICIT NONE
      INTEGER STATUS, I, J, ROW, COL
      CHARACTER*1 SCREEN(40,80)
      
C     Initialize screen with spaces
      
      DO 100 I = 1, 40
         DO 110 J = 1, 80
            SCREEN(I,J) = ' '
110      CONTINUE
100   CONTINUE
      
C     Draw detailed starfield with constellations
      
      SCREEN(2,5) = '*'
      SCREEN(2,6) = '-'
      SCREEN(2,7) = '*'
      SCREEN(3,15) = '.'
      SCREEN(3,16) = ':'
      SCREEN(3,17) = '.'
      SCREEN(1,25) = '*'
      SCREEN(2,26) = '|'
      SCREEN(3,25) = '*'
      SCREEN(4,35) = '.'
      SCREEN(2,50) = '*'
      SCREEN(3,51) = '*'
      SCREEN(4,50) = '*'
      SCREEN(5,55) = '.'
      SCREEN(3,8) = '.'
      SCREEN(1,42) = '*'
      SCREEN(4,60) = '.'
      SCREEN(2,68) = '*'
      SCREEN(5,12) = '.'
      SCREEN(3,48) = '*'
      SCREEN(1,65) = '.'
      SCREEN(4,20) = '*'
      SCREEN(5,38) = '.'
      SCREEN(1,72) = '*'
      SCREEN(2,33) = '.'
      SCREEN(4,76) = '*'
      SCREEN(3,28) = '.'
      SCREEN(5,70) = '.'
      SCREEN(1,10) = '*'
      SCREEN(4,45) = '.'
      
C     Draw Earth in distance with more detail
      
      SCREEN(7,8) = '.'
      SCREEN(7,9) = '-'
      SCREEN(7,10) = '~'
      SCREEN(7,11) = '-'
      SCREEN(7,12) = '.'
      SCREEN(8,7) = '/'
      SCREEN(8,8) = '~'
      SCREEN(8,9) = ' '
      SCREEN(8,10) = 'o'
      SCREEN(8,11) = ' '
      SCREEN(8,12) = '~'
      SCREEN(8,13) = CHAR(92)
      SCREEN(9,7) = '|'
      SCREEN(9,8) = ' '
      SCREEN(9,9) = '~'
      SCREEN(9,10) = ' '
      SCREEN(9,11) = '~'
      SCREEN(9,12) = ' '
      SCREEN(9,13) = '|'
      SCREEN(10,7) = CHAR(92)
      SCREEN(10,8) = '~'
      SCREEN(10,9) = ' '
      SCREEN(10,10) = ' '
      SCREEN(10,11) = ' '
      SCREEN(10,12) = '~'
      SCREEN(10,13) = '/'
      SCREEN(11,8) = '.'
      SCREEN(11,9) = '-'
      SCREEN(11,10) = '~'
      SCREEN(11,11) = '-'
      SCREEN(11,12) = '.'
      
C     Draw moon in background
      
      SCREEN(6,70) = '('
      SCREEN(6,71) = ' '
      SCREEN(6,72) = ')'
      SCREEN(7,69) = '/'
      SCREEN(7,70) = ' '
      SCREEN(7,71) = 'o'
      SCREEN(7,72) = ' '
      SCREEN(7,73) = CHAR(92)
      SCREEN(8,69) = '|'
      SCREEN(8,70) = ' '
      SCREEN(8,71) = ' '
      SCREEN(8,72) = ' '
      SCREEN(8,73) = '|'
      SCREEN(9,69) = CHAR(92)
      SCREEN(9,70) = ' '
      SCREEN(9,71) = ' '
      SCREEN(9,72) = ' '
      SCREEN(9,73) = '/'
      SCREEN(10,70) = '('
      SCREEN(10,71) = '_'
      SCREEN(10,72) = ')'
      
C     Draw distant mountains/craters on horizon
      
      SCREEN(30,10) = '/'
      SCREEN(30,11) = CHAR(92)
      SCREEN(29,11) = '/'
      SCREEN(29,12) = CHAR(92)
      SCREEN(30,25) = '/'
      SCREEN(29,25) = '/'
      SCREEN(29,26) = CHAR(92)
      SCREEN(30,26) = CHAR(92)
      SCREEN(30,52) = '/'
      SCREEN(29,52) = '/'
      SCREEN(29,53) = CHAR(92)
      SCREEN(30,53) = CHAR(92)
      SCREEN(30,68) = '/'
      SCREEN(30,69) = CHAR(92)
      
C     Draw detailed lunar surface
      
      DO 200 J = 1, 80
         SCREEN(34,J) = '='
         SCREEN(35,J) = '#'
         SCREEN(36,J) = '#'
         SCREEN(37,J) = 'X'
         SCREEN(38,J) = 'X'
         SCREEN(39,J) = 'X'
         SCREEN(40,J) = 'X'
200   CONTINUE
      
C     Add detailed craters with shadows
      
      SCREEN(34,12) = 'o'
      SCREEN(33,11) = '/'
      SCREEN(33,12) = 'U'
      SCREEN(33,13) = CHAR(92)
      SCREEN(32,12) = '^'
      
      SCREEN(34,22) = 'O'
      SCREEN(33,20) = '/'
      SCREEN(33,21) = ' '
      SCREEN(33,22) = 'U'
      SCREEN(33,23) = ' '
      SCREEN(33,24) = CHAR(92)
      SCREEN(32,21) = '/'
      SCREEN(32,22) = '^'
      SCREEN(32,23) = CHAR(92)
      
      SCREEN(34,48) = 'O'
      SCREEN(33,46) = '/'
      SCREEN(33,47) = ' '
      SCREEN(33,48) = 'U'
      SCREEN(33,49) = ' '
      SCREEN(33,50) = CHAR(92)
      SCREEN(32,47) = '/'
      SCREEN(32,48) = '^'
      SCREEN(32,49) = CHAR(92)
      
      SCREEN(34,65) = 'o'
      SCREEN(33,65) = '^'
      
C     Add rocks and boulders
      
      SCREEN(33,28) = '^'
      SCREEN(33,29) = '^'
      SCREEN(33,30) = '^'
      SCREEN(34,29) = 'o'
      
      SCREEN(33,58) = '^'
      SCREEN(33,59) = '^'
      SCREEN(34,58) = 'o'
      
      SCREEN(33,72) = '^'
      SCREEN(33,73) = '^'
      SCREEN(33,74) = '^'
      
      IF (STATUS .EQ. 1) THEN

C        Perfect landing - highly detailed intact lander

         ROW = 23
         COL = 38

C        Radio antenna with signal

         SCREEN(ROW-5,COL+2) = '.'
         SCREEN(ROW-4,COL+1) = '.'
         SCREEN(ROW-4,COL+2) = '|'
         SCREEN(ROW-4,COL+3) = '.'
         SCREEN(ROW-3,COL+2) = '|'
         SCREEN(ROW-2,COL+2) = '|'
         SCREEN(ROW-1,COL+2) = '|'

C        Docking port

         SCREEN(ROW,COL+1) = '('
         SCREEN(ROW,COL+2) = 'o'
         SCREEN(ROW,COL+3) = ')'

C        Command module top

         SCREEN(ROW+1,COL) = '/'
         SCREEN(ROW+1,COL+1) = '='
         SCREEN(ROW+1,COL+2) = '='
         SCREEN(ROW+1,COL+3) = '='
         SCREEN(ROW+1,COL+4) = CHAR(92)

C        Window

         SCREEN(ROW+2,COL) = '|'
         SCREEN(ROW+2,COL+1) = '['
         SCREEN(ROW+2,COL+2) = 'o'
         SCREEN(ROW+2,COL+3) = ']'
         SCREEN(ROW+2,COL+4) = '|'

C        USA marking with flag

         SCREEN(ROW+3,COL) = '|'
         SCREEN(ROW+3,COL+1) = 'U'
         SCREEN(ROW+3,COL+2) = 'S'
         SCREEN(ROW+3,COL+3) = 'A'
         SCREEN(ROW+3,COL+4) = '|'

C        Lower module

         SCREEN(ROW+4,COL) = '|'
         SCREEN(ROW+4,COL+1) = '='
         SCREEN(ROW+4,COL+2) = '='
         SCREEN(ROW+4,COL+3) = '='
         SCREEN(ROW+4,COL+4) = '|'

C        Descent stage

         SCREEN(ROW+5,COL-1) = '/'
         SCREEN(ROW+5,COL) = CHAR(92)
         SCREEN(ROW+5,COL+1) = '='
         SCREEN(ROW+5,COL+2) = 'H'
         SCREEN(ROW+5,COL+3) = '='
         SCREEN(ROW+5,COL+4) = '/'
         SCREEN(ROW+5,COL+5) = CHAR(92)

C        Engine nozzle

         SCREEN(ROW+6,COL+1) = '('
         SCREEN(ROW+6,COL+2) = 'V'
         SCREEN(ROW+6,COL+3) = ')'

C        Landing legs with shock absorbers

         SCREEN(ROW+7,COL-2) = '/'
         SCREEN(ROW+7,COL-1) = '|'
         SCREEN(ROW+7,COL) = '|'
         SCREEN(ROW+7,COL+1) = ' '
         SCREEN(ROW+7,COL+2) = ' '
         SCREEN(ROW+7,COL+3) = ' '
         SCREEN(ROW+7,COL+4) = '|'
         SCREEN(ROW+7,COL+5) = '|'
         SCREEN(ROW+7,COL+6) = CHAR(92)
         SCREEN(ROW+8,COL-3) = '/'
         SCREEN(ROW+8,COL-2) = '='
         SCREEN(ROW+8,COL-1) = '='
         SCREEN(ROW+8,COL) = '='
         SCREEN(ROW+8,COL+4) = '='
         SCREEN(ROW+8,COL+5) = '='
         SCREEN(ROW+8,COL+6) = '='
         SCREEN(ROW+8,COL+7) = CHAR(92)

C        Footpads with dust clouds

         SCREEN(ROW+9,COL-4) = '['
         SCREEN(ROW+9,COL-3) = '='
         SCREEN(ROW+9,COL-2) = '='
         SCREEN(ROW+9,COL-1) = ']'
         SCREEN(ROW+9,COL+5) = '['
         SCREEN(ROW+9,COL+6) = '='
         SCREEN(ROW+9,COL+7) = '='
         SCREEN(ROW+9,COL+8) = ']'
         SCREEN(ROW+10,COL-5) = '.'
         SCREEN(ROW+10,COL-4) = ':'
         SCREEN(ROW+10,COL-3) = '.'
         SCREEN(ROW+10,COL+6) = '.'
         SCREEN(ROW+10,COL+7) = ':'
         SCREEN(ROW+10,COL+8) = '.'

C        Success banner

         SCREEN(ROW-8,COL-12) = '*'
         SCREEN(ROW-8,COL-11) = '*'
         SCREEN(ROW-8,COL-10) = '*'
         SCREEN(ROW-8,COL-9) = ' '
         SCREEN(ROW-8,COL-8) = 'P'
         SCREEN(ROW-8,COL-7) = 'E'
         SCREEN(ROW-8,COL-6) = 'R'
         SCREEN(ROW-8,COL-5) = 'F'
         SCREEN(ROW-8,COL-4) = 'E'
         SCREEN(ROW-8,COL-3) = 'C'
         SCREEN(ROW-8,COL-2) = 'T'
         SCREEN(ROW-8,COL-1) = '!'
         SCREEN(ROW-8,COL) = ' '
         SCREEN(ROW-8,COL+1) = '*'
         SCREEN(ROW-8,COL+2) = '*'
         SCREEN(ROW-8,COL+3) = '*'

C        Flag on surface

         SCREEN(ROW+7,COL+10) = '|'
         SCREEN(ROW+8,COL+10) = '|'
         SCREEN(ROW+9,COL+10) = '|'
         SCREEN(ROW+7,COL+11) = '='
         SCREEN(ROW+7,COL+12) = '='
         SCREEN(ROW+8,COL+11) = '='
         SCREEN(ROW+8,COL+12) = '='
         
      ELSE IF (STATUS .EQ. 2) THEN

C        OK landing - damaged but functional lander

         ROW = 25
         COL = 37

C        Bent antenna

         SCREEN(ROW-4,COL+2) = '.'
         SCREEN(ROW-3,COL+2) = '/'
         SCREEN(ROW-2,COL+3) = '/'
         SCREEN(ROW-1,COL+3) = '|'

C        Cracked docking port

         SCREEN(ROW,COL+1) = '('
         SCREEN(ROW,COL+2) = 'x'
         SCREEN(ROW,COL+3) = ')'

C        Tilted command module

         SCREEN(ROW+1,COL+1) = '/'
         SCREEN(ROW+1,COL+2) = '='
         SCREEN(ROW+1,COL+3) = '='
         SCREEN(ROW+1,COL+4) = CHAR(92)

C        Cracked window

         SCREEN(ROW+2,COL) = '/'
         SCREEN(ROW+2,COL+1) = '['
         SCREEN(ROW+2,COL+2) = '*'
         SCREEN(ROW+2,COL+3) = ']'
         SCREEN(ROW+2,COL+4) = '|'

C        USA marking

         SCREEN(ROW+3,COL) = '|'
         SCREEN(ROW+3,COL+1) = 'U'
         SCREEN(ROW+3,COL+2) = 'S'
         SCREEN(ROW+3,COL+3) = 'A'
         SCREEN(ROW+3,COL+4) = '|'

C        Damaged descent stage

         SCREEN(ROW+4,COL) = CHAR(92)
         SCREEN(ROW+4,COL+1) = '='
         SCREEN(ROW+4,COL+2) = '~'
         SCREEN(ROW+4,COL+3) = '='
         SCREEN(ROW+4,COL+4) = '/'

C        Bent engine

         SCREEN(ROW+5,COL+1) = '('
         SCREEN(ROW+5,COL+2) = '/'
         SCREEN(ROW+5,COL+3) = ')'

C        Broken legs

         SCREEN(ROW+6,COL-1) = '/'
         SCREEN(ROW+6,COL) = '|'
         SCREEN(ROW+6,COL+1) = '~'
         SCREEN(ROW+6,COL+3) = '|'
         SCREEN(ROW+6,COL+4) = CHAR(92)
         SCREEN(ROW+7,COL-2) = '/'
         SCREEN(ROW+7,COL-1) = '='
         SCREEN(ROW+7,COL) = '='
         SCREEN(ROW+7,COL+4) = '='
         SCREEN(ROW+7,COL+5) = '='

C        Uneven footpads

         SCREEN(ROW+8,COL+4) = '['
         SCREEN(ROW+8,COL+5) = '='
         SCREEN(ROW+8,COL+6) = ']'

C        Dust and small debris

         SCREEN(ROW+9,COL-3) = '.'
         SCREEN(ROW+9,COL+6) = '.'
         SCREEN(ROW+9,COL+7) = ':'
         SCREEN(ROW+7,COL-4) = '-'
         SCREEN(ROW+8,COL+8) = 'o'
         
      ELSE

C        Crash landing - catastrophic destruction

         ROW = 26
         COL = 40

C        Large explosion cloud

         SCREEN(ROW-10,COL-4) = '.'
         SCREEN(ROW-10,COL-3) = '*'
         SCREEN(ROW-10,COL-2) = '.'
         SCREEN(ROW-10,COL-1) = '*'
         SCREEN(ROW-10,COL) = '.'
         SCREEN(ROW-10,COL+1) = '*'
         SCREEN(ROW-10,COL+2) = '.'
         SCREEN(ROW-10,COL+3) = '*'
         SCREEN(ROW-10,COL+4) = '.'
         SCREEN(ROW-9,COL-5) = '*'
         SCREEN(ROW-9,COL-4) = '.'
         SCREEN(ROW-9,COL-3) = ' '
         SCREEN(ROW-9,COL-2) = '*'
         SCREEN(ROW-9,COL-1) = ' '
         SCREEN(ROW-9,COL) = '.'
         SCREEN(ROW-9,COL+1) = ' '
         SCREEN(ROW-9,COL+2) = '*'
         SCREEN(ROW-9,COL+3) = ' '
         SCREEN(ROW-9,COL+4) = '.'
         SCREEN(ROW-9,COL+5) = '*'
         SCREEN(ROW-8,COL-6) = '.'
         SCREEN(ROW-8,COL-5) = '*'
         SCREEN(ROW-8,COL-4) = '.'
         SCREEN(ROW-8,COL-3) = ' '
         SCREEN(ROW-8,COL-2) = 'B'
         SCREEN(ROW-8,COL-1) = 'O'
         SCREEN(ROW-8,COL) = 'O'
         SCREEN(ROW-8,COL+1) = 'M'
         SCREEN(ROW-8,COL+2) = '!'
         SCREEN(ROW-8,COL+3) = ' '
         SCREEN(ROW-8,COL+4) = '.'
         SCREEN(ROW-8,COL+5) = '*'
         SCREEN(ROW-8,COL+6) = '.'
         SCREEN(ROW-7,COL-6) = '*'
         SCREEN(ROW-7,COL-5) = ' '
         SCREEN(ROW-7,COL-4) = '.'
         SCREEN(ROW-7,COL-3) = '*'
         SCREEN(ROW-7,COL-2) = ' '
         SCREEN(ROW-7,COL-1) = '.'
         SCREEN(ROW-7,COL) = ' '
         SCREEN(ROW-7,COL+1) = '*'
         SCREEN(ROW-7,COL+2) = ' '
         SCREEN(ROW-7,COL+3) = '.'
         SCREEN(ROW-7,COL+4) = '*'
         SCREEN(ROW-7,COL+5) = ' '
         SCREEN(ROW-7,COL+6) = '*'
         SCREEN(ROW-6,COL-5) = '.'
         SCREEN(ROW-6,COL-4) = '*'
         SCREEN(ROW-6,COL-3) = '.'
         SCREEN(ROW-6,COL-2) = ' '
         SCREEN(ROW-6,COL-1) = '*'
         SCREEN(ROW-6,COL) = '.'
         SCREEN(ROW-6,COL+1) = ' '
         SCREEN(ROW-6,COL+2) = '*'
         SCREEN(ROW-6,COL+3) = '.'
         SCREEN(ROW-6,COL+4) = '*'
         SCREEN(ROW-6,COL+5) = '.'

C        Fire and smoke

         SCREEN(ROW-5,COL-3) = '~'
         SCREEN(ROW-5,COL-2) = '}'
         SCREEN(ROW-5,COL-1) = '{'
         SCREEN(ROW-5,COL) = '~'
         SCREEN(ROW-5,COL+1) = '}'
         SCREEN(ROW-5,COL+2) = '{'
         SCREEN(ROW-5,COL+3) = '~'
         SCREEN(ROW-4,COL-2) = '}'
         SCREEN(ROW-4,COL-1) = '~'
         SCREEN(ROW-4,COL) = '{'
         SCREEN(ROW-4,COL+1) = '~'
         SCREEN(ROW-4,COL+2) = '}'

C        Main wreckage (detailed)

         SCREEN(ROW-2,COL-3) = CHAR(92)
         SCREEN(ROW-2,COL-2) = '|'
         SCREEN(ROW-2,COL-1) = '/'
         SCREEN(ROW-2,COL) = '*'
         SCREEN(ROW-2,COL+1) = CHAR(92)
         SCREEN(ROW-2,COL+2) = '|'
         SCREEN(ROW-2,COL+3) = '/'
         SCREEN(ROW-1,COL-4) = '/'
         SCREEN(ROW-1,COL-3) = '-'
         SCREEN(ROW-1,COL-2) = 'X'
         SCREEN(ROW-1,COL-1) = 'X'
         SCREEN(ROW-1,COL) = 'X'
         SCREEN(ROW-1,COL+1) = 'X'
         SCREEN(ROW-1,COL+2) = 'X'
         SCREEN(ROW-1,COL+3) = '-'
         SCREEN(ROW-1,COL+4) = CHAR(92)
         SCREEN(ROW,COL-4) = '|'
         SCREEN(ROW,COL-3) = 'X'
         SCREEN(ROW,COL-2) = 'X'
         SCREEN(ROW,COL-1) = 'X'
         SCREEN(ROW,COL) = 'X'
         SCREEN(ROW,COL+1) = 'X'
         SCREEN(ROW,COL+2) = 'X'
         SCREEN(ROW,COL+3) = 'X'
         SCREEN(ROW,COL+4) = '|'
         SCREEN(ROW+1,COL-3) = CHAR(92)
         SCREEN(ROW+1,COL-2) = '-'
         SCREEN(ROW+1,COL-1) = 'X'
         SCREEN(ROW+1,COL) = '*'
         SCREEN(ROW+1,COL+1) = '|'
         SCREEN(ROW+1,COL+2) = 'X'
         SCREEN(ROW+1,COL+3) = '-'
         SCREEN(ROW+1,COL+4) = '/'
         SCREEN(ROW+2,COL-2) = '/'
         SCREEN(ROW+2,COL-1) = '*'
         SCREEN(ROW+2,COL) = '|'
         SCREEN(ROW+2,COL+1) = CHAR(92)
         SCREEN(ROW+2,COL+2) = '*'

C        Large debris field

         SCREEN(ROW-3,COL+7) = '/'
         SCREEN(ROW-3,COL+8) = '-'
         SCREEN(ROW-2,COL+8) = '|'
         SCREEN(ROW-2,COL-7) = CHAR(92)
         SCREEN(ROW-2,COL-8) = '-'
         SCREEN(ROW-1,COL-8) = '|'
         SCREEN(ROW-1,COL+9) = CHAR(92)
         SCREEN(ROW,COL-9) = '='
         SCREEN(ROW,COL-8) = '='
         SCREEN(ROW,COL+10) = '/'
         SCREEN(ROW+1,COL-10) = '|'
         SCREEN(ROW+1,COL+10) = '|'
         SCREEN(ROW+2,COL+9) = '-'
         SCREEN(ROW+2,COL+10) = '='
         SCREEN(ROW+2,COL-9) = '='
         SCREEN(ROW+2,COL-10) = '-'

C        Scattered small debris

         SCREEN(ROW-4,COL+6) = 'o'
         SCREEN(ROW-3,COL-6) = '*'
         SCREEN(ROW-2,COL+11) = '.'
         SCREEN(ROW-1,COL-11) = '.'
         SCREEN(ROW,COL+12) = 'o'
         SCREEN(ROW,COL-12) = 'o'
         SCREEN(ROW+1,COL+11) = '*'
         SCREEN(ROW+1,COL-11) = '*'
         SCREEN(ROW+2,COL-12) = '.'
         SCREEN(ROW+3,COL-10) = ':'
         SCREEN(ROW+3,COL+11) = ':'

C        Broken parts with labels

         SCREEN(ROW+3,COL-7) = '['
         SCREEN(ROW+3,COL-6) = 'U'
         SCREEN(ROW+3,COL-5) = 'S'
         SCREEN(ROW+3,COL-4) = ']'
         SCREEN(ROW+3,COL+5) = '['
         SCREEN(ROW+3,COL+6) = 'A'
         SCREEN(ROW+3,COL+7) = ']'
         SCREEN(ROW+4,COL-5) = '('
         SCREEN(ROW+4,COL-4) = 'V'
         SCREEN(ROW+4,COL-3) = ')'
         SCREEN(ROW+4,COL+4) = '='
         SCREEN(ROW+4,COL+5) = '='

C        Impact crater (large and detailed)

         SCREEN(ROW+3,COL-2) = '.'
         SCREEN(ROW+3,COL-1) = ':'
         SCREEN(ROW+3,COL) = ':'
         SCREEN(ROW+3,COL+1) = ':'
         SCREEN(ROW+3,COL+2) = '.'
         SCREEN(ROW+4,COL-3) = '('
         SCREEN(ROW+4,COL-2) = '-'
         SCREEN(ROW+4,COL-1) = '-'
         SCREEN(ROW+4,COL) = '-'
         SCREEN(ROW+4,COL+1) = '-'
         SCREEN(ROW+4,COL+2) = '-'
         SCREEN(ROW+4,COL+3) = ')'
         SCREEN(ROW+5,COL-4) = '('
         SCREEN(ROW+5,COL-3) = '~'
         SCREEN(ROW+5,COL-2) = '~'
         SCREEN(ROW+5,COL-1) = '~'
         SCREEN(ROW+5,COL) = '~'
         SCREEN(ROW+5,COL+1) = '~'
         SCREEN(ROW+5,COL+2) = '~'
         SCREEN(ROW+5,COL+3) = '~'
         SCREEN(ROW+5,COL+4) = ')'
         SCREEN(ROW+6,COL-3) = '('
         SCREEN(ROW+6,COL-2) = '='
         SCREEN(ROW+6,COL-1) = '='
         SCREEN(ROW+6,COL) = '='
         SCREEN(ROW+6,COL+1) = '='
         SCREEN(ROW+6,COL+2) = '='
         SCREEN(ROW+6,COL+3) = ')'

C        Ejected rocks and lunar material

         SCREEN(ROW+5,COL-8) = 'o'
         SCREEN(ROW+5,COL+7) = 'o'
         SCREEN(ROW+6,COL-7) = '^'
         SCREEN(ROW+6,COL+8) = '^'
         SCREEN(ROW+7,COL-9) = '.'
         SCREEN(ROW+7,COL+9) = '.'
         
      END IF
      
C     Display the screen

      DO 300 I = 1, 40
         WRITE(*,'(80A1)') (SCREEN(I,J), J=1,80)
300   CONTINUE
      
      RETURN
      
      END
      
      
C     ****************************************************************
C     *                    SUBROUTINE CLRSCR                         *
C     *                  Clear screen display                        *
C     ****************************************************************
      
      SUBROUTINE CLRSCR
      
      IMPLICIT NONE
      INTEGER I
      
C     Print blank lines to clear screen
      
      DO 100 I = 1, 6
         WRITE(*,*)
100   CONTINUE
      
      RETURN
     
      END